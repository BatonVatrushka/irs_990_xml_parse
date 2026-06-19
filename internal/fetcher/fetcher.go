// Package fetcher handles all HTTP I/O against the IRS apps.irs.gov host.
//
// As of late 2021, the IRS moved 990 e-file data off AWS S3. The new layout:
//
//	Index: https://apps.irs.gov/pub/epostcard/990/xml/{YEAR}/index_{YEAR}.csv
//	XMLs:  only available inside bulk ZIP archives at the same base path
//
// ZIP naming conventions vary by year:
//
//	2016-2020:  download990xml_{YEAR}_{1-N}.zip   (old format, still live)
//	2021:       2021_TEOS_XML_01A.zip              (single ~3.7 GB annual file)
//	2022:       2022_TEOS_XML_01A.zip (2.6 GB) +
//	            2022_TEOS_XML_02A.zip (1.4 GB)     (two annual files)
//	2023+:      {YEAR}_TEOS_XML_{MM}A/B.zip        (monthly; some months have A+B)
//
// The 2021 and 2022 TEOS files are ZIP64 archives with >65 535 entries.
// Go's standard archive/zip handles them correctly on well-formed files; if it
// can't open one, ProcessZIP falls back to the system `unzip` binary.
package fetcher

import (
	stdbzip2 "compress/bzip2"
	"context"
	"encoding/binary"
	"encoding/csv"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	kzip "github.com/klauspost/compress/zip"
	"github.com/klauspost/compress/zstd"

	"github.com/BatonVatrushka/irs_990_xml_parse/internal/models"
)

// init registers decompressors for ZIP compression methods beyond the default
// Store (0) and Deflate (8). These cover the algorithms the IRS has been
// observed to use in TEOS bulk ZIPs as of 2025.
//
// If you see "open zip entry failed … method=N" in the logs for a method not
// listed here, add the corresponding decompressor and re-run.
func init() {
	// Method 12: bzip2 — standard library, zero new dependency.
	kzip.RegisterDecompressor(12, func(r io.Reader) io.ReadCloser {
		return io.NopCloser(stdbzip2.NewReader(r))
	})

	// Method 93: Zstandard — klauspost/compress/zstd is already a direct dep.
	kzip.RegisterDecompressor(93, func(r io.Reader) io.ReadCloser {
		d, err := zstd.NewReader(r)
		if err != nil {
			return io.NopCloser(strings.NewReader(""))
		}
		return zstdReadCloser{d}
	})
}

// zstdReadCloser wraps *zstd.Decoder to satisfy io.ReadCloser.
// zstd.Decoder.Close() does not return an error, so we need the adapter.
type zstdReadCloser struct{ *zstd.Decoder }

func (z zstdReadCloser) Close() error {
	z.Decoder.Close()
	return nil
}

const defaultBaseURL = "https://apps.irs.gov/pub/epostcard/990/xml"

// Fetcher handles all HTTP communication with the IRS data host.
type Fetcher struct {
	client  *http.Client
	baseURL string
}

func New() *Fetcher {
	return &Fetcher{
		// Long timeout — ZIP files can be 2-3 GB.
		client:  &http.Client{Timeout: 4 * time.Hour},
		baseURL: defaultBaseURL,
	}
}

// newForTest creates a Fetcher pointed at a custom base URL (used in tests).
func newForTest(baseURL string) *Fetcher {
	return &Fetcher{
		client:  &http.Client{Timeout: 30 * time.Second},
		baseURL: baseURL,
	}
}

// FetchIndex downloads and parses the CSV filing index for a given year.
func (f *Fetcher) FetchIndex(ctx context.Context, year int) ([]models.IndexEntry, error) {
	url := fmt.Sprintf("%s/%d/index_%d.csv", f.baseURL, year, year)
	body, err := f.get(ctx, url)
	if err != nil {
		return nil, fmt.Errorf("fetch index %d: %w", year, err)
	}
	defer body.Close()

	r := csv.NewReader(body)
	r.LazyQuotes = true
	r.TrimLeadingSpace = true

	headers, err := r.Read()
	if err != nil {
		return nil, fmt.Errorf("read header: %w", err)
	}
	idx := colIndex(headers)

	var entries []models.IndexEntry
	for {
		row, err := r.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			continue
		}
		entries = append(entries, models.IndexEntry{
			ReturnID:    col(row, idx, "RETURN_ID"),
			FilingType:  col(row, idx, "FILING_TYPE"),
			EIN:         col(row, idx, "EIN"),
			TaxPeriod:   col(row, idx, "TAX_PERIOD"),
			SubmittedOn: col(row, idx, "SUB_DATE"),
			CompanyName: col(row, idx, "TAXPAYER_NAME"),
			FormType:    col(row, idx, "RETURN_TYPE"),
			DLN:         col(row, idx, "DLN"),
			ObjectID:    col(row, idx, "OBJECT_ID"),
		})
	}
	return entries, nil
}

// XMLHandler is called for each matching XML file found inside a ZIP.
type XMLHandler func(objectID string, data []byte) error

// ProcessZIP downloads a ZIP archive to a temp file, then streams through it
// calling handler for every XML whose name matches an ID in targetIDs.
// Returns nil (not an error) if the URL 404s — callers should try all candidate
// URLs and let missing ones pass silently.
// Retries up to 3 times on mid-download failures (unexpected EOF, connection reset).
//
// If Go's archive/zip cannot open the file (e.g. some ZIP64 variants), the
// function automatically falls back to the system `unzip` binary.
func (f *Fetcher) ProcessZIP(ctx context.Context, url string, targetIDs map[string]struct{}, handler XMLHandler) error {
	slog.Info("downloading ZIP", "url", url)

	// Create temp file upfront — reused across retry attempts.
	tmp, err := os.CreateTemp("", "irs990-*.zip")
	if err != nil {
		return fmt.Errorf("create temp file: %w", err)
	}
	defer os.Remove(tmp.Name())
	defer tmp.Close()

	var written int64
	for attempt := range 3 {
		if attempt > 0 {
			slog.Warn("retrying ZIP download", "url", url, "attempt", attempt+1)
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(time.Duration(attempt*5) * time.Second):
			}
			if err := tmp.Truncate(0); err != nil {
				return err
			}
			if _, err := tmp.Seek(0, io.SeekStart); err != nil {
				return err
			}
		}

		body, err := f.get(ctx, url)
		if err != nil {
			if is404(err) {
				slog.Debug("ZIP not found, skipping", "url", url)
				return nil
			}
			if attempt < 2 {
				continue
			}
			return fmt.Errorf("download %s: %w", url, err)
		}

		written, err = io.Copy(tmp, body)
		body.Close()
		if err == nil {
			break
		}
		if attempt == 2 {
			return fmt.Errorf("write temp file: %w", err)
		}
	}
	slog.Info("ZIP downloaded", "url", url, "bytes", written)

	// Try native Go ZIP reader first.
	zr, err := kzip.OpenReader(tmp.Name())
	if err != nil {
		// The IRS TEOS bulk files (2021+) have a ZIP64 end-of-central-directory
		// locator where total_disks=0 instead of the required 1. Go's archive/zip
		// and old unzip both silently skip the ZIP64 EOCD when they see this,
		// then fall back to the regular EOCD whose values overflow the file size,
		// producing "zip: not a valid zip file". A 4-byte in-place patch fixes it.
		if patched, patchErr := patchZIP64TotalDisks(tmp, url); patchErr == nil && patched {
			zr, err = kzip.OpenReader(tmp.Name())
		}
	}
	if err != nil {
		// Still failing — fall back to the system `unzip` binary.
		slog.Warn("native zip reader failed — falling back to unzip subprocess",
			"url", url, "err", err)
		return f.processZIPUnzip(ctx, tmp.Name(), targetIDs, handler)
	}
	defer zr.Close()

	for _, zf := range zr.File {
		name := filepath.Base(zf.Name)
		objectID := strings.TrimSuffix(name, "_public.xml")
		if objectID == name { // no suffix stripped = not an XML we want
			continue
		}
		if _, ok := targetIDs[objectID]; !ok {
			continue
		}

		rc, err := zf.Open()
		if err != nil {
			// method= tells us exactly which compression ID to add a decompressor for.
			slog.Warn("open zip entry failed", "file", zf.Name, "method", zf.Method, "err", err)
			continue
		}
		data, err := io.ReadAll(rc)
		rc.Close()
		if err != nil {
			slog.Warn("read zip entry failed", "file", zf.Name, "err", err)
			continue
		}

		if err := handler(objectID, data); err != nil {
			return err
		}
	}
	return nil
}

// patchZIP64TotalDisks repairs a specific bug present in IRS TEOS bulk ZIP files
// (2021 onwards): the ZIP64 end-of-central-directory locator has total_disks=0
// instead of the required 1. Both Go's archive/zip and older unzip versions
// check this field and silently abandon ZIP64 parsing when it is 0, causing them
// to use the regular EOCD (which holds only sentinel/overflow values for ZIP64
// archives) and ultimately report "not a valid zip file".
//
// The function locates the ZIP64 locator immediately before the EOCD (the common
// no-comment layout all IRS bulk files use), verifies the bad field, and writes
// the corrected 4 bytes in place. Returns (true, nil) when a patch was applied.
func patchZIP64TotalDisks(f *os.File, url string) (bool, error) {
	fi, err := f.Stat()
	if err != nil {
		return false, err
	}
	size := fi.Size()

	// Fixed-size layout at end of file (no comment):
	//   [20 bytes: ZIP64 locator] [22 bytes: EOCD]
	const (
		eocdLen  = 22
		loc64Len = 20
	)
	if size < int64(eocdLen+loc64Len) {
		return false, nil
	}

	buf := make([]byte, eocdLen+loc64Len)
	if _, err := f.ReadAt(buf, size-int64(eocdLen+loc64Len)); err != nil {
		return false, err
	}

	// Confirm signatures.
	if string(buf[20:24]) != "PK\x05\x06" {
		return false, nil // EOCD not at expected offset (file has a comment)
	}
	if string(buf[0:4]) != "PK\x06\x07" {
		return false, nil // no ZIP64 locator immediately before EOCD
	}

	// ZIP64 locator layout:
	//   0-3:   PK\x06\x07 signature
	//   4-7:   disk number where ZIP64 EOCD starts
	//   8-15:  offset of ZIP64 EOCD record
	//   16-19: total number of disks  ← must be 1 for single-file archives
	totalDisks := binary.LittleEndian.Uint32(buf[16:20])
	if totalDisks == 1 {
		return false, nil // already valid, nothing to do
	}
	if totalDisks != 0 {
		return false, nil // unexpected value; don't touch it
	}

	patchOffset := size - int64(eocdLen+loc64Len) + 16
	if _, err := f.WriteAt([]byte{1, 0, 0, 0}, patchOffset); err != nil {
		return false, fmt.Errorf("patch ZIP64 locator: %w", err)
	}
	slog.Info("patched ZIP64 locator total_disks 0→1", "url", url)
	return true, nil
}

// processZIPUnzip is a fallback for ZIP files that Go's archive/zip cannot open.
// It uses the system `unzip` binary to list entries and extract matching files.
// Requires `unzip` to be installed (standard on macOS; `apt install unzip` on Debian/Ubuntu).
func (f *Fetcher) processZIPUnzip(ctx context.Context, tmpName string, targetIDs map[string]struct{}, handler XMLHandler) error {
	// Verify unzip is available before doing anything else.
	if _, err := exec.LookPath("unzip"); err != nil {
		return fmt.Errorf("unzip binary not found — install it or check the ZIP manually: %w", err)
	}

	// -Z1: zipinfo one-column mode — one filename per line, no header/footer.
	listOut, err := exec.CommandContext(ctx, "unzip", "-Z1", tmpName).Output()
	if err != nil {
		return fmt.Errorf("unzip -Z1 list failed: %w", err)
	}

	lines := strings.Split(strings.TrimSpace(string(listOut)), "\n")
	slog.Info("unzip fallback: listed entries", "count", len(lines))

	for _, entry := range lines {
		entry = strings.TrimSpace(entry)
		if entry == "" {
			continue
		}
		name := filepath.Base(entry)
		objectID := strings.TrimSuffix(name, "_public.xml")
		if objectID == name { // not a _public.xml file
			continue
		}
		if _, ok := targetIDs[objectID]; !ok {
			continue
		}

		// -p: extract to stdout (no path printed, just raw bytes).
		data, err := exec.CommandContext(ctx, "unzip", "-p", tmpName, entry).Output()
		if err != nil {
			slog.Warn("unzip -p extract failed", "entry", entry, "err", err)
			continue
		}

		if err := handler(objectID, data); err != nil {
			return err
		}
	}
	return nil
}

// ZIPURLsForYear returns all candidate ZIP URLs for a given year.
// The IRS naming scheme changed several times; we try all known patterns and
// rely on ProcessZIP to silently skip 404s.
//
// Confirmed patterns (probed May 2026):
//
//	2016–2020:  download990xml_{YEAR}_{1-10}.zip (old format, still live)
//	2021:       2021_TEOS_XML_01A.zip            (~3.7 GB, single ZIP64 file)
//	2022:       2022_TEOS_XML_01A.zip (2.6 GB)
//	            2022_TEOS_XML_02A.zip (1.4 GB)   (two annual ZIP64 files)
//	2023+:      {YEAR}_TEOS_XML_{MM}A/B.zip      (monthly, A+B variants)
func (f *Fetcher) ZIPURLsForYear(year int) []string {
	base := fmt.Sprintf("%s/%d", f.baseURL, year)
	var urls []string

	switch {
	case year >= 2023:
		// Monthly files, A and B variants (B exists for high-volume months).
		for m := 1; m <= 12; m++ {
			urls = append(urls,
				fmt.Sprintf("%s/%d_TEOS_XML_%02dA.zip", base, year, m),
				fmt.Sprintf("%s/%d_TEOS_XML_%02dB.zip", base, year, m),
			)
		}
	case year == 2022:
		// Two annual TEOS ZIP64 files confirmed live (probed May 2026).
		urls = append(urls,
			fmt.Sprintf("%s/%d_TEOS_XML_01A.zip", base, year),
			fmt.Sprintf("%s/%d_TEOS_XML_02A.zip", base, year),
		)
	case year == 2021:
		// Single annual TEOS ZIP64 file (~3.7 GB).
		// The old download990xml_2021_N.zip URLs return 404.
		urls = append(urls, fmt.Sprintf("%s/%d_TEOS_XML_01A.zip", base, year))
	default: // 2016–2020 — old download990xml format, up to 10 numbered files.
		for n := 1; n <= 10; n++ {
			urls = append(urls, fmt.Sprintf("%s/download990xml_%d_%d.zip", base, year, n))
		}
	}
	return urls
}

// get performs an HTTP GET with up to 3 retries and exponential backoff.
func (f *Fetcher) get(ctx context.Context, url string) (io.ReadCloser, error) {
	var lastErr error
	for attempt := range 3 {
		if attempt > 0 {
			select {
			case <-ctx.Done():
				return nil, ctx.Err()
			case <-time.After(time.Duration(attempt*2) * time.Second):
			}
		}

		req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
		if err != nil {
			return nil, err
		}

		resp, err := f.client.Do(req)
		if err != nil {
			lastErr = err
			continue
		}

		if resp.StatusCode == http.StatusNotFound {
			resp.Body.Close()
			return nil, fmt.Errorf("404: %s", url)
		}
		if resp.StatusCode != http.StatusOK {
			resp.Body.Close()
			lastErr = fmt.Errorf("status %d: %s", resp.StatusCode, url)
			continue
		}

		return resp.Body, nil
	}
	return nil, lastErr
}

func colIndex(headers []string) map[string]int {
	idx := make(map[string]int, len(headers))
	for i, h := range headers {
		idx[strings.ToUpper(strings.TrimSpace(h))] = i
	}
	return idx
}

func col(row []string, idx map[string]int, name string) string {
	i, ok := idx[name]
	if !ok || i >= len(row) {
		return ""
	}
	return strings.TrimSpace(row[i])
}

func is404(err error) bool {
	return err != nil && strings.Contains(err.Error(), "404")
}
