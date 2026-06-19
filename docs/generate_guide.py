#!/usr/bin/env python3
"""Generate the R to Go guide PDF using reportlab."""

import os
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle,
    HRFlowable, KeepTogether, PageBreak
)
from reportlab.lib.enums import TA_LEFT, TA_CENTER

OUTPUT_PATH = "/Users/baton/GitHub/irs_990_xml_parse/docs/r_to_go_guide.pdf"

# ── Color palette ──────────────────────────────────────────────────────────────
DARK_NAVY    = colors.HexColor("#1a2332")
MID_BLUE     = colors.HexColor("#2563eb")
LIGHT_BLUE   = colors.HexColor("#dbeafe")
CODE_BG      = colors.HexColor("#f1f5f9")
CODE_BORDER  = colors.HexColor("#cbd5e1")
HEADING_TEXT = colors.white
SUBHEAD_COL  = colors.HexColor("#1e40af")
BODY_TEXT    = colors.HexColor("#1e293b")
MUTED_TEXT   = colors.HexColor("#475569")
ACCENT       = colors.HexColor("#0ea5e9")
RULE_COL     = colors.HexColor("#93c5fd")
LABEL_R      = colors.HexColor("#dc2626")   # R label — red
LABEL_GO     = colors.HexColor("#16a34a")   # Go label — green
LABEL_R_BG   = colors.HexColor("#fef2f2")
LABEL_GO_BG  = colors.HexColor("#f0fdf4")
GLOSSARY_BG  = colors.HexColor("#f8fafc")

PAGE_W, PAGE_H = letter
LEFT_M = RIGHT_M = 0.75 * inch
TOP_M  = 0.6 * inch
BOT_M  = 0.65 * inch

# ── Style helpers ──────────────────────────────────────────────────────────────
def make_styles():
    base = getSampleStyleSheet()

    title_style = ParagraphStyle(
        "GuideTitle",
        fontName="Helvetica-Bold",
        fontSize=22,
        leading=28,
        textColor=HEADING_TEXT,
        alignment=TA_LEFT,
        spaceAfter=4,
    )
    subtitle_style = ParagraphStyle(
        "GuideSubtitle",
        fontName="Helvetica",
        fontSize=11,
        leading=16,
        textColor=colors.HexColor("#bfdbfe"),
        alignment=TA_LEFT,
        spaceAfter=0,
    )
    section_num = ParagraphStyle(
        "SectionNum",
        fontName="Helvetica-Bold",
        fontSize=10,
        leading=14,
        textColor=ACCENT,
        spaceBefore=18,
        spaceAfter=2,
    )
    section_title = ParagraphStyle(
        "SectionTitle",
        fontName="Helvetica-Bold",
        fontSize=15,
        leading=20,
        textColor=DARK_NAVY,
        spaceBefore=0,
        spaceAfter=6,
    )
    body = ParagraphStyle(
        "GuideBody",
        fontName="Helvetica",
        fontSize=9.5,
        leading=14.5,
        textColor=BODY_TEXT,
        spaceBefore=4,
        spaceAfter=4,
    )
    bullet = ParagraphStyle(
        "GuideBullet",
        fontName="Helvetica",
        fontSize=9.5,
        leading=14,
        textColor=BODY_TEXT,
        leftIndent=14,
        bulletIndent=0,
        spaceBefore=2,
        spaceAfter=2,
    )
    code_inline = ParagraphStyle(
        "CodeInline",
        fontName="Courier",
        fontSize=8.5,
        leading=13,
        textColor=BODY_TEXT,
        spaceBefore=0,
        spaceAfter=0,
    )
    label_r = ParagraphStyle(
        "LabelR",
        fontName="Helvetica-Bold",
        fontSize=8,
        leading=11,
        textColor=LABEL_R,
        spaceBefore=0,
        spaceAfter=2,
    )
    label_go = ParagraphStyle(
        "LabelGo",
        fontName="Helvetica-Bold",
        fontSize=8,
        leading=11,
        textColor=LABEL_GO,
        spaceBefore=0,
        spaceAfter=2,
    )
    caption = ParagraphStyle(
        "Caption",
        fontName="Helvetica-Oblique",
        fontSize=8.5,
        leading=12,
        textColor=MUTED_TEXT,
        spaceBefore=4,
        spaceAfter=8,
    )
    gloss_term = ParagraphStyle(
        "GlossTerm",
        fontName="Courier-Bold",
        fontSize=9,
        leading=13,
        textColor=DARK_NAVY,
    )
    gloss_def = ParagraphStyle(
        "GlossDef",
        fontName="Helvetica",
        fontSize=9,
        leading=13,
        textColor=BODY_TEXT,
    )
    return dict(
        title=title_style,
        subtitle=subtitle_style,
        section_num=section_num,
        section_title=section_title,
        body=body,
        bullet=bullet,
        code=code_inline,
        label_r=label_r,
        label_go=label_go,
        caption=caption,
        gloss_term=gloss_term,
        gloss_def=gloss_def,
    )

# ── Code block builders ────────────────────────────────────────────────────────
def code_block(lines, lang="r", width=None, styles=None):
    """Return a Table that looks like a styled code block."""
    if width is None:
        width = PAGE_W - LEFT_M - RIGHT_M

    bg = LABEL_R_BG if lang == "r" else LABEL_GO_BG
    label_col = LABEL_R if lang == "r" else LABEL_GO
    border_col = LABEL_R if lang == "r" else LABEL_GO

    label_text = "R" if lang == "r" else "Go"
    label_para = Paragraph(label_text, ParagraphStyle(
        "cblabel",
        fontName="Helvetica-Bold",
        fontSize=7.5,
        leading=10,
        textColor=label_col,
    ))
    header_row = [[label_para]]
    code_paras = []
    for line in lines:
        # escape XML entities
        line = line.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        code_paras.append(Paragraph(line if line.strip() else "&nbsp;", ParagraphStyle(
            "codetext",
            fontName="Courier",
            fontSize=8,
            leading=12,
            textColor=BODY_TEXT,
        )))
    code_cell = code_paras
    data = [header_row, [code_cell]]
    tbl = Table(data, colWidths=[width])
    tbl.setStyle(TableStyle([
        ("BACKGROUND",   (0, 0), (0, 0), bg),
        ("BACKGROUND",   (0, 1), (0, 1), CODE_BG),
        ("BOX",          (0, 0), (-1, -1), 1, border_col),
        ("LINEBELOW",    (0, 0), (0, 0), 0.5, border_col),
        ("LEFTPADDING",  (0, 0), (-1, -1), 8),
        ("RIGHTPADDING", (0, 0), (-1, -1), 8),
        ("TOPPADDING",   (0, 0), (0, 0), 4),
        ("BOTTOMPADDING",(0, 0), (0, 0), 4),
        ("TOPPADDING",   (0, 1), (0, 1), 8),
        ("BOTTOMPADDING",(0, 1), (0, 1), 8),
    ]))
    return tbl


def side_by_side(r_lines, go_lines, gap=8):
    """Return a Table with R code on the left, Go on the right."""
    avail = PAGE_W - LEFT_M - RIGHT_M
    col_w = (avail - gap) / 2

    r_tbl  = code_block(r_lines,  lang="r",  width=col_w)
    go_tbl = code_block(go_lines, lang="go", width=col_w)

    outer = Table([[r_tbl, go_tbl]], colWidths=[col_w, col_w],
                  spaceBefore=0, spaceAfter=0)
    outer.setStyle(TableStyle([
        ("LEFTPADDING",  (0, 0), (-1, -1), 0),
        ("RIGHTPADDING", (0, 0), (-1, -1), 0),
        ("TOPPADDING",   (0, 0), (-1, -1), 0),
        ("BOTTOMPADDING",(0, 0), (-1, -1), 0),
        ("VALIGN",       (0, 0), (-1, -1), "TOP"),
    ]))
    return outer


# ── Header / footer callbacks ──────────────────────────────────────────────────
def on_first_page(canvas, doc):
    canvas.saveState()
    # Full-bleed header banner
    banner_h = 1.45 * inch
    canvas.setFillColor(DARK_NAVY)
    canvas.rect(0, PAGE_H - banner_h, PAGE_W, banner_h, fill=1, stroke=0)
    # Accent stripe
    canvas.setFillColor(MID_BLUE)
    canvas.rect(0, PAGE_H - banner_h, 5, banner_h, fill=1, stroke=0)
    # Title
    canvas.setFillColor(colors.white)
    canvas.setFont("Helvetica-Bold", 22)
    canvas.drawString(LEFT_M + 12, PAGE_H - 0.65 * inch, "R to Go: Translating an IRS 990 Data Pipeline")
    canvas.setFont("Helvetica", 11)
    canvas.setFillColor(colors.HexColor("#bfdbfe"))
    canvas.drawString(LEFT_M + 12, PAGE_H - 0.95 * inch, "A practical guide for R programmers encountering Go for the first time")
    # Footer
    _draw_footer(canvas, doc, 1)
    canvas.restoreState()


def on_later_pages(canvas, doc):
    canvas.saveState()
    # Thin top rule
    canvas.setStrokeColor(RULE_COL)
    canvas.setLineWidth(0.5)
    canvas.line(LEFT_M, PAGE_H - 0.35 * inch, PAGE_W - RIGHT_M, PAGE_H - 0.35 * inch)
    canvas.setFont("Helvetica", 7.5)
    canvas.setFillColor(MUTED_TEXT)
    canvas.drawString(LEFT_M, PAGE_H - 0.52 * inch, "R to Go: Translating an IRS 990 Data Pipeline")
    _draw_footer(canvas, doc, doc.page)
    canvas.restoreState()


def _draw_footer(canvas, doc, page_num):
    canvas.setStrokeColor(RULE_COL)
    canvas.setLineWidth(0.5)
    canvas.line(LEFT_M, 0.5 * inch, PAGE_W - RIGHT_M, 0.5 * inch)
    canvas.setFont("Helvetica", 7.5)
    canvas.setFillColor(MUTED_TEXT)
    canvas.drawString(LEFT_M, 0.32 * inch, "github.com/BatonVatrushka/irs_990_xml_parse")
    canvas.drawRightString(PAGE_W - RIGHT_M, 0.32 * inch, f"Page {page_num}")


# ── Section heading helper ─────────────────────────────────────────────────────
def section_heading(num, title, styles):
    return [
        Spacer(1, 0.18 * inch),
        HRFlowable(width="100%", thickness=0.5, color=RULE_COL, spaceAfter=6),
        Paragraph(f"Section {num}", styles["section_num"]),
        Paragraph(title, styles["section_title"]),
    ]


def info_box(paras, width=None):
    """Light blue info callout box."""
    if width is None:
        width = PAGE_W - LEFT_M - RIGHT_M
    tbl = Table([[paras]], colWidths=[width])
    tbl.setStyle(TableStyle([
        ("BACKGROUND",   (0, 0), (-1, -1), LIGHT_BLUE),
        ("BOX",          (0, 0), (-1, -1), 1, MID_BLUE),
        ("LEFTPADDING",  (0, 0), (-1, -1), 10),
        ("RIGHTPADDING", (0, 0), (-1, -1), 10),
        ("TOPPADDING",   (0, 0), (-1, -1), 8),
        ("BOTTOMPADDING",(0, 0), (-1, -1), 8),
    ]))
    return tbl


# ── Build content ──────────────────────────────────────────────────────────────
def build_story(styles):
    s = styles
    story = []
    # Push content below header banner on page 1
    story.append(Spacer(1, 1.6 * inch))

    # ── Intro blurb ────────────────────────────────────────────────────────────
    story.append(Paragraph(
        "This guide walks through how an IRS 990 XML data pipeline that started "
        "life as a handful of R scripts ended up rewritten in Go. It is written "
        "for someone who knows R well — tidyverse, purrr, xml2 — and is reading "
        "Go code for the first time. Every section pairs an R pattern you "
        "already know with its Go equivalent.",
        s["body"]
    ))
    story.append(Spacer(1, 0.1 * inch))

    # ── Section 1: Why Go? ─────────────────────────────────────────────────────
    story.extend(section_heading("1", "Why Go?", s))
    story.append(Paragraph(
        "R is an excellent tool for interactive data exploration, one-off "
        "analyses, and prototyping pipelines. But when you want something that "
        "runs continuously as a service — polling for new IRS data every week, "
        "writing to a database, resuming where it left off after a restart — "
        "R's interactive model starts to feel like the wrong fit.",
        s["body"]
    ))
    story.append(Spacer(1, 0.06 * inch))

    comparisons = [
        ("R is great for...", "Go is better for..."),
        ("Interactive exploration in the console", "Long-running background services"),
        ("Quick scripts you run once", "Programs that run continuously for months"),
        ("Results that live in memory", "Results persisted to a database"),
        ("Dynamic, flexible data shapes", "Strictly-typed, predictable data contracts"),
    ]
    col_w = (PAGE_W - LEFT_M - RIGHT_M) / 2
    tbl_data = [[
        Paragraph(comparisons[0][0], ParagraphStyle("th", fontName="Helvetica-Bold", fontSize=9, textColor=colors.white)),
        Paragraph(comparisons[0][1], ParagraphStyle("th", fontName="Helvetica-Bold", fontSize=9, textColor=colors.white)),
    ]]
    for a, b in comparisons[1:]:
        tbl_data.append([
            Paragraph(a, ParagraphStyle("td", fontName="Helvetica", fontSize=9, leading=13, textColor=BODY_TEXT)),
            Paragraph(b, ParagraphStyle("td", fontName="Helvetica", fontSize=9, leading=13, textColor=BODY_TEXT)),
        ])
    comp_tbl = Table(tbl_data, colWidths=[col_w, col_w])
    comp_tbl.setStyle(TableStyle([
        ("BACKGROUND",    (0, 0), (-1, 0), DARK_NAVY),
        ("ROWBACKGROUNDS",(0, 1), (-1, -1), [colors.white, colors.HexColor("#f8fafc")]),
        ("BOX",           (0, 0), (-1, -1), 0.5, CODE_BORDER),
        ("INNERGRID",     (0, 0), (-1, -1), 0.3, CODE_BORDER),
        ("LEFTPADDING",   (0, 0), (-1, -1), 8),
        ("RIGHTPADDING",  (0, 0), (-1, -1), 8),
        ("TOPPADDING",    (0, 0), (-1, -1), 5),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 5),
    ]))
    story.append(comp_tbl)
    story.append(Spacer(1, 0.08 * inch))

    story.append(Paragraph(
        "Go is <b>statically typed</b> (every variable has a declared type, "
        "checked at compile time), <b>compiled</b> (a single binary you ship), "
        "and has <b>built-in concurrency primitives</b> — goroutines and channels "
        "are part of the language, not a library. The ideas from your R pipeline "
        "translate directly; the syntax just looks different.",
        s["body"]
    ))

    # ── Section 2: Project Structure ──────────────────────────────────────────
    story.extend(section_heading("2", "Project Structure", s))
    story.append(Paragraph(
        "In R, a pipeline is typically a flat set of <code>.R</code> script "
        "files you run in sequence. In Go, code is organized into "
        "<b>packages</b> — each directory is a package. Here is how this "
        "project maps to the R scripts:",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    r_struct = [
        "# R scripts (flat):",
        "schedule_J_parser.R",
        "schedule_J_cleaner.R",
        "xml_parse_code.R",
    ]
    go_struct = [
        "// Go packages (directories):",
        "cmd/main.go          // entry point",
        "internal/fetcher/    // HTTP downloads",
        "internal/parser/     // XML parsing",
        "internal/models/     // shared data types",
        "internal/db/         // database writes",
    ]
    story.append(side_by_side(r_struct, go_struct))
    story.append(Spacer(1, 0.08 * inch))

    story.append(Paragraph(
        "A <b>package</b> in Go is like a namespace. Code in "
        "<code>internal/parser/</code> belongs to <code>package parser</code>. "
        "Other packages import it by path: "
        "<code>\"github.com/.../internal/parser\"</code>. Functions that start "
        "with an uppercase letter are exported (public); lowercase names are "
        "package-private — there is no <code>export()</code> step.",
        s["body"]
    ))

    # ── Section 3: Static Typing ───────────────────────────────────────────────
    story.extend(section_heading("3", "Static Typing — The Biggest Mental Shift", s))
    story.append(Paragraph(
        "In R, a function can return anything — a character vector, a matrix, "
        "a list — and you figure out the shape at runtime. In Go, every "
        "function's return type is declared upfront, and the compiler rejects "
        "code that violates it. The payoff is that you can never accidentally "
        "pass the wrong thing to a function.",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    r_typing = [
        "xml_comp_parser <- function(xml) {",
        "  ein <- xml_file %>%",
        "    xml_find_first(xpath='//d1:EIN') %>%",
        "    xml_text()",
        "  values <- xml_file %>%",
        "    xml_find_all('//d1:IRS990ScheduleJ') %>%",
        "    xml_children() %>% xml_text()",
        "  # shape is whatever comes back",
        "  mat <- matrix(c(ein, taxyr, values), ...)",
        "  return(mat)   # could be anything",
        "}",
    ]
    go_typing = [
        "// Shape declared upfront as a struct",
        "type personComp struct {",
        "    PersonNm  string `xml:\"PersonNm\"`",
        "    TitleTxt  string `xml:\"TitleTxt\"`",
        "    TotalCompFilingOrgAmt string \\",
        "        `xml:\"TotalCompFilingOrgAmt\"`",
        "}",
        "",
        "// Parser returns a known type or an error",
        "func Parse(data []byte, ...) (*Result, error) {",
        "    var doc return990",
        "    xml.Unmarshal(data, &doc)",
        "    return &Result{...}, nil",
        "}",
    ]
    story.append(side_by_side(r_typing, go_typing))
    story.append(Spacer(1, 0.07 * inch))

    story.append(Paragraph(
        "The <code>CompensationRecord</code> struct in "
        "<code>internal/models/models.go</code> is the Go equivalent of the "
        "column selection you do in R with <code>str_detect(names(.), "
        "\"PersonNm.*|TitleTxt.*|Total.*\")</code> — except the shape is "
        "fixed at compile time, not discovered at runtime.",
        s["body"]
    ))

    # ── Section 4: Error Handling ──────────────────────────────────────────────
    story.extend(section_heading("4", "Error Handling", s))
    story.append(Paragraph(
        "R's <code>purrr::possibly()</code> wraps a function so that any "
        "error silently becomes <code>NA</code>. This is convenient for "
        "exploration, but in a long-running service you want to know "
        "<i>which</i> files failed and <i>why</i>. Go makes errors "
        "explicit: every function that can fail returns two values — the "
        "result and an error.",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    r_err = [
        "# Silently swallow errors, return NA",
        "comp <- files %>%",
        "  map(possibly(",
        "    xml_comp_parser,",
        "    otherwise = NA_real_",
        "  ))",
        "",
        "# You won't know which files failed",
        "# unless you check for NA afterward",
    ]
    go_err = [
        "// Every failure is explicit",
        "result, err := parser.Parse(",
        "    xmlData, objectID,",
        "    companyName, city, state, ntee,",
        ")",
        "if err != nil {",
        "    slog.Warn(\"parse failed\",",
        "        \"object_id\", objectID,",
        "        \"err\", err)",
        "    return nil  // skip, log, move on",
        "}",
    ]
    story.append(side_by_side(r_err, go_err))
    story.append(Spacer(1, 0.07 * inch))

    story.append(Paragraph(
        "The <code>if err != nil</code> pattern is repetitive, and Go "
        "programmers joke about it. But its advantage is visibility: "
        "every failure path is explicitly handled at the call site. "
        "There are no silent <code>NA</code>s propagating through your "
        "pipeline undetected.",
        s["body"]
    ))

    # ── Section 5: XML Parsing ─────────────────────────────────────────────────
    story.extend(section_heading("5", "XML Parsing — XPath vs. Struct Tags", s))
    story.append(Paragraph(
        "In R, you write XPath strings at runtime to extract values from "
        "XML nodes. In Go, you declare the mapping <i>once</i> as struct "
        "field tags, and the standard library figures out the rest when "
        "you call <code>xml.Unmarshal()</code>. The struct tags are "
        "checked at compile time; there are no typos in XPath strings "
        "that you only discover when you run the code.",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    r_xml = [
        "# XPath strings evaluated at runtime",
        "ein <- xml_file %>%",
        "  xml_find_first(",
        "    xpath = '//d1:EIN'",
        "  ) %>% xml_text()",
        "",
        "name <- xml_file %>%",
        "  xml_find_all(",
        "    xpath = '//d1:PersonNm'",
        "  ) %>% xml_text()",
    ]
    go_xml = [
        "// Struct tags declared once; no XPath",
        "type personComp struct {",
        "    PersonNm string `xml:\"PersonNm\"`",
        "    TitleTxt string `xml:\"TitleTxt\"`",
        "    BaseComp string \\",
        "  `xml:\"BaseCompensationFilingOrgAmt\"`",
        "}",
        "",
        "// One call maps the whole document",
        "var doc return990",
        "xml.Unmarshal(data, &doc)",
        "// doc.ReturnData.ScheduleJ.Persons",
        "// is now a []personComp — fully parsed",
    ]
    story.append(side_by_side(r_xml, go_xml))
    story.append(Spacer(1, 0.07 * inch))

    story.append(Paragraph(
        "The backtick notation — <code>`xml:\"PersonNm\"`</code> — is a "
        "<b>struct tag</b>. It is a string literal attached to a field "
        "that the <code>encoding/xml</code> package reads via reflection "
        "at runtime. Think of it as column metadata that lives right next "
        "to the field declaration, rather than in a separate XPath "
        "expression somewhere else in the code.",
        s["body"]
    ))

    # ── Section 6: Pipeline Pattern ───────────────────────────────────────────
    story.extend(section_heading("6", "The Pipeline Pattern — map() vs. Goroutines", s))
    story.append(Paragraph(
        "The R pipeline uses <code>purrr::map()</code> to apply "
        "<code>xml_comp_parser</code> to every file sequentially. Go "
        "replaces this with a <b>worker pool</b>: a fixed number of "
        "goroutines running in parallel, bounded by a semaphore so you "
        "do not overwhelm the IRS server or your database.",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    r_pipe = [
        "# Sequential — one file at a time",
        "comp <- files %>%",
        "  map(possibly(",
        "    xml_comp_parser,",
        "    otherwise = NA_real_",
        "  ))",
        "",
        "# map() runs in the R process,",
        "# one iteration at a time.",
    ]
    go_pipe = [
        "// Concurrent — workers run in parallel",
        "sem := make(chan struct{}, workers)",
        "g, ctx := errgroup.WithContext(ctx)",
        "",
        "for _, entry := range entries {",
        "    entry := entry  // capture loop var",
        "    sem <- struct{}{}",
        "    g.Go(func() error {",
        "        defer func() { <-sem }()",
        "        return processXML(ctx, entry)",
        "    })",
        "}",
        "return g.Wait()",
    ]
    story.append(side_by_side(r_pipe, go_pipe))
    story.append(Spacer(1, 0.07 * inch))

    story.append(Paragraph(
        "A <b>goroutine</b> is a lightweight concurrent function — not a "
        "thread, but something Go schedules across your CPU cores. "
        "<code>go func() { ... }()</code> starts one. "
        "The <b>semaphore channel</b> (<code>sem</code>) limits how many "
        "goroutines run at once: sending to a full channel blocks until "
        "a slot frees up; the deferred receive releases the slot when "
        "the goroutine finishes. <code>errgroup</code> collects errors "
        "from all goroutines and cancels the context if any fail.",
        s["body"]
    ))

    # ── Section 7: HTTP and the Database ──────────────────────────────────────
    story.extend(section_heading("7", "New Concepts: HTTP and the Database", s))
    story.append(Paragraph(
        "The original R scripts assumed files were already downloaded "
        "manually to a local directory, and results lived in memory "
        "until the R session ended. The Go pipeline handles both "
        "fetching and persistence natively.",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    # HTTP
    story.append(Paragraph("<b>Fetching data with net/http</b>", ParagraphStyle(
        "mini_head", fontName="Helvetica-Bold", fontSize=10, leading=14,
        textColor=SUBHEAD_COL, spaceBefore=4, spaceAfter=4)))
    go_http = [
        "// fetcher.go — simplified",
        "func (f *Fetcher) get(",
        "    ctx context.Context, url string,",
        ") (io.ReadCloser, error) {",
        "    req, _ := http.NewRequestWithContext(",
        "        ctx, http.MethodGet, url, nil)",
        "    resp, err := f.client.Do(req)",
        "    if err != nil { return nil, err }",
        "    return resp.Body, nil",
        "}",
        "",
        "// IRS ZIPs are 2-3 GB; client timeout: 4h",
    ]
    story.append(code_block(go_http, lang="go"))
    story.append(Spacer(1, 0.07 * inch))

    # Database
    story.append(Paragraph("<b>Writing results to Postgres with pgx</b>", ParagraphStyle(
        "mini_head2", fontName="Helvetica-Bold", fontSize=10, leading=14,
        textColor=SUBHEAD_COL, spaceBefore=4, spaceAfter=4)))
    go_db = [
        "// db/writer.go — batch insert via COPY",
        "func InsertCompensation(",
        "    ctx context.Context,",
        "    pool *pgxpool.Pool,",
        "    records []*models.CompensationRecord,",
        ") error {",
        "    _, err := pool.CopyFrom(ctx,",
        "        pgx.Identifier{\"schedule_j_compensation\"},",
        "        cols,",
        "        pgx.CopyFromRows(rows),",
        "    )",
        "    return err",
        "}",
    ]
    story.append(code_block(go_db, lang="go"))
    story.append(Spacer(1, 0.07 * inch))

    # Resume logic
    story.append(info_box([
        Paragraph(
            "<b>Resume where you left off.</b>  Before downloading any ZIP "
            "archive, the pipeline calls <code>db.GetProcessedIDs()</code> to "
            "bulk-check which filings are already in the database. ZIPs can be "
            "2-3 GB — skipping ones you have already processed saves hours. "
            "The R script had no equivalent: if it crashed, you started over.",
            ParagraphStyle("ib", fontName="Helvetica", fontSize=9, leading=14,
                           textColor=BODY_TEXT)
        ),
    ]))

    # ── Section 8: Glossary ────────────────────────────────────────────────────
    story.extend(section_heading("8", "Key Go Concepts Glossary", s))
    story.append(Paragraph(
        "Quick reference for terms that appear throughout the codebase:",
        s["body"]
    ))
    story.append(Spacer(1, 0.07 * inch))

    glossary = [
        ("package",   "A directory of <code>.go</code> files that share a namespace. "
                      "The first line of every file declares it: <code>package parser</code>. "
                      "Closest R analogy: a package you load with <code>library()</code>."),
        ("func",      "A function declaration. "
                      "<code>func Parse(data []byte) (*Result, error)</code> — "
                      "name, parameters with types, return types. All explicit, no guessing."),
        ("struct",    "A named collection of fields with declared types. "
                      "Like a row definition or a named list with a fixed schema. "
                      "Used everywhere data has a predictable shape."),
        ("interface", "A set of method signatures. Any type that implements those "
                      "methods satisfies the interface — no explicit <code>implements</code> "
                      "keyword needed. Think of it as duck typing, but checked at compile time."),
        ("goroutine", "A lightweight concurrent function started with the <code>go</code> "
                      "keyword. The Go runtime schedules thousands of them across OS threads. "
                      "Much cheaper than a thread; roughly analogous to a future or promise."),
        ("channel",   "A typed pipe for sending values between goroutines. "
                      "<code>ch &lt;- v</code> sends; <code>v := &lt;-ch</code> receives. "
                      "Used as a semaphore here to cap concurrency."),
        ("error",     "A built-in interface with one method: <code>Error() string</code>. "
                      "Returned as the last value from functions that can fail. "
                      "Check with <code>if err != nil</code>."),
        ("defer",     "Schedules a function call to run when the surrounding function "
                      "returns, regardless of how it returns (normal, panic, etc.). "
                      "Used to close files and release semaphore slots."),
        ("context",   "Carries deadlines, cancellation signals, and request-scoped values "
                      "across API boundaries. Pass it as the first argument to any function "
                      "that does I/O. Cancel it to abort in-flight work."),
    ]

    gloss_data = []
    for term, defn in glossary:
        term_para = Paragraph(term, ParagraphStyle(
            "gt", fontName="Courier-Bold", fontSize=9, leading=13, textColor=DARK_NAVY))
        def_para = Paragraph(defn, ParagraphStyle(
            "gd", fontName="Helvetica", fontSize=9, leading=13, textColor=BODY_TEXT))
        gloss_data.append([term_para, def_para])

    col1_w = 0.9 * inch
    col2_w = PAGE_W - LEFT_M - RIGHT_M - col1_w
    gloss_tbl = Table(gloss_data, colWidths=[col1_w, col2_w])
    gloss_tbl.setStyle(TableStyle([
        ("ROWBACKGROUNDS",  (0, 0), (-1, -1), [colors.white, GLOSSARY_BG]),
        ("BOX",             (0, 0), (-1, -1), 0.5, CODE_BORDER),
        ("LINEAFTER",       (0, 0), (0, -1), 0.5, CODE_BORDER),
        ("INNERGRID",       (0, 0), (-1, -1), 0.2, CODE_BORDER),
        ("LEFTPADDING",     (0, 0), (-1, -1), 8),
        ("RIGHTPADDING",    (0, 0), (-1, -1), 8),
        ("TOPPADDING",      (0, 0), (-1, -1), 6),
        ("BOTTOMPADDING",   (0, 0), (-1, -1), 6),
        ("VALIGN",          (0, 0), (-1, -1), "TOP"),
    ]))
    story.append(gloss_tbl)
    story.append(Spacer(1, 0.15 * inch))

    # ── Closing note ───────────────────────────────────────────────────────────
    story.append(HRFlowable(width="100%", thickness=0.5, color=RULE_COL, spaceAfter=8))
    story.append(Paragraph(
        "The full source is at "
        "<code>github.com/BatonVatrushka/irs_990_xml_parse</code>. "
        "The R scripts that started this pipeline are still in the repository "
        "root — <code>schedule_J_parser.R</code>, <code>schedule_J_cleaner.R</code> — "
        "if you want to compare the original R approach with the Go implementation "
        "side by side.",
        s["body"]
    ))

    return story


# ── Main ───────────────────────────────────────────────────────────────────────
def main():
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)

    doc = SimpleDocTemplate(
        OUTPUT_PATH,
        pagesize=letter,
        leftMargin=LEFT_M,
        rightMargin=RIGHT_M,
        topMargin=TOP_M + 1.6 * inch,   # account for banner on p1
        bottomMargin=BOT_M,
        title="R to Go: Translating an IRS 990 Data Pipeline",
        author="IRS 990 XML Parse Project",
    )

    styles = make_styles()
    story  = build_story(styles)

    doc.build(
        story,
        onFirstPage=on_first_page,
        onLaterPages=on_later_pages,
    )
    print(f"SUCCESS: {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
