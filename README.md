# irs_990_xml_parse
this repo includes code used to parse xml files from series 990 bulk downloads from irs.gov
the pain purpose of this project is to pull data concerning compensation of nonprofit CEOs and other "important" individuals in the respective organization.

You can retrieve the xml files from here: https://www.irs.gov/charities-non-profits/form-990-series-downloads

I'm using the files from the only zip folder under filing year 2022 (there's only one zip as of July 2022). You should be able to reproduce my code by unzipping the the files into a folder in your project directory called /xml_files_new

This repo is a work in progress. I'll add code in the future which shows how you can pulls thousands (or millions) of rows of data from these files. 
