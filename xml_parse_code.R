library(pacman)
p_load(xml2, tidyverse)

# read in an xml file
xml_file <- read_xml('xml_files_new/202200069349300055_public.xml')

# print the children out
print(xml_children(xml_file))

# children of the children
xml_file %>% 
  xml_children() %>% 
  xml_children() %>%
  print()

# find the namespace(s)
xml_file %>%
    xml_ns()

# we want the info for the schedule J 
# which is the compensation information
# =====================================
# get the EIN (tax number)
ein <- xml_file %>%
  xml_find_first(xpath = "//d1:EIN") %>%
  xml_text()

# get the tax year
taxyr <- xml_file %>%
  xml_find_first(xpath = "//d1:TaxYr") %>%
  xml_text()

# get the names of the fields
names <- xml_file %>% 
  xml_find_all(xpath = "//d1:IRS990ScheduleJ") %>%
  xml_children() %>%
  xml_children() %>%
  xml_name()

# get the values of the fields
values <- xml_file %>% 
  xml_find_all(xpath = "//d1:IRS990ScheduleJ") %>%
  xml_children() %>%
  xml_children() %>%
  xml_text()

# Enter the Matrix
mat <- matrix(c(ein, taxyr, values)
              , ncol = length(values) + length(ein) + length(taxyr)
              , dimnames = list(NULL, c("ein", "taxyr", names))
              , byrow = T)













