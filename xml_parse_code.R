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















