# install pacman if you don't have it
# p_load will install packages and dependencies for you
# install.packages("pacman")
library(pacman)
p_load(xml2, tidyverse, stringi, sjmisc)

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
# it's important to use the namespaces in the xpath
# because stripping namespace is computationally expensive
xml_file %>%
    xml_ns()
# =====================================
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
# notice we have to get the grandchildren
names <- xml_file %>% 
  xml_find_all(xpath = "//d1:IRS990ScheduleJ") %>%
  xml_children() %>%
  xml_children() %>%
  xml_name()

# get the values of the fields
# again with the grandchildren
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
# ==============================================================================
# create a function 
# ==============================================================================
xml_comp_parser <- function(xml){
  # read in the file
  xml_file <- read_xml(xml)
  
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
                , byrow = F)
  
  # Return to the Matrix
  return(mat)
}

# test the function 
test <- xml_comp_parser('xml_files_new/202200069349300100_public.xml')

# ==============================================================================
# Map the Function 
# ==============================================================================
# make a list of files
files <- list.files(path = './xml_files_new/', full.names = T)

# now run the function over the files
# and store the matrices in a list (comp = compensation)
comp <- files %>%
  map(possibly(xml_comp_parser
               , otherwise = NA_real_))
# ==============================================================================
# Test
# ==============================================================================
# make a small list from comp
test_list <- comp[1:10]

# count # of elements and get the max
test_list %>% 
  map_dbl(., length) %>%
  max()

# try to create a matrix from the list
test_matrix <- test_list %>% 
  stri_list2matrix(., byrow = T)

# the issue is that each sub-list in the main list (comp) has 
# varying numbers of elements. This creates a problem. We are
# not going to be able to build a df from this set of data in 
# its current form 

# check names
test_list %>% names()

# check dim names 
test_list[[1]] %>% dimnames()

# each sublist is a matrix so each element has column names. Set 
# the element names using the column names

# set the names to the list using the colnames
test_list_named <- map(test_list, ~ set_names(.x, colnames(.x)))

# see if it worked
test_list_named[[3]] %>% names() # success!

# ==============================================================================
# Set Names
# ==============================================================================
# set names for the full list
comp_named <- map(comp, ~ set_names(.x, colnames(.x)))

# check to make sure the code worked
comp_named[[3]] %>% names() # success!
# ==============================================================================
# Clean Up
# ==============================================================================
# The schedule J has information on NGO compensation to CEOs 
# and "important" types like that in the org. 

# filter out the lists which don't have a schedule J
# this will be lists w/ an element length of 2

# discard lists where the condition is true
# first create a mapper to set the condition
# we want lists whose lengths are > 2
t <- as_mapper(~ length(.x) > 2)

# now use keep() to filter comp_named
comp_filtered <- keep(comp_named, t)

# compare lengths
(orig <- length(comp))
(fil <- length(comp_filtered))
# ratio
fil/orig 

# makes names unique in the lists
# remember that element names refer to column names in the matrices
comp_filtered <-
  map(comp_filtered, ~ set_names(.x
                                 , .x %>% names() %>% make.names(unique = T)))








