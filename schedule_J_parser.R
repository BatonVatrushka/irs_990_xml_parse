# install pacman if you don't have it
# p_load will install packages and dependencies for you
# install.packages("pacman")
library(pacman)
p_load(xml2, tidyverse, data.table)

schedule_J_parser <- function(files) {
  
  # this function parses the individual xml files
  # and stores the necessary information
  # EIN and TaxYr are the business key
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
  
  files %>%
    # map the parsing function on the files
    map(possibly(xml_comp_parser
                 , otherwise = NA_real_)) %>% 
    # set element names based on the colnames 
    # ** each sublist is a matrix **
    map(.,~ set_names(. 
                      , make.names(colnames(.), unique = T))) %>%
    # only keep matrices where length is > 2
    # 2 elements means no schedule j was filed
    keep(.,~ length(.) > 2) %>% 
    # filter for the fields we want (ein/taxyr = business key)
    # Name, Title, Total (compensation)
    map(.,~ keep(., .p = str_detect(names(.)
                                    ,"ein|taxyr|PersonNm.*|TitleTxt.*|Total.*"))) %>%
    # row bind the list then create a tibble
    map(., ~ rbind(.) %>% as_tibble()) %>%
    # rbindlist from data.table
    # fill = T imputes NAs 
    rbindlist(fill = T)
} 
