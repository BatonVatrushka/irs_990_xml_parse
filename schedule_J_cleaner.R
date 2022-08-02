library(pacman)
p_load(tidyverse, tools, janitor)

# ===============================
# Schedule J Cleaning Function
# ===============================
# this function should be applied to xml data after it has been parsed 
# through the schedule_J_parser function
# the function works on a data.frame, csv, or list

schedule_J_cleaner <- function(x){
  # cleaning the data for the first person on the sched j form
  if (is.data.frame(x) == T || is.data.table(x) == T){
    x %>%
      select(c(1:6)) %>%
      mutate(across(c(3:4), tolower)
             , across(c(5:6), as.numeric)) %>%
      as_tibble()
  }
  else if (is.list(x) == T){
    x %>%
      rbindlist(fill = T) %>%
      # select the first six fields
      select(c(1:6)) %>%
      # make the character fields lowercase
      # make the 'total' fields numeric
      mutate(across(c(3:4), tolower)
             , across(c(5:6), as.numeric))
  }
  # if the file is a csv, do it up
  else if (file_ext(x) == "csv"){  
    # read in the csv
    read_csv(x) %>%
      # select the first six fields
      select(c(1:6)) %>%
      # make the character fields lowercase
      # make the 'total' fields numeric
      mutate(across(c(3:4), tolower)
             , across(c(5:6), as.numeric))
  } 
  # return an error if the file isn't a csv or a data.frame
  else if (file_ext(x) != "csv" || !is.data.frame(x) || !is.data.table(x) || !is.list(x)) {
    stop("Please use a csv, a data frame, or a list")
    }
  }


