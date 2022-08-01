library(pacman)
p_load(tidyverse, tools, janitor)

# ===============================
# Schedule J Cleaning Function
# ===============================
# this function should be applied to xml data after it has been parsed through the schedule_J_parser function
# currently a csv is required to run the function but it'll be updated to take a df as well to enable piping

df_J_clean <- function(csv){
  # throw an error when a csv isn't written
  if (file_ext(csv) != "csv")
  {stop("Please use a CSV")}
  # read in the csv
  df <- read_csv(csv) %>%
    # select the first six fields
    select(c(1:6)) %>%
    # make the character fields lowercase
    # make the 'total' fields numeric
    mutate(across(c(3:4), tolower)
           , across(c(5:6), as.numeric))
  # return the df
  return(df)
}
