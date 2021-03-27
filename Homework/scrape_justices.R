
library(tidyverse)
library(robotstxt)
library(rvest)
library(knitr)
library(janitor)

url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"
tables <- url %>%
read_html() %>%
html_nodes("table")

justice_table <- html_table(tables[[2]], fill = TRUE)

write_csv(justice_table,
"C:/Users/Lorraine/Dropbox/Data Science/Data-Science/Homework/justices.csv")




