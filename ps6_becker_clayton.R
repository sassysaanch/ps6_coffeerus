################################################################################
##
## [ PROJ ] < Problem set 6 >
## [ FILE ] < ps6_becker_clayton >
## [ AUTH ] <cnbecker14>
## [ INIT ] < February 18th, 2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------

data_dir <- file.path(".", "data_becker_clayton")
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'

files <- c('HD2017', 'HD2018', 'HD2019')
suffixes <- c('', '_Dict', '_Stata')

## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1

for (i in files) {
  writeLines(str_c(i))
}

# Question 2

for (i in files) {
  writeLines(str_c(i))
  for(z in suffixes) {
    print(z) 
}}

# Question 3 

file_name <- vector()

# for (i in files) {
#   writeLines(i)
#   for(z in suffixes) {
#     print(z)
#     file_name <- str_c(i,z,".zip")
#   }}


for(i in files) {
  writeLines(i)
  for(z in suffixes){
    print(z)
    file_names <- str_c(i, z, ".zip")
    file_name <- c(file_name, file_names)
  }
}

print(file_name)


# Question 4 

4. Next, you will construct the full 
download URL for the data file. Create an object 
called `file_url` that combines the `url` with the `file_name`. 
Next, add a line that wraps `str_c()` within `writeLines()` to print the value of the object `file_url`

_Hint_: `file_url` should look like `https://nces.ed.gov/ipeds/datacenter/data/HD2017.zip`, etc.

file_url <- vector()

file_url <- vector()

for(i in files) {
  print(i)
  for(z in suffixes){
    print(z)
    file_urls <- str_c(url, i, z, ".zip")
    file_url <- c(file_url, file_urls)
  }
}

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
