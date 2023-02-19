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

for (i in files) {
  writeLines(i)
  for(z in suffixes) {
    print(z)
    file_name <- str_c(i,z,".zip")
  }}


# for(i in files) {
#   writeLines(i)
#   for(z in suffixes){
#     print(z)
#     file_names <- str_c(i, z, ".zip")
#     file_name <- c(file_name, file_names)
#   }
# }

print(file_name)


# Question 4 

file_url <- vector()

for (i in files) {
  writeLines(i)
  for(z in suffixes) {
    print(z)
    file_name <- str_c(i,z,".zip")
    file_url <- str_c(url, file_name)
  }}

# for(i in files) {
#   print(i)
#   for(z in suffixes){
#     print(z)
#     file_urls <- str_c(url, i, z, ".zip")
#     file_url <- c(file_url, file_urls)
#   }
# }

# Question 5

for (i in files) {
  writeLines(i)
  for(z in suffixes) {
    print(z)
    file_name <- str_c(i,z,".zip")
    file_url <- str_c(url, file_name)
    download.file(url = file_url, destfile = file.path(data_dir, file_name))
  }}

# for (i in files) {
#   for (z in suffixes) {
#     file_url_download <- str_c(url, i, z, ".zip")
#     writeLines(str_c("file_url = ", file_url_download))
#     dest_file <- file.path(data_dir, str_c(i, z, ".zip"))
#     download.file(file_url_download, destfile = dest_file)
#   }
# }


## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

unzip(file.path(zipfile = data_dir, "HD2018.zip"), 
      exdir = data_dir)

# Question 1

for (i in 1:length(files)) {
  print(files[i])
  print(files[[i]])
}

# Question 2 

for (i in 1:length(files)) {
  print(files[i])
  print(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[[i]],".zip")), 
        exdir = data_dir)
}

# Question 3

for (i in 1:length(files)) {
  print(files[i])
  print(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[[i]],".zip")), 
        exdir = data_dir)
  df <- read.csv(file = file.path(data_dir, str_c(files[[i]],".csv")))
}

# Question 4

dfs <- vector(mode = "list")

# Question 5

for (i in 1:length(files)) {
  print(files[i])
  print(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[[i]],".zip")), 
        exdir = data_dir)
  df <- read.csv(file = file.path(data_dir, str_c(files[[i]],".csv")))
  dfs[[i]] <- df 
}


## -----------------------------------------------------------------------------
## Part 4 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1 

hd2019 <- dfs[[3]]

# Question 2 
    
hd2019_subset <- hd2019 %>% mutate(
  HBCU = if_else(HBCU == 1, 1, 0),
  TRIBAL = if_else(TRIBAL == 1, 1, 0),
  HOSPITAL = if_else(HOSPITAL == 1, 1, 0)) %>% select(HBCU, TRIBAL, HOSPITAL)


# Question 3 

for (i in names(hd2019_subset)) {
  writeLines(str_c("Column Name = " , i))
}

# Question 4

for (i in names(hd2019_subset)) {
  writeLines(str_c("Number of " , i, " = ", sum(hd2019_subset[[i]])))
}

# Question 5

nums <- vector()

for (i in 1:length(hd2019_subset)) {
  nums[[i]] <- str_c(sum(hd2019_subset[[i]]))
}
  

ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = seq_along(hd2019_subset), labels = names(hd2019_subset)) +
  xlab(NULL) + ylab(NULL)


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
