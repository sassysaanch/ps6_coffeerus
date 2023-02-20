################################################################################
##
## [ PROJ ] < Problem set #6 >
## [ FILE ] < Homework Edu 260B >
## [ AUTH ] < Saanchi Shah/ sassysaanch >
## [ INIT ] < 2/18/2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------
data_dir <- file.path("/Users/saanchishah/Desktop/rclass2/week6/ps6_coffeerus/data_shah_saanchi")


## -----------------------------------------------------------------------------
## Part 1 - Label each question using comments
## -----------------------------------------------------------------------------

# Construct files and urls
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'
files <- c('HD2017', 'HD2018', 'HD2019')
suffixes <- c('', '_Dict', '_Stata')

## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------


# Q1.1
for(i in files) {
  print(i)
  str_c("filename =", i)
}

# Q1.2

for(i in files) {
  print(i)
  for(z in suffixes){
    print(z)
  }
}

# Q1.3

for(i in files) {
  print(i)
  for(z in suffixes){
    print(z)
    file_name <- writeLines(str_c("filename = ", i, z, ".zip"))
  }
}


# Alternate code

file_name <- as.list(file_name)

for(i in files) {
  print(i)
  for(z in suffixes){
    print(z)
    file_names <- str_c("filename = ", i, z, ".zip")
    file_name <- c(file_name, file_names)
  }
}

# Q1.4

for(i in files) {
  print(i)
  for(z in suffixes){
    print(z)
    file_url <- writeLines(str_c("file_url = ", url, i, z, ".zip"))
  }
}

# Q1.5

for (i in files) {
  for (z in suffixes) {
    file_url_download <- str_c(url, i, z, ".zip")
    writeLines(str_c("file_url = ", file_url_download))
    dest_file <- file.path(data_dir, str_c(i, z, ".zip"))
    download.file(file_url_download, destfile = dest_file)
  }
}


## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

# Q3.1

output <- vector(mode = "numeric", length = length(files))

# accessing the element with [ ], looping over the index 
for(i in seq_along(files)) {
  output[i] <- print(i)
}

# accessing the elements using [[]]

for(i in seq_along(files)) {
  output[[i]] <- print(i)
}


# Q3.2

for(i in seq_along(files)) {
  print(files[i])
  print(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[i], ".zip")),   # Still not super clear why this can't be files[[i]]
        exdir = data_dir)
}

# OR

for(z in files){
  unzip(zipfile = file.path(data_dir, str_c(z, ".zip")),
        exdir = data_dir)
}


# Q3.3

library(readr)

for(i in seq_along(files)) {
  print(files[i])
  print(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[i], ".zip")),  
        exdir = data_dir)
  filecsv <- str_c(files[i], ".csv")
  print(filecsv)
  df <- read_csv(file = file.path(data_dir, filecsv))
}  

# Q3.4

dfs <- vector(mode = "list", length = length(files)) # why can't I simply use as.list? That works too

for(i in seq_along(files)) {
  print(files[i]) # Index
  print(files[[i]])  # element 
  unzip(zipfile = file.path(data_dir, str_c(files[i], ".zip")),  
        exdir = data_dir)
  filecsv <- str_c(files[i], ".csv")
  print(filecsv)
  df <- read_csv(file = file.path(data_dir, filecsv))
}  

# Q3.5

dfs <- vector(mode = "list", length = length(files))

for(i in seq_along(files)){
  filecsv <- str_c(files[i], ".csv")
  print(filecsv)
  dfs[[i]] <- read_csv(file.path(data_dir, filecsv))
}


## -----------------------------------------------------------------------------
## Part 4 - Label each question using comments
## -----------------------------------------------------------------------------

# Q4.1

hd2019 <- dfs[[3]]

# Q4.2

hd2019_subset <- hd2019 %>% 
  transmute(hbcu = if_else(HBCU == 1, 1, 0), 
            # or use mutate here and then re-pipe to use select()
            tribal = if_else(TRIBAL == 1, 1, 0),
            hospital = if_else(HOSPITAL == 1, 1, 0))

# Q4.3

for(i in names(hd2019_subset)) {
  print(i)
}

# Q4.4

for(i in names(hd2019_subset)) {
  print(i)
  writeLines(str_c("Number of ", i, " = ", sum(hd2019_subset[i], na.rm = TRUE)))
}

## Output

# [1] "hbcu"
# Number of hbcu = 102
# [1] "tribal"
# Number of tribal = 35
# [1] "hospital"
# Number of hospital = 96


# Q4.5

nums <- vector(mode = "numeric", length = length(hd2019_subset))

for(y in seq_along(hd2019_subset)){
  nums[[y]] <- str_c("Number of  ", names(hd2019_subset[y]), " = ", sum(hd2019_subset[[y]], na.rm = TRUE))
  writeLines(nums[[y]])
  
  # ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
  # geom_bar(stat = 'identity') +
  # scale_x_continuous(breaks = seq_along(hd2019_subset), labels = names(hd2019_subset)) +
  # xlab(NULL) + ylab(NULL)
}

# Extra credit plot

hd2019 %>% 
  mutate(hospital = if_else(HOSPITAL == 1, "True", "False")) %>% 
  filter(STABBR %in% c("CA", "FL", "AK", "AZ", "NY", "DC", "HI", "IL", "OR", "WA", "NV", "PA")) %>%
  group_by(STABBR, hospital) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot(mapping = aes(y = n, x = hospital, fill = STABBR)) +
  geom_col() +
  # scale_y_continuous(limits = c(0, 690))
  ggtitle("Number of hospitals attached to Unis by state Saanchi has traveled to") +
  ylab("Count") +
  labs(fill = "States")



## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
