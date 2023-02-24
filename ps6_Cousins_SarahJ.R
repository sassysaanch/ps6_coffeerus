################################################################################
##
## [ PROJ ] < Problem set #6 >
## [ FILE ] < ps6_Cousins_SarahJ >
## [ AUTH ] < Sarah J. Cousins/ SJC0usins >
## [ INIT ] < Due 2-24-23 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)

## ---------------------------
## directory creation & paths
## ---------------------------
dir.create(path = "data_Cousins_SarahJ", recursive = TRUE)
data_dir <- file.path('.', 'data_Cousins_SarahJ')

dir.create(path = "bonus_plot_Cousins_SarahJ", recursive = TRUE)
plots_dir <- file.path('.', 'bonus_plot_Cousins_SarahJ')

## ---------------------------
## objects provided
## ---------------------------
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'
files <- c('HD2017', 'HD2018', 'HD2019')
suffixes <- c('', '_Dict', '_Stata')


## -----------------------------------------------------------------------------
## Part 2 - Looping over elements
## -----------------------------------------------------------------------------

#Q1 - Looping over elements

for (i in files) {
  writeLines(str_c("filename = ", i))  
}


#Q2 - iterates over each element in suffixes

for (i in files) {
  writeLines(str_c("filename = ", i))  
  for(z in suffixes) {  
    writeLines(str_c("suffix = ", z))  
  }}


#Q3 - combine the filename, suffix, and the extension .zip

for (i in files) {
  writeLines(str_c("filename = ", i))  
  for(z in suffixes) {  
    writeLines(str_c("suffix = ", z))  
    file_name <-writeLines(str_c("filename = ", i, z, ".zip")) #trying to be efficent
  }}


#Q4 - object called file_url that combines the url with the file_name

for (i in files) {
  writeLines(str_c('filename=', i, sep = ''))
  for (z in suffixes) {
    writeLines(str_c('suffix=', z, sep = ''))
    file_name <- str_c(i, z, '.zip', sep = '')
    writeLines(str_c('file_name=', file_name, sep = ''))
    file_url <- str_c(url, file_name, sep = '')
    writeLines(str_c('file_url=', file_url, sep = '')) 
  }}


#Q5 - download the file at file_url into the data_dir.

for (i in files) {
  writeLines(str_c('filename=', i, sep = ''))
  for (z in suffixes) {
    writeLines(str_c('suffix=', z, sep = ''))
    file_name <- str_c(i, z, '.zip', sep = '')
    writeLines(str_c('file_name=', file_name, sep = ''))
    file_url <- str_c(url, file_name, sep = '')
    writeLines(str_c('file_url=', file_url, sep = ''))
    download.file(url = file_url,destfile = file.path(data_dir, file_name))
  }}

## -----------------------------------------------------------------------------
## Part 3 - Looping over indices
## -----------------------------------------------------------------------------

#Q1 - new loop that loops over the indices of the files vector

#using length 
length(files) #checking length for fun

for (i in 1:length(files)) {
  writeLines(str_c("file name =", i, "files[",i,"]", files[[i]], sep=" "))
}

#In office hours, "files[",i,"] was suggested to be included in below, but it TBD if it qualitatively matters. Therefore the below does not contain it. 
for (i in 1:length(files)) {
  writeLines(str_c("file name =", i, files[[i]], sep=" "))
}

#alternative with seq along which is equivalent to 1:length
for (i in seq_along(files)) {
  writeLines(str_c("file name =", i, files[[i]], sep=" "))
}

#Q2- Unzip the CSV files into the  data_dir

for (i in seq_along(files)) {
    writeLines(str_c("file name =", i, files[[i]], sep=" "))  
        unzip(zipfile = file.path(data_dir, str_c(files[[i]],".zip")), 
          exdir = data_dir)  
}

#Q3 - read in the unzipped CSV files.

for (i in seq_along(files)) {
  writeLines(str_c("file name =", i, files[[i]], sep=" "))  
  unzip(zipfile = file.path(data_dir, str_c(files[[i]],".zip")), 
        exdir = data_dir) 
  df <- read_csv(file = file.path(data_dir, str_c(files[i], '.csv')))
}


#Q4 - create a new object to store the dfs created from all 3 iterations of the loop

dfs<-vector(mode= 'list', length = length(files)) #new object/empty vector to loop 3 times for 3 csv files


#Q5 - Line of code to store each df object created within the loop to the dfs object

dfs<-vector(mode= 'list', length = length(files))
for (i in seq_along(files)) {
  writeLines(str_c("file name =", i, files[[i]], sep=" "))  
  unzip(zipfile = file.path(data_dir, str_c(files[[i]],".zip")), 
        exdir = data_dir) 
  df <- read_csv(file = file.path(data_dir, str_c(files[i], '.csv')))
  dfs[[i]]<- df  
  }


## -----------------------------------------------------------------------------
## Part 4 - Looping over NAMES
## -----------------------------------------------------------------------------

#Q1 - Subsetting operator to access the 2019 df from dfs and save it to an object called hd2019

hd2019 <- dfs[[3]] #2019 is in position 3


#Q2 - New object called hd2019_subset from hd2019 

hd2019_subset <- hd2019 %>% mutate(
  HBCU = if_else(HBCU == 1, 1, 0),# Modify  HBCU, TRIBAL, HOSPITAL vars into 0/1 indicator vars
  TRIBAL = if_else(TRIBAL == 1, 1, 0),
  HOSPITAL = if_else(HOSPITAL == 1, 1, 0)) %>% select(HBCU, TRIBAL, HOSPITAL) #Limit hd2019_subset to HBCU, TRIBAL, HOSPITAL vars 


#Q3 - New for loop to loop over the names of hd2019_subset 
for (i in names(hd2019_subset)) {
  writeLines(str_c("COLUMN NAME = " , i)) #Printe column name/var in the body of loop.
}


#Q4 - Calculate the sum of each column  and add that to your print statement from the previous step. Copy the outputs to your R script as a comment

for (i in names(hd2019_subset)) {
  writeLines(str_c("COLUMN NAME = " , i)) 
  writeLines(str_c("NUMBER OF ", i, " = ", sum(hd2019_subset[i], na.rm = TRUE)))
} 


#Q5 -  Create a new loop that loops over the indices of hd2019_subset instead. 

nums <- vector(mode = "numeric", length = length(hd2019_subset)) #store loops over indices  
for (i in names(hd2019_subset)) {
#  writeLines(str_c("COLUMN NAME = " , i)) #assignment seems to suggest not to do this.
  writeLines(str_c("  NUMBER OF ", i, " = ", sum(hd2019_subset[i], na.rm = TRUE)))
  for (i in seq_along(hd2019_subset)) {
    nums[[i]] <- sum(hd2019_subset[[i]]) 
  }}

#NUMBER OF HBCU = 102
#NUMBER OF TRIBAL = 35
#NUMBER OF HOSPITAL = 96

#note: the loop here is helping/simplifying our sums of 3 variables.

####steps to check the building of nums####
#nums <- vector(mode = "numeric", length = length(hd2019_subset)) #store in a new object called nums. 
#nums 
#[1] 0 0 0 #checking out nums for fun and we see it's empty
#length(nums) 
#[1] 3 #checking length for fun, it's 3 as we'd expect

#> nums
#[1] "102" "35"  "96" 

#If had we included column name in the loop, we'd see:
#COLUMN NAME = HBCU
#  NUMBER OF HBCU = 102
#COLUMN NAME = TRIBAL
#  NUMBER OF TRIBAL = 35
#COLUMN NAME = HOSPITAL
#  NUMBER OF HOSPITAL = 96

ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = seq_along(hd2019_subset), labels = names(hd2019_subset)) +
  xlab(NULL) + ylab(NULL)


## -----------------------------------------------------------------------------
## EXTRA CREDIT - GGPLOT
## -----------------------------------------------------------------------------

#The GG Plot 

  png(file.path(plots_dir, 'bonus_plot_Cousins_SarahJ.png'))
  ggplot(data= hd2019_degrees2, aes(x= HLOFFER,  group=collegetype)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1, face = "plain")) +
  theme(legend.key.size = unit(.25, 'cm'), legend.title = element_text(size=10)) +
  ggtitle("Figure 1. Highest Degrees Offered by College Setting")+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(labels = c("Graduate", "Undergraduate"))+
  labs(y = "Percent", fill="Degree", x="Highest Degree Offered") +
  facet_grid(~collegetype) +
  scale_y_continuous(labels = scales::percent)
  dev.off()
  
#DATA CREATION crude selection of vars and creating new var Not recommended for real research...

hd2019_degrees <- hd2019 %>% drop_na()

hd2019_degrees <- hd2019 %>% mutate(
  HBCU = if_else(HBCU == 1, 1, 0),
  TRIBAL = if_else(TRIBAL == 1, 2, 0),
  HOSPITAL = if_else(HOSPITAL == 1, 3, 0),
  HLOFFER = if_else(HLOFFER >5,1,0)) %>% select(HBCU, TRIBAL, HOSPITAL, HLOFFER)  

#crude deletion of missing values
hd2019_degrees<-subset(hd2019_degrees, HOSPITAL!="-2" | HOSPITAL!="-1")

#crude recode of colleges
hd2019_degrees$collegetype<-rowSums(hd2019_degrees[ , c("HBCU", "TRIBAL", "HOSPITAL")]) 

#check crude recode
table(hd2019_degrees$collegetype)
table(hd2019_degrees$HBCU)
table(hd2019_degrees$TRIBAL)
table(hd2019_degrees$HOSPITAL)

#crude selection/deletion of colleges with two types or type not listed
hd2019_degrees<-subset(hd2019_degrees, collegetype!="4")

table(hd2019_degrees$collegetype)
table(hd2019_degrees$HBCU)
table(hd2019_degrees$TRIBAL)
table(hd2019_degrees$HOSPITAL)

#only run this after code creation on subsequent lines following the ggplot
hd2019_degrees2 <- hd2019_degrees %>% mutate(HLOFFER = if_else(HLOFFER == 1, "Undergraduate", "Graduate") )

#should've done the below during inital subset
var_lab(hd2019_degrees2$collegetype) = "College"
val_lab(hd2019_degrees2$collegetype) = num_lab("
             0 OTHER
             1 HBCU    
             2 TRIBAL 
             3 HOSPITAL 
")"


## -----------------------------------------------------------------------------
##  POSTING ON GIT
## -----------------------------------------------------------------------------

#Posted: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/277
#Posted: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/276
#Replied: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/265 
  
## -----------------------------------------------------------------------------
## GIT TERMINAL COMMANDS FOR MY RECORD
## -----------------------------------------------------------------------------
# Command to initialize git repository
git init
# Command to clone repo
git clone https://github.com/anyone-can-cook/ps6_coffeerus
# create dev branch
git checkout -b dev_Cousins_SarahJ
git branch -a
#add or connect re
# Incorporate remote changes to current branch
git pull
#adding
git add .
#checking status
git status 
#committing 
git commit -m "inital commit of ps6_Cousins_SarahJ.R"
#push w upstream set for inital post--just watch out for
git push -u origin dev_Cousins_SarahJ   

##UPDATES
#adding
git add .
#checking status
git status 
#committing 
git commit -m "revision of R file"
#push 
git push origin dev_Cousins_SarahJ

##MERGE
#from dev branch
git add .
git commit -m "pre-merge commit"
#switch to main
git checkout main
git pull
git merge main
 git merge dev_Cousins_SarahJ

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
