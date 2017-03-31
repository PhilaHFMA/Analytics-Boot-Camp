################################################################################
#
#  Filename:          AnalyticsBootCamp.R
#  Client:            HFMA Analytics Boot Camp
#  Project:           R Introduction
#  Contact:           Shan Muthersbaugh (smuthersbaugh@kpmg.com)
#  Created:           2017-03-22
#  Description:       R Intorduction showing how to load data,merges/joins and 
#                     basic Exploitory Anlaysis
#
#  Input data:      * BadMembers.csv ,Claims.csv,DRGs.csv
#                     ,MemberMonths.csv,Members.csv
#  Output data:     * None
#
################################################################################


################################################################################
# install pacakges if pacakges do not exist
################################################################################
list.of.packages <- c("devtools","lubridate","tidyr","stringr","dplyr",
                      "Hmisc",'ggplot2',"sqldf","RODBC","rio","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# to loaded an R package use the library function
# Load dplyr pacakge
library(dplyr)
# help
?library 

# Some basic R command
# this return the second package name from list.of.packages list
list.of.packages[2]

# c() funcion is called combine/concatenate
a <- c(1,2,3,4,5,6)

# you can do addition with the numeric vector a
a[3] + a[2]

# Arithmetic base functions
sum(a)
mean(a)
sqrt(a)
median(a)
prod(a[2:4])


# The next section is how to load the data provided for the analytics boot camp

################################################################################
# initialize global varaiables
################################################################################
# print the current work directory
getwd()
# change this to the parent directory of where the data is stored
# remember to use \\ if you are ona windows computer
WORK_DIR = c("C:\\Users\\smuthersbaugh\\Documents\\HFMA Anlaytics Boot Camp 20170329")
setwd(WORK_DIR) # set working directory 
# change this to the sub directory that is in the work directory
# my data is saved in data\\raw which in the
# C:\\Users\\smuthersbaugh\\Documents\\HFMA Anlaytics Boot Camp 20170329directory
DATA_FILE_PATH = "data\\raw"
# print the current work directory
getwd()

################################################################################
# import data
################################################################################

# Import Claims.csv
# read in first 200 rows, let R auto-detect data types
# function file.path joins the variable DATA_FILE_PATH and "Claims.csv" to complete
# windows file path
sampledf <- read.csv(file.path(DATA_FILE_PATH,"Claims.csv")
                     ,stringsAsFactors=FALSE
                     , header=T
                     , nrows=200)

# call help documentation for read.csv
?read.csv

# str: Compactly Display the Structure of an Arbitrary R Object
str(sampledf)

# head and tail: Returns the first or last parts of a vector, matrix, table, data frame 
head(sampledf)

tail(sampledf,10) # show bottom 10 rows

# examine record with leading zero, missing leading zero
# "4484","11","71-Carrier Non-DME","2014-08-29","2014-08-29","05","","00810","68.030000000000001"
sampledf[32,]

# create a object with class types
# sapply {base}: Apply a Function over a List or Vector
colclasses <- sapply(sampledf,class)
str(colclasses)

# this is a simple for loop
# use a For loop to assign character type to all columns
# the function names creates a list of columns names for the dataframe sampedf
# output from names
# > names(sampledf)
# [1] "ClaimNumber"       "BeneNumber"        "ClaimType"         "ClaimFromDate"     "ClaimThruDate"    
# [6] "ProviderSpecialty" "DRGcode"           "HCPCScode"         "NewPaidAmount"  

for (i in names(sampledf)){
  colclasses[names(colclasses) == i] <- "character"
}

# to delete an R object use rm
# delete colclasses object
rm("colclasses")

# the one liner version is to use rep
# rep {base}: Replicate Elements of Vectors and Lists
colclasses <- rep(c("character"), length(names(sampledf)))
 
# Now  lets read in the full data set using colclasses to assign data types
Claims <- read.csv(file.path(DATA_FILE_PATH,"Claims.csv"),stringsAsFactors=FALSE
                   , header=T,na.strings=c("NA","NaN", ""), colClasses=colclasses)
# print the first 10 rows
head(Claims,10)

# print the bottom 10 rows
tail(Claims,10)

str(Claims)

# examine record with leading zero
Claims[32,]

# we can see below that the leading zero was not dropped becasue we assigned the classes
# and did not let read.csv function automatically determine the data types.
# > Claims[32,]
# ClaimNumber BeneNumber          ClaimType ClaimFromDate ClaimThruDate ProviderSpecialty DRGcode
# 32        4484         11 71-Carrier Non-DME    2014-08-29    2014-08-29                05    <NA>
#   HCPCScode      NewPaidAmount
# 32     00810 68.030000000000001


# Now we need to convert NewPaidAmount numeric data type
Claims[,'NewPaidAmount'] <- as.numeric(Claims[,'NewPaidAmount'])
# another way of doing writing this is the following
# Claims$NewPaidAmount <- as.numeric(Claims$NewPaidAmount)
str(Claims)

# Now we need to Convert Dates columns to date an R date format
# the lubridate pacakge make handling dates a lot easier when using
# R
library(lubridate)
Claims$ClaimFromDate <- ymd(Claims$ClaimFromDate)
Claims$ClaimThruDate <- ymd(Claims$ClaimThruDate)
str(Claims)

head(Claims)

tail(Claims,10)

# Now lets read in all the files
# first lets create a simple function to perform the task
# this will reduce the amout code to write
# functions

#' Read in data and assign charater data type
#' to all columns
#' 
#' @param file_path : path to the files to read into dataframes
#' @param file_name : name of the file to import
#'
#' @return data frame
#' @examples
#' DATA_FILE_PATH = "data\\raw"
#' read_data(DATA_FILE_PATH,'DRGs.csv')
read_data <- function(file_path,file_name){
  # read in the first line of data, so that we can get the number of columns
  sampledf <- read.csv(file.path(file_path,file_name),stringsAsFactors=FALSE, header=T, nrows=1)
  # no lets convert all the columns to character
  colclasses <- rep(c("character"), length(names(sampledf)))
  # read in the data and return a dataframe
  return(read.csv(file.path(file_path,file_name),stringsAsFactors=FALSE
                  , header=T,na.strings=c("NA","NaN", ""), colClasses=colclasses))
}

# for help on function
help("function")

# BadMembers.csv ,DRGs.csv
# ,MemberMonths.csv,Members.csv
DRGs <- read_data(DATA_FILE_PATH,'DRGs.csv')
DRGs[,c(8,9,10)] <- sapply(DRGs[, c(8,9,10)], as.numeric)

BadMembers <- read_data(DATA_FILE_PATH,'BadMembers.csv')
BadMembers$DOB <- ymd(BadMembers$DOB)

MemberMonths <- read_data(DATA_FILE_PATH,'MemberMonths.csv')
MemberMonths$MonthDate <- mdy(MemberMonths$MonthDate)

Members <- read_data(DATA_FILE_PATH,'Members.csv')
Members$DOB <- ymd(Members$DOB)

# if you want to download a file from github and read it into data frame
# the next steps show how to do that
# just uncomment the R code, starting at line with library(rio)
# download from github using the rio package
# I really like th rio packge it can read in all kinds of diffrent formatted data
# if you do the run the code below to download from github, column names will be diffrent
# you will have to change column names in the code below to get it to work
# library(rio)
# Claims <- import("https://raw.githubusercontent.com/PhilaHFMA/Analytics-Boot-Camp/master/Claims.csv",stringsAsFactors=FALSE,na.strings=c("NA","NaN", ""))
# Claims$ClaimFromDate <- ymd(Claims$ClaimFromDate)
# Claims$ClaimThruDate <- ymd(Claims$ClaimThruDate)
# Claims[,'NewPaidAmount'] <- as.numeric(Claims[,'NewPaidAmount'])
# 
# DRGs <- import("https://raw.githubusercontent.com/PhilaHFMA/Analytics-Boot-Camp/master/DRGs.csv",na.strings=c("NA","NaN", ""))
# DRGs[,c(8,9,10)] <- sapply(DRGs[, c(8,9,10)], as.numeric)
# 
# BadMembers <- import("https://raw.githubusercontent.com/PhilaHFMA/Analytics-Boot-Camp/master/BadMembers.csv",na.strings=c("NA","NaN", ""))
# BadMembers$DOB <- ymd(BadMembers$DOB)
# 
# MemberMonths <- import("https://raw.githubusercontent.com/PhilaHFMA/Analytics-Boot-Camp/master/MemberMonths.csv",na.strings=c("NA","NaN", ""))
# MemberMonths$MonthDate <- mdy(MemberMonths$MonthDate)
# 
# Members <- import("https://raw.githubusercontent.com/PhilaHFMA/Analytics-Boot-Camp/master/Members.csv",na.strings=c("NA","NaN", ""))
# Members$DOB <- ymd(Members$DOB)

################################################################################
# Summary Statistics
################################################################################

# Base function to create summary statistics
summary(Claims)
# generates really nice summary output really nice summary output
library(Hmisc)
Hmisc::describe(Claims)

Hmisc::describe(DRGs)

Hmisc::describe(BadMembers)

Hmisc::describe(MemberMonths)

Hmisc::describe(Members)


################################################################################
# Merge/joins
################################################################################
?merge
# http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
# Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
# 
# Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
# 
# Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
# 
# Cross join: merge(x = df1, y = df2, by = NULL)

# unique function returns a list of unique values
unique(Claims$DRGcode)
unique(DRGs$MS.DRG)

# Left join
# the stringr package is areally good package for working with strings
library(stringr)
# we need to keep only the right 3 characters for join the data
# we are creating a new column to match the column in DRGs dataframe.
Claims$MS.DRG <- str_sub(Claims$DRGcode, -3,-1)
#left join
# to do the left join in base R we use the parameter all.x and set it 
# to TRUE
sampleClaims <- merge(Claims,DRGs,by="MS.DRG",all.x = TRUE)

# Do not run
# THis is just an example, I always have trouble using this package
# but you can try it out, just uncomment the R code
# library(sqldf)
# the sqldf package mimcs SQL commands
# # clean . from names
# names(Claims) <- gsub( ".",  "", names(Claims), fixed = TRUE)
# names(DRGs) <- gsub( ".",  "", names(DRGs), fixed = TRUE)
# 
# df <- sqldf("SELECT * FROM Claims as A
#           LEFT JOIN DRGs  as B
#             ON A.MSDRG = B.MSDRG")
# 
# # if your data has a difftime class use method = "name__class"
# 
# df <- sqldf("SELECT * FROM Claims as A
#           LEFT JOIN DRGs  as B
#           ON A.MSDRG = B.MSDRG", method = "name__class")

# dplyr
# http://stat545.com/bit001_dplyr-cheatsheet.html#why-the-cheatsheet
sampleClaims <- left_join(Claims, DRGs,by=c("MS.DRG")) 

## INNER JOIN is a filter
sampleClaims_inner <- inner_join(Claims, DRGs,by=c("MS.DRG"="MS.DRG"))

# if you want to remove more than one object use
# list =c() inside of the rm function
rm(list=c("sampleClaims_inner"))

################################################################################
# Filtering rows
################################################################################
# base R
filtered_base <- sampleClaims
filtered_base[["DRGcode"]][is.na(filtered_base[["DRGcode"]])]<- '0000'
#filtered_base$DRGcode[is.na(filtered_base$DRGcode)] <- '0000'
filtered_base <- filtered_base[filtered_base$DRGcode != '0000',]
 
# dplyr
# we need to keep only the right 3 characters for join the data
# we are creating a new column to match the column in DRGs dataframe.
Claims <- Claims %>% mutate(MS.DRG = str_sub(DRGcode, -3,-1))
# now we can pipe together fuctions 1) join the data, 2) create new column
# 3) filter the data
sampleClaims <- left_join(Claims, DRGs,by=c("MS.DRG" ="MS.DRG")) %>% 
                    mutate(DRGcode = ifelse(is.na(DRGcode),'0000',DRGcode)) %>% 
                      filter(DRGcode != '0000')

# Filter down to medical DRGs
sampleClaims_med <- left_join(Claims, DRGs,by=c("MS.DRG" ="MS.DRG")) %>% 
                  mutate(DRGcode = ifelse(is.na(DRGcode),'0000',DRGcode)) %>% 
                    filter(DRGcode != '0000' & TYPE != "MED")

# magrittr pacakge this is really usefule package
# it allows you to pipe like dplyr but using base R fucntions
# 
# https://github.com/tidyverse/magrittr
Claims <- Claims %>%  transform(MS.DRG = str_sub(DRGcode, -3,-1))

# 1) merge the data 2) create new column 3) filter 4) head so that only
# the first 10 records are return
sampleClaims_med <- merge(Claims,DRGs,by="MS.DRG",all.x = TRUE) %>%
                      transform(DRGcode = ifelse(is.na(DRGcode),'0000',DRGcode)) %>%
                      subset(DRGcode != '0000'& TYPE != "MED") %>%
                      head(10)

# sampleClaims_med will have 10 records
rm(list=c("sampleClaims_med"))

################################################################################
# Calcualtions
################################################################################
## Calculate LOS
# Base R
Claims$LOS <- as.numeric(difftime(Claims$ClaimThruDate,Claims$ClaimFromDate, units = "days"))

# dplyr
sampleClaims <- sampleClaims %>% mutate(LOS = as.numeric(difftime(ClaimThruDate,ClaimFromDate, units = "days")))

################################################################################
# Plots
################################################################################
library(ggplot2)
library(scales)

# create a dataframe that contains aggergated dataframe with
# averages for LOS by TYPE,DRGandTitle
avg_los <- sampleClaims %>% 
            select(TYPE, DRGandTitle,LOS) %>%
            group_by(TYPE,DRGandTitle) %>%
            summarise(LOS_AVG = mean(LOS))

ggplot(data=avg_los, aes(x=DRGandTitle,y=LOS_AVG,fill=TYPE)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Average Length of Stay by DRGs")

# same plot using a black and white theme
ggplot(data=avg_los, aes(x=DRGandTitle,y=LOS_AVG,fill=TYPE)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Average Length of Stay by DRGs") + scale_fill_grey() +
  theme_bw()

# calculate total paid grouping by claimfromdate
tot_amount <- sampleClaims %>% 
            select(ClaimFromDate,NewPaidAmount) %>%
            group_by(ClaimFromDate) %>%
            summarise(Total_Paid = sum(NewPaidAmount))
# scatter plot
ggplot(aes(x = ClaimFromDate, y = Total_Paid), data = tot_amount) + geom_point()
# line plot
ggplot(aes(x = ClaimFromDate, y = Total_Paid), data = tot_amount) + geom_line()


################################################################################
# Using R with SQL Server
################################################################################
library(RODBC)
## Not run: 
# channel <- odbcConnect("test")
# sqlSave(channel, USArrests, rownames = "state", addPK=TRUE)
# sqlFetch(channel, "USArrests", rownames = "state") # get the lot
# foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
# foo[1,2] <- 222
# sqlUpdate(channel, foo, "USArrests")
# sqlFetch(channel, "USArrests", rownames = "state", max = 5)
# sqlDrop(channel, "USArrests") 
# close(channel)
## End(Not run
dbhandle <- odbcDriverConnect('driver={SQL Server};server=US6608558-X7002\\EAUDIT;database=R_AnalyticsBootCamp;trusted_connection=true')

# changed the errors parameter to FALSE
# if set to TRUE its halt the program
# if set to FALSE will return -1
check <- sqlDrop(dbhandle, "SampleClaims",errors = FALSE)
if (check == -1){
  print("Table not found.")
}
# use varTypes for R dates
varTypes = c(ClaimFromDate ="date",ClaimThruDate ="date")
sqlSave(dbhandle,Claims,tablename="SampleClaims",rownames=FALSE,append=FALSE,varTypes=varTypes)

check <- sqlDrop(dbhandle, "DRGs",errors = FALSE)
if (check == -1){
  print("Table not found.")
}
# to save dataframe SQL server
sqlSave(dbhandle,DRGs,tablename="DRGs",rownames=FALSE,append=FALSE)

# returns all the tables in the connected scheme
sqlQuery(dbhandle, 'select * from information_schema.tables')

## Extracting data from a table
res <- sqlQuery(dbhandle, "SELECT TOP 1000 ClaimNumber
                          ,BeneNumber
                          ,ClaimType
                          ,ClaimFromDate
                          ,ClaimThruDate
                          ,ProviderSpecialty
                          ,DRGcode
                          ,HCPCScode
                          ,NewPaidAmount
                          FROM dbo.SampleClaims")

head(res)

#left join
res <- sqlQuery(dbhandle, "SELECT * FROM dbo.SampleClaims as A
                            LEFT JOIN dbo.DRGs as B
                            ON B.MSDRG = RIGHT(A.DRGcode,3)
                            WHERE ISNULL(DRGcode,'0000') <> '0000'")

head(res)

# Summarize data by highest-paid Reference.dbo.DRGs
res <- sqlQuery(dbhandle, "SELECT DRGcode, SUM(cast(NewPaidAmount as float)) AS SumPaid
                            FROM dbo.SampleClaims
                            WHERE ISNULL(DRGcode,'0000') <> '0000'
                            GROUP BY DRGcode
                            ORDER BY SUM(cast(NewPaidAmount as float)) desc")
head(res)
# close the connection to sql server
close(dbhandle)



