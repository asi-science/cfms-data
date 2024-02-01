# All Years Net Hour Merging

# Clean up 2016 and 2017 files ####

## 2016 cleaning and prep ####

# Read spring and fall:
library(readxl)
ne16.f <- read_excel("Net Effort Data/Net Effort 2016 Copy - Unclean.xls", 
                        sheet = "Fall", na = "\"\"")

ne16.s <- read_excel("Net Effort Data/Net Effort 2016 Copy - Unclean.xls", 
                        sheet = "Spring", na = "\"\"")

#Reformat open and close times into intuitive formats. I don't really need to bother defining them as a specific date time class yet. 
# A little messy:

ne16.s$open <- sub("1899-12-31 ", "", ne16.s$open)  
ne16.s$close <- sub("1899-12-31 ", "", ne16.s$close)

ne16.f$open <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne16.f$open)
ne16.f$close <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne16.f$close)

ne16.f$open <- as.POSIXct(ne16.f$open, format="%H:%M")
ne16.f$open <- sub("2023-11-22", "", ne16.f$open)

ne16.f$close <- as.POSIXct(ne16.f$close, format="%H:%M")
ne16.f$close <- sub("2023-11-22", "", ne16.f$close)


# Adjust spring dataframe. Add a number_nets column for time conversions:

ne16.s$Number_Nets <- ""
ne16.s$Number_Nets <- as.numeric(ne16.s$Number_Nets)

# Some close times are missing. This includes everything on 4-24. That could be addressed later by adding times directly into cleaned file, if we can determine what close time for that date was...
# Otherwise, there are a couple nets on ne16 <- read_excel("Net Effort Data/Net_Effort_2016.xlsx", na = "\"\"") dates with missing close times. For all of these, I can assume close time is same as listed for other nets in area/station-wide

# Define net_numbers for each net, ass missing close times:
ne16.s$date <- as.factor(ne16.s$date)
library(dplyr)
ne16.s <- ne16.s %>% 
  mutate(Number_Nets = case_when(net %in% c(1,2,4,6,11, 13, 14, 17, 18, 20, 21, 22, 23, 24, 26, 27, 28, 30) ~ 1.0,
                                 net %in% c(3, 5, 19) ~ 2.0,
                                 net %in% c(7, 29) ~ 0.5,
                                 net==25 ~ 3.0,
                                 TRUE~Number_Nets)) %>%
  mutate(close = case_when(date=='2016-04-25' & net==11 ~ "12:00:00",
                           date=='2016-04-28' & net==23 ~ "12:00:00",
                           date=='2016-04-29' & net==22 ~ "12:05:00",
                           date=='2016-05-03' & net %in% c(20,21) ~ "13:05:00",
                           date=='2016-05-04' & net==30 ~ "12:00:00",
                           date=='2016-05-05' & net==13 ~ "12:00:00",
                           TRUE~close))



# Merge the files together now:
ne16 <- rbind.fill(ne16.f, ne16.s)

# Some open-close times are not very consistent per station norms on other years. These should be changed to closest 5-min interval for consistency, and changed to 24-hr time where necessary
unique(ne16$open)
unique(ne16$close)

# open - convert to nearest 5-min interval: 5:56, 6:11, 6:52, 6:56, 10:36, 7:36, 8:51, 11:06, 8:49, 8:59, 10:27, 8:57, 8:52, 8:47
# close - 1:03, 1:08

ne16 <- ne16 %>%
  mutate(open = case_when(open== " 05:56:00" ~ "05:55:00",
                          open == " 06:11:00" ~ "06:10:00",
                          open %in% c("06:49:00", " 06:52:00") ~ "06:50:00",
                          open == " 10:36:00" ~ "10:35:00",
                          open == " 07:36:00" ~ "07:35:00",
                          open %in% c("08:51:00", "08:49:00", "08:52:00") ~ "08:50:00",
                          open == "11:06:00" ~ "11:05:00",
                          open == "08:59:00" ~ "09:00:00",
                          open == "08:47:00" ~ "08:45:00",
                          open == "08:57:00" ~ "08:55:00",
                          open== "10:27:00" ~ "10:25:00",
                          TRUE~open)) %>%
  mutate(close = case_when(close %in% c("11:58:00", "12:01:00", "12:02:00") ~ "12:00:00",
                           close == "01:08:00" ~ "13:10:00",
                           close == "01:03:00" ~ "13:00:00",
                           TRUE~close))

# Now to calculate net durations and adjust to net hours by net qty
# Start by converting to workable datetime formats
ne16$open2 <- as.POSIXct(ne16$open, format="%H:%M:%S")
ne16$close2 <- as.POSIXct(ne16$close, format="%H:%M:%S")

ne16$Duration <- NULL
ne16$Total_Time <- NULL

# Calculate numeric time diff between close and open times, and multiply by net qty to calculate total net hrs:
ne16 <- ne16 %>%
  mutate(across(c(open2, close2), lubridate::ymd_hms),
         Duration = as.numeric(difftime(close2, 
                                          open2, units = 'hours')))

ne16$Total_Time <- ne16$Duration*ne16$Number_Nets

# Clean, remove dummy fields, and write file:
ne16 <- select(ne16, net, Number_Nets, date, open, close, Duration, Total_Time)
ne16$date <- as.Date(ne16$date)

library(xlsx)
write.xlsx(ne16, "Net Effort Data/Net_Effort_2016.xlsx", 
           row.names=FALSE)
# All done!

## 2017 cleaning and prep ####

# Note that 4/25 and 5/9 apparently are missing all data. Also the stacked nets separated into distinct rows
# Meanwhile, 2017 fall effort file on the laptop was incomplete. I had to access a version on Google Drive for the remaining days of effort--but since this has different formatting, I copied this into a separate tab on the source
# Time formats on the second half of that fall data were reformatted directly in Excel

# Read spring and fall:
library(readxl)
ne17.f <- read_excel("Net Effort Data/Net Effort 2017 Copy - Unclean.xlsx", 
                     sheet = "FALL", na = "\"\"")
ne17.f2 <- read_excel("Net Effort Data/Net Effort 2017 Copy - Unclean.xlsx", 
                     sheet = "FALL-2", na = "\"\"")
ne17.s <- read_excel("Net Effort Data/Net Effort 2017 Copy - Unclean.xlsx", 
                     sheet = "SPRING", na = "\"\"")


# First add net qty to spring df
unique(ne17.s$net)
library(dplyr)
ne17.s <- ne17.s %>% 
  mutate(Number_Nets = ifelse(net %in% c(7, 29), 0.5, 1.0))
      
# Fall-2: remove blank rows:
ne17.f2 <- ne17.f2[!is.na(ne17.f2$open),]

ne17.f2$open <- sub("1899-12-31 ", "", ne17.f2$open)  
ne17.f2$close <- sub("1899-12-31 ", "", ne17.f2$close)

# Tweak open/close time formats:
# fall
ne17.f$open <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne17.f$open)
ne17.f$close <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne17.f$close)

ne17.f$open <- as.POSIXct(ne17.f$open, format="%H:%M")
ne17.f$open <- sub("2023-11-22", "", ne17.f$open)
ne17.f$close <- as.POSIXct(ne17.f$close, format="%H:%M")
ne17.f$close <- sub("2023-11-22", "", ne17.f$close)

# spring
ne17.s$open <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne17.s$open)
ne17.s$close <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne17.s$close)

ne17.s$open <- as.POSIXct(ne17.s$open, format="%H:%M")
ne17.s$open <- sub("2023-11-22", "", ne17.s$open)
ne17.s$close <- as.POSIXct(ne17.s$close, format="%H:%M")
ne17.s$close <- sub("2023-11-22", "", ne17.s$close)

# Now merge together:
ne17 <- rbind.fill(ne17.s, ne17.f, ne17.f2) 

# Adjust weird open/close times as with 2016 data:
unique(ne17$open) # - 8:56, 8:52, 8:59, 5:56, 6:06, 6:52
unique(ne17$close)

ne17 <- ne17 %>%
  mutate(open = case_when(open== " 08:56:00" ~ "08:55:00",
                          open == " 06:06:00" ~ "06:05:00",
                          open == " 08:52:00" ~ "08:50:00",
                          open == "08:59:00" ~ "09:00:00",
                          open == " 05:56:00" ~ "05:55:00",
                          open == "06:52:00" ~ "06:50:00",
                          TRUE~open))


ne17$open2 <- as.POSIXct(ne17$open, format="%H:%M:%S")
ne17$close2 <- as.POSIXct(ne17$close, format="%H:%M:%S")

ne17$Duration <- NULL
ne17$Total_Time <- NULL

# Calculate numeric time diff between close and open times, and multiply by net qty to calculate total net hrs:
ne17 <- ne17 %>%
  mutate(across(c(open2, close2), lubridate::ymd_hms),
         Duration = as.numeric(difftime(close2, 
                                        open2, units = 'hours')))

ne17$Total_Time <- ne17$Duration*ne17$Number_Nets

# Clean, remove dummy fields, and write file:
ne17 <- ne16 <- read_excel("Net Effort Data/Net_Effort_2016.xlsx", na = "\"\"")(ne17, net, Number_Nets, date, open, close, Duration, Total_Time)
ne17$date <- as.Date(ne17$date)

library(xlsx)
write.xlsx(ne17, "Net Effort Data/Net_Effort_2017.xlsx", 
           row.names=FALSE)
# After writing this file, I noted that 9/27 and 9/29 dates had 2019 for the year. This was corrected directly in file
#
#

# Notes on preparing all-years effort file ####
# Nov 2023

# Each individual year through 2015 has an edited net effort file, prepared by AHS for migration timing analyses. These were found in a subfolder in the CFMS\Sue's Laptop directory on the banding laptop
## These files have the core info needed for net effort by day and net. Field names are consistent, and the 'HRS_Open' is the true net hour per net, corresponding to 'Total_Time' in the post 2016 dfs
## Open/close times are generally included up until about 1998, but not regularly after that
## Open/close weather and some other variables included in earlier files; these aren't needed for net effort compiling
## Likewise, some other variables like Julian date have been calculated in the pre-2016 dfs. They also have 'Season' field, as well as separate M/D/Y fields

# UPDATE: the files mentioned above are generally limited to fall data. Similar files including spring/summer data might be found on Google Drive. 
## Download and prepare those files accordingly. They shouldn't need too much cleaning, just need to import the whole-year sheet to R specifically
## Note that 2015 spring data still appear to be missing

# Not too much cleaning will really be needed when bringing all these files together. 
## However, I will plan to pull distinct m/d/y columns for the 2016-23 dfs. Eventually will want to distinguish season

# Due to consistency within 92-15 and 16-23 files, I will first create aggregate dfs for those two respective ranges. They can then be tweaked to match, and then merged

# Once large file is merged and final cleaning done, I can create summary tables/dfs for analysis and data sharing elsewhere (eg total net effort by date)

# Which pre-16 dfs are missing spring data?  2002-04, -2006-2015


# When bringing files together on the next step, may want to pull a separate field for year. Also one for Season eventually

# 
#
#
# Effort Merge Step 1: aggregate (partial) 92-15 and 16-23 data ####
# As of late Nov 2023, accessible cleaned 92-15 yearly files only have spring data up to 2000

## 1a: load and bind 92-15 data ####
# Tried to run a list call to merge clusters of files together, but there's enough difference in column numbers etc that it doesn't quite work
#library(data.table)
#files = list.files('./Net Effort Data/Effort92_04', pattern="*.csv")
#ne92_04.p = do.call(rbind, lapply(files, fread))

ne92 <- read.csv("Net Effort Data/Net_Effort_1992_copy.csv", na="empty")
ne93 <- read.csv("Net Effort Data/Net_Effort_1993_copy.csv", na="empty")
ne94 <- read.csv("Net Effort Data/Net_Effort_1994_copy.csv", na="empty")
ne95 <- read.csv("Net Effort Data/Net_Effort_1995_copy.csv", na="empty")
ne96 <- read.csv("Net Effort Data/Net_Effort_1996_copy.csv", na="empty")
ne97 <- read.csv("Net Effort Data/Net_Effort_1997_copy.csv", na="empty")
ne98 <- read.csv("Net Effort Data/Net_Effort_1998_copy.csv", na="empty")
ne99 <- read.csv("Net Effort Data/Net_Effort_1999_copy.csv", na="empty")
ne00 <- read.csv("Net Effort Data/Net_Effort_2000_copy.csv", na="empty")
ne01 <- read.csv("Net Effort Data/Net_Effort_2001_copy.csv", na="empty") # Reduced number of fields for this dataset
ne02 <- read.csv("Net Effort Data/Net_Effort_2002_copy.csv", na="empty")
ne03 <- read.csv("Net Effort Data/Net_Effort_2003_copy.csv", na="empty")
ne04 <- read.csv("Net Effort Data/Net_Effort_2004_copy.csv", na="empty")
ne05 <- read.csv("Net Effort Data/Net_Effort_2005_copy.csv", na="empty")
ne06 <- read.csv("Net Effort Data/Net_Effort_2006_copy.csv", na="empty")
ne07 <- read.csv("Net Effort Data/Net_Effort_2007_copy.csv", na="empty")
ne08 <- read.csv("Net Effort Data/Net_Effort_2008_copy.csv", na="empty")
ne09 <- read.csv("Net Effort Data/Net_Effort_2009_copy.csv", na="empty")
ne10 <- read.csv("Net Effort Data/Net_Effort_2010_copy.csv", na="empty")
ne11 <- read.csv("Net Effort Data/Net_Effort_2011_copy.csv", na="empty")
ne12 <- read.csv("Net Effort Data/Net_Effort_2012_copy.csv", na="empty")
ne13 <- read.csv("Net Effort Data/Net_Effort_2013_copy.csv", na="empty")
ne14 <- read.csv("Net Effort Data/Net_Effort_2014_copy.csv", na="empty")
ne15 <- read.csv("Net Effort Data/Net_Effort_2015_copy.csv", na="empty")

# Date formatting is a bit mismatched
# Some date issues had to get corrected in original datasets. Eg 1997 had some dates erroneously as 1995. 2014 had a 2012 template date. Fix also for 2013 and 2011 data

# For 2001, extract month, day, and year info
library(stringr)
ne01 <- within(ne01, {
  Month <-  substr(Date, 1, 1)
})

ne01$Day <- format(as.Date(ne01$Date,format="%m/%d/%y"), format = "%d")
# Bit of a roundabout way to do it, but it works. This will be needed later when date format is made consistent in oarger dataframe

# Approach one: create list of loaded datasets to bind. Be mindful of which datasets are currently loaded into environment when using the call below
ne.list <- lapply(ls(pattern="ne+"), function(x) get(x))

# data.table command has some dependencies I can specify for the unequal column lengths. This can help identify mismatches in names for columns of interest
ef92_15 = data.table::rbindlist(ne.list, fill=TRUE)

# Made a few manual revisions in source files. Include 1998 data: had Open2 + Close2 columns for reopening times, instead created new rows to reflect bouts each respective net was open on same day
# 'HRS' column is equivalent to 'Duration' in later data. 'No. Hours' is numeric time duration and can be ignored

# Lots of 'empty' rows with 'NA' characters throughout, get rid of them. Also remove net hour columns with 'NA'. For this dataset, we're ignoring 'negative' net data (ie rows for nets that weren't run on a given day)
# However, there are also some records I need to keep with empty values in some columns coded as true NAs, so need to be careful when filtering/subsetting
ef92_15 <- subset(ef92_15, Year!="NA")

# We can get rid of any HRS_Open values of 0 or "NAs"

ef92_15 <- subset(ef92_15, HRS_Open!=0 & HRS_Open!='NA')
# This should be about all the data we want. 

ef92_15 <- select(ef92_15, Date, Month, Day, Year, Net, Net92, Net93, Net94, Net95, Net96, Net97, Num_Nets, Open, Close, HRS, HRS_Open,  Season)

## 1b: load and bind 16-23 data ####

# If individual pre-2016 files are in current environment, remove them:
rm(list=ls(pattern="ne+"))


library(readxl)
ne16 <- read_excel("Net Effort Data/Net_Effort_2016.xlsx", na = "\"\"")
ne17 <- read_excel("Net Effort Data/Net_Effort_2017.xlsx", na = "\"\"")
ne18 <- read_excel("Net Effort Data/Net_Effort_2018.xlsx", na = "\"\"")
ne19 <- read_excel("Net Effort Data/Net_Effort_2019.xlsx", na = "\"\"")
ne20 <- read_excel("Net Effort Data/Net_Effort_2020.xlsx", na = "\"\"")
ne21 <- read_excel("Net Effort Data/Net_Effort_2021.xlsx", na = "\"\"")
ne22 <- read_excel("Net Effort Data/Net_Effort_2022.xlsx", na = "\"\"")
ne23 <- read_excel("Net Effort Data/Net_Effort_2023.xlsx", na = "\"\"")

# Fewer files here and need to fix formatting on some dates and times in 18-23 data as above

# First fix duration on 2022 file
ne22$Duration <- sub("1899-12-31 ", "", ne22$Duration)  

ne18$open <- sub("1899-12-31 ", "", ne18$open)  
ne18$close <- sub("1899-12-31 ", "", ne18$close)

ne21$open <- sub("1899-12-31 ", "", ne21$open)  
ne21$close <- sub("1899-12-31 ", "", ne21$close)

ne22$open <- sub("1899-12-31 ", "", ne22$open)  
ne22$close <- sub("1899-12-31 ", "", ne22$close)

ne23$open <- sub("1899-12-31 ", "", ne23$open)  
ne23$close <- sub("1899-12-31 ", "", ne23$close)

ne19$open <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne19$open)
ne19$close <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne19$close)

ne20$open <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne20$open)
ne20$close <- sub("(\\d+)(\\d{2})", "\\1:\\2",  ne20$close)

# Bind the data, use the list and data.table
ne.list <- lapply(ls(pattern="ne+"), function(x) get(x))
ef16_23 = data.table::rbindlist(ne.list, fill=TRUE)

rm(list=ls(pattern="ne+"))

# Junk column squeezed in 
ef16_23$...8 <- NULL

# Structure of this looks pretty good. I'm not worried about 'Duration' formatting right now. 

# Remove blank net data:
ef16_23 <- subset(ef16_23, Total_Time !=0)

# Will need to recode some net number values for stacked nets for consistency
unique(ef16_23$net)

ef16_23 <- ef16_23 %>%
  mutate(net = case_when(net=="5l" ~ '5L',
                         net=="5u" ~ '5U',
                         net=="3u" ~ '3U',
                         net=="3l" ~ '3L',
                         net=="19u" ~ '19U',
                         net=="19l" ~ '19L',
                         net=="25l" ~ '25L',
                         net=="25m" ~ '25M',
                         net=="25u" ~ '25U', TRUE ~ net))


# Handling date cleaning ####
## PRE-2016 data should have consistent dates before merging with other data
# Current format: # 1994-6: mm/dd/yy  #02-04: dd-mon #97, 06-13: yyyy-dd-yy ... everything else in mm/dd/yyyy format
# Additionally, would recommend adding m/d/y columns to 2016-23 data

ef16_23 <- within(ef16_23, {
  Year <-  substr(date, 1, 4)
  Month <- substr(date, 6,7)
  })

# Datasets are about ready to bind. Match column names and formatting 

# Convert older dates in tidyverse via formatting m d and y as numeric
ef92_15$Month <- as.numeric(ef92_15$Month)
ef92_15$Day <- as.numeric(ef92_15$Day)
ef92_15$Year <- as.numeric(ef92_15$Year)

library(lubridate)
ef92_15 <-  ef92_15 %>% 
  mutate(Date= make_date(Year, Month, Day))

ef92_15 <- ef92_15 %>% 
  dplyr::rename("Number_Nets"="Num_Nets", "Time_Elapsed"="HRS", "Net_Hours"="HRS_Open")

ef16_23 <- ef16_23 %>%
  dplyr::rename("Time_Elapsed"="Duration", "Net_Hours"="Total_Time", "Open"="open", "Close"="close", "Net"="net", "Date"="date")

# may need to add leading 0 to numeric dates in 92-15 data

# Bind 92-15 and 16-23 together ####
# This can serve as the overall framework to clean and add further net eff. data, and also extract dfs for analyses
# 'pef' title represents partial data (no spring for some years)

library(plyr)
pef92_23 <- rbind.fill(ef16_23, ef92_15)

pef92_23$Net_Hours <- as.numeric(pef92_23$Net_Hours)
# Potentially repeated or aggregated net hours in some data,investigate further:

# bighrs <- subset(pef92_23, Net_Hours >= 28)
# rm(bighrs)

# 2014 had stacked net hours multiplied twice! Correct below
# 2006 had a couple days where net 25 hrs were multiplied twice... these will just be corrected in source data

sef14 <- subset(pef92_23, Year=='2014' & Net %in% c('3', '5', '19', '25'))
sef14$Net_Hours <- as.numeric(sef14$Time_Elapsed)

sef14$Time_Elapsed <- sef14$Net_Hours/(as.numeric(sef14$Number_Nets))

temp <- filter(pef92_23, Year!='2014' | !(Net %in% c('3', '5', '19', '25')))

pef92_23 <- rbind.fill(temp, sef14)
rm(temp)

## Write/read merged effort files ####
library(readr)
write_csv(pef92_23, "CFMS_NetEffort_AllYears_Working.csv", na="")

pef92_23 <- read.csv("CFMS_NetEffort_AllYears_Working.csv", na="empty", check.names = FALSE)


# Wrangling and reorganizing 2006-16 spring effort ####
# Start with 2009 data. Note I did some small cleaning in the source files in Excel 
# (eg removing last column of total seasonal hours by net, removing no banding days, removing unused nets

# Reading entire dataset - first example:
ne09s <- read.csv("Net Effort Data/Net_Effort_2009spring_copy.csv", na="empty", check.names = FALSE)

# Basically just need to convert from "wide" format to "short"
# The 2009 spring data appears to not have the appropriate time conversions for stacked and half nets

# Use the 'melt' function, pretty simple:
library(data.table)
ne09s <- melt(setDT(ne09s), id.vars = 1:2, variable.name = "Date", value = "Total_Time")

# Read the other files starting with 2006

ne06s <- read.csv("Net Effort Data/Net_Effort_2006spring_copy.csv", na="empty", check.names = FALSE)
ne07s <- read.csv("Net Effort Data/Net_Effort_2007spring_copy.csv", na="empty", check.names = FALSE)
ne08s <- read.csv("Net Effort Data/Net_Effort_2008spring_copy.csv", na="empty", check.names = FALSE)
# can't find 2010 spring effort, only file on banding laptop is empty
ne11s <- read.csv("Net Effort Data/Net_Effort_2011spring_copy.csv", na="empty", check.names = FALSE)
ne12s <- read.csv("Net Effort Data/Net_Effort_2012spring_copy.csv", na="empty", check.names = FALSE)
# Spring 2014 was not entered on computer either. 2015 does not appear to be available 

ne06s <- melt(setDT(ne06s), id.vars = 1:2, variable.name = "Date", value = "Total_Time")
ne07s <- melt(setDT(ne07s), id.vars = 1:2, variable.name = "Date", value = "Total_Time")
ne08s <- melt(setDT(ne08s), id.vars = 1:2, variable.name = "Date", value = "Total_Time")
ne11s <- melt(setDT(ne11s), id.vars = 1:2, variable.name = "Date", value = "Total_Time")
ne12s <- melt(setDT(ne12s), id.vars = 1:2, variable.name = "Date", value = "Total_Time")

ne.list <- lapply(ls(pattern="ne+"), function(x) get(x))
sef06_12 = data.table::rbindlist(ne.list, fill=TRUE)

rm(list=ls(pattern="ne+"))

sef06_12$Net_Hours <- sef06_12$Number_Nets*sef06_12$Total_Time

# Spring effort now binded. Next: reformat date, add season variable, and extract month, year info etc

sef06_12 <- subset(sef06_12, Total_Time !=0)

sef06_12$Season <- 1

# Extract m d y info
sef06_12$Year <- format(as.Date(sef06_12$Date, format="%m/%d/%Y"), format = "%Y")
sef06_12$Month <- format(as.Date(sef06_12$Date, format="%m/%d/%Y"), format = "%m")
sef06_12$Day <- format(as.Date(sef06_12$Date, format="%m/%d/%Y"), format = "%d")

# Old version:
#library(stringr)
#sef06_12 <- within(sef06_12, {
 # Month <-  substr(Date, 1,1)
  #Year <- str_sub(Date, -4)
#})

sef06_12 <-  sef06_12 %>% 
  mutate(Date=make_date(Year, Month, Day))

sef06_12 <- sef06_12 %>% 
  dplyr::rename("Time_Elapsed"="Total_Time")

## Bind spring 06-12 data with all-years data ####

pef92_23 <- rbind.fill(pef92_23, sef06_12)

## Add julian date and final cleaning ####

pef92_23$J_Date <- yday(pef92_23$Date)
# This df now includes all daily net effort from 92-23, with the exception of 2010, 2014, and 2015 spring data, and possibly some missing days from 2017


write_csv(pef92_23, "CFMS_NetEffort_AllYears_Working.csv", na="")
pef92_23 <- read.csv("CFMS_NetEffort_AllYears_Working.csv", na="empty", check.names = FALSE)

#
#
# Summarize net effort by date ####

net.sum <- pef92_23 %>%
  group_by(Date) %>%
  reframe(Net_Hours = sum(Net_Hours),
          Month = unique(Month),
          Day = unique(Day),
          Year = unique(Year),
          Season = unique(Season))

write_csv(net.sum, "CFMS_NetEffort_Summary_Partial.csv", na="")

test <- read_csv("CFMS_NetEffort_Summary.csv")
