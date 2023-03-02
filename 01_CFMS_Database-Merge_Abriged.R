##Script created to clean and piece together ASI data
### Notes from RS (Jan. 2022): try replicating this with recent data (eg from 2021)


#install.packages("pillar")## load this package before 'car' if 'car package is not loading properly
#library(pillar)
#install.packages("car")## this package allows you to use the function 'recode'
library (car)
#install.packages("magrittr") ## allows you to use '%>%' when seperating 03-12 data for changing codes
library(magrittr)
# install.packages("tidyverse")
# library(tidyverse)
## RS: can't seem to install tidyverse set but the associated packages all seem to be available already
install.packages("dplyr")## allows you to use the function 'mutate' on banding date 2014 and 2015 to add season variable and 'tidyr' in 2016 data to separate dates into month day year variables
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
#install.packages("readxl")
library(readxl)

####BANDING####
#load banding data 
#band92.02 <- read.csv("CFMS92_2002.csv", header=T, strip.white=T, as.is=T)
#band03.12 <- read.csv("CFMS03_2012.csv", header=T, strip.white=T, as.is=T)
#band13 <- read.csv("CFMS2013.csv", header=T, strip.white=T, as.is=T)
#band14 <- read.csv("CFMS2014.csv", header=T, strip.white=T, as.is=T) #manually edited some variables
#band15 <- read.csv("CFMS2015.csv", header=T, strip.white=T, as.is=T)
#band16 <- read.csv("CFMS2016.csv", header=T, strip.white=T, as.is=T)

band19 <- read.csv("CFMS2019.copy.csv", header=T, strip.white=T, as.is=T)
band20 <- read.csv("CFMS2020.copy.csv", header=T, strip.white=T, as.is=T)
band21 <- read.csv("CFMS2021.copy.csv", header=T, strip.white=T, as.is=T)
band92_18 <- read.csv("CFMS92_18.copy.csv", header=T, strip.white=T, as.is=T)

# RS annotation: Would suggest making *copies* of 2019-2021 data to save on my file directory

# remove some variables:

# RS current plan March 2022: Modify column names directly in .csv copies for 2019-2021 data to match those in 1992-2018 data
# Can sort out inconsistencies and unique columns in R later on

## Code to remove some existing columns, I ended up removing some manually before importing. I originally set it up to name files "raw" upon import, before trimming them down
#band19 <- subset(band19.raw, select=-c(Page..,flat_wing,nest_id:submit_time))
#band20 <- subset(band20.raw, select=-c(Page..,sample_id, flat_wing,nest_id:submit_time))
#band21 <- subset(band21.raw, select=-c(Band.Year.Page))

## RS: May need to change a number of variables to factors
# 2019: Doesn't seem to be any returns for band code ... follow up on this?
# Same goes for 2020 ... all Recaps are reported as 2. Might need to sift through this later, but for now we'll go with it


band19_20 <- rbind.fill(band19, band20)
str(band19_20)
# 3/10: check FFWear, RectWear, RectShape, RectMolt

# Need to split Molt Limits into separate columns for 19, 20 data
# Also remove zeroes

# Try "separate" function from tidyr package


band19_20 <- separate(band19_20, col=MOLT.LIMITS, into=c('MOLT.LIMIT.1', 'MOLT.LIMIT.2', 'MOLT.LIMIT.3'), sep=',')

# band19_20 %>% separate(MOLT.LIMITS, c('MOLT.LIMIT.1', 'MOLT.LIMIT.2', 'MOLT.LIMIT.3'))

band19_21 <- rbind.fill(band19_20, band21)
str(band19_20)

# Check duplicates


# Can convert ML columns to factors and check levels to see what needs to be modified

# band19_20$MOLT.LIMIT.1 <- as.factor(band19_20$MOLT.LIMIT.1)
# levels(band19_20$MOLT.LIMIT.1)


## Will probably best to convert some of the 19_21 variable levels to 92_18 version (eg band code) before merging files



#######band92_21 <- rbind.fill(band92_18, band19_21)
str(band92_21)

#change Code for clarity
#band92.02$Code <- as.factor(band92.02$Code)

band19_21$Code <- as.factor(band19_21$Code)
# levels(band19_21$Code)

band19_21$Code <- recode(band19_21$Code, '1'="new", '2'="recap", '3'="foreign", '4'="destroy", '5'="reband", '6'="return", '7'="inreturn", '8'="lost", '9'="unbanded")


# Working on dates/times
# Time formatted as character in 2021 data, but integer on 2019, 2020 data **needs attention fall 2022**
#fix date, change Date from this format mm/dd/yy to mm/dd/yyyy

library(lubridate)
# change date to full mm/dd/yyyy format ... looks like I may need to do the same for 92-18 data
# band19_21$Date <- format(as.Date(band19_21$Date, format=guess_formats(band19_21$Date, c('mdy'))))
# having trouble re running this code, strangely. Not sure why 

band19_21$Date <- as.Date(band19_21$Date, "%m/%d/%y")
# converts to yyyy-mm-dd

#check out age

band19_21$Age <- as.factor(band19_21$Age)
levels(band19_21$Age)

# Already coded alphabetically for some years

# band92.02$Age <- as.factor(band92.02$Age)
#Changes age to U, HY, and AHY ## AHS did not do this, left ages alone so she could archive the data. 

band19_21$Age <- recode(band19_21$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")
levels(band19_21$Age)

## RS notes: age codes will be important to suss out, probably depends on type of analysis I'd want to do. But for starters, especially w/ fall data I'd want to use broad categories.
## Will also need to approach use of spring vs. fall data


# Modify "Time" levels
# Time initially entered numerically in excel, could just convert the column format in excel files 

library(stringr)
## Older versions for converting time 
 band19_21$Time <- str_pad(band19_21$Time, width=4, side="left", pad="0")
# Older version:
# band19_21$Time <- (strftime(band19_21$Time, format="%H:%M:%S"))

## Other attempts to modify time values 
band19_21$Time <- as.POSIXct(sprintf("%04s", band19_21$Time), format="%H%M")
band19_21$Time <- format(band19_21$Time, format="%H:%M:%S")


#create JDate variable for julian date
band19_21$JDate <- yday(band19_21$Date)

#########
# Progress as of 4/21
#########
#########

write_csv(band19_21, "CFMS19_21.csv")

#########
#########

## Work on modifying earlier 92-18 data **(SKIP THIS ONCE I'VE MADE THESE ADJUSTMENTS)

band92_18$MOLT.LIMIT.1 <- as.factor(band92_18$MOLT.LIMIT.1)
levels(band92_18$MOLT.LIMIT.1)
band9218_2 <- band92_18[band92_18$MOLT.LIMIT.1 %in% c("")]


## Merge with 92-18 dataset

band92_21 <- rbind.fill(band92_18, band19_21)


############################

# Incorporating additional single-year data files
# After years 92 through 21 have been merged together (All Years Dataframe.R)

# 2022 data:

band92_21 <- read.csv("CFMS92_21.csv")
band92_21[is.na(band92_21)] <- ""

band22 <- read.csv("CFMS22.copy.csv")
band22[is.na(band22)] <- ""       

# Subsequent individual year files can just have field names changed manually as needed

band22$Code <- as.factor(band22$Code)
# levels(band22$Code)

band22$Code <- recode(band22$Code, '1'="new", '2'="recap", '3'="foreign", '4'="destroy", '5'="reband", '6'="return", '7'="inreturn", '8'="lost", '9'="unbanded")


band22$Age <- as.factor(band22$Age)
levels(band22$Age)

band22$Age <- recode(band22$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")
levels(band22$Age)

band22$Time <- str_pad(band22$Time, width=4, side="left", pad="0")
band22$Time <- as.POSIXct(sprintf("%04s", band22$Time), format="%H%M")
band22$Time <- format(band22$Time, format="%H:%M:%S")
# str(band22$Time)

band22$Date <- format(as.Date(band22$Date, format=guess_formats(band22$Date, c('mdy'))))
band22$Date <- as.Date(band22$Date, "%m/%d/%y")


#create JDate variable for julian date
band22$Date2 <- as.POSIXlt((band22$Date), format="%m/%d/%Y")
str(band22$Date2) <- NULL

band22$JDate <- yday(band22$Date2)
band22$Date2 <- NULL

# Now merge

band92_22 <- rbind.fill(band92_21, band22)

# Looks good. Updated all-years dataset complete! See:"All Years Dataframe" file for further cleaning

write.csv(band92_22, "CFMS92_22.csv", row.names =FALSE)

band92_22 <- read.csv("CFMS92_22.csv")
band92_22[is.na(band92_22)] <- ""
