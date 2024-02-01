###

# Read and merge files ####
#
# Note: in 2023, manually generated a "Date" field combining Mo/Day/Year columns in excel data,
# and deleted a few other unnecessary fields from data entry template (Entered/Proofed initials and date)

# Two approaches. Read in a csv file. This was initial approach. Source file was downloaded from Google Sheets entry file, then saved as a .csv
# Reading as a csv allows for empty cells to stay empty and not be converted to NAs
library(readr)
fall23 <- read.csv("Annual Banding Data/CFMS23-Fall.csv", na="empty")
spring23 <- read.csv("Annual Banding Data/CFMS23-Spring.csv", na="empty")
summer23 <- read.csv("Annual Banding Data/CFMS23-Summer.csv", na="empty")
fall23[is.na(fall23)] <- "" 
spring23[is.na(spring23)] <- "" 
summer23[is.na(summer23)] <- ""

# Alternative is to keep source file as .xslx. This way, can adjust formatting of source file so fields like "Page" don't autocorrect to date formats etc. Would rather fix those issues in Excel rather than R
library(readxl)
spring23 <- read_excel("Annual Banding Data/CFMS23-Spring.xlsx", na="empty", guess_max=10000)
fall23 <- read_excel("Annual Banding Data/CFMS23-Fall.xlsx", na="empty",  guess_max=10000)
summer23 <- read_excel("Annual Banding Data/CFMS23-Summer.xlsx", na="empty",  guess_max=10000)

# most columns appear to have been read with appropriate data formats. However, any fields that are either entirely empty or otherwise have no "zero" values (eg # ticks) may be interpreted as 'logical' data types
# To avoid column types being guessed as logical, may need to manually set column formats as numeric etc in Excel, then defining guess_max operator. Note that any columns entirely empty with this method will still be guessed as 'logical'

library(plyr)
band23 <- rbind.fill(spring23, fall23, summer23)
band23[is.na(band23)] <- ""

library(lubridate)
band23$Date <- format.Date(band23$Date, "%Y-%m-%d") 

# If reading as csv and col_names not specified, may need to manually change values in Sample_Type:
# band23[band23==FALSE] <- "F"

# Write/Read basic compiled 2023 data as excel file ####

library(xlsx)
write.xlsx(band23, "Annual Banding Data/CFMS23.xlsx", row.names=FALSE, showNA=FALSE)

library(readxl)
band23 <- read_excel("Annual Banding Data/CFMS23.xlsx", guess_max=100000)
band23 <- read.csv("Annual Banding Data/CFMS23.csv", na="empty")

# 'CFMS23' file will serve as source for cleaning and updating to master database, along with submissions to BBL etc


# Prepare 2023 file to be merged with master dbase####

# Designate temporary dataframe for this modified data
library(dplyr)

mband23 <- band23 

library(readr)
band92_22 <- read.csv("CFMS92_22_Working.csv", stringsAsFactors = T)
band92_22[is.na(band92_22)] <- ""


#remove some empty rows:
mband23 <- mband23[complete.cases(mband23[,1]),]

mband23$Code <- as.factor(mband23$Code)
mband23$Code <- recode(mband23$Code, '1'="new", '2'="recap", '3'="foreign", '4'="destroy", '5'="reband", '6'="return", '7'="inreturn", '8'="lost", '9'="unbanded")

mband23$Age <- as.factor(mband23$Age)
mband23$Age <- recode(mband23$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")

library(stringr)
mband23$Time <- str_pad(mband23$Time, width=4, side="left", pad="0")
mband23$Time <- as.POSIXct(sprintf("%04s", mband23$Time), format="%H%M")
mband23$Time <- format(mband23$Time, format="%H:%M:%S")

# Will likely need to adjust date format in master dbase
# For whatever reason, one date for a BADE record on 8/22 can't seem to get formatted correctly in Excel, so fix here with mutate()

mband23 <- mband23 %>% 
  mutate(Date=case_when(Band_Number==291022791 ~ '2023-08-22',TRUE~Date))

#create JDate variable for julian date
library(lubridate)

mband23$Date2 <- ymd(mband23$Date)
mband23$J_Date <- as.numeric(strftime(mband23$Date2, format = "%j"))
mband23$Date2 <- NULL

# Add some columns that are in the archived database:
mband23$Org <- "ASI"
mband23$Station <- "CFMS"
mband23$Season <- ""
mband23$Band_Size <- ""

# Define seasons. For the 2023 dataset, will specify that demo banding on 6/6 is 'summer'
mband23 <- mband23 %>%
  mutate(Season = case_when(Month %in% c('7', '8', '9')~"3",
                            Month=='5' | Month=='6' & Day!='6' ~ "1",
                            TRUE~"2"))

# Define Band Size
mband23$page_size <- substr(mband23$Page, 1,2)
unique(mband23$page_size)
# Since 3As were included on 3 page, will have to manually coerce this as band size

mband23 <- mband23 %>%
  mutate(Band_Size = case_when(Band_Size=='' & page_size=='0A'~"0A",
                              Band_Size=='' & page_size=='0-'~"0",
                              Band_Size=='' & page_size=='2-'~"2",
                              Band_Size=='' & page_size=='1A'~"1A",
                              Band_Size=='' & page_size=='1B'~"1B",
                              Band_Size=='' & page_size=='R-'~"R",
                              Band_Size=='' & page_size=='U-'~"U",
                              Band_Size=='' & page_size=='1-'~"1",
                              Band_Size=='' & page_size=='3-' & Band_Number>=138340900~"3",
                              Band_Size=='' & page_size=='3-'& Band_Number<=130000000~"3A",
                        TRUE~Band_Size))
mband23$page_size <- NULL

# This method is a bit manually forced. For future, could convert Months/Days to numeric and use greater than/less than operators, or convert date format

# Tweak a few more col names in 2023 dataset:
mband23$Molt_Lim2 <- mband23$Molt_lim2
mband23$Molt_lim2 <- NULL


##  Merge 2023 dataframe with 92-22 ####

### Modify column names to match dataframes ####

#Focus mostly on 92_22 data, but some from 2023

mband23 <- mband23 %>% rename("P6_Emarg"="p6_Emarg")
mband23 <- mband23 %>% rename("Culmen"="Exp_Culmen")

#
a <- colnames(band92_22)
b <- colnames(mband23)

setdiff(a,b)
setdiff(b,a)

band92_22 <- band92_22 %>% 
  rename("Page"="Pages", "Band_Size"="BandSize", "Band_Number"="Band", 'Spp_Code'='SPP', 'Spp_Name'='SpName',
         'How_Aged1'='HA1', 'How_Aged2'='HA2', 'How_Sexed1'='HS1', 'How_Sexed2'='HS2', 'Skull'='SK',
         'Body_Molt'='BMOLT', 'Wing_Molt'='FFMOLT', 'PP_Wear'='FFWear', 'Molt_Lim1'='MOLT.LIMIT.1', "Molt_Lim2"="MOLT.LIMIT.2", "Molt_Lim3"="MOLT.LIMIT.3",
         "Eye_Color"= "Eye", "Crown_Code"="CRCode", "Auriculars"="AURICS", "Back_Feathers"="Back.Feathers", "Median_Covs"="Median.Covs", "Rect_Color"="REC.Color",
         "Wing_Chord"="Wing", "Nares_Tip"="Nares.Tip", "Bill_Width"="Bill.Width","Bill_Depth"="Bill.Depth", "Tarsus_Length"="Tarsus", "Tail_Length"="Tail", "Wing_Tail"="Wing.Tail",
         "P10_PPCov"="PP10.PPCOV","P9_P5"="P9.P5", "P9_P6"="P9.P6", "P10_P5"="P10.P5", "ED_Crop"="ED", "J_Date"="JDate", "Crown_Length"= "Cr.Length", "Rect_Color"="REC.Color",
         "Rect_Wear"="RECWear", "Rect_Shape"="RECShape", "Rect_Molt"="RECMolt", "Body_Color"="BodyColor", "Hood_Color" = "Hood.Color", "P10_P6"="P10.P6", "Color_Band"="COLBAND",
         "SS_Grow" = "SSGRO", "SS_New"="SSNEW", "PP_Grow"="PPGRO", "Bill_Color"="BillColor", "Number_Ticks"="Ticks","Number_Hippoboscids"="Hippoboscids", "Bill_Length"="Bill.Length",
         "PP_New"="PPNEW", "Uppertail_Covs"="Uppertail.Covs", "Buffy_Tips"="BuffyTips", "P6_Emarg"="P6.Emarg", "Molt_Prog_P"= "MoltProgPP", "Molt_Prog_S"="MoltProgSS",
         "Molt_Prog_R"="MoltProgRR")


### Merge command ####
library(plyr)
band92_23 <- rbind.fill(band92_22, mband23)

### Write dataframe ####

library(readr)
write_csv(band92_23, "CFMS92_23_Working.csv")


# Apply filters for certain data exports ####

## BBL Submission ####

### BBL: New ####
# Note that these should include any banding mortalities that were left unbanded
bbl23_new <- subset(band23, Code %in% c(1, 4, 5, 8) | Status=='5')
bbl23_new$Location <- "CF"

# 2023: select 'necessary' BBL data
bbl23_new <- select(bbl23_new, 'Band_Number', 'Spp_Code', 'Code',  'Year', 'Month', 'Day', 'Age', 'Sex', 'Status', 'Location', 'Notes', 'Old_Band_Number')

bbl23_new$Blood_Sample_Taken <- "N"

# Recode some columns. Needed for Age
# Code ('Disposition' for BBL template), should already be following BBL numeric system except for unbanded mortalities

bbl23_new$Age <- recode(bbl23_new$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")

bbl23_new$Code <- as.factor(bbl23_new$Code)
bbl23_new <-  bbl23_new %>% 
  mutate(
    Code = case_when(Status=='5' ~ "X", TRUE~Code))

bbl23_new$Present_Condition <- ifelse(bbl23_new$Status=='6', "04", "07")

# Status - BBL uses 3-digit codes. With no aux. markers or other tagging, or swab/blood samples etc, can just do a 1:1 recode

bbl23_new$Status <- as.factor(bbl23_new$Status)
bbl23_new$Status <- recode(bbl23_new$Status,  '5'="---", '3'="500", '4'="500", '1'="300")

## Note for future: BADE records need minimal data. Just band number, status, and date. This should be adjusted here in future.

# Write dataset. Just copy values directly into BBL template (hide columns that aren't being used)
write.xlsx(as.data.frame(bbl23_new), "Annual Banding Data/BBL_New_2023.xlsx", row.names=FALSE, showNA=FALSE)

### BBL: Return (No Colors) ####
# Prioritize this subset for recap submission. Given processing lags for new submissions, submitting all recaps around same time of new bandings will generate warnings for all within-season recaps.
# So Code 2s can be sorted out for later submission

bbl23_ret <- subset(band23, Code == 6 & Status!='2')

bbl23_ret$How_Obtained <- ''
bbl23_ret$Location <- "CF"
bbl23_ret$Present_Condition <- ''

bbl23_ret <- select(bbl23_ret, 'Band_Number', 'Spp_Code', 'Code',  'Year', 'Month', 'Day', 'Age', 'Sex', 'Status', 'How_Obtained', 'Present_Condition', 'Location', 'Notes', 'Wing_Chord', 'Mass')

bbl23_ret$Age <- recode(bbl23_ret$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")

bbl23_ret$Code <- "R"

bbl23_ret$Present_Condition <- ifelse(bbl23_ret$Status=='6', "04", "07")
bbl23_ret$How_Obtained <- ifelse(bbl23_ret$Status=='6', "10", "66")


bbl23_ret$Status <- as.factor(bbl23_ret$Status)
bbl23_ret$Status <- recode(bbl23_ret$Status,  '6'="---", '3'="500", '4'="500", '1'="300")

write.xlsx(as.data.frame(bbl23_ret), "Annual Banding Data/BBL_Returns_NoMarkers_2023.xlsx", row.names=FALSE, showNA=FALSE)

### BBL: Recap (Colors) ####

bbl23_col <- subset(band23, Status=='2')

bbl23_col$How_Obtained <- "66"
bbl23_col$Location <- "CF"
bbl23_col$Present_Condition <- '07'

bbl23_col <- select(bbl23_col, 'Band_Number', 'Spp_Code', 'Code',  'Year', 'Month', 'Day', 'Age', 'Sex', 'Status', 'How_Obtained', 'Present_Condition', 'Location', 'Notes')

bbl23_col$Code <- "R"
bbl23_col$Status <- '301'
bbl23_col$Age <- recode(bbl23_col$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")


bbl23_col$Marker_Type_1 <- ''
bbl23_col$Marker_Color_1 <- ''
bbl23_col$Side_1 <- 'L'
bbl23_col$Leg_1 <- 'LB'

bbl23_col$Marker_Type_2 <- ''
bbl23_col$Marker_Color_2 <- ''
bbl23_col$Side_2 <- 'L'
bbl23_col$Leg_2 <- 'LB'

bbl23_col$Marker_Type_3 <- ''
bbl23_col$Marker_Color_3 <- ''
bbl23_col$Side_3 <- 'R'
bbl23_col$Leg_3 <- 'LB'

bbl23_col$Marker_Type_4 <- ''
bbl23_col$Marker_Color_4 <- ''
bbl23_col$Side_4 <- 'R'
bbl23_col$Leg_4 <- 'LB'

# 249063340 = RB/AO
# 249063623 = AO/RY
# 249063622 = AO/RO
# Colors: R=1, Y=2, B=3, O=7
#Marker type: A=00, cols=01A

bbl23_col$Marker_Type_1B <- ''
bbl23_col$Marker_Color_1B <- ''
bbl23_col$Side_1B <- 'L'
bbl23_col$Leg_1B <- 'LB'

bbl23_col$Marker_Type_2B <- ''
bbl23_col$Marker_Color_2B <- ''
bbl23_col$Side_2B <- 'L'
bbl23_col$Leg_2B <- 'LB'

bbl23_col$Marker_Type_3B <- ''
bbl23_col$Marker_Code_3B <- ''
bbl23_col$Marker_Color_3B <- ''
bbl23_col$Side_3B <- 'R'
bbl23_col$Leg_3B <- 'LB'

bbl23_col$Marker_Type_4B <- ''
bbl23_col$Marker_Code_4B <- ''
bbl23_col$Marker_Color_4B <- ''
bbl23_col$Side_4B <- 'R'
bbl23_col$Leg_4B <- 'LB'

write.xlsx(as.data.frame(bbl23_col), "Annual Banding Data/BBL_Returns_Markers_2023.xlsx", row.names=FALSE, showNA=FALSE)



## Feather samples ####

feathers23 <- subset(band23, Sample_Type=="F", select=c('Band_Number', 'Date', 'Spp_Code', 'Spp_Name',  'Sex', 'Age', 'Code', 'Sample_Type', 'Notes', 'Bander'))

feathers23$Sample_Type[feathers23$Sample_Type=='F'] <- "Rectrices"

library(dplyr)
feathers23$Age <- recode(feathers23$Age, '0'="U", '1'="HY", '2' = "HY", '3' = "AHY", '4'="SY", '5'="ASY", '6'="TY", '7'="ATY")
feathers23$Code <- recode(feathers23$Code, '1'="New", '2'="Recapture", '6'="Recapture", '7'="Recapture")


feathers23$Bander <- recode(feathers23$Bander, 'ROS'="Robert Snowden", 'LVB'="Larissa Babicz", 'LLD'="Laurel Devaney", 'AHP'="Alexandra Pearcy", 'SEK'="Sarah Kennedy")

feathers23$Location_Code <- "CF"
feathers23$Contact <- "Robert Snowden"
feathers23$Country_Code <- "USA"
feathers23$State <- "AK"
feathers23$City_Town <- "Fairbanks"
feathers23$Latitude <- 64.867
feathers23$Longitude <- 147.748
feathers23$Area_Reserve <- "Creamer's Field Migratory Waterfowl Refuge"

feathers23 <- select(feathers23, Band_Number, Date, Spp_Code, Spp_Name, Location_Code, Sex, Age, Code, Sample_Type, Contact, Notes, Country_Code, State,
                     City_Town, Area_Reserve, Latitude, Longitude, Bander)

write_csv(feathers23, "CFMS_Bird_Genoscape_2023.csv")
feathers23 <- read_csv("Annual Banding Data/CFMS_Bird_Genoscape_2023.csv")



