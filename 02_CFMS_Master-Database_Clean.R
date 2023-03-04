# Working on merging all years CFMS data 

#################

# Step 1: add 2019-21 ("New") data to 92-18 ("Old") data

library(readr)
band19_21 <- read_csv("CFMS19_21.csv", na = "empty")
band92_18 <- read.csv("CFMS92_18.copy.csv", header=T, strip.white=T, as.is=T)


#################


# Get rid of NAs
band92_18[is.na(band92_18)] <- ""
band19_21[is.na(band19_21)] <- ""

library(plyr)
band92_21 <- rbind.fill(band92_18, band19_21)

# "Bill.Depth," "P6.Emarg," "Streaks," and "PP10.PPCOV" are redundant fields in 92-18 data already. 
# Delete Bill.Depth.1, merge the rest (no overlap) or delete redundant columns where necessary
# I just  deleted manually one of the redundant "Streaks" columns in the 92_18 data


band92_21$Bill.Depth.1 <-  NULL
band92_21$P6.Emarg <- NULL
band92_21$P6.Emarg <- band92_21$P6.Emarg.1
band92_21$P6.Emarg.1 <- NULL

band92_21[is.na(band92_21)] <- ""

band92_21$PP10.PPCOV <- paste(band92_21$PP10.PPCOV, band92_21$PP10.PPCOV.1, sep="")
band92_21$PP10.PPCOV.1 <- NULL

write_csv(band92_21, "CFMS92_21.csv", na="")

library(readr)
band92_21 <- read.csv("CFMS92_21.csv")
band92_21[is.na(band92_21)] <- ""


# Data polishing: clearing up redundant or inconsistent values etc
levels(band92_21$BandSize)

# "O", "R," "Recap" among variables thart can be renamed, with consistent capitalization

library(dplyr)
band92_21$BandSize <- recode(band92_21$BandSize, 'O'="0", 'OA'="0A", 'RECAP'="Recap", 'R'="Recap")
levels(band92_21$BandSize)

#length(which(band92_21$BandSize=="OA"))
#length(which(band92_21$BandSize=="0A"))

# Clean up 'OTHER' band sizes. For easier exploration, can isolate these variables as subset
otherband = band92_21[band92_21$BandSize == "OTHER",]

# Mix of band statuses. A number of unbandeds, a few full band #s from the Size 3 class in 2017, and recaps without a band number.
# For starters, can assign Recap/Unbanded values to those corresponding codes


band92_21 <-within(band92_21, BandSize[BandSize=='OTHER' & Code == 'unbanded'] <- 'U')
length(which(band92_21$BandSize=="OTHER"))

# Finding some other factor levels that need cleaning
# Time:
levels(band92_21$Time)
length(which(band92_21$Time=="-NA:NA:NANA"))
length(which(band92_21$Time=="#VALUE!"))   



#########################################
#
# Step 2: 1992-2022 data assembled, can be further cleaned below

# Initial call of "unclean" copy of 92-22 data
band92_22 <- read.csv("CFMS92_22_unclean.csv", stringsAsFactors = T)
band92_22[is.na(band92_22)] <- ""

#########################################
#########################################

# As I continue to clean data, I can save and call the 'working' version of the file
library(readr)
write_csv(band92_22, "CFMS92_22_Working.csv")

band92_22 <- read.csv("CFMS92_22_Working.csv", stringsAsFactors = T)
band92_22[is.na(band92_22)] <- ""

library(dplyr)
# Check to see how certain categorical variables are named etc

levels(band92_22$Code)
# May need to investigate "" results. Also some "destroyed" codes have species encounter data 
# **UNRESOLVED:** about 50 blanks from various years, probably mostly recaps 

levels(band92_22$SPP)
# Will need to change GRAJ to CAJA, check unknowns etc. Reflects 2022 AOU codes

band92_22$SPP <- recode(band92_22$SPP, 'GRAJ'="CAJA", 'GOSH' = "NOGO", 'unk'="UNKN", 'UNKNOWN'="UNKN")
band92_22$SPP <- recode(band92_22$SPP, 'GROUSE'="UNGR", 'NSHO'="NOSH")


band92_22$Status <- as.factor(band92_22$Status)
levels(band92_22$Status)

# Has some blanks--okay if destroyed, lost etc. But also 3-digit BBL codes. Change these to reflect CFMS codes
# Some codes don't have perfect correspondence: eg 319 is blood sample and color banded. Will change this to code 2 to prioritize color band info
# Several responses have swab BBL codes but we don't have CFMS codes for this.

#stat92_22 <- band92_22[band92_22$Status %in% c("100", "315", "319"),] 
#inj92_22 <- band92_22[band92_22$Status %in% c("500"),] 
stat0_92_22 <- band92_22[band92_22$Status %in% c("0"),]
# Generally no details on the 500 status, so will just default to injury code
# Change zeros to blanks for now 
# Add another field ("Samples?" to reflect what type of sample taken--feather, blood, mouth, etc)

band92_22$Status <- recode(band92_22$Status, '300'="1", '301'="2", '318'="7", '100'="1", '500'="3", '0'="0")

# Add new category: Sample_Type
# Levels = F (feather pull), B (blood), M (mouth swab), C (cloacal swab)
band92_22 <- band92_22 %>% 
  mutate(Sample_Type = case_when(Status=='7'~ "B",
                                 FeatherPull!='' ~ "F",
                                 Status=='319' ~ "B",
                                 Status=='315' ~ "M",
                                 TRUE ~ ''
                                 ))
band92_22$Sample_Type <- as.factor(band92_22$Sample_Type)
levels(band92_22$Sample_Type)

band92_22$Status <- recode(band92_22$Status, '319'="2", '315'="1", '0'="")
# Everything taken care of save for blanks

# Finding that a lot of minor measurement categories (eg bill depth) have 'NAs' appended and are read as characters. 
# Use gsub() function to drop any appended 'NAs'


# 2/23: Going through from the top, checking consistency
# Station. Double check that 'CRF' can be transformed to 
crf <- band92_22[band92_22$Station %in% c("CRF"),]
# Current code per BBL is "CF" ... could ask permission from Master to change to CFMS

band92_22$Station <- recode(band92_22$Station, 'CRF'="CFMS")
band92_22$Station <- sub("^$", "CFMS", band92_22$Station)
band92_22$Station <- as.factor(band92_22$Station)

# Band Sizes. *UNRESOLVED* Will need to figure out "unknown" band sizes
band92_22$BandSize <- recode(band92_22$BandSize, 'Recap'="R")
levels(band92_22$BandSize)


# Species names
levels(band92_22$SpName)
# **UNRESOLVED** Lots of work will be needed to make this consistent and clean ... but not huge priority
band92_22$SpName <-  recode(band92_22$SpName, "american robin" = 'American Robin', "AMERICAN rOBIN" = 'American Robin', "AMERICAN ROBIN" = 'American Robin',
                            "American Three-toed Woodpecer" = 'American Three-toed Woodpecker', "American Three-Toed Woodpecker"='American Three-toed Woodpecker',
                            "American tree sparrow" = 'American Tree Sparrow', "American Tree sparrow"='American Tree Sparrow',
                            "BAND DEST"='Band Destroyed', "Band destroyed"='Band Destroyed',
                            "Black-capped chickadee"='Black-capped Chickadee', "Black-Capped chickadee"='Black-capped Chickadee', "Black-Capped Chickadee"='Black-capped Chickadee', "BLack-capped chickadee"='Black-capped Chickadee', "Black-chapped Chickadee"='Black-capped Chickadee', "BLACK CAP"='Black-capped Chickadee', "Black Capped Chickadee"='Black-capped Chickadee',
                            "Blackp0ll Warbler"='Blackpoll Warbler', "BlackpOll Warbler"='Blackpoll Warbler',
                            "Boreal chickadee"='Boreal Chickadee', "BOREAL CHICKADEE"='Boreal Chickadee',
                            "Common redpoll"='Common Redpoll',
                            "FOX SPARROW"='Fox Sparrow',
                            "Gambel's White-Crowned Sparrow"='Gambel\'s White-crowned Sparrow', "Gambel's White Crowned Sparrow"='Gambel\'s White-crowned Sparrow', "GAMBELL'S WHITE-CR SP"='Gambel\'s White-crowned Sparrow', "Gambell's White-crowned Sparrow"='Gambel\'s White-crowned Sparrow', "Gambells White-crowned Sparrow"='Gambel\'s White-crowned Sparrow', "Gamble's White-crowned Sparrow"='Gambel\'s White-crowned Sparrow', "Gamble<d5>s White-crowned Sparrow"='Gambel\'s White-crowned Sparrow', "Gambles White-Crown Sparrow"='Gambel\'s White-crowned Sparrow',
                            "Golden-crowned kinglet"='Golden-crowned Kinglet',
                            "Goshawk"='Northern Goshawk',
                            "Gray-Cheeked Thrush"='Gray-cheeked Thrush', "Grey-cheeked Thrush"='Gray-cheeked Thrush', "Grey-Cheeked Thrush"='Gray-cheeked Thrush',
                            "Gray Jay"='Canada Jay',
                            "Green-winged Teal"='American Green-winged Teal', "GREEN-WINGED TEAL"='American Green-winged Teal',
                            "Hammonds Fly Catcher"='Hammond\'s Flycatcher', "Hammonds Flycatcher"='Hammond\'s Flycatcher',
                            "HERMIT THRUSH"='Hermit Thrush',
                            "LINC"='Lincoln\'s Sparrow', "Lincoln's sparrow"='Lincoln\'s Sparrow', "Lincoln Sparrow"='Lincoln\'s Sparrow', "LINCOLN SPARROW"='Lincoln\'s Sparrow', "LINC SP"='Lincoln\'s Sparrow',
                            "MALLARD"='Mallard',
                            "MERLIN"='Merlin',
                            "MYRTLE WAR"='Myrtle Yellow-rumped Warbler', "Myrtle warbler"='Myrtle Yellow-rumped Warbler', "Myrtle Warbler"='Myrtle Yellow-rumped Warbler', "MYRTLE WARBLER"='Myrtle Yellow-rumped Warbler',
                            "NORTHERN PINTAIL"='Northern Pintail', "NORTHERN SHOVELER"='Northern Shoveler',
                            "Northern waterthrush"='Northern Waterthrush',
                            "ORANGE"='Orange-crowned Warbler', "Orange-crown warbler"='Orange-crowned Warbler', "Orange-Crowned warbler"='Orange-crowned Warbler', "ORANGE-CROWNED WARBLER"='Orange-crowned Warbler', "ORANGE CR"='Orange-crowned Warbler', "ORANGE CR WAR"='Orange-crowned Warbler', "Orange Crowned Warbler"='Orange-crowned Warbler',
                            "Pine siskin"='Pine Siskin',
                            "Red-breasted nuthatch"="Red-breasted Nuthatch", "Red-breasted Nutchatch"='Red-breasted Nuthatch',
                            "Ruby-Crowned Kinglet"='Ruby-crowned Kinglet', "RUBY-CROWNED KINGLET"='Ruby-crowned Kinglet', "RUBY CR"='Ruby-crowned Kinglet', "RUBY CR KINGLET"='Ruby-crowned Kinglet',
                            "Rusty blackbird"='Rusty Blackbird', 
                            "savannah Sparrow"='Savannah Sparrow', "Savannahsparrow"="Savannah Sparrow",
                            "Sharp-shinned hawk"='Sharp-shinned Hawk', "Sharp-Shinned Hawk"='Sharp-shinned Hawk', "SHARP SHIN"='Sharp-shinned Hawk', "Sharp Shinned Hawk"='Sharp-shinned Hawk',
                            "Slate-colored junco"='Slate-colored Junco', "slate-colored Junco"='Slate-colored Junco', "SLate-Colored Junco"='Slate-colored Junco', "Slate-Colored Junco"='Slate-colored Junco', "SLATE-COLORED JUNCO"='Slate-colored Junco', "Slate colored junco"='Slate-colored Junco', "Slate Colored Junco"='Slate-colored Junco',
                            "Swainson<d5>s Thrush"='Swainson\'s Thrush', "Swainsons Thrush"='Swainson\'s Thrush',
                            "Three-toed Woodpecker"='American Three-toed Woodpecker', "Three-Toed Woodpecker"='American Three-toed Woodpecker', 
                            "TOWNSEND'S WARBLER"='Townsend\'s Warbler', "Townsends Warbler"='Townsend\'s Warbler',
                            "White-crowned Sparrow (Gambel's)"='Gambel\'s White-crowned Sparrow', "White-crowned Sparrow"='Gambel\'s White-crowned Sparrow', "White-crowned sparrow"='Gambel\'s White-crowned Sparrow', "White-Crowned Sparrow"='Gambel\'s White-crowned Sparrow',
                            "Unknown"='Unidentified Species',
                            "Unknow Yellow Warbler"='Yellow Warbler', "VARIED THRUSH"='Varied Thrush',
                            "WIL WAR"='Wilson\'s Warbler', "WILSON'S WARBLER"='Wilson\'s Warbler', "Wilsons Warbler"='Wilson\'s Warbler',
                            "Yellow shafted flicker"='Northern Flicker (Yellow-shafted)', "YELLOW WAR"='Yellow Warbler'
)
levels(band92_22$SpName)

# UNRESOLVED: Check consistency of month entries
unique(band92_22$Month)
offmonth <- band92_22[band92_22$Month %in% c("", "1", "2", "3", "10", "11"),]

# A few records with missing date info--maybe from 2017 or 2018. A few unbandeds plus band no 225199333 (recap SWTH)
# Lots of BADE/BALO in offseason, can ignored. Some late fall records in late 90s (in November), no associated season code. May need to dig into this.
# In mid-00s there was some Feb/March trap banding around CFMS, with Season = 0. Can apply this code potentially to earlier post-Oct records
# May also need to consider whether Oct onwards should be considered fall or not. 
levels(band92_22$Month)

unique(band92_22$Year)

levels(band92_22$Net)
# Some older notations, plus traps, boxes, etc. Missing net #s can just be changed to blanks. Can correct some capitalizations
band92_22$Net <- recode(band92_22$Net, 'net # missing'="Unknown", '19l'="19L", '19u'="19U", '25l'="25L", '25m'="25M", '25u'="25U", '3l'="3L", '3u'="3U", '5l'="5L", '5u'="5U", '13L'="3L", '9L'="19L")

# Subset to investigate suspect or older net numbers
nets <- band92_22[band92_22$Net %in% c("0", "0.9", "2L", "50", "56", "73", "266", "910", "98", "99"),]
nets2 <- band92_22[band92_22$Net %in% c("5", "19", "25"),]
#99 and 0 can be recorded to 'unknown', as they appear to have been captured as part of regular mist netting operation; others will need investigation
# Sites 5, 19, and 25 with unspecified nets (l/m/u) for 1000+ records, especially in 2018 and 2017. Will just leave as is for now.


band92_22$Net <- recode(band92_22$Net, '0'="Unknown", '99'="Unknown", '910'="9", '56'="5L", '50'="5U", '266'="26", "2L"="3L", "73"="23", "98"="Unknown")

# Checking some more records, from 1999. Net numbers post '99 should correspond with current net numbers:
# 35637 is unknown, 36018 is net 11, 36729 is unknown, 37497 is net 9, 37645 is unknown

band92_22[35637, "Net"]="Unknown"
band92_22[36018, "Net"]=11
band92_22[36729, "Net"]="Unknown"
band92_22[37497, "Net"]=9
band92_22[37645, "Net"]="Unknown"
band92_22[39032, "Net"]="Unknown"
band92_22[41562, "Net"]=9

levels(band92_22$Net)



# Preview some band numbers. Let's look at band numbers under certain sizes
bands8 <- subset(band92_22, nchar(band92_22$Band)==8)
# There are a number of records with 8 digit band numbers, generally from the same string. I suspect that these may simply have leading 0s?
bands7 <- subset(band92_22, nchar(band92_22$Band)==7)
# One SCJU recap missing last two digits. This one cant be determined easily. Also string of size 1s from '94 systematically missing two digits, not sure where
bands4 <- subset(band92_22, nchar(band92_22$Band)==4)
bands2 <- subset(band92_22, nchar(band92_22$Band)==2)
# Investigate 2013 records and 2000 records
# 2013 BOCH recapture missing suffix band number.



# Work on adding updating Season info
band92_22$Season <- as.factor(band92_22$Season)
spring <- subset(band92_22, Season==1)
summer <- subset(band92_22, Season==2)
fall <- subset(band92_22, Season==3)
noseason <- subset(band92_22, Season=="")

# Spring should be all dates up to and ending on Jun 7
# Summer/fall will be harder to define. Generally, there's a July 15 distinction for start of fall, though some analyses have grouped "fall" as August 1.
# For this master database, it's better to define seasons by the effort (eg fall as the start of regularly daily or near-daily migration period, or training beforehand)
# Some records with spring banding dates are assigned the fall season code

band92_22$Season <- as.factor(band92_22$Season)

# Conditional recoding here seems somewhat complicated, so I'm just going to create a dummy "old Season" folder for now, and set up new season
band92_22$Old_Season <- band92_22$Season

# For now, approach should be to assign *every* record between April to Jun 7 as spring. Otherwise, establish fall season for blank records post 2018, where there's clear understanding of when fall season starts

band92_22 <- band92_22  %>% 
  mutate(
    Season = case_when(Month %in% '4':'5'| Month == '6' & Day <='7'~'1', 
                       Month %in% '8':'9'| Month == '7' & Day >= '29' & Year>='2018'~'3',
                       TRUE~Season))





# Skull
unique(band92_22$SK)
noskull <- subset(band92_22, SK == "0")


# Some skulls recorded as 0 should be blank. Especially for AHY or U age. Could spot check with some raw data to confirm 
# Change skull to factor

band92_22$SK <- as.factor(band92_22$SK)
# Capture time
levels(band92_22$Time)
# Many levels with times not at 5-minute periods, but okay to leave as is I think. Questionable record of "3:12", this was daytime capture and could be manually proofed
# Confirm that nocturnal times are for owl banding. 





band92_22$Culmen <- gsub("NA", "", as.character(band92_22$Culmen))
band92_22$Culmen <- as.factor(band92_22$Culmen)
levels(band92_22$Culmen)

# Mostly fixed! Make sure to convert to numeric
# **PARTIALLY RESOLVED** Looks like there's a few measurements that need decimal places ... maybe double check in excel. Also an "AL" and "M"