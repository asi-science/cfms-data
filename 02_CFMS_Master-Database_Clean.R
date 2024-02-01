# Merging and Preparing Old Dataframes ####
# Step 1: add 2019-21 ("New") data to 92-18 ("Old") data

library(readr)
band19_21 <- read_csv("CFMS19_21.csv", na = "empty")
band92_18 <- read.csv("CFMS92_18.copy.csv", header=T, strip.white=T, as.is=T)


###
###

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



# All years database, pre-cleaning ####
# Step 2: 1992-2022 data assembled, can be further cleaned below

# Initial call of "unclean" copy of 92-22 data
band92_22 <- read.csv("CFMS92_22_unclean.csv", stringsAsFactors = T)
band92_22[is.na(band92_22)] <- ""

###
###
###
# Read and write working database version ####

# As I continue to clean data, I can save and call the 'working' version of the file
library(readr)
write_csv(band92_22, "CFMS92_22_Working.csv")

band92_22 <- read.csv("CFMS92_22_Working.csv", stringsAsFactors = T)
band92_22[is.na(band92_22)] <- ""

library(dplyr)


# Check to see how certain categorical variables are named etc

# Initial check and clean: Code, Status, Species ####
levels(band92_22$Code)
# May need to investigate "" results. Also some "destroyed" codes have species encounter data 
# **UNRESOLVED:** about 50 blanks from various years, probably mostly recaps 

destroy <- band92_22[(band92_22$Code=="destroy"),]
nocode <- band92_22[(band92_22$Code==""),]
# Three Sept 2017 records listed as "other" w/ single-digit band sizes should probably be recoded as unbanded; no record of any missing corresponding band numbers



# May check recap/return status band number by band number
recaps16 <- band92_22[(band92_22$Band %in% c('225198949', '225198951', '249062543', '262137045')),] 

band92_22 <- band92_22 %>%
  mutate(Code=case_when(BandSize=="R" & Code=="" & Band!='262137045' ~ "recap",
                        TRUE~Code))
 



levels(band92_22$SPP)
# Will need to change GRAJ to CAJA, check unknowns etc. Reflects 2022 AOU codes

band92_22$SPP <- recode(band92_22$SPP, 'GRAJ'="CAJA", 'GOSH' = "NOGO", 'unk'="UNKN", 'UNKNOWN'="UNKN")
band92_22$SPP <- recode(band92_22$SPP, 'GROUSE'="UNGR", 'NSHO'="NOSH")

wcsp <- band92_22[(band92_22$SPP=='WCSP'),]
band92_22 <- band92_22 %>% 
  mutate(SPP=case_when(SPP=='WCSP' & SpName=='Gambel\'s White-crowned Sparrow' ~ "GWCS",
         TRUE~SPP)) %>%
  mutate(SpName=case_when(SPP=='WCSP'~"White-crowned Sparrow", TRUE~SpName),
         SPP=as.factor(SPP), 
         SpName=as.factor(SpName))


unkspp <- band92_22[band92_22$SPP %in% c ("", "UNKN"),]
# 2020 record is unidentified net mortality and is properly accounted for
# other records, like 2019, lack context and will be deleted...outright deletion and correction of records should occur at very end of this script to avoid confounding with row number based adjustments earlier in script
band92_22 <- band92_22 %>%
  mutate(SPP=case_when(Code=="destroy" & SPP=="" ~ "BADE",
                       TRUE~SPP))


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



# Initial check and clean: additional default/metadata ####

# 2/23: Going through from the top, checking consistency
# Station. Double check that 'CRF' can be transformed to 
crf <- band92_22[band92_22$Station %in% c("CRF"),]
# Current code per BBL is "CF" ... could ask permission from Master to change to CFMS

band92_22$Station <- recode(band92_22$Station, 'CRF'="CFMS")
band92_22$Station <- sub("^$", "CFMS", band92_22$Station)
band92_22$Station <- as.factor(band92_22$Station)

###
# Band Sizes. *UNRESOLVED* Will need to figure out "unknown" band sizes
band92_22$BandSize <- recode(band92_22$BandSize, 'Recap'="R")
levels(band92_22$BandSize)

# Assign sizes for blanks
nosize <- band92_22[(band92_22$BandSize==""),]
# Lots of blanks, might take a lot of work to adjust
# Can at least add for any recap codes

band92_22 <- band92_22 %>%
  mutate(BandSize=case_when(Code %in% c('foreign', 'recap', 'inreturn', 'return') & BandSize=='' ~ "R",
         TRUE~BandSize))

levels(nosize$Code)
# To extract BandSize info from page numbers, I could create a new column filled with the suffix from page number
# This would lead to straightforward mutation command and I could then delete new column

band92_22$page_size <- substr(band92_22$Pages, 1,2)
unique(band92_22$page_size)

band92_22 <- band92_22 %>%
  mutate(BandSize = case_when(BandSize=='' & page_size=='0A'~"0A",
                              BandSize=='' & page_size=='0-'~"0",
                              BandSize=='' & page_size=='2-'~"2",
                              BandSize=='' & page_size=='1A'~"1A",
                              BandSize=='' & page_size=='3A'~"3A",
                              BandSize=='' & page_size=='1B'~"1B",
                              BandSize=='' & page_size=='R-'~"R",
                              BandSize=='' & page_size=='U-'~"U",
                              BandSize=='' & page_size=='1-'~"1",
                              BandSize=='' & page_size=='3-'~"3",
                              BandSize=='' & page_size=='3B'~"3B",
                              BandSize=='' & page_size=='1C'~"1C",
                              TRUE~BandSize))

# Check remaining page number categories without band sizes
# After each of these mutation rounds I can repeat the command below and continue:
nosize$page_size <- substr(nosize$Pages, 1,2)
unique(nosize$page_size)

# I won't change the 'weird' page codes, but will subset from the broader data to see which need band sizes filled. Some are just likely bigger sizes but want to confirm
weirdpage <- band92_22[band92_22$page_size %in% c('1/', '1S', '2S', '5-', '02', '4-', '7a', '2/', '7A', '3/', 'CF', '4/'),]
# Pretty much all of these codes correspond to their band size, so I can change this too. 'CF' prefix has a few records where the band size is at the *end* of the page number


band92_22 <- band92_22 %>%
  mutate(BandSize = case_when(BandSize=='' & page_size=='1/'~"1",
                              BandSize=='' & page_size=='1S-'~"1",
                              BandSize=='' & page_size=='2S'~"2",
                              BandSize=='' & page_size=='5-'~"5",
                              BandSize=='' & page_size=='02'~"2",
                              BandSize=='' & page_size=='4-'~"4",
                              BandSize=='' & page_size=='7a'~"7A",
                              BandSize=='' & page_size=='2/'~"2",
                              BandSize=='' & page_size=='7A'~"7A",
                              BandSize=='' & page_size=='3/'~"3",
                              BandSize=='' & page_size=='4/'~"4",
                              TRUE~BandSize))


# Checking more pages/sizes...
# The pages with the xF/xS prefixes are pretty much all from 1992. 
# Spring 2018 has some blank pages/blank band size combos; these can mostly be done species by species

band92_22 <- band92_22 %>%
  mutate(BandSize = case_when(BandSize=='' & Year=='2018' & SPP=='SCJU'~"1",
                              BandSize=='' & Year=='2018' & SPP=='ATSP'~"1",
                              BandSize=='' & Year=='2018' & SPP=='SAVS'~"1",
                              BandSize=='' & Year=='2018' & SPP=='LISP'~"1",
                              BandSize=='' & Year=='2018' & SPP=='BLPW'~"1",
                              BandSize=='' & Year=='2018' & SPP=='WCSP'~"1B",
                              BandSize=='' & Year=='2018' & SPP=='SWTH'~"1B",
                              BandSize=='' & Year=='2018' & SPP=='BCCH'~"0",
                              BandSize=='' & Year=='2018' & SPP=='AMRO'~"2",
                              BandSize=='' & Year=='2018' & SPP=='HAWO'~"2",
                              BandSize=='' & Year=='2018' & SPP=='SOSA'~"1A",
                              BandSize=='' & Year=='2018' & SPP=='SSHA'~"3",
                              BandSize=='' & Year=='2018' & SPP=='NOWA'~'1',
                              BandSize=='' & Year=='2018' & grepl('26800', Band) ~ '0A',
                              BandSize=='' & Year=='2018' & SPP=='HAFL' ~ '0A',
                              TRUE~BandSize))
# *in above code, realized partway through that I could use a grepl() function to consolidate by certain band predixes
# Continue with blank page records in 2015, 2014 based on band string prefixes; can simplify and exclude years from commands:

band92_22 <- band92_22 %>%
  mutate(BandSize = case_when(BandSize=='' & grepl('262137', Band) ~"1",
                              BandSize=='' & grepl('251141', Band) ~"1",
                              BandSize=='' & grepl('251142', Band) ~"1",
                              BandSize=='' & grepl('249062', Band) ~"0",
                              BandSize=='' & grepl('249061', Band) ~"0",
                              BandSize=='' & grepl('249091', Band) ~"0",
                              BandSize=='' & grepl('263015', Band)~"0A",
                              BandSize=='' & grepl('15812', Band)~"1A",
                              BandSize=='' & grepl('123295', Band)~"2",
                              BandSize=='' & grepl('138340', Band)~"3",
                              BandSize=='' & grepl('248047', Band)~"0A",
                              BandSize=='' & grepl('248046', Band)~"0A",
                              BandSize=='' & grepl('246059', Band)~"0A",
                              BandSize=='' & grepl('246060', Band)~"0A",
                              BandSize=='' & grepl('225196', Band) ~"1B",
                              BandSize=='' & grepl('225197', Band) ~"1B",
                              BandSize=='' & grepl('225198', Band) ~"1B",
                              BandSize=='' & grepl('231122', Band)~"1",
                              BandSize=='' & grepl('123217', Band)~"2",
                              BandSize=='' & grepl('129397', Band)~"3A",
                              BandSize=='' & grepl('221117', Band)~"1A",
                              BandSize=='' & grepl('123296', Band)~"2",
                              BandSize=='' & grepl('231123', Band)~"1",
                              BandSize=='' & Code=='unbanded' & Band==''~"U",
                              BandSize=='' & grepl('2460', Band) & SPP=='BOCH'~"0",
                              TRUE~BandSize))
                              
# Check 2022 sizes
band22 <- band92_22[(band92_22$Year>='2021'),]
unique(band22$BandSize)
# Looks like I didn't get most Page numbers and Band sizes extracted from the 2022 data ...

# Realized I could nest case_when conditions to make syntax tidier: 
band92_22 <- band92_22 %>% 
  mutate(BandSize = case_when(BandSize=="" ~ case_when(
    Year=="2022" ~ case_when(
      grepl('2900589', Band) ~ "0A",
      grepl('290059', Band) ~ "0A",
      grepl('285065', Band)~ "0",
      grepl('282014', Band) ~ "0",
      grepl('281178', Band) ~ "1",
      grepl('281123', Band) ~ "1",
      grepl('291022', Band) ~ "0A",
      grepl('249063', Band) ~"0",
      grepl('225199', Band) ~ "1B",
      grepl('255142', Band) ~"1B",
      grepl('301114', Band) ~ "1B",
      grepl('123216', Band) ~ "2",
      grepl('200392', Band) ~ "3B",
      TRUE~BandSize
    ), TRUE ~ BandSize
  ), TRUE~BandSize))
  
unique(nosize$Year)
# 2015 has one record w/ no size, some BADE in 2010, and 2006. Anything earlier may either be mortalities or early-year CFMS data

# Adjusting 2015 record: should be coded as unbanded; in field was assigned duplicate band number
band92_22[8761, "BandSize"]="U"
band92_22[8761, "Code"]="unbanded"
band92_22[8761, "Net"]="23"
band92_22[8761, "Cr.Length"]=''

weirdsize <- band92_22[band92_22$BandSize %in% c("unknown", "OTHER"),]
# Mostly 2015 and 2019 records here, can adjust like we did above


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

# Look through blank sp names with destroyed bands
bade <- band92_22[(band92_22$SPP=="BADE"),]
# Most blanks can be converted to "Band Destroyed"; but a few records with bird processing data need to be combed through
band92_22 <- band92_22 %>%
  mutate(SpName=case_when(SPP=="BADE" & SpName=="" & Net==""~ "Band Destroyed",
                          TRUE~SpName))

# Work through assigning other blank species names, but ignore any destr
noname <- band92_22[(band92_22$SpName==""),]
levels(noname$SPP)

noname <- noname %>% 
  mutate(SpName = case_when(SpName=="" ~ 
                              case_when(SPP=='AMRO' ~ "American Robin",
                                        SPP=='ALFL' ~ "Alder Flycatcher",
                                        SPP=='ATTW' ~ "American Three-toed Woodpecker",
                                        SPP=='AGTW' ~ "American Green-winged Teal",
                                        SPP=='AMKE' ~ "American Kestrel",
                                        SPP=='AMPI' ~ "American Pipit",
                                        SPP=='ARWA' ~ "Arctic Warbler",
                                        SPP=='ATSP' ~ "American Tree Sparrow",
                                        TRUE ~ SpName), 
                            TRUE ~ SpName))




# Work out weird band numbers, unbandeds with band numbers etc
# check for undbanded codes with band numbers
weirdband <- band92_22[(band92_22$Code=="unbanded" & band92_22$Band!=''),]
# Record 4926 needs to have status changed to "dead" -- change this later in the data 
# Recode some clear unbandeds

band92_22 <- band92_22 %>% 
  mutate(Band=case_when(Code=="unbanded"& BandSize=="U" & Band!='' ~ "",
                        TRUE~Band))





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
bands2 <- subset(band92_22, nchar(band92_22$Band)==2 )
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

# Conditional re-coding here seems somewhat complicated, so I'm just going to create a dummy "old Season" folder for now, and set up new season
# band92_22$Old_Season <- band92_22$Season

# For now, approach should be to assign *every* record between April to Jun 7 as spring. Otherwise, establish fall season for blank records post 2018, where there's clear understanding of when fall season starts

band92_22 <- band92_22  %>% 
  mutate(
    Season = case_when(Month %in% '4':'5'| Month == '6' & Day <='7'~'1', 
                       Month %in% '8':'9'| Month == '7' & Day >= '29' & Year>='2018'~'3',
                       TRUE~Season))



# Capture time
levels(band92_22$Time)
# Many levels with times not at 5-minute periods, but okay to leave as is I think. Questionable record of "3:12", this was daytime capture and could be manually proofed
# Confirm that nocturnal times are for owl banding. 

notime <- band92_22[band92_22$Time %in% c("#NAME?", "#VALUE!"),]
night <- band92_22[band92_22$Time %in% c("20:00:00","3:12:00","21:00:00","22:00:00","0:00:00","0:30:00","1:00:00","1:30:00"),]
# Looks like these measurements are all mis-transcribed daytime encounters. Directly change 1:00/1:30 times to 24 hr designation; correct others by verifying in raw data
# 2021, 2022 0:00 times should be changed to blank, same with anything destroyed band record with 0:00 time. 
# Check 2010 thru 2013 records

band92_22$Time <- recode(band92_22$Time, '1:00:00'="13:00:00", '1:30:00'="13:30:00")

band92_22 <- band92_22 %>%
  mutate(
    Time=case_when(Time=="#NAME?" & SPP=="BALO"~"",
                   Time=="#VALUE!" & SPP=="BADE"~"",
                   TRUE~Time),
    Time=as.factor(Time))


band92_22 <- band92_22 %>%
  mutate(
    Time=case_when(Time=="0:00:00" & Year>="2021"~"",
                   Time=="0:00:00" & SPP=="BADE"~"",
                   TRUE~Time),
    Time=as.factor(Time))


# line by line adjustments
band92_22[80349, "Time"]="6:30:00"
band92_22[105546, "Time"]="12:00:00"
band92_22[105246, "Time"]="12:00:00"
band92_22[83337, "Time"]="12:00:00"
band92_22[107486, "Time"]="10:00:00"
band92_22[107718, "Time"]="11:00:00"  
band92_22[111658, "Time"]="12:30:00"  
band92_22[10987, "Time"]="12:30:00"
band92_22[10988, "Time"]="12:30:00"
band92_22[10990, "Time"]="12:30:00"
band92_22[10991, "Time"]="12:30:00"
band92_22[10852, "Time"]="12:00:00"
band92_22[11223, "Time"]="12:00:00"


# Initial check and clean: Age, Sex, front page fields ####
# Age
levels(band92_22$Age)

levels(band92_22$HA1)
levels(band92_22$HA2)

# Some corrections to make. Check some weird codes.

badHA1 <- band92_22[band92_22$HA1 %in% c("BAND", "BP/CP", "VM", "SS"),]
unkHA1 <- band92_22[band92_22$HA1 %in% c("unk", "U"),]
badHA2 <- band92_22[band92_22$HA2 %in% c("0", "3", ";", "BP/CP"),]

# Let's screen some age/skull mismatches
# Apply this to data from 2006-on, as numbering system for a lot of codes was changed in 2006

age.skull.unk <- subset(band92_22, Year>=2006 & Age=="U" & SK < "4" & SK !="")

# I am also wary of some "0" skull values as they could be mistaken unrecorded measurements--eg in above subset, skull=0 for a return record (so bird should be AHY), as well as for an unbanded bird
# Correct age scores for other skull values here:

band92_22 <- band92_22 %>%
  mutate(Age = case_when(SK > "0" & SK < "4" & Year>=2006 ~ "HY",
         TRUE~Age),
         HA1 = case_when(SK > "0" & SK < "4" & Year>=2006 ~ "S",
                         TRUE~HA1))

# Now check for birds aged AHY despite having incomplete skulls
age.skull <- subset(band92_22, Year>=2006 & Age!="HY" & SK <"4" & SK !="")
# Doesn't appear to be any other bad skull/age assignments, all these records have SK=0. Suspect some should actually be unknown/blank values

#Check some How Aged codes

# Any U/unknown How Aged codes cab be changed to blank
band92_22$HA1 <- recode(band92_22$HA1, 'unk'="", 'U'="", 'CP'="C", 'BP'="B", 'v'="V", 'JP'="J", 's'="S")
band92_22$HA2 <- recode(band92_22$HA2, 'unk'="", 'U'="", 'CP'="C", 'BP'="B", 'v'="V", 'j'="J", '0'="", '3'="")

# CP/BP appears to be code used in 2015, ideally this should be changed to "B" or "C" respectively

# Parsing out BP/CP code
band92_22 <- band92_22 %>%
  mutate(HA1 = case_when(HA1=="BP/CP" & CP %in% '1':'3'  ~ "C",
                         TRUE~HA1),
         HA1 = case_when(HA1== "BP/CP" & BP %in% '1':'4' ~ "B",
                         TRUE~HA1),
         HA2 = case_when(HA1=="VM" ~ "M", 
                         TRUE~HA2),
         HA1 = case_when(HA1=="VM" ~ "V",
                         TRUE~HA1),
         HA1 = as.factor(HA1),
         HA2 = as.factor(HA2))

band92_22 <- band92_22 %>%
  mutate(HA2 = case_when(HA2=="BP/CP" & CP %in% '1':'3'  ~ "C",
                         TRUE~HA2),
         HA2 = case_when(HA2== "BP/CP" & BP %in% '1':'4' ~ "B",
                         TRUE~HA2),
         HA2 = as.factor(HA2))



# Sex
levels(band92_22$Sex)
# Check a few bad codes
sex <- band92_22[band92_22$Sex %in% c("L", "A"),]
band92_22[122878, "HA2"]=""
band92_22[123078, "HA2"]=""
band92_22[121706, "HA2"]=""
band92_22[122878, "Sex"]="M"
band92_22[123078, "Sex"]="M"
band92_22[121706, "Sex"]="F"
band92_22[122878, "HS1"]="L"
band92_22[123078, "HS1"]="L"
band92_22[121706, "HS1"]="P"

band92_22$Sex <- recode(band92_22$Sex, 'u'="U")

# Check some How Sexed codes:
levels(band92_22$HS1)
levels(band92_22$HS2)

unkHS1 <- band92_22[band92_22$HS1 %in% c("unk", "U"),]
unkHS2 <- band92_22[band92_22$HS2 %in% c("unk", "U"),]
badHS1 <- band92_22[band92_22$HS1 %in% c("BP/CP", "PQ", "BP"),]
badHS2 <- band92_22[band92_22$HS2 %in% c("0", "57", "BP/CP"),]

band92_22$HS1 <- recode(band92_22$HS1, 'u'="", "U"="", "unk"="", "CP"="C" )
band92_22$HS2 <- recode(band92_22$HS2, "U"="", "unk"="", "CP"="C", "57"="", "0"="", "BP"="B")

# More sex adjustments: change blanks to U where appropriate (leave blank for BADE and BALO)
nosex <- band92_22[band92_22$Sex %in% c(""),]
levels(nosex$SPP)

band92_22 <- band92_22 %>%
  mutate(Sex = case_when(Sex=="" & SPP!="BADE" & SPP!="BALO" ~ "U",
                         TRUE~Sex))

# Parsing combined codes
band92_22 <- band92_22 %>%
  mutate(HS1 = case_when(HS1=="BP/CP" & CP %in% '1':'3'  ~ "C",
                         HS1== "BP/CP" & BP %in% '1':'4' ~ "B",
                         HS1=="BP" & BP %in% '1':'6' ~ "B",
                         TRUE~HS1),
         HS2 = case_when(HS1=="PQ" ~ "Q",
                         TRUE~HS2),
         HS1 = case_when(HS1=="PQ"~"P",
                         TRUE~HS1),
         HS1 = as.factor(HS1),
         HS2 = as.factor(HS2))

band92_22 <- band92_22 %>%
  mutate(HS2 = case_when(HS2=="BP/CP" & CP %in% '1':'3'  ~ "C",
                         HS2== "BP/CP" & BP %in% '1':'4' ~ "B",
                         TRUE~HS2),
         HS2=as.factor(HS2))
         

# Skull
unique(band92_22$SK)
# Some scores of 9, which were probably indistinguishable skulls. Just change to blank
skull9 <- band92_22[band92_22$SK=="9",]

band92_22$SK <- recode(band92_22$SK, "9"="")

# Change skull to factor
band92_22$SK <- as.factor(band92_22$SK)


# Cloacal Protuberance
unique(band92_22$CP)
unkcp <- band92_22[band92_22$CP %in% c("9", "4"),]
# Just a few CPs of 4, one of which corresponds with assigned sex of M by C, the others are unknown sex. These others should likely not have a CP score, but could spot check with data


# Brood patch
unique(band92_22$BP)
badbp <- band92_22[band92_22$BP %in% c("6", "9", "50"),]
# A lot of code 6s appear to be legitimate brood patches, not sure if typos or older coding. Some records are from as recent as 2017 when BP score system should be same as now.
# A couple of records need verification with raw data

# Specific issue: row 80257 is a full record of a banded redpoll that died during processing, and is listed as BADE. Capture details never recorded as unbanded. So need to split this into a BADE record and unbanded record
# Test this in badbp df first

row <- badbp[which(badbp$Code=="destroy"),] %>%
  recode()
badbp <- rbind(badbp, row)

times=2

badbp %>% 
  slice(rep(row, times))

badbp[rep(row,times),]

datafix <- tibble(x=80257)





band92_22$Culmen <- gsub("NA", "", as.character(band92_22$Culmen))
band92_22$Culmen <- as.factor(band92_22$Culmen)
levels(band92_22$Culmen)

# Mostly fixed! Make sure to convert to numeric
# **PARTIALLY RESOLVED** Looks like there's a few measurements that need decimal places ... maybe double check in excel. Also an "AL" and "M"

# Flag bad records####
# To better catalogue issues I find with species disagreement, etc, will create a new column 

band92_22$Bad_Data

band92_22 <- band92_22 %>% 
  mutate(Bad_Data=case_when(Year=="2015" & Band=='262137045' ~ "Recap Spp Disagreement",
                            TRUE~Bad_Data
))


#
#
#

# Specific adjustments by row number ####
# # As certain problem records are encountered in cleaning, they can be indexed here

band92_22[4926, "Status"]='5'

# 
#
#

# Delete specific bad data rows - FINAL STEP####

# Rows to delete (insufficient capture + species data): 118508 118516 2759
# Double check that these are still the same records (filter data by 'UNKN' spp)
