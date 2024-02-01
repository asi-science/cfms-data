# Exploring and summarizing capture trends across years

# Load working datasets ####

library(readr)
# Banding data:
band92_23 <- read.csv("CFMS92_23_Working.csv", stringsAsFactors = T)
band92_23[is.na(band92_23)] <- ""

# Effort data:
pef92_23 <- read.csv("CFMS_NetEffort_AllYears_Working.csv", na="empty", check.names = FALSE)

# As of early Jan. 2024, effort data consists of spring/fall and is missing spring 2010, 2014, and 2015

# Summarize net effort by date ####
library(tidyr)
library(dplyr)

net.sum <- pef92_23 %>%
  group_by(Date) %>%
  reframe(Net_Hours = sum(Net_Hours),
          Month = unique(Month),
          Day = unique(Day),
          Year = unique(Year),
          J_Date = unique(J_Date),
          Season = unique(Season))
net.sum <- net.sum %>%
  mutate(Org = case_when(Year>=2013 ~ "ASI",
                         Year<=2013~ "ABO")) 

# Summarize captures
cols <- c("Date", "Spp_Code")
Mass <- as.numeric(band92_23$Mass)

temp <- band92_23
temp$n <- 1

band.sum <- temp %>%
  filter(Code == "new") %>%
  group_by(across(all_of(cols))) %>%
  summarise(banded = sum(n))

# Number of records by species for different conditions ####

# Filter out destroyed/lost bands, look at observations by season/year/cap. code

## Fall total individuals #### 

fallcaps <- band92_23 %>%
  filter(!(Spp_Code %in% c('BADE', 'BALO') )) %>%
  filter(Month %in% c('8', '9')) 

fallcaps %>%
  group_by(Spp_Code, Year) %>%
  filter(Code != "unbanded") %>%
  summarise(Unique_Individuals = length(unique(Band_Number))) %>%
  print(n=100) # This represents total number of unique non-unbanded individuals captured each fall season (after 8/1)

## Fall unique recaptures ####              
fallcaps %>%
  group_by(Spp_Code) %>%
  filter(Code %in% c("recap", "return", "inreturn")) %>%
  summarise(Unique_Recaps = length(unique(Band_Number))) %>%
  print(n=100) #this returns all unique individuals recaptured for species
  # note it excludes reband events

## Unique recaptures spring + fall ####  
band92_23 %>%
  filter(Month %in% c('4', '5', '8', '9')) %>%
  filter(!(Spp_Code %in% c('BADE', 'BALO') )) %>%
  filter(Code %in% c("recap", "return")) %>%
  group_by(Spp_Code) %>%
  summarise(Unique_Recaps = length(unique(Band_Number))) %>%
  print(n=75) # Coarse all-season unique individuals that were recaptured
  # excludes rebandeds (would need to account for later-recaptured rebands included in this data)
  # excludes June - July caps
  
# Non-resident spp with >20 recaptured individuals up to 2023: 
  # ALFL (28), AMRO (234), ATSP (629), BLPW (26), CORE* (542), FOSP (94), GCTH (184), GWCS (132), HAFL (222), HETH (173),
  # LISP (1459), MYWA (574), NOWA (141), OCWA (599), RCKI (83), SAVS (174), SCJU (1292), SWTH (550), WIWA (98), YEWA (115)
  
# Of note - AMRO and CORE have dramatically more recaptured individuals across year when you account for spring. 
  
# Can we distinguish spring - fall in different columns?

recap.unique <- band92_23 %>%
  filter(Month %in% c('4', '5', '8', '9')) %>%
  filter(!(Spp_Code %in% c('BADE', 'BALO') )) %>%
  filter(Code %in% c("recap", "return", "inreturn")) %>%
  group_by(Spp_Code) %>%
  summarise(Spring = length(unique(Band_Number[Month %in% c('4', '5')])),
            Fall = length(unique(Band_Number[Month %in% c('8', '9')]))) %>%
  print(n=50)
     # This returns number of individuals that were recaptured at least once in a spring season (April, May), 
     # and number that were recaptured at least once in the fall (Aug, Sept). Ignores rebands and some individuals of course were captured in both spring and fall (ie sum of these two columns double counts some birds)

## Recap individuals as fraction of all banded individuals
band92_23 %>%
  filter(Month %in% c('4', '5', '8', '9')) %>%
  filter(!(Code %in% c('unbanded', 'lost', 'destroy'))) %>%
  group_by(Spp_Code) %>%
  summarise(Total = length(unique(Band_Number[Code=='new'])),
            Recaptured = length(unique(Band_Number[Code %in% c('recap', 'return', 'inreturn')])),
            Returned = length(unique(Band_Number[Code %in% c('return')])),
            Prop_Recap = (Recaptured/Total),
            Prop_Return = (Returned/Total)) %>%
  filter(Total>=10 & Recaptured >=1) %>%
  print(n=50)
# This gives a rough recapture rate, ie proportion of all individuals banded that were later recaptured

# Below code needs some improvement
comband.fall <- fallcaps %>% 
  group_by(Spp_Code) %>%
  filter(Code == 'new') %>%
  filter(n_distinct(Year)>=10) %>%
  count(Year)

# Data viz ####

plot(Net_Hours~J_Date, data=net.sum, col=factor(Org))

net.asi <- subset(net.sum, Org=='ASI')
plot(Net_Hours~J_Date, data=net.asi)

# Bar charts for capture totals etc ####
library(ggplot2)
library(tidyr)
df_long <- gather(recap.unique, key=var, value=value, Spring, Fall)
df_long <- subset(df_long, Spp_Code %in% c('AMRO', 'ATSP', 'CORE', 'FOSP', 'GCTH', 'GWCS', 'HAFL', 'HETH', 'LISP', 'MYWA',
                                           'NOWA', 'OCWA', 'RCKI', 'SAVS', 'SCJU', 'SWTH', 'WIWA', 'YEWA'))
# This chart will show unique individuals with >0 recaps by season for some of more common spp
ggplot(data=df_long, aes(x=Spp_Code, y=value, fill=var)) +
  geom_col(position = position_dodge())
