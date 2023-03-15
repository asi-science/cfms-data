# Started March 2023
# Script is to serve as initial development for processes used to verify and QC banding data post-entry and post-inital proof
# Special focus on flagging and resolving inconsistencies in sex, age, spp etc between recapture records, and 'fatal' errors with respect to age/sex assignments based on criteria used

# Generate small data subsets to trial ####

# Derived from 'master' database (currently in process of getting cleaned)

band92_22 <- read.csv("CFMS92_22_Working.csv", stringsAsFactors = T)
band92_22[is.na(band92_22)] <- ""

library(dplyr)

# Create subset to serve as trial 'master' dbase, only select for a portion of the fields

df_old <- subset(band92_22, Year>='2016' & Year<='2021', select = c('BandSize', 'Bander', 'Code', 'Band', 'SPP','Date', 'Month', 'Day', 'Year', 'Time',
                                                     'Net', 'Age', 'HA1', 'HA2', 'Sex', 'HS1', 'HS2', 'SK', 'CP', 'BP', 'Fat', 'BMOLT', 'FFMOLT', 'FFWear', 'JP', 'Wing', 'Mass', 'Status', 'Notes'))

# And a subset for the 'new' data to read in:


df_new <- subset(band92_22, Year=='2022', select = c('BandSize', 'Bander', 'Code', 'Band', 'SPP','Date', 'Month', 'Day', 'Year', 'Time',
                                                                    'Net', 'Age', 'HA1', 'HA2', 'Sex', 'HS1', 'HS2', 'SK', 'CP', 'BP', 'Fat', 'BMOLT', 'FFMOLT', 'FFWear', 'JP', 'Wing', 'Mass', 'Status', 'Notes'))

