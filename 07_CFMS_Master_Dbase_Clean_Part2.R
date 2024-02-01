#
# Master Banding Database Clean and Organization, Part II
#
# December 2023
#
# Continuation of 02_CFMS-Master-Database_Clean script. Progressed on this generally stopped in April for field season.
# Since then, the 2023 data has been merged with the all-years file. 
# It seems some of the cleaning done in the prior script hasn't been maintained in the working all-years dataframe so much of this script may serve as an attempt to re-do that
# Note that most field/variable names were changed when merging with the 2023 data

# Read and write working dataframe ####
library(readr)
write_csv(band92_23, "CFMS92_23_Working.csv")

band92_23 <- read.csv("CFMS92_23_Working.csv", stringsAsFactors = T)
band92_23[is.na(band92_23)] <- ""
library(dplyr)
library(tidyr)

# Rename Variables ####
# Most fields have already been renamed, but rect color still has a residual duplicate column that can be brought together with the correctly-named one:
band92_23 <- unite(band92_23, col='Rect_Color', c('Rect_Color', 'REColor'), sep='')

# Drop the 'Station' field, as this is all CFMS data
band92_23$Station <- NULL

# 'Essential' Variable Assessment ####
# Start doing some QC checks for essential data: Band Code, Band Number, Spp, etc 

levels(band92_23$Code)
levels(band92_23$Spp_Code)
# These codes look good
levels(band92_23$Spp_Name)
# Most of these look good. Need to fix GCSP, GCTH
# Also check for blank spp names

## Fix Species Names ####
nospp <- subset(band92_23, Spp_Name=='')
# Looks like majority of Spp Names are blank. So can just run a mutate - case_when command based on 4-letter codes
# Below command was used in previous data clean script but didn't seem to 'stick' with earlier dataframe

# Also one record doesn't have a spp name or code -- unbanded from 8/14/16. This should be coded as an Unkn Spp

band92_23 <- band92_23 %>% 
  mutate(Spp_Code = case_when(Spp_Code=='' ~ 'UNKN',
                              TRUE~Spp_Code)) %>%
  mutate(Spp_Name = case_when(Spp_Name=="" ~ 
                              case_when(Spp_Code=='AMRO' ~ "American Robin",
                                        Spp_Code=='ALFL' ~ "Alder Flycatcher",
                                        Spp_Code=='ATTW' ~ "American Three-toed Woodpecker",
                                        Spp_Code %in% c('AGTW', 'AGWT') ~ "American Green-winged Teal",
                                        Spp_Code=='AMKE' ~ "American Kestrel",
                                        Spp_Code=='AMPI' ~ "American Pipit",
                                        Spp_Code=='ARWA' ~ "Arctic Warbler",
                                        Spp_Code=='ATSP' ~ "American Tree Sparrow",
                                        Spp_Code=='BADE' ~ "Band Destroyed",
                                        Spp_Code=='BALO' ~ "Band Lost",
                                        Spp_Code=='BANS' ~ "Bank Swallow",
                                        Spp_Code=='BBWO' ~ "Black-backed Woodpecker",
                                        Spp_Code=='BCCH' ~ "Black-capped Chickadee",
                                        Spp_Code=='BEKI' ~ "Belted Kingfisher",
                                        Spp_Code=='BLPW' ~ "Blackpoll Warbler",
                                        Spp_Code=='BOCH' ~ "Boreal Chickadee",
                                        Spp_Code=='BOOW' ~ "Boreal Owl",
                                        Spp_Code=='BOWA' ~ "Bohemian Waxwing",
                                        Spp_Code=='BRCR' ~ "Brown Creeper",
                                        Spp_Code=='BWTE' ~ "Blue-winged Teal",
                                        Spp_Code=='CAJA' ~ "Canada Jay",
                                        Spp_Code=='CHSP' ~ "Chipping Sparrow",
                                        Spp_Code=='CLSW' ~ "Cliff Swallow",
                                        Spp_Code=='CORE' ~ "Common Redpoll",
                                        Spp_Code=='DOWO' ~ "Downy Woodpecker",
                                        Spp_Code=='FOSP' ~ "Fox Sparrow",
                                        Spp_Code=='GCBT' ~ "Gray-cheeked/Bicknell\'s Thrush",
                                        Spp_Code=='GCKI' ~ "Golden-cheeked Kinglet",
                                        Spp_Code=='GCSP' ~ "Golden-crowned Sparrow",
                                        Spp_Code=='GCTH' ~ "Gray-cheeked Thrush",
                                        Spp_Code=='GWCS' ~ "Gambel\'s White-crowned Sparrow",
                                        Spp_Code=='GWTE' ~ "Green-winged Teal",
                                        Spp_Code=='HAFL' ~ "Hammond's Flycatcher",
                                        Spp_Code=='HAWO' ~ "Hairy Woodpcker",
                                        Spp_Code=='HETH' ~ "Hermit Thrush",
                                        Spp_Code=='HORE' ~ "Hoary Redpoll",
                                        Spp_Code=='LALO' ~ "Lapland Longspur",
                                        Spp_Code=='LBDO' ~ "Long-billed Dowitcher",
                                        Spp_Code=='LESA' ~ "Least Sandpiper",
                                        Spp_Code=='LESC' ~ "Lesser Scaup",
                                        Spp_Code=='LEYE' ~ "Lesser Yellowlegs",
                                        Spp_Code=='LISP' ~ "Lincoln\'s Sparrow",
                                        Spp_Code=='MALL' ~ "Mallard",
                                        Spp_Code=='MERL' ~ "Merlin",
                                        Spp_Code=='MOWA' ~ "Mourning Warbler",
                                        Spp_Code=='MYWA' ~ "Myrtle Yellow-rumped Warbler",
                                        Spp_Code=='NHOW' ~ "Northern Hawk-Owl",
                                        Spp_Code=='NOGO' ~ "Northern Goshawk",
                                        Spp_Code=='NOPI' ~ "Northern Pintail",
                                        Spp_Code=='NOSH' ~ "Northern Shoveler",
                                        Spp_Code=='NOWA' ~ "Northern Waterthrush",
                                        Spp_Code=='NOHA' ~ "NOrthern Harrier",
                                        Spp_Code=='NSHR' ~ "Northern Shrike",
                                        Spp_Code=='OCWA' ~ "Orange-crowned Warbler",
                                        Spp_Code=='ORJU' ~ "Dark-eyed Junco (Oregon)",
                                        Spp_Code=='PESA' ~ "Pectoral Sandpiper",
                                        Spp_Code=='PISI' ~ "Pine Siskin",
                                        Spp_Code=='RBNU' ~ "Red-breasted Nuthatch",
                                        Spp_Code=='RCKI' ~ "Ruby-crowned Kinglet",
                                        Spp_Code=='RECR' ~ "Red Crossbill",
                                        Spp_Code=='RUBL' ~ "Rusty Blackbird",
                                        Spp_Code=='RUGR' ~ "Ruffed Grouse",
                                        Spp_Code=='SAVS' ~ "Savannah Sparrow",
                                        Spp_Code=='SCJU' ~ "Slate-colored Junco",
                                        Spp_Code=='SOSA' ~ "Solitary Sandpiper",
                                        Spp_Code=='SPGR' ~ "Spruce Grouse",
                                        Spp_Code=='SPSA' ~ "Spotted Sandpiper",
                                        Spp_Code=='SSHA' ~ "Sharp-shinned Hawk",
                                        Spp_Code=='SWTH' ~ "Swainson\'s Thrush",
                                        Spp_Code=='TEWA' ~ "Tennessee Warbler",
                                        Spp_Code=='TOSO' ~ "Townsend\'s Solitaire",
                                        Spp_Code=='TOWA' ~ "Townsend\'s Warbler",
                                        Spp_Code=='TRES' ~ "Tree Swallow",
                                        Spp_Code=='UNGR' ~ "Unknown Grouse",
                                        Spp_Code=='UNKN' ~ "Unknown Species",
                                        Spp_Code=='UYRW' ~ "Unknown Yellow-rumped Warbler",
                                        Spp_Code=='VATH' ~ "Varied Thrush",
                                        Spp_Code=='VGSW' ~ "Violet-Green Swallow",
                                        Spp_Code=='WCSP' ~ "White-crowned Sparrow",
                                        Spp_Code=='WEWP' ~ "Western Wood-Pewee",
                                        Spp_Code=='WIWA' ~ "Wilson\'s Warbler",
                                        Spp_Code=='WISN' ~ "Wilson's Snipe",
                                        Spp_Code=='WPWA' ~ "Western Palm Warbler",
                                        Spp_Code=='WWCR' ~ "White-winged Crossbill",
                                        Spp_Code=='YBFL' ~ "Yellow-bellied Flycatcher",
                                        Spp_Code=='YBSA' ~ "Yellow-bellied Sapsucker",
                                        Spp_Code=='YSFL' ~ "Northern Flicker (Yellow-shafted)",
                                        Spp_Code=='YEWA' ~ "Yellow Warbler",
                                        TRUE ~ Spp_Name), 
                            TRUE ~ Spp_Name))

# Fixing Band Codes ####

## Resolving Blank Codes####
nocode <- subset(band92_23, Code=='')

# A few dozen blank codes. One blank from 2023 is a escaped recap with no recorded band. All other missing codes w/ no band numbers can be coded 'unbanded'
