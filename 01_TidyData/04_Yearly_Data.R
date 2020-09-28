############################################################################
######                                                               #######
######        Yearly Spatial Data for Moran's I                      #######
######    For creating local Moran maps, boxplots and global Moran   #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
############################################################################


########################


Yearly_Data




library(tidyverse) 
library(sf) # for read_sf()
library(spatialEco) # for sp.na.omit()
library(spdep) # for spatial weights
library(raster) # for size of polygon (pop density)
library(dplyr)
library(rgdal)

# importing to get the simple spatialpolygon
Eng_Poly_SpatialPolygon <- shapefile("England_MSOA.shp")
Wal_Poly_SpatialPolygon <- shapefile('Wales_MSOA.shp')
Eng_Wal_SpatialPolygon <- rbind(Eng_Poly_SpatialPolygon, Wal_Poly_SpatialPolygon)
#Eng_Wal_SpatialPolygon <- dplyr::select(Eng_Wal_SpatialPolygon, -c("name":"label"))

## importing to get simple sp data.frame, for purpose of mapping
Eng_Poly_SP_Dataframe <- st_read("England_MSOA.shp")
Wal_Poly_SP_Dataframe <- st_read('Wales_MSOA.shp')
Eng_Wal_SP_Dataframe <- rbind(Eng_Poly_SP_Dataframe, Wal_Poly_SP_Dataframe)

# quickly cleaning spatial spatialpolygon data, getting rid of Scilly.
Eng_Wal_SpatialPolygon <- Eng_Wal_SpatialPolygon[Eng_Wal_SpatialPolygon$code != "E02006781",] # getting rid of scilly
# Eng_Wal_SpatialPolygon <- na.omit(Eng_Wal_SpatialPolygon) # just getting rid of NAs

# quickly cleaning spatial sp dataframe data
Eng_Wal_SP_Dataframe <- Eng_Wal_SP_Dataframe[Eng_Wal_SP_Dataframe$code != "E02006781",] # getting rid of scilly
# Eng_Wal_SP_Dataframe <- na.omit(Eng_Wal_SP_Dataframe) # just checking all clean





#### importing non-spatial data ####
LSOA_Births <- read_csv("LSOA_Births_Neww.csv") 
summary(LSOA_Births)
LSOA_Pop <- read_csv("LSOA_Pop_Neww.csv")
summary(LSOA_Pop)
Lookup_Table <- read_csv("Lookup_Table.csv")

## tidying lookup table, so only hosting LSOA and MSOA
Lookup_Table_Tidy = dplyr::select(Lookup_Table, -c(OA11CD:OAC11NM, SOAC11CD:SOAC11NM, LAD17CD, LACCD:FID))
Lookup_Table_Tidy = distinct(Lookup_Table_Tidy, LSOA11CD, .keep_all = TRUE) # includes Scotland

# merging the Lookup Table to LSOA_Births, so this adds the births to the OA file. Combine pop and births
summary(LSOA_Pop_tidy)

LSOA_Births_tidy <- merge(Lookup_Table_Tidy, LSOA_Births, by.x="LSOA11CD", by.y="Code", all.x=TRUE)
LSOA_Pop_tidy <- merge(Lookup_Table_Tidy, LSOA_Pop, by.x="LSOA11CD", by.y="LSOA11CD", all.x=TRUE)
LSOA_Combined <- merge(LSOA_Births_tidy, LSOA_Pop_tidy, by.x="LSOA11CD", by.y="LSOA11CD", all.x=TRUE)

# make NAs 0s, as they ought to be - and can look into the complications later.
# But I think this is what's happening, when there aren't enough women/births
LSOA_Combined[is.na(LSOA_Combined)] <- 0
LSOA_Combined[is.finite(LSOA_Combined)] <- 0
summary(LSOA_Combined)
# B - births by age group, and P - pop of age group.
S2011 = LSOA_Combined %>% group_by(MSOA11CD.x) %>%
  summarise(B15 = sum(`B24_2011`), B25 = sum(`B25_2011`), B35 = sum(`B35_2011`), 
            P15 = sum(`P24_2011`), P25 = sum(`P25_2011`), P35 = sum(`P35_2011`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
summary(S2011)

names(LSOA_Combined)
LAD17NM.x
S2011_LAD = LSOA_Combined %>% group_by(LAD17NM.x) %>%
  summarise(B15 = sum(`B24_2011`), B25 = sum(`B25_2011`), B35 = sum(`B35_2011`), 
            P15 = sum(`P24_2011`), P25 = sum(`P25_2011`), P35 = sum(`P35_2011`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2011_LAD <- na.omit(S2011_LAD)
summary(S2011_LAD)
# This one is the issue w/ gives me 7152 (exl. Scilly) # S2011 <- S2011 %>% drop_na()
# So I can't be doing tht NA stuff. 
summary(S2011) # gives 8480 characters. I think I need to exclude Scotland/NI
# so we have 1327 NAs for TFR, but different for some MSOAs, which will be caused
# by low numbers of women in some MSOAs
8480-1327 # gives 7153 (too low) - NA TFRs
8480-1279 # gives 7201 (the correct number)
# Conclusion: the calculation above does not allow for 0s. Need to convert NAs to 0s

##### TEMP: FIGURING OUT WHAT WENT WRONG WITH 2016 #####




#### Getting yearly data for Moran's I ####
### would need to remove the scilly isles from this ###
LSOA_Combined <- rename(LSOA_Combined, Code = `MSOA11CD.x`)
S2002 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2002`), B25 = sum(`B25_2002`), B35 = sum(`B35_2002`), 
            P15 = sum(`P24_2002`), P25 = sum(`P25_2002`), P35 = sum(`P35_2002`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2003 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2003`), B25 = sum(`B25_2003`), B35 = sum(`B35_2003`), 
            P15 = sum(`P24_2003`), P25 = sum(`P25_2003`), P35 = sum(`P35_2003`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2004 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2004`), B25 = sum(`B25_2004`), B35 = sum(`B35_2004`), 
            P15 = sum(`P24_2004`), P25 = sum(`P25_2004`), P35 = sum(`P35_2004`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2005 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2005`), B25 = sum(`B25_2005`), B35 = sum(`B35_2005`), 
            P15 = sum(`P24_2005`), P25 = sum(`P25_2005`), P35 = sum(`P35_2005`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2006 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2006`), B25 = sum(`B25_2006`), B35 = sum(`B35_2006`), 
            P15 = sum(`P24_2006`), P25 = sum(`P25_2006`), P35 = sum(`P35_2006`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2007 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2007`), B25 = sum(`B25_2007`), B35 = sum(`B35_2007`), 
            P15 = sum(`P24_2007`), P25 = sum(`P25_2007`), P35 = sum(`P35_2007`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2008 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2008`), B25 = sum(`B25_2008`), B35 = sum(`B35_2008`), 
            P15 = sum(`P24_2008`), P25 = sum(`P25_2008`), P35 = sum(`P35_2008`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2009 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2009`), B25 = sum(`B25_2009`), B35 = sum(`B35_2009`), 
            P15 = sum(`P24_2009`), P25 = sum(`P25_2009`), P35 = sum(`P35_2009`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2010 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2010`), B25 = sum(`B25_2010`), B35 = sum(`B35_2010`), 
            P15 = sum(`P24_2010`), P25 = sum(`P25_2010`), P35 = sum(`P35_2010`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2011 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2011`), B25 = sum(`B25_2011`), B35 = sum(`B35_2011`), 
            P15 = sum(`P24_2011`), P25 = sum(`P25_2011`), P35 = sum(`P35_2011`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2012 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2012`), B25 = sum(`B25_2012`), B35 = sum(`B35_2012`), 
            P15 = sum(`P24_2012`), P25 = sum(`P25_2012`), P35 = sum(`P35_2012`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2013 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2013`), B25 = sum(`B25_2013`), B35 = sum(`B35_2013`), 
            P15 = sum(`P24_2013`), P25 = sum(`P25_2013`), P35 = sum(`P35_2013`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2014 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2014`), B25 = sum(`B25_2014`), B35 = sum(`B35_2014`), 
            P15 = sum(`P24_2014`), P25 = sum(`P25_2014`), P35 = sum(`P35_2014`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2015 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2015`), B25 = sum(`B25_2015`), B35 = sum(`B35_2015`), 
            P15 = sum(`P24_2015`), P25 = sum(`P25_2015`), P35 = sum(`P35_2015`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2016 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2016`), B25 = sum(`B25_2016`), B35 = sum(`B35_2016`), 
            P15 = sum(`P24_2016`), P25 = sum(`P25_2016`), P35 = sum(`P35_2016`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2017 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2017`), B25 = sum(`B25_2017`), B35 = sum(`B35_2017`), 
            P15 = sum(`P24_2017`), P25 = sum(`P25_2017`), P35 = sum(`P35_2017`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
S2018 = LSOA_Combined %>% group_by(Code) %>%
  summarise(B15 = sum(`B24_2018`), B25 = sum(`B25_2018`), B35 = sum(`B35_2018`), 
            P15 = sum(`P24_2018`), P25 = sum(`P25_2018`), P35 = sum(`P35_2018`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35),
            TFR = sum((ASFR15+ASFR25+ASFR35)*10))
summary(S2018)
summary(S2017)




#### Combining yearly ASFR and TFR Data 2002-2018 ####
### Getting rid of unnecessary columns ###
names(S2002)
S2002 <- dplyr::select(S2002, -c("B15":"P35"))
S2003 <- dplyr::select(S2003, -c("B15":"P35"))
S2004 <- dplyr::select(S2004, -c("B15":"P35"))
S2005 <- dplyr::select(S2005, -c("B15":"P35"))
S2006 <- dplyr::select(S2006, -c("B15":"P35"))
S2007 <- dplyr::select(S2007, -c("B15":"P35"))
S2008 <- dplyr::select(S2008, -c("B15":"P35"))
S2009 <- dplyr::select(S2009, -c("B15":"P35"))
S2010 <- dplyr::select(S2010, -c("B15":"P35"))
S2011 <- dplyr::select(S2011, -c("B15":"P35"))
S2012 <- dplyr::select(S2012, -c("B15":"P35"))
S2013 <- dplyr::select(S2013, -c("B15":"P35"))
S2014 <- dplyr::select(S2014, -c("B15":"P35"))
S2015 <- dplyr::select(S2015, -c("B15":"P35"))
S2016 <- dplyr::select(S2016, -c("B15":"P35"))
S2017 <- dplyr::select(S2017, -c("B15":"P35"))
S2018 <- dplyr::select(S2018, -c("B15":"P35"))

### Rename all columns to end with 
S2002$year <- 2002; S2003$year <- 2003; S2004$year <- 2004; S2005$year <- 2005
S2006$year <- 2006; S2007$year <- 2007; S2008$year <- 2008; S2009$year <- 2009
S2010$year <- 2010; S2011$year <- 2011; S2012$year <- 2012; S2013$year <- 2013
S2014$year <- 2014; S2015$year <- 2015; S2016$year <- 2016; S2017$year <- 2017
S2018$year <- 2018

# Combining the years w/6 columns
combo <- bind_rows(S2002,S2003,S2004,S2005,S2006,S2007,S2008,S2009,S2010,S2011,S2012,S2013,S2014,S2015,S2016,S2017,S2018)
summary(combo)

# Adding Region
Region_names <- dplyr::select(S2011_134, -c(2:"Non_Religious", "Income"))
Yearly_ASFR_TFR <- merge(combo, Region_names, by.x="Code", by.y="MSOA11CD.x", all.x=TRUE)
Yearly_ASFR_TFR <- rename(Yearly_ASFR_TFR, Region = `Region name`)
Yearly_ASFR_TFR <- rename(Yearly_ASFR_TFR, Code = `MSOA11CD.x`)
Yearly_ASFR_TFR <- Yearly_ASFR_TFR[Yearly_ASFR_TFR$Code != "E02006781",]

### combining Yearly_ASFR_TFR with spatial data ###
## spatialpolygon ##
#Yearly_ASFR_TFR_SpatialPolygon <- merge(Eng_Wal_SpatialPolygon, Yearly_ASFR_TFR, by.x="code", by.y="Code", all.x=TRUE)
#Yearly_ASFR_TFR_SpatialPolygon <- Yearly_ASFR_TFR_SpatialPolygon[Yearly_ASFR_TFR_SpatialPolygon$code != "E02006781",]
#Yearly_ASFR_TFR_SpatialPolygon <- na.omit(Yearly_ASFR_TFR_SpatialPolygon) # I think this may cause some issues

## SP Dataframe ##
Yearly_ASFR_TFR_Dataframe <- merge(Eng_Wal_SP_Dataframe, Yearly_ASFR_TFR, by.x="code", by.y="Code", all.x=TRUE) # spatialpolydataframe
Yearly_ASFR_TFR_Dataframe <- Yearly_ASFR_TFR_Dataframe[Yearly_ASFR_TFR_Dataframe$code != "E02006781",]
Yearly_ASFR_TFR_Dataframe <- na.omit(Yearly_ASFR_TFR_Dataframe) # I think this may cause some issues
# dataframe works, but polygon doesn't
summary(Yearly_ASFR_TFR_Dataframe)

saveRDS(Yearly_ASFR_TFR_Dataframe, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\Yearly_ASFR_TFR_Dataframe.rds")



