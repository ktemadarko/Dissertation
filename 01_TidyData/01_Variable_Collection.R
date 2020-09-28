############################################################################
######                                                               #######
######        Cleaning the data                                      #######
######        Creating Variables                                     #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
############################################################################

library(tidyverse) 
library(sf) # for read_sf()
library(spatialEco) # for sp.na.omit()
library(spdep) # for spatial weights
library(raster) # for size of polygon (pop density)
library(dplyr) 


#### importing non-spatial data ####
LSOA_Births <- read_csv("LSOA_Births_0709.csv")
LSOA_Pop <- read_csv("LSOA_Pop_0709.csv")
Lookup_Table <- read_csv("Lookup_Table.csv")

## tidying lookup table, so only hosting LSOA and MSOA
Lookup_Table_Tidy = dplyr::select(Lookup_Table, -c(OA11CD:OAC11NM, SOAC11CD:SOAC11NM, LAD17CD:FID))
Lookup_Table_Tidy = distinct(Lookup_Table_Tidy, LSOA11CD, .keep_all = TRUE) # includes Scotland

# merging the Lookup Table to LSOA_Births, so this adds the births to the OA file. Combine pop and births
LSOA_Births_tidy <- merge(Lookup_Table_Tidy, LSOA_Births, by.x="LSOA11CD", by.y="LSOA code", all.x=TRUE)
LSOA_Pop_tidy <- merge(Lookup_Table_Tidy, LSOA_Pop, by.x="LSOA11CD", by.y="Area Codes", all.x=TRUE)
LSOA_Combined <- merge(LSOA_Births_tidy, LSOA_Pop_tidy, by.x="LSOA11CD", by.y="LSOA11CD", all.x=TRUE)

# B - births by age group, and P - pop of age group.
S2011 = LSOA_Combined %>% group_by(MSOA11CD.x) %>%
  summarise(B15 = sum(`15_24_2011.births`), B25 = sum(`25_34_2011.births`), B35 = sum(`35_44_2011.births`), 
            P15 = sum(`15_24_2011.women`), P25 = sum(`25_34_2011.women`), P35 = sum(`35_44_2011.women`),
            ASFR15 = sum(B15/P15), ASFR25 = sum(B25/P25), ASFR35 = sum(B35/P35))
S2011$ASFR15[is.na(S2011$ASFR15)] <- 0 # for ASFR15, so it doesn't mess up calculations
S2011$TFR <- (S2011$ASFR15+S2011$ASFR25+S2011$ASFR35)*10
    



#### Variables ####
#### 3. Population density
# Getting area size
Eng_Poly_SpatialPolygon_popsize <- shapefile("England_MSOA.shp")
Wal_Poly_SpatialPolygon_popsize <- shapefile('Wales_MSOA.shp')
Eng_Wal_SpatialPolygon_popsize <- rbind(Eng_Poly_SpatialPolygon_popsize, Wal_Poly_SpatialPolygon_popsize)
Eng_Wal_SQKM <- Eng_Wal_SpatialPolygon_popsize  # only works w/ SpatialPolygon
raster::crs(Eng_Wal_SQKM)
Eng_Wal_SQKM$area_sqkm <- raster::area(Eng_Wal_SQKM) / 1000000

# 3.3. getting base data for population and pop density
MSOA_Religion_Pop <- read_csv("4. No_religion.csv") # using religion as has total pop

# religion file merging with S2011.
S2011_34_ <- merge(S2011, MSOA_Religion_Pop, by.x="MSOA11CD.x", by.y="GEO_CODE", all.x=TRUE)

# getting rid of unnecessary columns
S2011_34 <- dplyr::select(S2011_34_, -c("GEO_TYP2", "GEO_TYPE", "GEO_LABEL", "CDU_ID"))

# sqkm file, essentially tidying data
Eng_Wal_SQKMtibble <- as_tibble(Eng_Wal_SQKM)
S2011_34__ <- merge(S2011_34, Eng_Wal_SQKMtibble, by.x="MSOA11CD.x", by.y="code", all.x=TRUE)
S2011_34 <- dplyr::select(S2011_34__, -c("label", "name"))

# 3.4. getting pop/sqkm now that the data is tidy
S2011_34 <- S2011_34 %>%
  mutate(
    Pop_Density = Total / area_sqkm, .keep = "all")

### 4. Religion ###
S2011_34 <- S2011_34 %>%
  mutate(
    Non_Religious = No_religion / Total, .keep = "all")

### 1. Total Weekly Income ###
MSOA_Income <- read_csv("1. Net_weekly_income.csv")

# 1.1. This nicely also adds region
S2011_134 <- merge(S2011_34, MSOA_Income, by.x="MSOA11CD.x", by.y="MSOA code", all.x=TRUE)
summary(S2011_134)
### 2. Socially rented ###
MSOA_Rented <- read_csv("2. Socially_rented.csv") # social housing / all housing 
S2011_1234 <- merge(S2011_134, MSOA_Rented, by.x="MSOA11CD.x", by.y="code", all.x=TRUE)

### 5. Education ###
MSOA_Education <- read_csv("5. Education.csv") # those aged above 25
S2011_12345 <- merge(S2011_1234, MSOA_Education, by.x="MSOA11CD.x", by.y="code", all.x=TRUE)

### 6. Divorced ###
MSOA_Divorced <- read_csv("6. Divorced.csv") # those aged above 16
S2011_123456 <- merge(S2011_12345, MSOA_Divorced, by.x="MSOA11CD.x", by.y="code", all.x=TRUE)

### 7. Workday Population ###
MSOA_WorkdayPopulation <- read_csv("7. Workday Population.csv") 
S2011_1234567 <- merge(S2011_123456, MSOA_WorkdayPopulation, by.x="MSOA11CD.x", by.y="Code", all.x=TRUE)

### 8. Ethnicity ### Women aged 15-44
# Pakistani
# Bangladeshi
# Black African
MSOA_Ethnicity <- read_csv("8. Ethnicity.csv",
col_types = cols(
  GEO_CODE = col_character(),
  Pakistani = col_double(),
  Bangladeshi = col_double(),
  Back_African = col_double()
)
)
summary(MSOA_Ethnicity)
S2011_12345678 <- merge(S2011_1234567, MSOA_Ethnicity, by.x="MSOA11CD.x", by.y="GEO_CODE", all.x=TRUE)


#### Getting rid of unnecessary columns ####
## moving region to second place and doing dome renaming ##
#### Getting rid of unnecessary columns ####
summary(S2011_123456)
EW_Variables <- dplyr::select(S2011_12345678, -c("B15":"P35", "No_religion":"area_sqkm"))
EW_Variables <- na.omit(EW_Variables) # removing Scotland
EW_Variables <- EW_Variables %>% relocate(`Region name`, .after = MSOA11CD.x)
EW_Variables <- rename(EW_Variables, Region = `Region name`)
EW_Variables <- EW_Variables %>% relocate(`LAD`, .after = Region)
EW_Variables <- rename(EW_Variables, Code = `MSOA11CD.x`)
EW_Variables <- rename(EW_Variables, population = Total)
EW_Variables <- EW_Variables[!is.na(EW_Variables$Pop_Density), ]
EW_Variables <- rename(EW_Variables, Black_African = `Back_African`)

# getting rid of Scilly
EW_Variables <- EW_Variables[EW_Variables$Code != "E02006781",]
summary(EW_Variables)

# renaming variables in EW_Variables for later ease
# quickly renaming variables, should move to tidyverse
EW_Variables$PDENS <- EW_Variables$Pop_Density
EW_Variables$NONRG <- EW_Variables$Non_Religious
EW_Variables$INC <- EW_Variables$Income
EW_Variables$SOCHO <- EW_Variables$Social_Housing
EW_Variables$EDU <- EW_Variables$Education
EW_Variables$DIV <- EW_Variables$Divorced
EW_Variables <- dplyr::select(EW_Variables, -c("population":"Divorced"))

names(EW_Variables)

## Making the % variables non-decimal
EW_Variables$NONRG <- EW_Variables$NONRG*100
EW_Variables$SOCHO <- EW_Variables$SOCHO*100
EW_Variables$EDU <- EW_Variables$EDU*100
EW_Variables$DIV <- EW_Variables$DIV*100
EW_Variables$Pakistani <- EW_Variables$Pakistani*100
EW_Variables$Bangladeshi <- EW_Variables$Bangladeshi*100
EW_Variables$Black_African <- EW_Variables$Black_African*100


