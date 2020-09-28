################################################################################
#                                                                              #                         
# IDEM 156 - Spatial Demography Course 2018 (based on)                                  #                          
#              #                                                                                                                                 #
# Sebastian Kl?sener, MPIDR                                                    #                         
#                 
# Seeing how spatially autocorrelated fertility is 2002-2018

################################################################################
# Load libraries
library(spdep)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(spdep)
library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)
library(sp)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(ggmap)
library(maps)
library(modelr)
library(ggthemes)
library(spdep)
library(tmap)
library(plyr)



# Erase all objects in workspace
rm(list=ls(all=TRUE))
Yearly_Data <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\Yearly_ASFR_TFR_Dataframe.rds") # with the data
EW_Poly <- read_sf("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp") # for mapping
neighbour <- read.gal("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\neighbour.gal")
queen <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\queen.rds")
## EW_Variables <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\EW_Variables.rds")
## Need a file with Yearly data

## so now we have the imported: lw.FO
#### From 05.1e Global Indicators ####
# Define IDgh 

glimpse(Yearly_Data)
S2002 <- filter(Yearly_Data, year == "2002")
S2002$Comparison[S2011$TFR<=1.3]<-'Low'
S2002$Comparison[S2011$TFR>=2.1]<-'High'
S2002$Comparison[is.na(S2002$Comparison)] <- 'Middle'
glimpse(S2002)
S2003 <- filter(Yearly_Data, year == "2003")
S2003$Comparison[S2011$TFR<=1.3]<-'Low'
S2003$Comparison[S2011$TFR>=2.1]<-'High'
S2003$Comparison[is.na(S2003$Comparison)] <- 'Middle'
S2004 <- filter(Yearly_Data, year == "2004")
S2004$Comparison[S2011$TFR<=1.3]<-'Low'
S2004$Comparison[S2011$TFR>=2.1]<-'High'
S2004$Comparison[is.na(S2004$Comparison)] <- 'Middle'
S2005 <- filter(Yearly_Data, year == "2005")
S2005$Comparison[S2011$TFR<=1.3]<-'Low'
S2005$Comparison[S2011$TFR>=2.1]<-'High'
S2005$Comparison[is.na(S2005$Comparison)] <- 'Middle'
S2006 <- filter(Yearly_Data, year == "2006")
S2006$Comparison[S2011$TFR<=1.3]<-'Low'
S2006$Comparison[S2011$TFR>=2.1]<-'High'
S2006$Comparison[is.na(S2006$Comparison)] <- 'Middle'
S2007 <- filter(Yearly_Data, year == "2007")
S2007$Comparison[S2011$TFR<=1.3]<-'Low'
S2007$Comparison[S2011$TFR>=2.1]<-'High'
S2007$Comparison[is.na(S2007$Comparison)] <- 'Middle'
S2008 <- filter(Yearly_Data, year == "2008")
S2008$Comparison[S2011$TFR<=1.3]<-'Low'
S2008$Comparison[S2011$TFR>=2.1]<-'High'
S2008$Comparison[is.na(S2008$Comparison)] <- 'Middle'
S2009 <- filter(Yearly_Data, year == "2009")
S2009$Comparison[S2011$TFR<=1.3]<-'Low'
S2009$Comparison[S2011$TFR>=2.1]<-'High'
S2009$Comparison[is.na(S2009$Comparison)] <- 'Middle'
S2010 <- filter(Yearly_Data, year == "2010")
S2010$Comparison[S2011$TFR<=1.3]<-'Low'
S2010$Comparison[S2011$TFR>=2.1]<-'High'
S2010$Comparison[is.na(S2010$Comparison)] <- 'Middle'
S2011 <- filter(Yearly_Data, year == "2011")
S2011$Comparison[S2011$TFR<=1.3]<-'Low'
S2011$Comparison[S2011$TFR>=2.1]<-'High'
S2011$Comparison[is.na(S2011$Comparison)] <- 'Middle'
S2012 <- filter(Yearly_Data, year == "2012")
S2012$Comparison[S2011$TFR<=1.3]<-'Low'
S2012$Comparison[S2011$TFR>=2.1]<-'High'
S2012$Comparison[is.na(S2012$Comparison)] <- 'Middle'
S2013 <- filter(Yearly_Data, year == "2013")
S2013$Comparison[S2011$TFR<=1.3]<-'Low'
S2013$Comparison[S2011$TFR>=2.1]<-'High'
S2013$Comparison[is.na(S2013$Comparison)] <- 'Middle'
S2014 <- filter(Yearly_Data, year == "2014")
S2014$Comparison[S2011$TFR<=1.3]<-'Low'
S2014$Comparison[S2011$TFR>=2.1]<-'High'
S2014$Comparison[is.na(S2014$Comparison)] <- 'Middle'
S2015 <- filter(Yearly_Data, year == "2015")
S2015$Comparison[S2011$TFR<=1.3]<-'Low'
S2015$Comparison[S2011$TFR>=2.1]<-'High'
S2015$Comparison[is.na(S2015$Comparison)] <- 'Middle'
S2016 <- filter(Yearly_Data, year == "2016")
S2016$Comparison[S2011$TFR<=1.3]<-'Low'
S2016$Comparison[S2011$TFR>=2.1]<-'High'
S2016$Comparison[is.na(S2016$Comparison)] <- 'Middle'
S2017 <- filter(Yearly_Data, year == "2017")
S2017$Comparison[S2011$TFR<=1.3]<-'Low'
S2017$Comparison[S2011$TFR>=2.1]<-'High'
S2017$Comparison[is.na(S2017$Comparison)] <- 'Middle'
S2018 <- filter(Yearly_Data, year == "2018")
S2018$Comparison[S2011$TFR<=1.3]<-'Low'
S2018$Comparison[S2011$TFR>=2.1]<-'High'
S2018$Comparison[is.na(S2018$Comparison)] <- 'Middle'

# Define number of permutations that should be performed for the Monte Carlo-
# TFR
lw.FOQ <- queen
moranI.mc.2002.1 <- moran.mc(S2002$TFR, lw.FOQ, 100)
moranI.mc.2003.1 <- moran.mc(S2003$TFR, lw.FOQ, 100)
moranI.mc.2004.1 <- moran.mc(S2004$TFR, lw.FOQ, 100)
moranI.mc.2005.1 <- moran.mc(S2005$TFR, lw.FOQ, 100)
moranI.mc.2006.1 <- moran.mc(S2006$TFR, lw.FOQ, 100)
moranI.mc.2007.1 <- moran.mc(S2007$TFR, lw.FOQ, 100)
moranI.mc.2008.1 <- moran.mc(S2008$TFR, lw.FOQ, 100)
moranI.mc.2009.1 <- moran.mc(S2009$TFR, lw.FOQ, 100)
moranI.mc.2010.1 <- moran.mc(S2010$TFR, lw.FOQ, 100)
moranI.mc.2011.1 <- moran.mc(S2011$TFR, lw.FOQ, 100)
moranI.mc.2012.1 <- moran.mc(S2012$TFR, lw.FOQ, 100)
moranI.mc.2013.1 <- moran.mc(S2013$TFR, lw.FOQ, 100)
moranI.mc.2014.1 <- moran.mc(S2014$TFR, lw.FOQ, 100)
moranI.mc.2015.1 <- moran.mc(S2015$TFR, lw.FOQ, 100)
moranI.mc.2016.1 <- moran.mc(S2016$TFR, lw.FOQ, 100)
moranI.mc.2017.1 <- moran.mc(S2017$TFR, lw.FOQ, 100)
moranI.mc.2018.1 <- moran.mc(S2018$TFR, lw.FOQ, 100)

moranI.mc.2002.1# <- moran.mc(S2002$TFR, lw.FOQ, 100)
moranI.mc.2003.1# <- moran.mc(S2003$TFR, lw.FOQ, 100)
moranI.mc.2004.1# <- moran.mc(S2004$TFR, lw.FOQ, 100)
moranI.mc.2005.1# <- moran.mc(S2005$TFR, lw.FOQ, 100)
moranI.mc.2006.1# <- moran.mc(S2006$TFR, lw.FOQ, 100)
moranI.mc.2007.1# <- moran.mc(S2007$TFR, lw.FOQ, 100)
moranI.mc.2008.1# <- moran.mc(S2008$TFR, lw.FOQ, 100)
moranI.mc.2009.1# <- moran.mc(S2009$TFR, lw.FOQ, 100)
moranI.mc.2010.1# <- moran.mc(S2010$TFR, lw.FOQ, 100)
moranI.mc.2011.1# <- moran.mc(S2011$TFR, lw.FOQ, 100)
moranI.mc.2012.1# <- moran.mc(S2012$TFR, lw.FOQ, 100)
moranI.mc.2013.1# <- moran.mc(S2013$TFR, lw.FOQ, 100)
moranI.mc.2014.1# <- moran.mc(S2014$TFR, lw.FOQ, 100)
moranI.mc.2015.1# <- moran.mc(S2015$TFR, lw.FOQ, 100)
moranI.mc.2016.1# <- moran.mc(S2016$TFR, lw.FOQ, 100)
moranI.mc.2017.1# <- moran.mc(S2017$TFR, lw.FOQ, 100)
moranI.mc.2018.1# <- moran.mc(S2018$TFR, lw.FOQ, 100)

# ASFR15
moranI.mc.2002.2 <- moran.mc(S2002$ASFR15, lw.FOQ, 100)
moranI.mc.2003.2 <- moran.mc(S2003$ASFR15, lw.FOQ, 100)
moranI.mc.2004.2 <- moran.mc(S2004$ASFR15, lw.FOQ, 100)
moranI.mc.2005.2 <- moran.mc(S2005$ASFR15, lw.FOQ, 100)
moranI.mc.2006.2 <- moran.mc(S2006$ASFR15, lw.FOQ, 100)
moranI.mc.2007.2 <- moran.mc(S2007$ASFR15, lw.FOQ, 100)
moranI.mc.2008.2 <- moran.mc(S2008$ASFR15, lw.FOQ, 100)
moranI.mc.2009.2 <- moran.mc(S2009$ASFR15, lw.FOQ, 100)
moranI.mc.2010.2 <- moran.mc(S2010$ASFR15, lw.FOQ, 100)
moranI.mc.2011.2 <- moran.mc(S2011$ASFR15, lw.FOQ, 100)
moranI.mc.2012.2 <- moran.mc(S2012$ASFR15, lw.FOQ, 100)
moranI.mc.2013.2 <- moran.mc(S2013$ASFR15, lw.FOQ, 100)
moranI.mc.2014.2 <- moran.mc(S2014$ASFR15, lw.FOQ, 100)
moranI.mc.2015.2 <- moran.mc(S2015$ASFR15, lw.FOQ, 100)
moranI.mc.2016.2 <- moran.mc(S2016$ASFR15, lw.FOQ, 100)
moranI.mc.2017.2 <- moran.mc(S2017$ASFR15, lw.FOQ, 100)
moranI.mc.2018.2 <- moran.mc(S2018$ASFR15, lw.FOQ, 100)

moranI.mc.2002.2# <- moran.mc(S2002$ASFR15, lw.FOQ, 100)
moranI.mc.2003.2# <- moran.mc(S2003$ASFR15, lw.FOQ, 100)
moranI.mc.2004.2#<- moran.mc(S2004$ASFR15, lw.FOQ, 100)
moranI.mc.2005.2# <- moran.mc(S2005$ASFR15, lw.FOQ, 100)
moranI.mc.2006.2# <- moran.mc(S2006$ASFR15, lw.FOQ, 100)
moranI.mc.2007.2# <- moran.mc(S2007$ASFR15, lw.FOQ, 100)
moranI.mc.2008.2# <- moran.mc(S2008$ASFR15, lw.FOQ, 100)
moranI.mc.2009.2# <- moran.mc(S2009$ASFR15, lw.FOQ, 100)
moranI.mc.2010.2# <- moran.mc(S2010$ASFR15, lw.FOQ, 100)
moranI.mc.2011.2# <- moran.mc(S2011$ASFR15, lw.FOQ, 100)
moranI.mc.2012.2# <- moran.mc(S2012$ASFR15, lw.FOQ, 100)
moranI.mc.2013.2# <- moran.mc(S2013$ASFR15, lw.FOQ, 100)
moranI.mc.2014.2# <- moran.mc(S2014$ASFR15, lw.FOQ, 100)
moranI.mc.2015.2# <- moran.mc(S2015$ASFR15, lw.FOQ, 100)
moranI.mc.2016.2# <- moran.mc(S2016$ASFR15, lw.FOQ, 100)
moranI.mc.2017.2# <- moran.mc(S2017$ASFR15, lw.FOQ, 100)
moranI.mc.2018.2# <- moran.mc(S2018$ASFR15, lw.FOQ, 100)



# ASFR25
moranI.mc.2002.3 <- moran.mc(S2002$ASFR25, lw.FOQ, 100)
moranI.mc.2003.3 <- moran.mc(S2003$ASFR25, lw.FOQ, 100)
moranI.mc.2004.3 <- moran.mc(S2004$ASFR25, lw.FOQ, 100)
moranI.mc.2005.3 <- moran.mc(S2005$ASFR25, lw.FOQ, 100)
moranI.mc.2006.3 <- moran.mc(S2006$ASFR25, lw.FOQ, 100)
moranI.mc.2007.3 <- moran.mc(S2007$ASFR25, lw.FOQ, 100)
moranI.mc.2008.3 <- moran.mc(S2008$ASFR25, lw.FOQ, 100)
moranI.mc.2009.3 <- moran.mc(S2009$ASFR25, lw.FOQ, 100)
moranI.mc.2010.3 <- moran.mc(S2010$ASFR25, lw.FOQ, 100)
moranI.mc.2011.3 <- moran.mc(S2011$ASFR25, lw.FOQ, 100)
moranI.mc.2012.3 <- moran.mc(S2012$ASFR25, lw.FOQ, 100)
moranI.mc.2013.3 <- moran.mc(S2013$ASFR25, lw.FOQ, 100)
moranI.mc.2014.3 <- moran.mc(S2014$ASFR25, lw.FOQ, 100)
moranI.mc.2015.3 <- moran.mc(S2015$ASFR25, lw.FOQ, 100)
moranI.mc.2016.3 <- moran.mc(S2016$ASFR25, lw.FOQ, 100)
moranI.mc.2017.3 <- moran.mc(S2017$ASFR25, lw.FOQ, 100)
moranI.mc.2018.3 <- moran.mc(S2018$ASFR25, lw.FOQ, 100)


moranI.mc.2002.3# <- moran.mc(S2002$ASFR25, lw.FOQ, 100)
moranI.mc.2003.3# <- moran.mc(S2003$ASFR25, lw.FOQ, 100)
moranI.mc.2004.3# <- moran.mc(S2004$ASFR25, lw.FOQ, 100)
moranI.mc.2005.3# <- moran.mc(S2005$ASFR25, lw.FOQ, 100)
moranI.mc.2006.3# <- moran.mc(S2006$ASFR25, lw.FOQ, 100)
moranI.mc.2007.3# <- moran.mc(S2007$ASFR25, lw.FOQ, 100)
moranI.mc.2008.3# <- moran.mc(S2008$ASFR25, lw.FOQ, 100)
moranI.mc.2009.3# <- moran.mc(S2009$ASFR25, lw.FOQ, 100)
moranI.mc.2010.3# <- moran.mc(S2010$ASFR25, lw.FOQ, 100)
moranI.mc.2011.3# <- moran.mc(S2011$ASFR25, lw.FOQ, 100)
moranI.mc.2012.3# <- moran.mc(S2012$ASFR25, lw.FOQ, 100)
moranI.mc.2013.3# <- moran.mc(S2013$ASFR25, lw.FOQ, 100)
moranI.mc.2014.3# <- moran.mc(S2014$ASFR25, lw.FOQ, 100)
moranI.mc.2015.3# <- moran.mc(S2015$ASFR25, lw.FOQ, 100)
moranI.mc.2016.3# <- moran.mc(S2016$ASFR25, lw.FOQ, 100)
moranI.mc.2017.3# <- moran.mc(S2017$ASFR25, lw.FOQ, 100)
moranI.mc.2018.3# <- moran.mc(S2018$ASFR25, lw.FOQ, 100)

# ASFR35
moranI.mc.2002.4 <- moran.mc(S2002$ASFR35, lw.FOQ, 100)
moranI.mc.2003.4 <- moran.mc(S2003$ASFR35, lw.FOQ, 100)
moranI.mc.2004.4 <- moran.mc(S2004$ASFR35, lw.FOQ, 100)
moranI.mc.2005.4 <- moran.mc(S2005$ASFR35, lw.FOQ, 100)
moranI.mc.2006.4 <- moran.mc(S2006$ASFR35, lw.FOQ, 100)
moranI.mc.2007.4 <- moran.mc(S2007$ASFR35, lw.FOQ, 100)
moranI.mc.2008.4 <- moran.mc(S2008$ASFR35, lw.FOQ, 100)
moranI.mc.2009.4 <- moran.mc(S2009$ASFR35, lw.FOQ, 100)
moranI.mc.2010.4 <- moran.mc(S2010$ASFR35, lw.FOQ, 100)
moranI.mc.2011.4 <- moran.mc(S2011$ASFR35, lw.FOQ, 100)
moranI.mc.2012.4 <- moran.mc(S2012$ASFR35, lw.FOQ, 100)
moranI.mc.2013.4 <- moran.mc(S2013$ASFR35, lw.FOQ, 100)
moranI.mc.2014.4 <- moran.mc(S2014$ASFR35, lw.FOQ, 100)
moranI.mc.2015.4 <- moran.mc(S2015$ASFR35, lw.FOQ, 100)
moranI.mc.2016.4 <- moran.mc(S2016$ASFR35, lw.FOQ, 100)
moranI.mc.2017.4 <- moran.mc(S2017$ASFR35, lw.FOQ, 100)
moranI.mc.2018.4 <- moran.mc(S2018$ASFR35, lw.FOQ, 100)



moranI.mc.2002.4# <- moran.mc(S2002$ASFR35, lw.FOQ, 100)
moranI.mc.2003.4# <- moran.mc(S2003$ASFR35, lw.FOQ, 100)
moranI.mc.2004.4# <- moran.mc(S2004$ASFR35, lw.FOQ, 100)
moranI.mc.2005.4# <- moran.mc(S2005$ASFR35, lw.FOQ, 100)
moranI.mc.2006.4# <- moran.mc(S2006$ASFR35, lw.FOQ, 100)
moranI.mc.2007.4# <- moran.mc(S2007$ASFR35, lw.FOQ, 100)
moranI.mc.2008.4# <- moran.mc(S2008$ASFR35, lw.FOQ, 100)
moranI.mc.2009.4# <- moran.mc(S2009$ASFR35, lw.FOQ, 100)
moranI.mc.2010.4# <- moran.mc(S2010$ASFR35, lw.FOQ, 100)
moranI.mc.2011.4# <- moran.mc(S2011$ASFR35, lw.FOQ, 100)
moranI.mc.2012.4# <- moran.mc(S2012$ASFR35, lw.FOQ, 100)
moranI.mc.2013.4# <- moran.mc(S2013$ASFR35, lw.FOQ, 100)
moranI.mc.2014.4# <- moran.mc(S2014$ASFR35, lw.FOQ, 100)
moranI.mc.2015.4# <- moran.mc(S2015$ASFR35, lw.FOQ, 100)
moranI.mc.2016.4# <- moran.mc(S2016$ASFR35, lw.FOQ, 100)
moranI.mc.2017.4# <- moran.mc(S2017$ASFR35, lw.FOQ, 100)
moranI.mc.2018.4# <- moran.mc(S2018$ASFR35, lw.FOQ, 100)

# creating table with all my Moran's I values:
# done in Excel. Import
Moran_Table <- read.csv("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\02_Descriptive\\Historic_Moran.csv")
library(tidyverse)
Moran_Table_tidy <- gather(Moran_Table, key = Measurement, value = "Value", - Year)

library(RColorBrewer)
Yearly_Moran <- ggplot(data = Moran_Table_tidy, mapping = aes(x = Year, y = Value)) +
  geom_point(mapping = aes(color = Measurement)) +
               geom_line(mapping = aes(color = Measurement)) +
  theme_bw() +
  labs(x = "Year", y = "Global Moran's I Value")

Yearly_Moran + theme_bw() + scale_colour_colorblind()











#### Descriptive statistics ####
library(ggplot2)
#adding a title
S2002_plot <- ggplot(S2002, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2002")+ 
  theme_bw()

S2003_plot <- ggplot(S2003, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2003")+ 
  theme_bw()

S2004_plot <- ggplot(S2004, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2004")+ 
  theme_bw()

S2005_plot <- ggplot(S2005, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2005")+ 
  theme_bw()

S2006_plot <- ggplot(S2006, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2006")+ 
  theme_bw()

S2007_plot <- ggplot(S2007, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2007")+ 
  theme_bw()

S2008_plot <- ggplot(S2008, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2008")+ 
  theme_bw()

S2009_plot <- ggplot(S2009, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2009")+ 
  theme_bw()

S2010_plot <- ggplot(S2010, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2010")+ 
  theme_bw()

S2011_plot <- ggplot(S2011, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2011")+ 
  theme_bw()

S2012_plot <- ggplot(S2012, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2012")+ 
  theme_bw()

S2013_plot <- ggplot(S2013, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2013")+ 
  theme_bw()

S2014_plot <- ggplot(S2014, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2014")+ 
  theme_bw()

S2015_plot <- ggplot(S2015, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2015")+ 
  theme_bw()

S2016_plot <- ggplot(S2016, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2016")+ 
  theme_bw()

S2017_plot <- ggplot(S2017, aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2017")+ 
  theme_bw()

S2018_plot <- ggplot(S2018, mapping = aes(x=TFR, fill = Comparison)) +
  geom_histogram(binwidth=.1) +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of MSOAs", limits = c(0, 1000)) +
  scale_fill_manual("", values = c("#D55E00", "#56B4E9", "#E69F00")) +
  geom_vline(aes(xintercept=mean(TFR)),color="black", linetype="dashed", size=1)+
  ggtitle("2018") +
  theme_bw()

S2002_plot
S2010_plot
S2018_plot
install.packages("Rmisc")
library(Rmisc)
library(ggfortify)
# Load libraries
library(spdep)
library(RColorBrewer)
library(rgdal)
library(maptools)
library(lrmest)
library(fBasics)
library(perturb)
library(broom)
library(sp)
library(spatialreg)
library(ggplot2)
library(ggpubr)
library(spdep)
library(ggplot2)
??multiplot
tiff(file="02_2011_Multiplot.tif",width = 2200, height = 3500, res=225, 
     compression="lzw")
Rmisc::multiplot(S2002_plot, S2003_plot, S2004_plot, S2005_plot, 
          S2006_plot, S2007_plot, S2008_plot, S2009_plot, 
          S2010_plot, S2011_plot,cols=2)
dev.off()

tiff(file="12_2018_Multiplot.tif",width = 2200, height = 3500, res=225, 
     compression="lzw")          
Rmisc::multiplot(S2012_plot, S2013_plot,
          S2014_plot, S2015_plot, S2016_plot, S2017_plot,
          S2018_plot,cols=2)
dev.off()


ggplot(S2011) +
  geom_boxplot(aes(Region, TFR, group = Region))


df <- rbind(S2002, S2011, S2018)
ggplot(df, aes(Region, TFR)) + 
  geom_boxplot() +
  facet_wrap(~year, scales='free_y')

ggplot(df, aes(x=Region, y=TFR)) + geom_boxplot() + facet_grid(~ year)

tiff(file="2002_Box.tif",width = 2000, height = 2000, res=300, 
     compression="lzw")
ggplot(S2002, aes(Region, TFR)) + 
  geom_boxplot()  + coord_flip() +
  theme_bw()
dev.off()
tiff(file="2011_Box.tif",width = 2000, height = 2000, res=300, 
     compression="lzw")
ggplot(S2011, aes(Region, TFR)) + 
  geom_boxplot()  + coord_flip() +
  theme_bw()
dev.off()
tiff(file="2018_Box.tif",width = 2000, height = 2000, res=300, 
     compression="lzw")
ggplot(S2018, aes(Region, TFR)) + 
  geom_boxplot()  + coord_flip() +
  theme_bw()
dev.off()



# this is bad, ignore for now.
#### Just testing stuff out ####
Facet_wrapped <- ggplot(shape.shp, aes(x=TFR)) +
  geom_histogram(aes(fill = ..count..), binwidth=.1) +
  scale_fill_gradient("Count", low = "yellow", high = "red") +
  scale_x_continuous(name = "TFR", limits = c(0, 5)) +
  scale_y_continuous(name = "Count of values", limits = c(0, 1000)) +
  geom_vline(aes(xintercept=mean(TFR)),color="blue", linetype="dashed", size=1)+
  facet_grid(year ~ .)

#### had to make year a character
shape.shp %>%
  ggplot(aes(x=TFR, fill=yearr))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TFR")
