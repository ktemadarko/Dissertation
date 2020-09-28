############################################################################
######                                                               #######
######        Descriptive part of the research                       #######
######        Answers research Question 1 & Descriptive Statistics   #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
############################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))

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
library(tidyverse)

# Loading the data
SpatialData_SpatialPolygon <-readOGR("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp")
EW_Variables <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\EW_Variables.rds") ## need to move location, so can download
neighbour <- read.gal("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\neighbour.gal")
queen <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\queen.rds")

head(SpatialData_SpatialPolygon$ID)
is.vector(SpatialData_SpatialPolygon$ID)
id <- as.numeric(paste(SpatialData_SpatialPolygon$ID))/1
head(id)
is.vector(id)
n <- length(id)
sq <- c(1:n)
sq_text <- paste(sq)
coords <- coordinates(SpatialData_SpatialPolygon)
coords.ll <- coordinates(SpatialData_SpatialPolygon)
attach(SpatialData_SpatialPolygon@data)
LOGINC <- log(INC)
LOGPDENS <- log(PDENS)

# Define neigborhood weight matrix to create lagged variables
listw <- queen

data.frame <- SpatialData_SpatialPolygon
modeltext <- TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG


lagvar <- data.frame(TFR,EDU,Pakistn, Bngldsh, Blck_Af, INC, PDENS, SOCHO, DIV, NONRG)
#### Descriptive statistics ####
EDU_graph <- ggplot(lagvar, aes(x=EDU, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  scale_x_continuous(name = "University-level education") +
  theme_bw()

Pksta_graph <- ggplot(lagvar, aes(x=Pakistn, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
scale_x_continuous(name = "Female Pakistani Population") +
  theme_bw()

Bnglad_graph <- ggplot(lagvar, aes(x=Bngldsh, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+  
  scale_x_continuous(name = "Female Bangladeshi Population") +
  theme_bw()

BlackAfr_graph <- ggplot(lagvar, aes(x=Blck_Af, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+  
  scale_x_continuous(name = "Female Black African Population") +
  theme_bw()

INC_graph <- ggplot(lagvar, aes(x=INC, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+ 
  scale_x_continuous(name = "Net Income (£)") +
  theme_bw()

PDENS_graph <- ggplot(lagvar, aes(x=PDENS, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+  
  scale_x_continuous(name = "Population Density (Km Squared)") +
  theme_bw()

SOCHO_graph <- ggplot(lagvar, aes(x=SOCHO, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+ 
  scale_x_continuous(name = "Socially Rented Dwellings") +
  theme_bw()

DIV_graph <- ggplot(lagvar, aes(x=DIV, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+  
  scale_x_continuous(name = "Divorced Population") +
  theme_bw()

NONRG_graph12 <- ggplot(lagvar, aes(x=NONRG, y=TFR)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE))+  
  scale_x_continuous(name = "Non-Religious Population") +
  theme_bw()



tiff(file="TFR_Variable_Graphs.tif",width = 2200, height = 3500, res=225, 
     compression="lzw")   
ggarrange(EDU_graph, Pksta_graph, Bnglad_graph, BlackAfr_graph, INC_graph, PDENS_graph, SOCHO_graph, DIV_graph, NONRG_graph12+rremove("x.text"), 
          labels = c("A", "B", "C","D","E","F","G","H","I"),
          ncol = 2, nrow = 5)
dev.off()


#### Correlation Matrix ####
av.var <- data.frame(TFR,EDU,Pakistani, Bangladeshi, Black_African, INC, LOGPDENS, SOCHO, DIV, NONRG)
av.var.nam <- colnames(av.var)
len.av.var <- length(av.var[1,])

cor.mat <- data.frame(matrix(nrow=len.av.var, ncol=len.av.var))


## for pretty ggplot plot
install.packages("ggcorrplot")                      # Install ggcorrplot package
library("ggcorrplot") 
ggcorrplot(cor(av.var)) 

## to get sig levels
install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(av.var))
mydata.rcorr
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
















for (i in 1:len.av.var) {
  for (j in 1:len.av.var) {
    cor.mat[i,j] <- round(cor(av.var[,i],av.var[,j]),3) 
  }
}
colnames(cor.mat) <- av.var.nam
rownames(cor.mat) <- av.var.nam
res.cor.mat <- cor.mat
res.cor.mat[upper.tri(res.cor.mat)] <- c(" ")
res.cor.mat # this is what I'd want to print

write.table(res.cor.mat,"Correlation-matrix_1608.csv", sep=",")

### Descriptive linear
SpatialData_SpatialPolygon %>%
  ggplot(aes(x = NONRG, y = TFR)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", fill = NA)

names(SpatialData_SpatialPolygon)


## Descriptive boxplots
ggplot(EW_Variables, aes(Region, TFR, group = Region)) + geom_boxplot()# + coord_flip()


test_data <-  SpatialData_SpatialPolygon %>%
  gather('Region', 'TFR', 'PDENS','NONRG','INC','SOCHO','DIV', key =  value = 'TFR')


names(SpatialData_SpatialPolygon)

library(reshape2)
EW_Variables_melted <- melt(EW_Variables, id.vars = "Code", measure.vars = c("TFR") )

ggplot(EW_Variables_melted) +
  geom_boxplot(aes(Region, EDU, group = Region)) #, colour = Region))

### Beautiful graphs for bivariate analysis ###
ggplot(EW_Variables_melted) +
  geom_smooth(mapping = aes(x = EDU, y = TFR, colour = Region))

ggplot(EW_Variables_melted) +
  geom_smooth(mapping = aes(x = DIV, y = TFR, colour = Region))

ggplot(EW_Variables_melted) +
  geom_smooth(mapping = aes(x = SOCHO, y = TFR, colour = Region))

ggplot(EW_Variables_melted) +
  geom_smooth(mapping = aes(x = NONRG, y = TFR, colour = Region))


                
ggplot(EW_Variables_melted,mapping = aes(x = EDU, y = TFR)) +
  geom_point(mapping = aes(colour = Region)) +
  geom_smooth(mapping = aes(linetype = Region))


#### Histograms
ggplot(data = EW_Variables_melted, mapping = aes(x = TFR)) +
 geom_histogram(binwidth=0.05)

#### Scatterplots with transparency
ggplot(EW_Variables)+
  geom_point(mapping = aes(x = EDU, y = TFR),
             alpha = 1/6)+
  geom_smooth(aes( method="lm", se=FALSE))

EDU_graph <- ggplot(EW_Variables, aes(x=EDU, y=TFR)) + 
  geom_point( alpha = 1/6) +
  geom_smooth(aes( method="lm", se=FALSE))

modeltext <- TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG

### Standard table for descritpvie statistics
library(arsenal)
names(EW_Variables)
table_one <- tableby(~ TFR + EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG)
table_one <- summary(table_one, title = "Variables")
as.data.frame(table_one)
write.csv(table_one, "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\02_Descriptive\\descriptive.variables.csv")

library(table1)
table2 <- table1::table1(~TFR + PDENS + NONRG + INC + SOCHO + EDU + DIV, data = EW_Variables)

## quick Moran test for variables
moranI.pakistani <- moran.mc(SpatialData_SpatialPolygon$Pakistn, queen, 100)
moranI.blackafrican <- moran.mc(SpatialData_SpatialPolygon$Blck_Af, queen, 100)
moranI.bangladeshi <- moran.mc(SpatialData_SpatialPolygon$Bngldsh, queen, 100)
