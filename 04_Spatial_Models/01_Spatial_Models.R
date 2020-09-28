############################################################################
######                                                               #######
######        Spatial Models, building on OLS                        #######
######        Answers research Question 3                            #######
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

# Loading the data, need to read in with OGR or you the order is a mess
SpatialData_SpatialPolygon <-readOGR("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp")
EW_Variables <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\EW_Variables.rds") ## need to move location, so can download

neighbour <- read.gal("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\neighbour.gal")
queen <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\queen.rds")
proj4string(SpatialData_SpatialPolygon) <- proj4string(SpatialData_SpatialPolygon) # I think this might only work with sp dataframe, not needed anyway (I think)
shape.ll <- spTransform(SpatialData_SpatialPolygon, CRS("+proj=longlat +datum=WGS84")) # to get long and lat, not needed here


head(SpatialData_SpatialPolygon$row_num)
is.vector(SpatialData_SpatialPolygon$row_num)
id <- as.numeric(paste(SpatialData_SpatialPolygon$row_num))
head(id)
is.vector(id)
n <- length(id)
sq <- c(1:n)
sq_text <- paste(sq)
coords <- coordinates(SpatialData_SpatialPolygon)
coords.ll <- coordinates(SpatialData_SpatialPolygon)
attach(SpatialData_SpatialPolygon@data)
LOGPDENS <- log(PDENS)

listw <- queen

data.frame <- SpatialData_SpatialPolygon

# In case your test statistics indicate that substantial spatial 
# auto-correlation remains in every meaningful lm-Model specifiation, you can
# run a spatial lag and/ or spatial error model
modeltext <- TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG

WorkdayRatio
# Issue that the Spatial File has the data in wrong order.

# Linear model
lmmodel <- lm(modeltext, data = SpatialData_SpatialPolygon)

#### Spatial lag model ####
lagmodel <- lagsarlm(modeltext, method="Matrix_J", listw=queen, data = SpatialData_SpatialPolygon)

#### Spatial Error Model ####
errormodel <- errorsarlm(modeltext,method="Matrix_J", listw=queen, data = SpatialData_SpatialPolygon)

#### Spatial Durbin Model ####
durbinmodel <- lagsarlm(modeltext,method="Matrix",listw=queen,type="Durbin", data = SpatialData_SpatialPolygon) # original(doesn't work)
summary(errormodel)

#### Spatial Durbin Error Model ####
durbinerrormodel <- errorsarlm(modeltext,method="Matrix_J", listw=queen, etype="mixed", data = SpatialData_SpatialPolygon)

# Summaries of these models, for write-up but not for exploration
options(digits = 4)
summary(lmmodel)
summary(lagmodel) 
summary(errormodel)
summary(durbinmodel)
summary(durbinerrormodel)

### Residual Analysis and model selection ###
lm.morantest(lmmodel, queen, alternative="two.sided")
lm.LMtests(lmmodel, queen, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
moran.test(TFR, queen)


Hausman.test(errormodel, varformula=NULL, studentize = TRUE, data=list())) 
??Hausman.test

bptest.sarlm(lagmodel, studentize = TRUE)
bptest.sarlm(errormodel, studentize = TRUE)
bptest.sarlm(durbinmodel)
bptest.sarlm(durbinerrormodel)
 
summary(EW_Variables)

## https://maczokni.github.io/crimemapping_textbook_bookdown/spatial-regression-models.html ##

## https://rspatial.org/spatial/Spatialdata.pdf

## https://cds.cern.ch/record/1252081/files/9780387781709_TOC.pdf ##

W <- as(queen, "CsparseMatrix")
trMC <- trW(W, type="MC")
im<-impacts(lagmodel, tr=trMC, R=100)
sums<-summary(im,  zstats=T)
#To print the coefficients
data.frame(sums$res)
data.frame(sums$pzmat)
write.csv(sums$res,"C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\04_Spatial_Models\\lagmodel_impacts_res.csv")
write.csv(sums$pzmat,"C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\04_Spatial_Models\\lagmodel_impacts_p.csv")

#(durbinmodel,listw=listw.wts)
#summary(impacts(reg5,listw=listw.wts,R=500),zstats=TRUE)#for p values

#trMatc <- trW(W, type="mult")

#??eigenw()
imdurbin<-impacts(durbinmodel, tr=trMC, R=50)
sumsdurbin<-summary(imdurbin,  zstats=T)
data.frame(sumsdurbin$res)
data.frame(sumsdurbin$pzmat)
names(sumsdurbin)
write.csv(sumsdurbin$res,"C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\04_Spatial_Models\\durbinmodel_impacts_res.csv")
write.csv(sumsdurbin$pzmat,"C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\04_Spatial_Models\\durbinmodel_impacts_p.csv")


## It worked this time, and I think it's because I reran the model with Matrix rather than J_matrix, but I'm not sure
#To print the coefficients
imdurbinerrormodel<-impacts(durbinerrormodel, tr=trMC, R=50)
sumsdurbinerrormodel<-summary(imdurbinerrormodel,  zstats=T)
data.frame(sumsdurbinerrormodel$impacts)
data.frame(sumsdurbinerrormodel$pzmat)
names(sumsdurbinerrormodel)


write.csv(sumsdurbinerrormodel$impacts,"C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\04_Spatial_Models\\sumsdurbinerrormodel_impacts_res.csv")
write.csv(sumsdurbinerrormodel$pzmat,"C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\04_Spatial_Models\\sumsdurbinerrormodel_impacts_p.csv")




summary(SpatialData_SpatialPolygon)


















## follwoing the how to guide by Bivand: https://rdrr.io/cran/spatialreg/man/impacts.html
W <- as(queen, "CsparseMatrix")
trMatc <- trW(W, type = "mult")
trMC <- trW(W, type = "MC")



### Now tyring with the lagmode, this data seems to be for the lag model
#impacts(lagmodel, listw=queen)
#impacts(lagmodel, tr=trMatc)
#impacts(lagmodel, tr=trMC)
#impacts(lagmodel, evalues=ev)

# nicely talks about results
# ://rural-urban.eu/sites/default/files/05_Spatial%20Autocorrelation%20and%20the%20Spatial%20Durbin%20Model_Eilers.pdf

library(coda)
lagmodelIQ5 <- impacts(lagmodel, tr=trMatc, R=200, Q=5)



## Trying to run the Dubin model
#impacts(durbinmodel, tr=trMC) # do not run

## focussing on this now
## run this when I'm ready
summary(impacts(durbinmodel, tr=trMatc, R=1000), zstats=TRUE, short=TRUE) # this is for type "mixed"

# recieve an error
is.positive.definite(queen)

# I think there is a problem with my matrix, so I've run this:
ev <- eigenw(queen)

library(corpcor)
help("make.positive.definite")

## now trying to make everything positive, I think. 
queennn <- make.positive.definite(queen, tol=1e-3)




### Trying to reryb things but with a neighbour file
listw <- spdep::nb2listw(neighbour)
ev <- eigenw(listw)



#Spatial Hausman Test
Hausman.test(errormodel)














##### Part 2 of his Burkey's Informative work, no more than this ####
#Go the the very end for the commands from the first video!  
#I highly recommend watching all of the previous videos: Playlist: https://www.youtube.com/playlist?list=PLlnEW8MeJ4z6Du_cbY6o08KsU6hNDkt4k
#At least watch the one before this one:L https://youtu.be/b3HtV2Mhmvk

#1) Download BurkeyAcademy's R Spatial Regression ZIP file from https://sites.google.com/a/burkeyacademy.com/spatial/home/files, click on R Spatial Regression 2.zip
#2) Extract to a directory
#3) Open RStudio
# I could handle the rest for you, but you need to learn!
#4) Click File, New Project, and create the project inside the directory from 1). Name it what you like.
#5) Click File, New File, R Script. Paste this text document into the window that opens

#To get caught up from the first video, run the following Block of commands:
#You can highlight them and click run (top right of this window)
#************************Begin Block******************
install.packages("spdep")
install.packages("rgdal")
install.packages("rgeos")
library(rgdal)
spat.data = readOGR(dsn = ".", layer = "NCVACO")
spat.data$PCI=as.numeric(levels(spat.data$PCI))[spat.data$PCI]
library(spdep)
queen.nb=poly2nb(spat.data) 
reg.eq1=DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX+ENTRECP
queen.listw=nb2listw(queen.nb) #convert nb to listw type
listw1= queen.listw

reg1=lm(reg.eq1,data=spat.data)                  #OLS
reg2=lmSLX(reg.eq1,data=spat.data, listw1)       #SLX
reg3=lagsarlm(reg.eq1,data= spat.data, listw1)   #Lag Y
reg4=errorsarlm(reg.eq1,data=spat.data, listw1)  #Spatial Error
#************************End Block******************

#****************************New Commands for Video 2**************************
#SDEM Spatial Durbin Error Model (add lag X to SEM)   y=XB+WxT+u,   u=LWu+e
reg5=errorsarlm(reg.eq1, data=spat.data, listw1, etype="emixed")

#SDM Spatial Durbin Model (add lag X to SAR) y=pWy+XB+WXT+e 
reg6=lagsarlm(reg.eq1, data=spat.data,listw1, type="mixed")

#Manski All-inclusive Model: y=pWy+XB+WXT+u,   u=LWu+e (not recommended)
reg7=sacsarlm(reg.eq1,data=spat.data, listw1, type="sacmixed") #listw2 allows for a different weights matrix for the error structure if desired

#SARAR a.k.a. Kelejian-Prucha, Cliff-Ord, or SAC If all T=0,y=pWy+XB+u, u=LWu+e
reg8=sacsarlm(reg.eq1,data=spat.data,listw1, type="sac") #listw2 allows for a different weights matrix for the error structure if desired

#SARMA (like SARAR, but more local error structure)
#y=ρWy+Xβ+u,   u=I-λWε   or
#y=ρWy+Xβ+u,   u=λWε+ε (2 ways of writing the same thing)
#Can't be done easily in R from what I can tell. Investigating further...

# We could type "summary(reg1)" to get a summary. I like making a shortcut:
s=summary
#Now use s() instead of summary()

s(reg1)#OLS
s(reg2)#SLX
s(reg3)#Lag Y
s(reg4)#Lag Error (SEM)
s(reg5)#Durbin Error (SDEM)
s(reg6)#Durbin (SDM)
s(reg7)#Manski kitchen sink!
s(reg8)#SARAR lag Y and lag e (aka SAC, many other names)

#Anything with a lag Y in it: 
#***You cannot interpret the coeficients as marginal effects***
#This includes the LagY, SDM, MANSKI, SARAR
#                 reg3, reg6, reg7,  reg8 
#You need to use the "impacts" command to calculate the 
#direct, indirect, and total marginal effects.
#Marginal effects
s(reg5)
impacts(reg5,listw=listw.wts)
summary(impacts(reg5,listw=listw.wts,R=500),zstats=TRUE)#for p values

#LR Tests: Test Model Restrictions: 
LR.sarlm(reg5, reg4) #likelihood ratio test to see if SDEM should be restricted to the SEM. 
#You can test any model to see if it should be restricted to a simpler, NESTED model. 
#SDM and SDEM are not nested, as one cannot be simplified into the other.
LR.sarlm(reg4, reg5) #order you put the models in doesn't matter

LR.sarlm(reg5, reg2) #likelihood ratio test to see if SDEM should be restricted to the SLX. 
LR.sarlm(reg5, reg1) #likelihood ratio test to see if SDEM should be restricted to OLS. 


#Do a spatial Breusch-Pagan Test for Heteroskedasticity
bptest.sarlm(reg5,studentize=TRUE)

#If we want to get an idea of how accurately our model "Fits" the data, we can 
#calculate a Pseudo R^2
1-(reg5$SSE/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)-1)))





#########Commands from Video 1#########################
#1) Download BurkeyAcademy's R Spatial Regression ZIP file from https://sites.google.com/a/burkeyacademy.com/spatial/home/files, click on R Spatial Regression 1.zip
#2) Extract to a directory
#3) Open RStudio
# I could handle the rest for you, but you need to learn!
#4) Click File, New Project, and create the project inside the directory from 1). Name it what you like.
#5) Click File, New File, R Script. Paste this text document into the window that opens
#If you have not previously downloaded these packages, run these three commands. 
#You can highlight them and click run (top right of this window)
install.packages("spdep")
install.packages("rgdal")
install.packages("rgeos")
#Let's Read in the SHP file (Map + Data in one) using rgdal
#See video https://www.youtube.com/watch?v=FHBDAGc9-8E for how create this
library(rgdal)
spat.data = readOGR(dsn = ".", layer = "NCVACO")
names(spat.data) #show variable names
summary(spat.data)
#See how some of our quantitative variables are being treated like qualitative (e.g.PCI)
#which is Per Capita Income (R lists frequencies rather than summary stats). 
#This will cause problems. Fix this by overwriting these variables with a numeric version:
spat.data$PCI=as.numeric(levels(spat.data$PCI))[spat.data$PCI]
#Now summary will treat it as a number, calculating a mean. If we don't do this R thinks
#These variables are categorical.
spplot(spat.data,"SALESPC") #make map
#load library spdep, make weights matrix (nb type)
library(spdep)
queen.nb=poly2nb(spat.data) 
rook.nb=poly2nb(spat.data,queen=FALSE) 
queen.listw=nb2listw(queen.nb) #convert nb to listw type
rook.listw=nb2listw(rook.nb) #convert nb to listw type
listw1= queen.listw
#define our regression equation so we don't have to type it each time
reg.eq1=DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX+ENTRECP
#turn off scientific notation for reasonably-sized values
options(scipen=7)
#Let's run the Four simplest models: OLS, SLX, Lag Y, and Lag Error
#OLS
reg1=lm(reg.eq1,data=spat.data)
reg1b=lm(DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX+ENTRECP-1,data=spat.data)
summary(reg1)
summary(reg1b)
lm.morantest(reg1,listw1)
lm.LMtests(reg1,listw1,test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
#p=rho, T=theta, and L=lambda
#SLX Spatially Lagged X y=Xß+WXT+e
reg2=lmSLX(reg.eq1,data=spat.data, listw1)
summary(reg2)
impacts(reg2,listw=listw1)
summary(impacts(reg2,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals; R=500 not needed for SLX
#or just use OLS, doing it "by hand"
#create lagged x's
x1=model.matrix(reg1) #x values used in OLS regression #1
#create lagged X values, change name prepending "lagx."
lagx1=create_WX(x1,listw1,prefix="lagx") 
spat.data2=cbind(spat.data,lagx1)
reg2b=lm(DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX+ENTRECP+ lagx.SALESPC+ lagx.COLLENRP +lagx.BKGRTOABC +lagx.BAPTISTSP +lagx.BKGRTOMIX +lagx.ENTRECP, data=spat.data2)
reg2c=lm(DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX+ENTRECP+ lagx.SALESPC+ lagx.COLLENRP +lagx.BKGRTOABC +lagx.BAPTISTSP +lagx.BKGRTOMIX +lagx.ENTRECP-1, data=spat.data2)
summary(reg2b) #only difference is in R^2 calculation& F-Stat? Strange! 
summary(reg2c)
#Which is "right"? Doing it by hand give the correct answers- bug in lmSLX.
rsq.reg2=1-sum(reg2$residuals^2)/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)-1))
rsq.reg2b=1-sum(reg2b$residuals^2)/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)-1))
rsq.reg2  
rsq.reg2b
#I confirmed this bug with Roger Bivand and he has fixed the code- 
#it might take a little while to make it into the next spdep update.
#To follow this see https://github.com/r-spatial/spdep/commit/becba8b9f8861421124f6a947390fb4e57b8e0ef
#SAR (Sorry Roger Bivand!) Spatial Lag (Autoregressive) Model y=pWy+XB+e 
reg3=lagsarlm(reg.eq1,data= spat.data, listw1)
summary(reg3)
#Anything with a Lag Y in it: You Cannot interpret Betas as marginal effects.
#Anything with SLX in it: Also need to think about total impact (Direct+Indirect)
#You must use the "impacts" Command:
impacts(reg2,listw=listw1)
summary(impacts(reg2,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals; R=500 not needed for SLX
impacts(reg3,listw=listw1)
summary(impacts(reg3,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals
#Caution: These pvalues are simulated, and seem to vary a bit from run to run.
#SEM Spatial Error Model  y=XB+u,   u=LWu+e
reg4=errorsarlm(reg.eq1,data=spat.data, listw1)
summary(reg4)
#Spatial Hausman Test
Hausman.test(reg4)
######End Commands from Video 1##########################
