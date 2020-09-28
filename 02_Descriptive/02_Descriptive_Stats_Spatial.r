############################################################################
######                                                               #######
######        Descriptive part of the research                       #######
######        Spatial: Moran's I, time differences, spatial scales   #######
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
# Loading the data
SpatialData_SpatialPolygon <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.rds")
neighbour <- read.gal("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\neighbour.gal")
queen <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\queen.rds")
proj4string(SpatialData_SpatialPolygon) <- proj4string(SpatialData_SpatialPolygon) # I think this might only work with sp dataframe, not needed anyway (I think)
shape.ll <- spTransform(SpatialData_SpatialPolygon, CRS("+proj=longlat +datum=WGS84")) # to get long and lat, not needed here
EW_Variables <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\EW_Variables.rds")

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

lagvar <- data.frame(TFR,PDENS,INC,NONRG,SOCHO,EDU,
                     DIV, LOGPDENS, LOGINC)

#### Weights: showing them ####
tiff(file="EW_neighbours.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
plot(SpatialData_SpatialPolygon, border="NA")
plot.nb(neighbour, coords, add=TRUE) #### I adore this! Such fun my good god
dev.off()

summary(neighbour)
summary(queen)

moranI.TFR <- moran.mc(TFR, queen, 100) #
moranI.PDENS <- moran.mc(PDENS, queen, 100) #
moranI.INC <- moran.mc(INC, queen, 100) # 
moranI.NONRG <- moran.mc(NONRG, queen, 100) #
moranI.SOCHO <- moran.mc(SOCHO, queen, 100) # 
moranI.EDU <- moran.mc(EDU, queen, 100) # 
moranI.DIV <- moran.mc(DIV, queen, 100) # 

## These go in the same place as descriptive statistics
moranI.TFR
moranI.PDENS 
moranI.LOGPDENS
moranI.INC
moranI.NONRG 
moranI.SOCHO
moranI.EDU
moranI.DIV


#### Plotting Moran I
mp <- moran.plot(SpatialData_SpatialPolygon$TFR, queen,
                 labels=as.character(SpatialData_SpatialPolygon$code), pch=19)

moran.plot(as.vector(scale(SpatialData_SpatialPolygon$TFR)), queen,
           labels=as.character(SpatialData_SpatialPolygon$code), xlim=c(-2, 4), ylim=c(-2,4), pch=19)

if (require(ggplot2, quietly=TRUE)) {
  xname <- attr(mp, "xname")
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) +
    geom_smooth(formula=y ~ x, method="lm") +
    geom_hline(yintercept=mean(mp$wx), lty=2) +
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() +
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    xlab("TFR") + ylab("Spatially lagged TFR") +
    theme_bw()
}


#### Plotting lagged variables ####
# Plot lagged variables
# Define names of variables 
# Create lagged variables
lagvar <- data.frame(TFR,EDU,Pakistn, Bngldsh, Blck_Af, INC, PDENS, SOCHO, DIV, NONRG)

llv <- length(lagvar[1,])
for (i in 1:llv) {
  lagvar[,i] <- lag.listw(queen,lagvar[,i])
}

colnames(lagvar) <- c("TFRlag","EDUlag", "Pakistnlag", "Bngldshlag","Blck_Aflag","INClag", "PDENSlag","SOCHOlag", "DIVlag",
                      "NONRGlag")
attach(lagvar)

lagvar1 <- data.frame(TFR,EDU,Pakistn, Bngldsh, Blck_Af, INC, PDENS, SOCHO, DIV, NONRG)

png(file="Plot_lagged_variables_September.png", 1200, 1200)
par(mfrow=c(4,3))
for (i in 1:llv) {
  plot(lagvar1[,i],lagvar[,i], main=colnames(lagvar1[i]), 
       xlab=colnames(lagvar1[i]), 
       ylab=paste("Spatially Lagged",colnames(lagvar1[i])))
  legend("bottomright",paste("Cor:",round(cor(lagvar1[,i],lagvar[,i]),2)), 
         bty="n")  
}
dev.off()

lagvar1 <- data.frame(TFR,EDU,Pakistn, Bngldsh, Blck_Af, INC, PDENS, SOCHO, DIV, NONRG)

#### Descriptive statistics ####
c <- ggplot(lagvar, aes(x=EDU, y=EDUlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

b <- ggplot(lagvar, aes(x=Pakistn, y=Pakistnlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

c <- ggplot(lagvar, aes(x=Bngldsh, y=Bngldshlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

d <- ggplot(lagvar, aes(x=Blck_Af, y=Blck_Aflag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

e <- ggplot(lagvar, aes(x=INC, y=INClag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

f <- ggplot(lagvar, aes(x=PDENS, y=PDENSlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

g <- ggplot(lagvar, aes(x=SOCHO, y=SOCHOlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

h <- ggplot(lagvar, aes(x=DIV, y=DIVlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

i <- ggplot(lagvar, aes(x=NONRG, y=NONRGlag)) + 
  geom_point(alpha = 1/13) +
  geom_smooth(aes( method="lm", se=FALSE)) +
  theme_bw()

tiff(file="lagged_variables.tif",width = 2200, height = 3500, res=225, 
     compression="lzw")
ggarrange(a, b, c, d, e, f, g, h, i+rremove("x.text"), 
          labels = c("A", "B", "C","D","E","F","G","H","I"),
          ncol = 2, nrow = 5)
dev.off()




ggarrange(EDU_graph, Pksta_graph, Bnglad_graph, BlackAfr_graph, INC_graph, PDENS_graph, SOCHO_graph, DIV_graph, NONRG_graph12+rremove("x.text"), 
          labels = c("A", "B", "C","D","E","F","G","H","I"),
          ncol = 2, nrow = 5)
dev.off()





################### Mapping Moran's I ##############
# Load libraries
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

## Example of successful GGPLOT2
model <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG)
EW_Poly <- read_sf("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp")
EW_Combined <- fortify(EW_Poly)
EW_Combined <- merge(EW_Poly, SpatialData_SpatialPolygon, by.x = "code", by.y = "code", all.x=TRUE)
class(EW_Poly)

tiff(file="Residuals_National.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(EW_Poly) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_bw()+
  theme(panel.grid.major = element_line("white")) +
  theme(panel.background = element_rect(fill = "azure"))
dev.off()







# Load libraries
library(spdep)
library(RColorBrewer)
library(maptools)
library(rgdal)
rm(list=ls(all=TRUE))
EW_Poly <- read_sf("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp")
EW_Variables <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\EW_Variables.rds")
EW_Combined <- fortify(EW_Poly)
EW_Combined <- merge(EW_Poly, EW_Variables, by.x = "code", by.y = "Code", all.x=TRUE)
neighbour <- read.gal("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\neighbour.gal")


EW_Poly <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.rds")

########## Mapping Lisa
# Define ID
id <- EW_Poly$code

# Number of observations
n <- length(id)
attach(EW_Poly@data)

# Define variable of interest
varofint <- EW_Poly$TFR

# Define name of the variable of interest 
varofint.name <- c("TFR in 2011")

# Define neighborhood matrix (type nb) 
# (choice in this case nb.FOQ.cor, d50km, nb.5NN)
nb <- neighbour

# Define weight style (W=row-standardized)
ws <- c("W")

# Define significance level for the cluster maps 
# (0.0001,0.001,0.01,0.05)
significance <- 0.05

# p-adjustment method (can be "none", "bonferroni", "holm",
# "hochberg","hommel","fdr")
p.ad.meth <- c("none")

# Should the cluster map show only regions belonging to significent clusters, 
# or all regions
plot.only.significant <- TRUE


################################################################################
# 2.2) In this section no changes to the code are necessary                    #
################################################################################

# Transfer nb-object in listwise object
listw <- nb2listw(nb, style=ws,zero.policy=T)

# Create lagged values of variable of interest
varlag <- lag.listw(listw, EW_Poly$TFR)

# Calculate Lisa Test
lisa.outcomes <- localmoran(varofint,listw, alternative="two.sided", p.adjust.method=p.ad.meth)

# Get significance level
vec <- c(1:n)
vec <- ifelse(lisa.outcomes[,5] < significance, 1,0)

# Result object
lisa.outcomes


################################################################################            
# 3) Plot LISA-maps                                                            #
# 3.1) Preparations (no changes necessary)                                     #
################################################################################

# Calculate Mean of Variable of interest and lagged value of variable of 
# interest
m.varofint <- mean(varofint)
m.varlag <- mean(varlag)

# Derive sector
sec <- c(1:n)
for (i in 1:n) {
  if (varofint[[i]]>=m.varofint & varlag[[i]]>=m.varlag) sec[i] <- 1
  if (varofint[[i]]<m.varofint & varlag[[i]]<m.varlag) sec[i] <- 2
  if (varofint[[i]]<m.varofint & varlag[[i]]>=m.varlag) sec[i] <- 3
  if (varofint[[i]]>=m.varofint & varlag[[i]]<m.varlag) sec[i] <- 4
}

# Define colors for sectors
sec.all <- sec
colors1 <- c(1:n)
for (i in 1:n) {
  if (sec.all[i]==1) colors1[i] <- "brown2"
  if (sec.all[i]==2) colors1[i] <- "royalblue3"
  if (sec.all[i]==3) colors1[i] <- "lightblue"
  if (sec.all[i]==4) colors1[i] <- "pink"
  if (sec.all[i]==0) colors1[i] <- "white"
}

# Mark all non-significant regions white
loc.m.data <- sec*vec
colors2 <- colors1
for (i in 1:n) {
  if (loc.m.data[i]==0) colors2[i] <- "white"
}


################################################################################            
# 3.2) Plotting maps - here you might want to change the specifications of     #
#      some specifcations such as the format of the png-file which you want to #
#      export or the size of the text.                                         #                                     
################################################################################
options(sf_max.plot=2)
png(file="LISA_2011_National.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(EW_Poly, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(EW_Poly, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))

# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(EW_Poly, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()

#region can be done by editing / subsetting the shapefile
#if I have time, move to ggplot


## Now that the national works, we want to see regions. 
table(EW_Poly$Region)
Wales <- EW_Poly[EW_Poly$Region == "Wales",] 



png(file="LISA_2011_Wales.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(Wales, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(Wales, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(Wales, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()

London <- EW_Poly[EW_Poly$Region == "London",] 
png(file="LISA_2011_London.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(London, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(London, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(London, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()



South_East_EW <- EW_Poly[EW_Poly$Region == "South East",] 
png(file="LISA_2011_South_East_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(South_East_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(South_East_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(South_East_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()


South_West_EW <- EW_Poly[EW_Poly$Region == "South West",] 
png(file="LISA_2011_South_West_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(South_West_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(South_West_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(South_West_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()



West_Midlands_EW <- EW_Poly[EW_Poly$Region == "West Midlands",] 
png(file="LISA_2011_West_Midlands_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(West_Midlands_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(West_Midlands_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(West_Midlands_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()




East_Midlands_EW <- EW_Poly[EW_Poly$Region == "East Midlands",] 
png(file="LISA_2011_East_Midlands_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(East_Midlands_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(East_Midlands_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(East_Midlands_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()


North_West_EW <- EW_Poly[EW_Poly$Region == "North West",] 
png(file="LISA_2011_North_West_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(North_West_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(North_West_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(North_West_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()


Yorkshire_EW <- EW_Poly[EW_Poly$Region == "Yorkshire and The Humber",] 
png(file="LISA_2011_Yorkshire_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(Yorkshire_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(Yorkshire_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(Yorkshire_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()



North_East_EW <- EW_Poly[EW_Poly$Region == "North East",] 
png(file="LISA_2011_North_East_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(North_East_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(North_East_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(North_East_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()



East_EW <- EW_Poly[EW_Poly$Region == "East of England",] 
png(file="LISA_2011_East_EW.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(East_EW, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(East_EW, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))
# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(East_EW, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()














##### Now trying it at the LAD level ####
S2011_LAD <- readRDS("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\S2011_LAD.rds")

library(tidyverse) 
library(sf) # for read_sf()
library(spatialEco) # for sp.na.omit()
library(spdep) # for spatial weights
library(raster) # for size of polygon (pop density)
library(dplyr)
library(rgdal)


Eng_Poly_LAD <- shapefile("England_LAD.shp")
summary(Eng_Poly_LAD)
Wal_Poly_LAD <- shapefile('Wales_LAD.shp')
summary(Wal_Poly_LAD)
summary(S2011_LAD)
Eng_Wal_SP_LAD <- rbind(Eng_Poly_LAD, Wal_Poly_LAD)

Eng_Poly_LAD <- st_read("England_LAD.shp")
Wal_Poly_LAD <- st_read('Wales_LAD.shp')
Eng_Wal_SP_LAD <- rbind(Eng_Poly_LAD, Wal_Poly_LAD)
summary(Eng_Wal_SP_LAD)
class(Eng_Wal_SP_LAD)

EW_Combined_LAD <- fortify(Eng_Wal_SP_LAD)
EW_Combined_Lad <- merge(Eng_Wal_SP_LAD, S2011_LAD, by.x = "name", by.y = "LAD17NM.x")
summary(EW_Combined_Lad)

neighbour <- poly2nb(EW_Combined_Lad, queen=TRUE, row.names=EW_Combined_Lad$code) # define neighbouring 
summary(neighbour) # 7200, 41870 nonzero links, 5.81 average number
queen <- nb2listw(neighbour, style="W", zero.policy = TRUE) # define weights

## For mapping
#writeOGR(obj=Eng_Wal_SP_LAD, dsn="C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData", layer="Eng_Wal_SP_LAD", driver="ESRI Shapefile") # this is in geographical projection
#EW_Poly <- read_sf("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\Eng_Wal_SP_LAD.shp")

## merging
EW_Combined_LAD <- fortify(EW_Combined_Lad)
glimpse(S2011_LAD)
glimpse(EW_Poly)
summary(EW_Combined_Lad)
plot(EW_Combined_Lad)

s########## Mapping Lisa
# Define ID
EW_Poly <- EW_Combined_Lad
id <- EW_Combined_Lad$name

# Number of observations
n <- length(id)
attach(EW_Combined_Lad@data)

# Define variable of interest
varofint <- EW_Combined_Lad$TFR
S2011_LAD
EW_Combined_Lad
names(EW_Poly)

# Define name of the variable of interest 
varofint.name <- c("TFR in 2011")

# Define neighborhood matrix (type nb) 
# (choice in this case nb.FOQ.cor, d50km, nb.5NN)
nb <- neighbour

# Define weight style (W=row-standardized)
ws <- c("W")

# Define significance level for the cluster maps 
# (0.0001,0.001,0.01,0.05)
significance <- 0.05

# p-adjustment method (can be "none", "bonferroni", "holm",
# "hochberg","hommel","fdr")
p.ad.meth <- c("none")

# Should the cluster map show only regions belonging to significent clusters, 
# or all regions
plot.only.significant <- TRUE


################################################################################
# 2.2) In this section no changes to the code are necessary                    #
################################################################################

# Transfer nb-object in listwise object
listw <- nb2listw(neighbour, style=ws,zero.policy=T)
??nb2listw

summary(listw)
# Create lagged values of variable of interest
varlag <- lag.listw(listw, EW_Combined_Lad$TFR, zero.policy=T)
listw <- queen
# Calculate Lisa Test
lisa.outcomes <- localmoran(varofint,listw, alternative="two.sided", p.adjust.method=p.ad.meth, zero.policy=T)
summary(EW_Combined_Lad)

# Get significance level
vec <- c(1:n)
vec <- ifelse(lisa.outcomes[,5] < significance, 1,0)

# Result object
lisa.outcomes


################################################################################            
# 3) Plot LISA-maps                                                            #
# 3.1) Preparations (no changes necessary)                                     #
################################################################################

# Calculate Mean of Variable of interest and lagged value of variable of 
# interest
m.varofint <- mean(varofint)
m.varlag <- mean(varlag)

# Derive sector
sec <- c(1:n)
for (i in 1:n) {
  if (varofint[[i]]>=m.varofint & varlag[[i]]>=m.varlag) sec[i] <- 1
  if (varofint[[i]]<m.varofint & varlag[[i]]<m.varlag) sec[i] <- 2
  if (varofint[[i]]<m.varofint & varlag[[i]]>=m.varlag) sec[i] <- 3
  if (varofint[[i]]>=m.varofint & varlag[[i]]<m.varlag) sec[i] <- 4
}

# Define colors for sectors
sec.all <- sec
colors1 <- c(1:n)
for (i in 1:n) {
  if (sec.all[i]==1) colors1[i] <- "brown2"
  if (sec.all[i]==2) colors1[i] <- "royalblue3"
  if (sec.all[i]==3) colors1[i] <- "lightblue"
  if (sec.all[i]==4) colors1[i] <- "pink"
  if (sec.all[i]==0) colors1[i] <- "white"
}

x = TRUE
# Mark all non-significant regions white
loc.m.data <- sec*vec
colors2 <- colors1
for (i in 1:n) {
  if (loc.m.data[i]==0) colors2[i] <- "white"
}


################################################################################            
# 3.2) Plotting maps - here you might want to change the specifications of     #
#      some specifcations such as the format of the png-file which you want to #
#      export or the size of the text.                                         #                                     
################################################################################
options(sf_max.plot=2)
png(file="LISA_2011_National.png",width = 3200, height = 1500, res=300)
# Cluster map
par(mfrow=c(1,2),mar=c(1, 1, 3, 1))
if (plot.only.significant==TRUE) 
{plot(EW_Poly, col=colors2, border="lightgrey",lwd=0.1)} else 
{plot(EW_Poly, col=colors1, border="lightgrey",lwd=0.1)}
legend("topleft",fill=c("brown2","royalblue3","lightblue",
                        "pink","white"),
       legend=c("High-High","Low-Low","Low-High","High-Low"),
       border = "grey25", cex=0.8, bg="white", 
       box.col="white")
title(paste("Significant Clusters\n",varofint.name))

# Significance map
sig.data <- data.frame(lisa.outcomes[,5])
m <- length(sig.data)
colors <- matrix(nrow=n,ncol=m, data=rep(0,n*m))
for (i in 1:m) {
  signific <- sig.data[,i]
  lb <- 5
  bins <- c(0,0.0001,0.001,0.01,0.05,1)
  colpal <- c(rev(brewer.pal(lb+1, "YlGn")[-c(1:2)]),"white")
  colors[,i] <- colpal[findInterval(signific, bins, rightmost.closed=T)]
  colors[vec==0,i] <- c("white")
  plot(EW_Poly, col=colors[,i], border="lightgrey", lwd=0.1)
  title("Significance Level")    
  colpalad <- colpal
  colpalad[c(which(bins==significance):length(colpalad))] <- c("white")
  binsp <- c("0","0.0001","0.001","0.01","0.05",1)
  legend("topleft",fill=colpalad,
         legend=paste(binsp[-length(bins)],"-",binsp[-1]), border="grey25", 
         cex=0.8, bg="white", box.col="white")
}
dev.off()

