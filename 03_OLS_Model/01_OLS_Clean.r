############################################################################
######                                                               #######
######        OLS Results and Diagnostics                            #######
######        Answers research Question 2                            #######
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
library(tidyverse)
library(modelr)

# Loading the data
SpatialData_SpatialPolygon <-readOGR("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp")
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

#############################
# Here we write a function that assigns significance stars to p-values
# Whenever we will call sig.stars(x) with x being a number, it will perform 
# this code.

sig.stars <- function(x) {
  len <- length(x)
  res <- c(1:len)       
  for (i in 1:len) {
    pv <- x[i]
    if (pv <0.001) pv <- c("***")
    if (pv >=0.001 & pv <0.01) pv <- c("**")
    if (pv >=0.01 & pv <0.05) pv <- c("*")
    if (pv >=0.05 & pv <0.1) pv <- c("'")
    if (pv >=0.1) pv <- c(" ")
    res[i] <- pv
  }    
  res
}


################################################################################
# 4)   Model Data                                                              #
# 4.1) Define Linear Models - Here you can fill in as many models as you       #
#      like with models numbered in ascending order                            #
################################################################################


# reminder of varaibles:
# (TFR,
our.models           <- list()
our.weight.mats      <- list()
LOGINC <- log(INC)
LOGPDENS <- log(PDENS)
modeltext <- TFR~ EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG
listwise <- queen
lw.FOQ <- queen


## just using logged PDENS
our.models[[1]]        <- lm(TFR~EDU)
our.weight.mats[[1]]   <- lw.FOQ
our.models[[2]]        <- lm(TFR~EDU + Pakistn)
our.weight.mats[[2]]   <- lw.FOQ
our.models[[3]]        <- lm(TFR~EDU + Pakistn + Bngldsh)
our.weight.mats[[3]]   <- lw.FOQ
our.models[[4]]        <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af)
our.weight.mats[[4]]   <- lw.FOQ
our.models[[5]]        <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC)
our.weight.mats[[5]]   <- lw.FOQ
our.models[[6]]        <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS)
our.weight.mats[[6]]   <- lw.FOQ
our.models[[7]]        <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO)
our.weight.mats[[7]]   <- lw.FOQ
our.models[[8]]        <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV)
our.weight.mats[[8]]   <- lw.FOQ
our.models[[9]]        <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG)
our.weight.mats[[9]]   <- lw.FOQ



# Obtain information on number of models
num.models <- length(our.models)

# Derive names of variables used as covariates in the models
namvar <- list()
for (i in 1:num.models) {
  namvar[[i]] <- c(colnames(our.models[[i]]$model)[-1])
}
namvarl <- unique(unlist(namvar))
l.namvarl <- length(namvarl)


# Summary of models
for (i in 1:num.models) {
  our.models[[i+num.models]] <- summary(our.models[[i]])
}

# Calculate Moran's I test for Spatial Autocorrelation in the error term
for (i in 1:num.models) {
  our.models[[i+2*num.models]] <- lm.morantest(our.models[[i]],
                                               our.weight.mats[[i]])
}

# Calculate Lagrange Multiplier tests (Spatial Lag vs. Spatial Error)
for (i in 1:num.models) {
  our.models[[i+3*num.models]] <- lm.LMtests(our.models[[i]], 
                                             our.weight.mats[[i]], test=c("LMerr","RLMerr","LMlag","RLMlag"))
}

# Calculate Moran's I test for Dependent Variables
for (i in 1:num.models) {
  dpvar <- our.models[[i]]$model[1]
  our.models[[i+4*num.models]] <- moran.test(dpvar[,1], our.weight.mats[[i]]) 
}

# Calculate AIC
for (i in 1:num.models) {
  our.models[[i+5*num.models]] <- AIC(our.models[[i]])
}

# Checking for Multicollinearity with kappa function
for (i in 1:num.models) {
  our.models[[i+6*num.models]] <- colldiag(our.models[[i]])
}

# Calculate Jarque-Bera Test on Residuals (whether skewness and kurtosis are
# matching a normal distribution)
for (i in 1:num.models) {
  our.models[[i+7*num.models]] <- jarqueberaTest(our.models[[i]]$residuals)
}

################################################################################
# 4.3) Code for table showing results of your models                           #
################################################################################

# The following code automatically creates a result table based on the model 
# specifications which you set in section 4.1
results <- data.frame(matrix(ncol=num.models*2,nrow=3+l.namvarl))

for (i in 1:num.models) {
  colnames(results)[(i*2)-1] <- paste("Model",i)
  colnames(results)[(i*2)] <- paste(" ")
  results[1,(i*2)-1] <- paste(colnames(our.models[[i]]$model)[1])
  results[1,(i*2)] <- paste(" ")
  results[2,] <- paste(" ")
  results[3,] <- rep(c("Coefficients","p-value"),num.models)
  paste("  ",our.models[[i]]$call[2],"  ")
  varinmod <- c(colnames(our.models[[i]]$model)[-1])
  ncof <- length(varinmod)
  ord <- c(1:ncof)
  for (j in 1:ncof) {
    ord[j] <- which(namvarl == varinmod[j])+1
  }
  ordi <- c(1,ord)
  l.ordi <- length(ordi)
  for (k in 1:l.ordi) {
    results[ordi[k]+3,(i*2)-1] <- 
      round(our.models[[i+num.models]]$coefficients[k,1],3)
    results[ordi[k]+3,(i*2)] <- 
      sig.stars(our.models[[i+num.models]]$coefficients[k,4])
  }
  results[5+l.namvarl,] <- rep("------",2*num.models)
  results[6+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+num.models]]$adj.r.squared,2)
  results[6+l.namvarl,(i*2)] <- c("-")
  results[7+l.namvarl,(i*2)-1] <- round(our.models[[i+5*num.models]],2)
  results[7+l.namvarl,(i*2)] <- c("-")
  results[8+l.namvarl,] <- rep("------",2*num.models) 
  results[9+l.namvarl,(i*2)-1] <- 
    paste(attributes(our.weight.mats[[i]])$call[2])
  results[9+l.namvarl,(i*2)] <- c("-")
  results[10+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+4*num.models]]$estimate[1],2)
  results[10+l.namvarl,(i*2)] <- 
    sig.stars(our.models[[i+4*num.models]]$p.value)
  results[11+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+2*num.models]]$estimate[1],2)
  results[11+l.namvarl,(i*2)] <- 
    sig.stars(our.models[[i+2*num.models]]$p.value)
  results[12+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+3*num.models]]$LMerr$statistic,3)
  results[12+l.namvarl,(i*2)] <- 
    sig.stars(our.models[[i+3*num.models]]$LMerr$p.value)
  results[13+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+3*num.models]]$RLMerr$statistic,3)
  results[13+l.namvarl,(i*2)] <- 
    sig.stars(our.models[[i+3*num.models]]$RLMerr$p.value)
  results[14+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+3*num.models]]$LMlag$statistic,3)
  results[14+l.namvarl,(i*2)] <- 
    sig.stars(our.models[[i+3*num.models]]$LMlag$p.value)
  results[15+l.namvarl,(i*2)-1] <- 
    round(our.models[[i+3*num.models]]$RLMlag$statistic,3)
  results[15+l.namvarl,(i*2)] <- 
    sig.stars(our.models[[i+3*num.models]]$RLMlag$p.value)
  results[16+l.namvarl,(i*2)-1] <- 
    round(sort(our.models[[i+6*num.models]]$condindx)[length(our.models[[i+6*num.models]]$condindx)],3)
  results[16+l.namvarl,(i*2)] <- c("-")
  results[17+l.namvarl,(i*2)-1] <- 
    round(as.numeric(paste(our.models[[i+7*num.models]]@test$statistic)),3)
  results[17+l.namvarl,(i*2)] <- 
    sig.stars(as.numeric(paste(our.models[[i+7*num.models]]@test$p.value)))
}

# Change row names
row.names(results) <- c("Dependent Variable"," ","   ","Intercept",namvarl,
                        "-----------","Adj.r2", "AIC","+-+-+-+-+-+","NB Weight Matrix",
                        "Moran's I (Dep. Var.)", "Moran's I (Model)", "LMerr", "RLMerr","LMlag",
                        "RLMlag", "Multicollinearity","Jarque Bera")

# Write results in file
write.table(results,"results_21_09.csv", sep=",")


#### OLS Diagnostics ####
model <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG)
model.diag.metrics <- augment(model)
head(model.diag.metrics)
SpatialData_SpatialPolygon <- SpatialData_SpatialPolygon %>%
  add_residuals(model)
#### OLS DIAGNOSTICS QUICKLY
ggplot(model.diag.metrics, aes(TFR, EDU)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = TFR, yend = .fitted), color = "red", size = 0.1)

par(mfrow = c(2, 2))
plot(model)

library(ggfortify)
png(file="Residual_Autoplot.png",width = 2400, height = 2400, res=200)
autoplot(model)
dev.off()

# Cook's distance
plot(model, 4)
# Residuals vs Leverage
plot(model, 5)

### Normal Distribution
png(file="Residual_Normal_Distribution1.png",width = 2400, height = 2400, res=300)
model$residuals %>% # Pipe the residuals to a data frame
  data.frame() %>% # Pipe the data frame to ggplot
  ggplot(aes(model$residuals)) +
  geom_histogram(bins = 200)
dev.off()
png(file="Residual_Normal_Distribution2.png",width = 2400, height = 2400, res=300)
model$residuals %>% # Pipe the residuals to a data frame
  data.frame() %>% # Pipe the data frame to ggplot
  ggplot(aes(model$residuals)) +
  geom_density(adjust = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(model$residuals),
                                         sd = sd(model$residuals)),
                color = "red")
dev.off()

## collinearity
library(mctest)
omcdiag(model)#, TFR)
Call:
  omcdiag(x = model, y = TFR)

imcdiag(model)




### Using R book for model exploration with ggplot
LOGINC <- log(INC)
LOGPDENS <- log(PDENS)
modeltext <- TFR~LOGPDENS + INC + NONRG + SOCHO + EDU + DIV
model <- lm(modeltext, data = EW_Variables)
sim1 <- TFR + LOGPDENS + INC + NONRG + SOCHO + EDU + DIV

SpatialData_SpatialPolygon <- SpatialData_SpatialPolygon %>%
  add_predictions(model)
SpatialData_SpatialPolygon <- SpatialData_SpatialPolygon %>%
  add_residuals(model)

################################################################################


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

model <- lm(TFR~EDU + Pakistn + Bngldsh + Blck_Af + INC + LOGPDENS + SOCHO + DIV  + NONRG)

#### Just ttring to simply map, this works fine
EW_Poly <- read_sf("C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\Tidy_Data\\SpatialData_SpatialPolygon.shp")
EW_Combined <- fortify(EW_Poly)
EW_Combined <- merge(EW_Poly, SpatialData_SpatialPolygon, by.x = "code", by.y = "code", all.x=TRUE)
EW_Poly <- EW_Poly %>%
  add_residuals(model)


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



### Moving onto mapping regionally
London_EW <- filter(EW_Poly, Region == "London")
tiff(file="Residuals_London.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(London_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (London)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()

South_East_EW <- filter(EW_Poly, Region == "South East")
tiff(file="Residuals_South_East_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(South_East_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (South East)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()


South_West_EW <- filter(EW_Poly, Region == "South West")
tiff(file="Residuals_South_West_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(South_West_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (South West)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()


West_Midlands_EW <- filter(EW_Poly, Region == "West Midlands")
tiff(file="Residuals_SWest_Midlands_EW_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(West_Midlands_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (West Midlands)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()


East_Midlands_EW <- filter(EW_Poly, Region == "East Midlands")
tiff(file="Residuals_East_Midlands_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(East_Midlands_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (East Midlands)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()


Wales_EW <- filter(EW_Poly, Region == "Wales")
tiff(file="Residuals_Wales_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(Wales_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (Wales)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()

North_West_EW <- filter(EW_Poly, Region == "North West")
tiff(file="Residuals_North_West_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(North_West_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (North West)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()

Yorkshire_EW <- filter(EW_Poly, Region == "Yorkshire and The Humber")
tiff(file="Residuals_Yorkshire_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(Yorkshire_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (Yorkshire and The Humber)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()

North_East_EW <- filter(EW_Poly, Region == "North East")
tiff(file="Residuals_North_East_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(North_East_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (North East)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()


East_EW <- filter(EW_Poly, Region == "East of England")
tiff(file="Residuals_East_EW.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(East_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (East of England)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()
dev.off()







##### Now saving within this workspace to arrange as pretty graphs
London <- ggplot(London_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (London)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()

South_East <- ggplot(South_East_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (South East)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()

South_West <- ggplot(South_West_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (South West)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()

West_Midlands <- ggplot(West_Midlands_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (West Midlands)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()



East_Midlands <- ggplot(East_Midlands_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (East Midlands)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()


Wales <- ggplot(Wales_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (Wales)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()


North_West <- ggplot(North_West_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (North West)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()

Yorkshire <- ggplot(Yorkshire_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (Yorkshire and The Humber)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()


North_East <- ggplot(North_East_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (North East)") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()

East <- ggplot(East_EW) +
  geom_sf(size = 0.1, aes(fill=resid), colour = "grey") +
  ggtitle("OLS Residuals (East of England(") +
  scale_fill_gradient2(low = "#0072B2",
                       mid = "white",
                       high = "#D55E00",
                       midpoint = 0,
                       space = "Lab",
                       limits = c(-0.96211, 2.63742),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  theme_map()

## just leave them separate for now
