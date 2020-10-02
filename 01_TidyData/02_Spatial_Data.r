############################################################################
######                                                               #######
######        Importing Spatial Data                                 #######
######        Cleaning the data                                      #######
######        Calculating weights matrix                             #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
############################################################################

# Variables: 
# 1. Total_weekly_income
# 2. Socially_rented
# 3. Pop_Density
# 4. No_Religion
# 5. Education
# 6. Divorced

# Workflow:
# 1. Import data
# 2. Tidy initial data
# 3. Download variables
# 4. Clean this variable document

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
# Eng_Wal_SpatialPolygon <- dplyr::select(Eng_Wal_SpatialPolygon, -c("code":"label"))

## importing to get simple sp data.frame, for purpose of mapping
Eng_Poly_SP_Dataframe <- st_read("England_MSOA.shp")
Wal_Poly_SP_Dataframe <- st_read('Wales_MSOA.shp')
Eng_Wal_SP_Dataframe <- rbind(Eng_Poly_SP_Dataframe, Wal_Poly_SP_Dataframe)

# quickly cleaning spatial spatialpolygon data
Eng_Wal_SpatialPolygon <- Eng_Wal_SpatialPolygon[Eng_Wal_SpatialPolygon$code != "E02006781",] # getting rid of scilly

# quickly cleaning spatial sp dataframe data
Eng_Wal_SP_Dataframe <- Eng_Wal_SP_Dataframe[Eng_Wal_SP_Dataframe$code != "E02006781",] # getting rid of scilly



#### Combining the data ####
### Spatial Poltgon ###
#EW_Variables <- na.omit(EW_Variables)
SpatialData_SpatialPolygon <- merge(Eng_Wal_SpatialPolygon, EW_Variables, by.x="code", by.y="Code", all.x=TRUE) # spatialpolydataframe

### SP Dataframe ###
SpatialData_SP_Dataframe <- merge(Eng_Wal_SP_Dataframe, EW_Variables, by.x="code", by.y="Code", all.x=TRUE) # spatialpolydataframe


## with SP Dataframe
neighbour <- poly2nb(SpatialData_SpatialPolygon, queen=TRUE, row.names=SpatialData_SpatialPolygon$code) # define neighbouring 
summary(neighbour) # 7200, 41870 nonzero links, 5.81 average number
queen <- nb2listw(neighbour, style="W", zero.policy = TRUE) # define weights
#summary(queen)


# Polygon neigbours, neighbours matrix is right
coords.polygon <- coordinates(SpatialData_SpatialPolygon)
tiff(file="neighbourhood_0709.png",width = 2400, height = 2400, res=300, 
     compression="lzw")
plot(SpatialData_SpatialPolygon, border="darkgrey")
plot.nb(neighbour, coords.polygon, add=TRUE) 
# text(coords-0.07, paste(id), cex=0.8, col="red") # avoid this
dev.off()

## by adding numbers to the file, add row numbers
SpatialData_SpatialPolygon$row_num <- seq.int(nrow(SpatialData_SpatialPolygon)) 
SpatialData_SpatialPolygon$row_num <- as.character(SpatialData_SpatialPolygon$row_num)
summary(SpatialData_SpatialPolygon)








### simpler appraoch
x <- rbind(Eng_Poly_SpatialPolygon, Wal_Poly_SpatialPolygon, makeUniqueIDs = TRUE)
x <- x[x$code != "E02006781",] # getting rid of scilly
EW_Variables <- EW_Variables[order(x$code), ]
pm <- merge(x, EW_Variables, by.x=c('code'), by.y=c('Code'))
summary(pm)


## with SP Dataframe
neighbour <- poly2nb(pm, queen=TRUE, row.names=pm$code) # define neighbouring 
summary(neighbour) # 7200, 41870 nonzero links, 5.81 average number
queen <- nb2listw(neighbour, style="W", zero.policy = TRUE) # define weights
summary(queen)


#### Testing weights matrix now fully correct ####
moranI.0709 <- moran.mc(pm$TFR, queen, 100, zero.policy = FALSE) # 0.3167 - strange, got a different number
moranI.0709 <- moran.mc(EW_Variables$TFR, queen, 5, zero.policy = FALSE) # 0.3142 - strange, got a different number


## I think it's how I merge the england and the wales file, just randomly without any detail
row.names(as(Eng_Poly_SpatialPolygon, "data.frame"))
Eng_Poly_SpatialPolygon1 <- spChFIDs(Eng_Poly_SpatialPolygon, as.character(Eng_Poly_SpatialPolygon$code))
row.names(as(Eng_Poly_SpatialPolygon1, "data.frame"))





### Testing just with ENgland
Eng_Poly_SpatialPolygon <- shapefile("England_MSOA.shp", stringsAsFactors = TRUE)
England <- EW_Variables[EW_Variables$Region != "Wales",]
England_Spatial <- merge(Eng_Poly_SpatialPolygon, England, by.x=c('code'), by.y=c('Code'))
England_Spatial <- England_Spatial[England_Spatial$code != "E02006781",] # getting rid of scilly
neighbour <- poly2nb(England_Spatial, queen=TRUE, row.names=England_Spatial$code) # define neighbouring 
queen <- nb2listw(neighbour, style="W", zero.policy = TRUE) # define weights
moranI.0709 <- moran.mc(England_Spatial$TFR, queen, 100, zero.policy = FALSE) ## England is 0.324


write.nb.gal(neighbour,'neighbour.GAL', ind=England_Spatial$code)
nieghbour_tets <- read.gal('neighbour.GAL', override.id=TRUE)

library(maptools)
library(spdep)
England_Polygon <- readShapeSpatial("England_MSOA.shp")
England_Neighbours <- read.gal("neighbour.GAL", region.id = row.names(England_Polygon))
England_Polygon <- England_Polygon[England_Polygon$code != "E02006781",] # getting rid of scilly
neighbour <- poly2nb(England_Polygon, queen=TRUE, row.names=England_Polygon$code) # define neighbouring 
England_Spatial <- merge(England_Polygon, England, by.x=c('code'), by.y=c('Code'))
queen <- nb2listw(neighbour, style="W", zero.policy = TRUE) # define weights
moranI.0709 <- moran.mc(England_Spatial$TFR, queen, 100, zero.policy = FALSE) ## England is 0.324



writeOGR(obj=England_Spatial, dsn="C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData", layer="SpatialData_SpatialPolygon", driver="ESRI Shapefile") # this is in geographical projection
??writeOGR


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
library(gghighlight)

### showing Output Area agglomeration ###
SotonLSOA.shp <- read_sf("SotonLSOA.shp")
SotonMSOA.shp <- read_sf("SotonMSOA.shp")
SotonOA.shp <- read_sf("SotonOA.shp")
AllSotonMSOA.shp <- read_sf("AllSotonMSOA.shp")


plot(SotonOA.shp)
plot(SotonLSOA.shp)
plot(SotonMSOA.shp)
plot(AllSotonMSOA.shp)
AllSotonMSOA_pic <- ggplot(AllSotonMSOA.shp) +
  geom_sf(size = 0.3, colour = "black") +
  ggtitle("Southampton MSOAs with Highfield highlighted") +
  gghighlight(label == 'E06000045E02003557') +
  theme_map()
SotonOA_pic <- ggplot(SotonOA.shp) +
  geom_sf(size = 0.3, colour = "black") +
  ggtitle("Highfield OAs") +
  theme_map()
SotonLSOA_pic <- ggplot(SotonLSOA.shp) +
  geom_sf(size = 0.3, colour = "black") +
  ggtitle("Highfield LSOAs") +
  theme_map()

tiff(file="AllSotonMSOA_pic.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(AllSotonMSOA.shp) +
  geom_sf(size = 0.3, colour = "black") +
  ggtitle("Southampton MSOAs with Highfield highlighted") +
  gghighlight(label == 'E06000045E02003557') +
  theme_map()
dev.off()

tiff(file="SotonOA_pic.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(SotonOA.shp) +
  geom_sf(size = 0.3, colour = "black") +
  ggtitle("Highfield OAs") +
  theme_map()
dev.off()

tiff(file="SotonLSOA_pic.tif",width = 2400, height = 2400, res=300, 
     compression="lzw")
ggplot(SotonLSOA.shp) +
  geom_sf(size = 0.3, colour = "black") +
  ggtitle("Highfield LSOAs") +
  theme_map()
dev.off()


### Explanation of weights matrix, need to change read in file type
AllSotonMSOA.shp <- shapefile("AllSotonMSOA.shp")
coords <- coordinates(AllSotonMSOA.shp)

# Derive total number of units
n <- length(AllSotonMSOA.shp)

# Derive id, according to which the attributes are sorted
id <-AllSotonMSOA.shp$label
nb.FOQ <- poly2nb(AllSotonMSOA.shp, queen=TRUE, row.names=AllSotonMSOA.shp$label)
nb.FOR <- poly2nb(AllSotonMSOA.shp , queen=FALSE, row.names=AllSotonMSOA.shp$label)
tiff(file="Soton_neighbours_illustration.tif",width = 2400, height = 2400, res=400, 
     compression="lzw")
plot(AllSotonMSOA.shp, border="darkgrey")
plot.nb(nb.FOQ, coords, add=TRUE) 
dev.off()





## number of neighbours by unit

# Derive number of neighbors per unit
nom.of.neigh <- c(rep(0,n))
nom.of.neigh1 <- nom.of.neigh
for (i in 1:n) {
  nom.of.neigh[i] <- as.integer(length(nb.FOQ[[i]]))
} 
# for-loop, which is correct, but creates warnings:
for (i in 1:n) {
  if (as.integer(nb.FOQ[[i]])==0) nom.of.neigh1[i] <- 0
  else nom.of.neigh1[i] <- 1
}
# Same function without warnings:
for (i in 1:n) {
  if (mean(as.integer(nb.FOQ[[i]])==0)) nom.of.neigh1[i] <- 0
  else nom.of.neigh1[i] <- 1
} 
non <- nom.of.neigh*nom.of.neigh1

# Create color scheme for map, in which regions with no neighbor are highlighted 
# in red
nonc <- nom.of.neigh*nom.of.neigh1
for (i in 1:n) {
  if (nonc[i]<1) nonc[i] <- "red"
  else nonc[i] <- "white" 
} 

# This is an example where the nonc-information is matched to the shape-file. 
# However, this is not neccessary, as these can be kept seperate as long as the
# order of the shapefile-attribute table and the order of the derived data
# is not changed.
shape.shp.nb <- spCbind(AllSotonMSOA.shp, nonc)

# First plot - First order queen neighborhood weight file
plot(AllSotonMSOA.shp, border="darkgrey")
title("First order queen neighborhood weight matrix")
plot.nb(nb.FOQ, coords, add= TRUE, col="red")

# Second plot - Map, in which regions without neighbor are highlighted
plot(shape.shp.nb, col=nonc)
title("Regional units without neighbor")
legend("topleft",fill=c("white","red"), 
       legend=paste(c("neighors","no neighbors")), cex=1, bg="white")

# Create map, in which regions are colored according to number of neighbors
non10 <- non
non10[non10>8] <-8
colpal   = brewer.pal(9, "YlOrRd")
color <- rep(0,n)

for (i in 1:n) {
  color[i] <- colpal[non10[i]+1] 
} 

# Map indicating the number of neighbors
plot(shape.shp.nb, col=color)
title("Neighbors per regions")
legend("right",fill=colpal, legend=c(paste(c(0:7)),">7"),
       cex=1, bg="white")
