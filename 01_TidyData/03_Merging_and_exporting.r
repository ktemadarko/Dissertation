############################################################################
######                                                               #######
######        Merging spatial and data                               #######
######        Exporting in appropriate format                        #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
######                                                               #######
############################################################################

library(rgdal)
# Export neighborhood matrix (might be helpful, if it takes long to compute it)
write.nb.gal(neighbour,'neighbour.GAL')
writeOGR(obj=SpatialData_SpatialPolygon, dsn="C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData", layer="SpatialData_SpatialPolygon", driver="ESRI Shapefile") # this is in geographical projection
saveRDS(SpatialData_SpatialPolygon, file="C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\SpatialData_SpatialPolygon.rds") # this is in geographical projection

saveRDS(queen, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\queen.rds")
saveRDS(EW_Variables, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\EW_Variables.rds")

saveRDS(EW_Variables, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\EW_Variables.rds")
saveRDS(Eng_Wal_SpatialPolygon, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\Eng_Wal_SpatialPolygon.rds")
saveRDS(Eng_Wal_SP_Dataframe, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\Eng_Wal_SP_Dataframe.rds")
saveRDS(SpatialData_SP_Dataframe, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\SpatialData_SP_Dataframe.rds")
saveRDS(S2011_LAD, file = "C:\\Users\\rbarker\\Documents\\1_MSc_Dissertation\\7. Data\\01_TidyData\\S2011_LAD.rds")
