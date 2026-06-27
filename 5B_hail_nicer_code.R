# 1. Admin #####################################################################
library(raster)
library(sf)
library(tidyverse)
library(ncdf4) 
library(stars)
library(chron)
library(lattice)
library(RColorBrewer)
library(readxl)

# 2. Load data #################################################################
#country
country_data <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/Land/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp")
#canton
kantone <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/Kanton/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp")
zürich <- kantone[6,]
#municipality
muniShape <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/Gemeinden/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
muniShape <- sf::st_zm(muniShape, drop = TRUE, what = "ZM")


# 3. Make statistical model ####################################################
## 3.1 Haildays where hailgrain is > 2 cm ######################################
nc_haildays2cm <- nc_open("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/hailsize/haildays2cmclimY_ch01r.swiss.lv95_20020401000000_20250930000000.nc")

lon <- ncvar_get(nc_haildays2cm, "E")
lat <- ncvar_get(nc_haildays2cm, "N")

## Get time
time <- ncvar_get(nc_haildays2cm,"time")
nt <- dim(time)
tunits <- ncatt_get(nc_haildays2cm,"time","units")

## Get variable of interest
dname = "haildays2cmclimY"
tmp_array <- ncvar_get(nc_haildays2cm,dname)
dlname <- ncatt_get(nc_haildays2cm,dname,"long_name")
dunits <- ncatt_get(nc_haildays2cm,dname,"units")
fillvalue <- ncatt_get(nc_haildays2cm,dname,"_FillValue")
dim(tmp_array)

## Close connection
nc_close(nc_haildays2cm)

## TBD
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))
tmp_array[tmp_array==fillvalue$value] <- NA
dim(tmp_array) #wieso hier nur zwei Variabeln

#2013
m <- 12 #the dataset has information from 2002 up to now. The first two columns
#provide the coordinates, column 3 starts with the year 2002. Column 14 therefore
#has information on the year 2013. 
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2013_2cm <- tmp_df01

#2014
m <- 13
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2014_2cm <- tmp_df01
haildays_2cm <- full_join(haildays2013_2cm, haildays2014_2cm)

#2015
m <- 14
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2015_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2015_2cm)

#2016
m <- 15
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2016_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2016_2cm)

#2017
m <- 16
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2017_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2017_2cm)

#2018
m <- 17
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2018_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2018_2cm)

#2019
m <- 18
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2019_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2019_2cm)

#2020
m <- 19
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2020_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2020_2cm)

#2021
m <- 20
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2021_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2021_2cm)

#2022
m <- 21
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2022_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2022_2cm)

#2023
m <- 22
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2023_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2023_2cm)

#2024
m <- 23
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2024_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2024_2cm)

#2025
m <- 24
tmp_slice <- tmp_array[,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
haildays2025_2cm <- tmp_df01
haildays_2cm <- full_join(haildays_2cm, haildays2025_2cm)

head(na.omit(haildays_2cm))

haildays_2cm$sumOfHaildays_2cm <- rowSums(haildays_2cm[, c("haildays2cmclimY_13", "haildays2cmclimY_14", "haildays2cmclimY_15",
                                                           "haildays2cmclimY_16","haildays2cmclimY_17",
                                                           "haildays2cmclimY_18", "haildays2cmclimY_19",
                                                           "haildays2cmclimY_20", "haildays2cmclimY_21",
                                                           "haildays2cmclimY_22", "haildays2cmclimY_23",
                                                           "haildays2cmclimY_24")], na.rm = TRUE)
haildays_2cm_extract <- haildays_2cm[,c(1, 2, 16)]
View(haildays_2cm_extract) #works

#add haildays to merged_data_all
head(merged_data_all)
merged_data_all_haildays <- merged_data_all[,-c(3, 11:44)]
merged_data_all_haildays <- st_as_sf(merged_data_all_haildays, crs = 2056)
haildays_2cm_extract <- st_as_sf(haildays_2cm_extract, crs = 2056, coords = c("lon", "lat"))
merged_data_all_haildays <- st_join(merged_data_all_haildays, haildays_2cm_extract)


## 3.2 Maximum of theoretical hail size starting at 2 cm #######################
#Theoretically maximal grain size based on radar data
nc_hailsize <- nc_open("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/hailsize/MZCY_ch01r.swiss.lv95_20020401000000_20250930000000.nc")
head(nc_hailsize)

## Extract coordinates
lon <- ncvar_get(nc_hailsize, "E")
lat <- ncvar_get(nc_hailsize, "N")

## Get time
time <- ncvar_get(nc_hailsize,"time")
nt <- dim(time)
tunits <- ncatt_get(nc_hailsize,"time","units")

## Get variable of interest
dname = "MZCY"
tmp_array <- ncvar_get(nc_hailsize,dname)
dlname <- ncatt_get(nc_hailsize,dname,"long_name")
dunits <- ncatt_get(nc_hailsize,dname,"units")
fillvalue <- ncatt_get(nc_hailsize,dname,"_FillValue")
dim(tmp_array)

## Close connection
nc_close(nc_hailsize)

## TBD
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

tmp_array[tmp_array==fillvalue$value] <- NA
dim(tmp_array)

#2013
m <- 12 #the dataset has information from 2002 up to now. The first column
#therefore has the year 02, the 2nd column 03 etc.
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2013_2cm <- tmp_df01

#2014
m <- 13 
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2014_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize2013_2cm, hailsize2014_2cm)

#2015
m <- 14
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2015_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2015_2cm)

#2016
m <- 15
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2016_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2016_2cm)

#2017
m <- 16
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2017_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2017_2cm)

#2018
m <- 17
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2018_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2018_2cm)

#2019
m <- 18
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2019_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2019_2cm)

#2020
m <- 19
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2020_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2020_2cm)

#2021
m <- 20
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2021_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2021_2cm)

#2022
m <- 21
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2022_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2022_2cm)

#2023
m <- 22
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2023_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2023_2cm)

#2024
m <- 23
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2024_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2024_2cm)

#2025
m <- 24
tmp_slice <- tmp_array[,,m]
# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_slice)
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
hailsize2025_2cm <- tmp_df01
hailsize_2cm <- full_join(hailsize_2cm, hailsize2025_2cm)

hailsize_2cm$meanHailsize <- rowMeans(hailsize_2cm[, c("MZCY_13", "MZCY_14", "MZCY_15",
                                               "MZCY_16", "MZCY_17",
                                               "MZCY_18", "MZCY_19",
                                               "MZCY_20", "MZCY_21",
                                               "MZCY_22", "MZCY_23",
                                               "MZCY_24")], na.rm = TRUE)
head(hailsize_2cm)
#View(hailsize_2cm)
hailsize_2cm_extract <- hailsize_2cm[c(1, 2, 16)]
merged_data_all_hailsize <- merged_data_all[,-c(3, 11:44)]
merged_data_all_hailsize <- st_as_sf(merged_data_all_hailsize, crs = 2056)
hailsize_2cm_extract <- st_as_sf(hailsize_2cm_extract, crs = 2056, coords = c("lon", "lat"))

#produces many NAs
merged_data_all_hailsize <- st_join(merged_data_all_hailsize, hailsize_2cm_extract)
dim(merged_data_all_haildays)

#when grain is larger than 2cm, column "Hagelgross" gets a 1, otherwise a 0
merged_data_all_hailsize$meanHailsize <- as.numeric(merged_data_all_hailsize$meanHailsize) 
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$meanHailsize > 2, 1, 0)



# 4. Plots #####################################################################
## 4.1 Haildays where hailgrain is > 2 cm ######################################
## 4.2 Maximum of theoretical hail size starting at 2 cm #######################