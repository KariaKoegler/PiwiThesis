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
nc_haildays2cm <- nc_open("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/haildays/haildaysY_ch01r.swiss.lv95_20020401000000_20250930000000.nc")

lon <- ncvar_get(nc_haildays2cm, "E")
lat <- ncvar_get(nc_haildays2cm, "N")

## Get time
time <- ncvar_get(nc_haildays2cm,"time")
nt <- dim(time)
tunits <- ncatt_get(nc_haildays2cm,"time","units")

## Get variable of interest
dname = "haildaysY"
tmp_array_haildays_2cm <- ncvar_get(nc_haildays2cm,dname)
dlname <- ncatt_get(nc_haildays2cm,dname,"long_name")
dunits <- ncatt_get(nc_haildays2cm,dname,"units")
fillvalue <- ncatt_get(nc_haildays2cm,dname,"_FillValue")
dim(tmp_array_haildays_2cm)

## Close connection
nc_close(nc_haildays2cm)

## TBD
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))
tmp_array_haildays_2cm[tmp_array_haildays_2cm==fillvalue$value] <- NA
dim(tmp_array_haildays_2cm)

## Convert the whole array to a data frame
### Reshape the array into vector
tmp_vec_long <- as.vector(tmp_array_haildays_2cm)
# length(tmp_vec_long)

### Reshape the vector into a matrix
nlat <- dim(lat)
nlon <- dim(lon)
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)

#note: the following code determines the percentage in NAs found. In this case,
#more than half of the data is NA (?) This will be later changed to 0 as I
#assume that when no data is available, no hail was measured
# sum(is.na(tmp_mat))/prod(dim(tmp_mat))  
# dim(tmp_mat)
# head(tmp_mat) #here the NAs are also visible

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02_haildays_2cm <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02_haildays_2cm) <- c("lon","lat","days2002","days2003","days2004","days2005","days2006","days2007",
                     "days2008","days2009","days2010","days2011","days2012","days2013","days2014",
                     "days2015","days2016","days2017","days2018","days2019","days2020","days2021",
                     "days2022", "days2023", "days2024", "days2025")

### Get the annual mean and MTWA and MTCO
# Ich ersetze zuerst NA mit0 in den Spalten der Jahre und danach erst in den
#letzten Zeilen, so dass der mean etc. berechnet werden kann (ist sonst NA wenn
#in einer Spalte davor irgendwo NA steht)
tmp_df02_haildays_2cm$days2002[is.na(tmp_df02_haildays_2cm$days2002)] <- 0
tmp_df02_haildays_2cm$days2003[is.na(tmp_df02_haildays_2cm$days2003)] <- 0
tmp_df02_haildays_2cm$days2004[is.na(tmp_df02_haildays_2cm$days2004)] <- 0
tmp_df02_haildays_2cm$days2005[is.na(tmp_df02_haildays_2cm$days2005)] <- 0
tmp_df02_haildays_2cm$days2006[is.na(tmp_df02_haildays_2cm$days2006)] <- 0
tmp_df02_haildays_2cm$days2007[is.na(tmp_df02_haildays_2cm$days2007)] <- 0
tmp_df02_haildays_2cm$days2008[is.na(tmp_df02_haildays_2cm$days2008)] <- 0
tmp_df02_haildays_2cm$days2009[is.na(tmp_df02_haildays_2cm$days2009)] <- 0
tmp_df02_haildays_2cm$days2010[is.na(tmp_df02_haildays_2cm$days2010)] <- 0
tmp_df02_haildays_2cm$days2011[is.na(tmp_df02_haildays_2cm$days2011)] <- 0
tmp_df02_haildays_2cm$days2012[is.na(tmp_df02_haildays_2cm$days2012)] <- 0
tmp_df02_haildays_2cm$days2013[is.na(tmp_df02_haildays_2cm$days2013)] <- 0
tmp_df02_haildays_2cm$days2014[is.na(tmp_df02_haildays_2cm$days2014)] <- 0
tmp_df02_haildays_2cm$days2015[is.na(tmp_df02_haildays_2cm$days2015)] <- 0
tmp_df02_haildays_2cm$days2016[is.na(tmp_df02_haildays_2cm$days2016)] <- 0
tmp_df02_haildays_2cm$days2017[is.na(tmp_df02_haildays_2cm$days2017)] <- 0
tmp_df02_haildays_2cm$days2018[is.na(tmp_df02_haildays_2cm$days2018)] <- 0
tmp_df02_haildays_2cm$days2019[is.na(tmp_df02_haildays_2cm$days2019)] <- 0
tmp_df02_haildays_2cm$days2020[is.na(tmp_df02_haildays_2cm$days2020)] <- 0
tmp_df02_haildays_2cm$days2021[is.na(tmp_df02_haildays_2cm$days2021)] <- 0
tmp_df02_haildays_2cm$days2022[is.na(tmp_df02_haildays_2cm$days2022)] <- 0
tmp_df02_haildays_2cm$days2023[is.na(tmp_df02_haildays_2cm$days2023)] <- 0
tmp_df02_haildays_2cm$days2024[is.na(tmp_df02_haildays_2cm$days2024)] <- 0
tmp_df02_haildays_2cm$days2025[is.na(tmp_df02_haildays_2cm$days2025)] <- 0

tmp_df02_haildays_2cm$maxHaildays <- apply(tmp_df02_haildays_2cm[3:24],1,max) # mtwa: maximum of haildays
tmp_df02_haildays_2cm$minHaildays <- apply(tmp_df02_haildays_2cm[3:24],1,min) # mtco: minimum of hailays
tmp_df02_haildays_2cm$RowMeans <- apply(tmp_df02_haildays_2cm[3:24],1,mean) # annual (i.e. row) means
tmp_df02_haildays_2cm$sumOfHaildays <- apply(tmp_df02_haildays_2cm[3:24],1,sum) # sum of haildays across all years

tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  select(lon, lat, maxHaildays, minHaildays, RowMeans, sumOfHaildays)

#replace NA with 0
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans = ifelse(is.na(RowMeans), 0, RowMeans))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(maxHaildays = ifelse(is.na(maxHaildays), 0, maxHaildays))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(minHaildays = ifelse(is.na(minHaildays), 0, minHaildays))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays = ifelse(is.na(sumOfHaildays), 0, sumOfHaildays))

### Make data frame
dfr2_haildays_2cm <- rasterFromXYZ(tmp_df02_haildays_2cm)  

#reduce to data only in Zurich
tmp_df02_haildays_2cm <- st_as_sf(tmp_df02_haildays_2cm, crs = 2056, coords = c("lon", "lat"))
zürich <- st_as_sf(zürich, crs = 2056)
zürich <- st_transform(zürich, 2056)
haildays_2cm_extract_zurich <- st_filter(tmp_df02_haildays_2cm, zürich)

#merge data on hail with data on grape variety
merged_data_all <- merged_data_all_original #copy of original data with grape variety and coordinates
merged_data_all <- st_as_sf(merged_data_all, crs = 2056)
haildays_2cm_extract_zurich <- st_as_sf(haildays_2cm_extract_zurich, crs = 2056, coords = c("lon", "lat"))

st_write(merged_data_all, "merged_data_all.shp")
st_write(haildays_2cm_extract_zurich, "haildays_2cm_extract_zurich.shp")
#a look at those shapefiles in arcGIS shows that the points dont overlap often with
#the vineyards. Because of this, only ca. 200 rows could be used for the model.
#I therefore add a square buffer in arcgis so that the entirety
#of Zurich is covered and each vineyard has an assigned hail value.
haildays_2cm_buffer <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/haildays_2cm_buffer.shp")
haildays_2cm_buffer <- st_as_sf(haildays_2cm_buffer, crs = 2056)
merged_data_all_haildays_2cm <- st_join(merged_data_all, haildays_2cm_buffer)

merged_data_all_haildays_2cm$Weinmerkmal <- ifelse(merged_data_all_haildays_2cm$Rebsorte %in% interspezifisch, 1, 0)
merged_data_all_haildays_2cm$Betrieb <- as.factor(merged_data_all_haildays_2cm$Betrieb)
merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[!is.na(merged_data_all_haildays_2cm$mxHldys),]
merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[!is.na(merged_data_all_haildays_2cm$mnHldys),]

#do a model to check whether the results make sense --> order of magnitude
#is what I expect and its significant but why would more hail cause less PiWis?
summary(lm(Weinmerkmal ~ RowMens + plantation_year + Betrieb, data = merged_data_all_haildays_2cm))
#estimator: - 0.2645
#standard error: 0.014
#adjusted r2: 0.358

#plot to further check the model
par(mfrow = c(2, 2))
plot(lm(Weinmerkmal ~ RowMens + plantation_year + Betrieb, data = merged_data_all_haildays_2cm))
#qq plot is a bit weird but as everything else works, I think its ok


## 3.2 Maximum of theoretical hail size starting at 2 cm #######################
#Theoretically maximal grain size based on radar data
nc_hailsize <- nc_open("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/hailsize/MZCY_ch01r.swiss.lv95_20020401000000_20250930000000.nc")
# head(nc_hailsize)

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
# dim(tmp_array)

## Convert the whole array to a data frame
### Reshape the array into vector
tmp_vec_long <- as.vector(tmp_array)
# length(tmp_vec_long)

### Reshape the vector into a matrix
nlat <- dim(lat)
nlon <- dim(lon)
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
# dim(tmp_mat)
# head(na.omit(tmp_mat))

### Create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02_hailsize <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02_hailsize) <- c("lon","lat","MZCY_02", "MZCY_03", "MZCY_04", "MZCY_05",
                              "MZCY_06",
                              "MZCY_07", "MZCY_08", "MZCY_09", "MZCY_10", "MZCY_11",
                              "MZCY_12",
                              "MZCY_13", "MZCY_14", "MZCY_15",
                     "MZCY_16", "MZCY_17",
                     "MZCY_18", "MZCY_19",
                     "MZCY_20", "MZCY_21",
                     "MZCY_22", "MZCY_23",
                     "MZCY_24", "MZCY_25")

### Get the annual mean and MTWA and MTCO
#wenn ich überall 0 statt NA einfülle, stimmt das max, min und means nicht. Ich
#ersetze deshalb zuerst NA mit  0 in den Spalten der Jahre und danach erst in den
#letzten Zeilen.
tmp_df02_hailsize$MZCY_02[is.na(tmp_df02_hailsize$MZCY_02)] <- 0
tmp_df02_hailsize$MZCY_03[is.na(tmp_df02_hailsize$MZCY_03)] <- 0
tmp_df02_hailsize$MZCY_04[is.na(tmp_df02_hailsize$MZCY_04)] <- 0
tmp_df02_hailsize$MZCY_05[is.na(tmp_df02_hailsize$MZCY_05)] <- 0
tmp_df02_hailsize$MZCY_06[is.na(tmp_df02_hailsize$MZCY_06)] <- 0
tmp_df02_hailsize$MZCY_07[is.na(tmp_df02_hailsize$MZCY_07)] <- 0
tmp_df02_hailsize$MZCY_08[is.na(tmp_df02_hailsize$MZCY_08)] <- 0
tmp_df02_hailsize$MZCY_09[is.na(tmp_df02_hailsize$MZCY_09)] <- 0
tmp_df02_hailsize$MZCY_10[is.na(tmp_df02_hailsize$MZCY_10)] <- 0
tmp_df02_hailsize$MZCY_11[is.na(tmp_df02_hailsize$MZCY_11)] <- 0
tmp_df02_hailsize$MZCY_12[is.na(tmp_df02_hailsize$MZCY_12)] <- 0
tmp_df02_hailsize$MZCY_13[is.na(tmp_df02_hailsize$MZCY_13)] <- 0
tmp_df02_hailsize$MZCY_14[is.na(tmp_df02_hailsize$MZCY_14)] <- 0
tmp_df02_hailsize$MZCY_15[is.na(tmp_df02_hailsize$MZCY_15)] <- 0
tmp_df02_hailsize$MZCY_16[is.na(tmp_df02_hailsize$MZCY_16)] <- 0
tmp_df02_hailsize$MZCY_17[is.na(tmp_df02_hailsize$MZCY_17)] <- 0
tmp_df02_hailsize$MZCY_18[is.na(tmp_df02_hailsize$MZCY_18)] <- 0
tmp_df02_hailsize$MZCY_19[is.na(tmp_df02_hailsize$MZCY_19)] <- 0
tmp_df02_hailsize$MZCY_20[is.na(tmp_df02_hailsize$MZCY_20)] <- 0
tmp_df02_hailsize$MZCY_21[is.na(tmp_df02_hailsize$MZCY_21)] <- 0
tmp_df02_hailsize$MZCY_22[is.na(tmp_df02_hailsize$MZCY_22)] <- 0
tmp_df02_hailsize$MZCY_23[is.na(tmp_df02_hailsize$MZCY_23)] <- 0
tmp_df02_hailsize$MZCY_24[is.na(tmp_df02_hailsize$MZCY_24)] <- 0
tmp_df02_hailsize$MZCY_25[is.na(tmp_df02_hailsize$MZCY_25)] <- 0

tmp_df02_hailsize$maxHailsize <- apply(tmp_df02_hailsize[3:26],1,max) # mtwa: maximum of hailsize
tmp_df02_hailsize$minHailsize <- apply(tmp_df02_hailsize[3:26],1,min) # mtco: minimum of hailsize
tmp_df02_hailsize$RowMeans <- apply(tmp_df02_hailsize[3:26],1,mean) # annual (i.e. row) means

head(na.omit(tmp_df02_hailsize))
#if NAs are omitted, the smallest size is 2 so when grains are smaller than 2 cm, its an NA

tmp_df02_hailsize <- tmp_df02_hailsize %>%
  select(lon, lat, maxHailsize, minHailsize, RowMeans)

tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(maxHailsize = ifelse(is.na(maxHailsize), 0, maxHailsize))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(minHailsize = ifelse(is.na(minHailsize), 0, minHailsize))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans = ifelse(is.na(RowMeans), 0, RowMeans))

### Make data frame
dfr2 <- rasterFromXYZ(tmp_df02_hailsize)  

#reduce to data only in Zurich
tmp_df02_hailsize <- st_as_sf(tmp_df02_hailsize, crs = 2056, coords = c("lon", "lat"))
zürich <- st_as_sf(zürich, crs = 2056)
zürich <- st_transform(zürich, 2056)
hailsize_extract_zurich <- st_filter(tmp_df02_hailsize, zürich)

#merge data on hail with data on grape variety
merged_data_all <- merged_data_all_original
merged_data_all <- st_as_sf(merged_data_all, crs = 2056)
hailsize_extract_zurich <- st_as_sf(hailsize_extract_zurich, crs = 2056, coords = c("lon", "lat"))

st_write(hailsize_extract_zurich, "hailsize_extract_zurich.shp")
#add square buffer in arcGIS like in the chapter before (https://support.esri.com/en-us/knowledge-base/how-to-create-a-square-buffer-around-a-point-feature-in-000023198)

hailsize_buffer <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/hailsize_buffer.shp")
merged_data_all_hailsize <- st_join(merged_data_all, hailsize_buffer)

merged_data_all_hailsize$Weinmerkmal <- ifelse(merged_data_all_hailsize$Rebsorte %in% interspezifisch, 1, 0)
merged_data_all_hailsize$Betrieb <- as.factor(merged_data_all_hailsize$Betrieb)
merged_data_all_hailsize <- merged_data_all_hailsize[!is.na(merged_data_all_hailsize$maxHlsz),]
merged_data_all_hailsize <- merged_data_all_hailsize[!is.na(merged_data_all_hailsize$minHlsz),]

summary(lm(Weinmerkmal ~ RowMens + plantation_year + Betrieb, data = merged_data_all_hailsize))
#estimator: - 0.07459
#standardfehler: = 0.007457
#adjusted r2: 0.3554 

#--> ok

par(mfrow = c(2, 2))
plot(lm(Weinmerkmal ~ RowMens + plantation_year + Betrieb, data = merged_data_all_hailsize))
#interesting


## 3.3 Haildays regardless of size #############################################
# NetCDF data
## Open connection
nc <- nc_open("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/haildays/haildaysY_ch01r.swiss.lv95_20020401000000_20250930000000.nc")

## Extract coordinates
lon <- ncvar_get(nc, "E")
lat <- ncvar_get(nc, "N")

## Get time
time <- ncvar_get(nc,"time")
nt <- dim(time)
tunits <- ncatt_get(nc,"time","units")

## Get variable of interest
dname = "haildaysY"
tmp_array_haildays <- ncvar_get(nc,dname)
dlname <- ncatt_get(nc,dname,"long_name")
dunits <- ncatt_get(nc,dname,"units")
fillvalue <- ncatt_get(nc,dname,"_FillValue")
# dim(tmp_array)

## Close connection
nc_close(nc)

## TBD
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
# chron(time,origin=c(tmonth, tday, tyear))
tmp_array_haildays[tmp_array_haildays==fillvalue$value] <- NA

## Convert the whole array to a data frame
### Reshape the array into vector
tmp_vec_long <- as.vector(tmp_array_haildays)
# length(tmp_vec_long)

### Reshape the vector into a matrix
nlat <- dim(lat)
nlon <- dim(lon)
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
# dim(tmp_mat)
# head(na.omit(tmp_mat))

### Create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02_haildays <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02_haildays) <- c("lon","lat","days2002","days2003","days2004","days2005","days2006","days2007",
                     "days2008","days2009","days2010","days2011","days2012","days2013","days2014",
                     "days2015","days2016","days2017","days2018","days2019","days2020","days2021",
                     "days2022", "days2023", "days2024", "days2025") 

tmp_df02_haildays$days2002[is.na(tmp_df02_haildays$days2002)] <- 0
tmp_df02_haildays$days2003[is.na(tmp_df02_haildays$days2003)] <- 0
tmp_df02_haildays$days2004[is.na(tmp_df02_haildays$days2004)] <- 0
tmp_df02_haildays$days2005[is.na(tmp_df02_haildays$days2005)] <- 0
tmp_df02_haildays$days2006[is.na(tmp_df02_haildays$days2006)] <- 0
tmp_df02_haildays$days2007[is.na(tmp_df02_haildays$days2007)] <- 0
tmp_df02_haildays$days2008[is.na(tmp_df02_haildays$days2008)] <- 0
tmp_df02_haildays$days2009[is.na(tmp_df02_haildays$days2009)] <- 0
tmp_df02_haildays$days2010[is.na(tmp_df02_haildays$days2010)] <- 0
tmp_df02_haildays$days2011[is.na(tmp_df02_haildays$days2011)] <- 0
tmp_df02_haildays$days2012[is.na(tmp_df02_haildays$days2012)] <- 0
tmp_df02_haildays$days2013[is.na(tmp_df02_haildays$days2013)] <- 0
tmp_df02_haildays$days2014[is.na(tmp_df02_haildays$days2014)] <- 0
tmp_df02_haildays$days2015[is.na(tmp_df02_haildays$days2015)] <- 0
tmp_df02_haildays$days2016[is.na(tmp_df02_haildays$days2016)] <- 0
tmp_df02_haildays$days2017[is.na(tmp_df02_haildays$days2017)] <- 0
tmp_df02_haildays$days2018[is.na(tmp_df02_haildays$days2018)] <- 0
tmp_df02_haildays$days2019[is.na(tmp_df02_haildays$days2019)] <- 0
tmp_df02_haildays$days2020[is.na(tmp_df02_haildays$days2020)] <- 0
tmp_df02_haildays$days2021[is.na(tmp_df02_haildays$days2021)] <- 0
tmp_df02_haildays$days2022[is.na(tmp_df02_haildays$days2022)] <- 0
tmp_df02_haildays$days2023[is.na(tmp_df02_haildays$days2023)] <- 0
tmp_df02_haildays$days2024[is.na(tmp_df02_haildays$days2024)] <- 0
tmp_df02_haildays$days2025[is.na(tmp_df02_haildays$days2025)] <- 0

### Get the annual mean and MTWA and MTCO
tmp_df02_haildays$maxHaildays <- apply(tmp_df02_haildays[3:24],1,max) # mtwa: maximum of haildays
tmp_df02_haildays$minHaildays <- apply(tmp_df02_haildays[3:24],1,min) # mtco: minimum of hailays
tmp_df02_haildays$RowMeans <- apply(tmp_df02_haildays[3:24],1,mean) # annual (i.e. row) means
tmp_df02_haildays$sumOfHaildays <- apply(tmp_df02_haildays[3:24],1,sum) # sum of haildays across all years
head(na.omit(tmp_df02_haildays))
tmp_df02_haildays <- tmp_df02_haildays %>%
  select(lon, lat, maxHaildays, minHaildays, RowMeans, sumOfHaildays)

tmp_df02_haildays <- tmp_df02_haildays %>%
  mutate(maxHaildays = ifelse(is.na(maxHaildays), 0, maxHaildays))
tmp_df02_haildays <- tmp_df02_haildays %>%
  mutate(minHaildays = ifelse(is.na(minHaildays), 0, minHaildays))
tmp_df02_haildays <- tmp_df02_haildays %>%
  mutate(RowMeans = ifelse(is.na(RowMeans), 0, RowMeans))
tmp_df02_haildays <- tmp_df02_haildays %>%
  mutate(sumOfHaildays = ifelse(is.na(sumOfHaildays), 0, sumOfHaildays))

### Make data frame
dfr2_haildays <- rasterFromXYZ(tmp_df02_haildays)  

#reduce to data only in Zurich
tmp_df02_haildays <- st_as_sf(tmp_df02_haildays, crs = 2056, coords = c("lon", "lat"))
zürich <- st_as_sf(zürich, crs = 2056)
zürich <- st_transform(zürich, 2056)
haildays_extract_zurich <- st_filter(tmp_df02_haildays_2cm, zürich)

#merge data on hail with data on grape variety
merged_data_all <- merged_data_all_original
merged_data_all <- st_as_sf(merged_data_all, crs = 2056)
haildays_extract_zurich <- st_as_sf(haildays_extract_zurich, crs = 2056, coords = c("lon", "lat"))

st_write(haildays_extract_zurich, "haildays_extract_zurich.shp")
#a look at those shapefiles in arcGIS shows that the points dont overlap often with
#the vineyards. I therefore add a square buffer in arcgis so that the entirety
#of Zurich is covered
haildays_buffer <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/haildays_buffer.shp")
haildays_buffer <- st_as_sf(haildays_buffer, crs = 2056)
merged_data_all_haildays <- st_join(merged_data_all, haildays_buffer)

merged_data_all_haildays$Weinmerkmal <- ifelse(merged_data_all_haildays$Rebsorte %in% interspezifisch, 1, 0)
merged_data_all_haildays$Betrieb <- as.factor(merged_data_all_haildays$Betrieb)
merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[!is.na(merged_data_all_haildays_2cm$mxHldys),]
merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[!is.na(merged_data_all_haildays_2cm$mnHldys),]

summary(lm(Weinmerkmal * Fläche ~ smOfHld + plantation_year + Betrieb, data = merged_data_all_haildays))
#estimate: -8.789
#standard error: 0.9075
#adjusted R2: 0.1706 

#ab hier ist für die statistische Analyse irrelevant
# 4. Plots #####################################################################
## 4.1 Haildays where hailgrain is > 2 cm ######################################
## 4.2 Maximum of theoretical hail size starting at 2 cm #######################


# dataframe with coordinates and days of hail
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02) <- c("lon","lat","days2002","days2003","days2004","days2005","days2006","days2007",
                     "days2008","days2009","days2010","days2011","days2012","days2013","days2014",
                     "days2015","days2016","days2017","days2018","days2019","days2020","days2021",
                     "days2022")

### Get the annual mean and MTWA and MTCO
tmp_df02$mtwa <- apply(tmp_df02[3:23],1,max) # mtwa
tmp_df02$mtco <- apply(tmp_df02[3:23],1,min) # mtco
tmp_df02$mat <- apply(tmp_df02[3:23],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df02))

tmp_df02 <- tmp_df02 %>%
  dplyr::select(lon, lat, mat)
tmp_df02 <- tmp_df02 %>%
  mutate(mat = ifelse(is.na(mat), 0, mat))

### Make data frame
dfr2 <- rasterFromXYZ(tmp_df02)  
head(dfr2)


muniShape <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/Gemeinden/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
muniShape <- sf::st_zm(muniShape, drop = TRUE, what = "ZM")
sumStats <- function(x, na.rm) c(mean = mean(x, na.rm = na.rm))
meanHail <- raster::extract(dfr2, muniShape, weights = FALSE, df = TRUE, fun = sumStats)
meanHail$BFS_NUMMER <- as.numeric(muniShape$BFS_NUMMER)
muniShape$meanHail <- meanHail$mat[match(muniShape$BFS_NUMMER, meanHail$BFS_NUMMER)]



merged_data_all <- st_as_sf(merged_data_all, crs = 2056)

joined <- st_join(merged_data_all, muniShape, join = st_within)
joined$Weinmerkmal <- with(joined, ifelse(
  reben == 0, 0,
  ifelse(Weinmerkmal == "interspezifisch", 2,
         ifelse(Weinmerkmal == "europäisch", 1, NA)))
)
View(joined)

HailsizeandPiwi <- ggplot() +
  geom_sf(data = subset(muniShape), fill = "grey95", color = "grey60") +
  geom_sf(data = joined, aes(color = meanHail), size = 0.7) +
  scale_color_gradient(low = "white", high = "red", name = "Average size of hail grain") +
  geom_sf(data = Zürisee, fill = "lightblue") +
  geom_sf(data = Griifesee, fill = "lightblue") +
  geom_sf(data = joined, aes(fill = as.factor(Weinmerkmal)), color = NA) +
  scale_fill_manual(name = "Land Use", 
                    breaks = c(0,1,2), 
                    values = c("gray", "navyblue", "green"), 
                    labels = c("Agricultural area without grapes", "Conventional grapes", "PiWi")) +
  
  coord_sf(xlim = c(2669087.36213217, 2713875.597690578), ylim = c(1227293.256352458, 1276966.884853686)) +
  theme_minimal() +
  labs(
    title = "Average size of hail from 2013 to 2023 and grown grape varieties",
    subtitle = "Lighter red = fewer haildays, darker red = many haildays"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  ) 
HailsizeandPiwi
ggsave("3_output/HailsizeAndPiwi.png", plot = HailsizeandPiwi, width = 20, height = 20, dpi = 300, units = "cm", bg = "white")

joined <- joined[joined$reben != 0,]
HailsizeandPiwiWithoutAcker <- ggplot() +
  geom_sf(data = subset(muniShape), fill = "grey95", color = "grey60") +
  geom_sf(data = joined, aes(color = meanHail), size = 0.7) +
  scale_color_gradient(low = "white", high = "red", name = "Average size of hail grain") +
  geom_sf(data = Zürisee, fill = "lightblue") +
  geom_sf(data = Griifesee, fill = "lightblue") +
  geom_sf(data = joined, aes(fill = as.factor(Weinmerkmal)), color = NA) +
  scale_fill_manual(name = "Land Use", 
                    breaks = c(1,2), 
                    values = c("navyblue", "green"), 
                    labels = c("Conventional grapes", "piwi")) +
  
  coord_sf(xlim = c(2669087.36213217, 2713875.597690578), ylim = c(1227293.256352458, 1276966.884853686)) +
  theme_minimal() +
  labs(
    title = "Hagelgrösse von 2013 bis 2023 und Piwi",
    subtitle = "Weiss = kleine Körner, Rot = grosse Körner"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  ) 
ggsave("3_output/HailsizeAndPiwi2.png", plot = HailsizeandPiwiWithoutAcker, width = 20, height = 20, dpi = 300, units = "cm", bg = "white")

## 4.3 Haildays with hail of all sizes
haildaysPlot <- ggplot() +
  geom_sf(data = subset(country_data, NAME == "Schweiz"), fill = "grey95", color = "grey60") +
  geom_sf(data = haildays_sf, aes(color = sumOfHaildays), size = 3) +
  scale_color_gradient(low = "white", high = "red", name = "Hail days") +
  theme_minimal() +
  labs(
    title = "Hail days in Switzerland from 2013 to 2024",
    subtitle = "Lighter red = fewer haildays, darker red = many haildays"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )
ggsave("3_output/Hageltage.png", plot = haildaysPlot, width = 20, height = 20, dpi = 300, units = "cm", bg = "white")

#now zoomed in to Zurich
FormZürich <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/ZürichForm.shp")
FormZürich <- st_as_sf(FormZürich, crs = 2056) 
Zürisee <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/Zürichsee.shp")
Griifesee <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/Griifesee.shp")

#to do: add outline of zurich and cover the lake blue
haildaysPlotCropped <- ggplot() +
  #geom_sf(data = subset(country_data, NAME == "Schweiz"), fill = "grey95", color = "grey60") +
  geom_sf(data = haildays_sf, aes(color = sumOfHaildays), size = 3, shape = 15) +
  scale_color_gradient(low = "white", high = "red", name = "Hail days") +
  geom_sf(data = muniShape, fill = NA, colour = "grey", size = 0.05) +
  geom_sf(data = FormZürich, fill = NA, color = "black") +
  geom_sf(data = Zürisee, fill = "lightblue") +
  geom_sf(data = Griifesee, fill = "lightblue") +
  coord_sf(xlim = c(2661000, 2720500), ylim = c(1220000, 1290000)) +
  theme_minimal() +
  labs(
    title = "Hail days in Zurich from 2013 to 2024",
    subtitle = "Lighter red = fewer haildays, darker red = many haildays"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  ) 
haildaysPlotCropped
ggsave("3_output/HageltageZürich.png", plot = haildaysPlotCropped, width = 20, height = 20, dpi = 300, units = "cm", bg = "white")

## 4.2 haildays 2cm 
#the plot itself was made in arcGIS, but I want a nice title that fits with
#the other plots
FormZürich #shapefile of Umriss kanton Zürich
#this file was made by overlapping hailsize_2cm with Zurich and only keeping
#the part inside Zurich for faster data handling
hailsize2cmplot <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/hailsize_2cm_zurich.shp")
muniShape <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/Gemeinden/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
muniShape <- sf::st_zm(muniShape, drop = TRUE, what = "ZM")

hailsize2cm <- ggplot() +
  #geom_sf(data = subset(country_data, NAME == "Schweiz"), fill = "grey95", color = "grey60") +
  geom_sf(data = hailsize2cmplot, aes(color = menHlsz), size = 3, shape = 15) +
  scale_color_gradient(low = "white", high = "red", name = "Hail size") +
  geom_sf(data = muniShape, fill = NA, colour = "grey", size = 0.05) +
  geom_sf(data = FormZürich, fill = NA, color = "black") +
  geom_sf(data = Zürisee, fill = "lightblue") +
  geom_sf(data = Griifesee, fill = "lightblue") +
  coord_sf(xlim = c(2661000, 2720500), ylim = c(1220000, 1290000)) +
  theme_minimal() +
  labs(
    title = "Mean hailsize days in Zurich from 2013 to 2024",
    subtitle = "Lighter red = smaller average hail grains, darker red = larger average hail grains"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  ) 
hailsize2cm

# 5. Development of haildays and size over the years############################
#I want to know of the number of haildays > 2 cm for the entirety of Switzerland has¨
#increased or decreased

tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

### Reshape the vector into a matrix
nlat <- dim(lat)
nlon <- dim(lon)
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

### Create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02) <- c("lon","lat","days2002","days2003","days2004","days2005","days2006","days2007",
                     "days2008","days2009","days2010","days2011","days2012","days2013","days2014",
                     "days2015","days2016","days2017","days2018","days2019","days2020","days2021",
                     "days2022")



head(haildays2013_2cm)
#reduce to only zurich

zürich <- st_as_sf(zürich, crs = 2056)
st_crs(zürich) <- 2056

haildays2013_2cm <- st_as_sf(haildays2013_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2013_2cm) <- 2056
haildays2014_2cm <- st_as_sf(haildays2014_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2014_2cm) <- 2056
haildays2015_2cm <- st_as_sf(haildays2015_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2015_2cm) <- 2056
haildays2016_2cm <- st_as_sf(haildays2016_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2016_2cm) <- 2056
haildays2017_2cm <- st_as_sf(haildays2017_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2017_2cm) <- 2056
haildays2018_2cm <- st_as_sf(haildays2018_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2018_2cm) <- 2056
haildays2019_2cm <- st_as_sf(haildays2019_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2019_2cm) <- 2056
haildays2020_2cm <- st_as_sf(haildays2020_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2020_2cm) <- 2056
haildays2021_2cm <- st_as_sf(haildays2021_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2021_2cm) <- 2056
haildays2022_2cm <- st_as_sf(haildays2022_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2022_2cm) <- 2056
haildays2023_2cm <- st_as_sf(haildays2023_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2023_2cm) <- 2056
haildays2024_2cm <- st_as_sf(haildays2024_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2024_2cm) <- 2056
haildays2025_2cm <- st_as_sf(haildays2025_2cm, crs = 2056, coords = c("lon", "lat"))
st_crs(haildays2025_2cm) <- 2056


haildays2013_2cm_zurich <- st_join(haildays2013_2cm, zürich)
haildays2014_2cm_zurich <- st_join(haildays2014_2cm, zürich)
haildays2015_2cm_zurich <- st_join(haildays2015_2cm, zürich)
haildays2016_2cm_zurich <- st_join(haildays2016_2cm, zürich)
haildays2017_2cm_zurich <- st_join(haildays2017_2cm, zürich)
haildays2018_2cm_zurich <- st_join(haildays2018_2cm, zürich)
haildays2019_2cm_zurich <- st_join(haildays2019_2cm, zürich)
haildays2020_2cm_zurich <- st_join(haildays2020_2cm, zürich)
haildays2021_2cm_zurich <- st_join(haildays2021_2cm, zürich)
haildays2022_2cm_zurich <- st_join(haildays2022_2cm, zürich)
haildays2023_2cm_zurich <- st_join(haildays2023_2cm, zürich)
haildays2024_2cm_zurich <- st_join(haildays2024_2cm, zürich)
haildays2025_2cm_zurich <- st_join(haildays2025_2cm, zürich)

View(haildays2025_2cm_zurich)

numberOfHaildays <- c(
                      sum(na.omit(haildays2013_2cm_zurich$haildays2cmclimY_14)),
                      sum(na.omit(haildays2014_2cm_zurich$haildays2cmclimY_15)),
                      sum(na.omit(haildays2015_2cm_zurich$haildays2cmclimY_16)),
                      sum(na.omit(haildays2016_2cm_zurich$haildays2cmclimY_17)),
                      sum(na.omit(haildays2017_2cm_zurich$haildays2cmclimY_18)),
                      sum(na.omit(haildays2018_2cm_zurich$haildays2cmclimY_19)),
                      sum(na.omit(haildays2019_2cm_zurich$haildays2cmclimY_20)),
                      sum(na.omit(haildays2020_2cm_zurich$haildays2cmclimY_21)),
                      sum(na.omit(haildays2021_2cm_zurich$haildays2cmclimY_22)),
                      sum(na.omit(haildays2022_2cm_zurich$haildays2cmclimY_23)),
                      sum(na.omit(haildays2023_2cm_zurich$haildays2cmclimY_24)),
                      sum(na.omit(haildays2024_2cm_zurich$haildays2cmclimY_25)),
                      sum(na.omit(haildays2025_2cm_zurich$haildays2cmclimY_26)))

jahre <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
#anzahl forschungstationen
4400 / nrow(haildays2025_2cm_zurich)
df <- data.frame(jahre, numberOfHaildays)
ggplot(data = df, aes(x = jahre, y = numberOfHaildays)) +
  geom_line()  +
  labs(title = "Days with hail larger than 2 cm in the canton of Zurich", x = "Years", y =  "Number of days with hail") +
  geom_smooth(method=lm)
