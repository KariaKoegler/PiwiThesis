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

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02_haildays_2cm <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02_haildays_2cm) <- c("lon","lat","days2002","days2003","days2004","days2005","days2006","days2007",
                     "days2008","days2009","days2010","days2011","days2012","days2013","days2014",
                     "days2015","days2016","days2017","days2018","days2019","days2020","days2021",
                     "days2022", "days2023", "days2024", "days2025")

#calculate the mean for the last 10 years for each year --> for 2013, the mean
#from 2003 to 2013
tmp_df02_haildays_2cm$RowMeans2013 <- apply(tmp_df02_haildays_2cm[4:14],1,mean) # average haildays from 2003 to 2013
tmp_df02_haildays_2cm$RowMeans2015 <- apply(tmp_df02_haildays_2cm[6:16],1,mean)
tmp_df02_haildays_2cm$RowMeans2017 <- apply(tmp_df02_haildays_2cm[8:18],1,mean)
tmp_df02_haildays_2cm$RowMeans2018 <- apply(tmp_df02_haildays_2cm[9:19],1,mean)
tmp_df02_haildays_2cm$RowMeans2019 <- apply(tmp_df02_haildays_2cm[10:20],1,mean)
tmp_df02_haildays_2cm$RowMeans2020 <- apply(tmp_df02_haildays_2cm[11:21],1,mean)
tmp_df02_haildays_2cm$RowMeans2021 <- apply(tmp_df02_haildays_2cm[12:22],1,mean)
tmp_df02_haildays_2cm$RowMeans2022 <- apply(tmp_df02_haildays_2cm[13:23],1,mean)
tmp_df02_haildays_2cm$RowMeans2023 <- apply(tmp_df02_haildays_2cm[14:24],1,mean)
tmp_df02_haildays_2cm$RowMeans2024 <- apply(tmp_df02_haildays_2cm[15:25],1,mean)
tmp_df02_haildays_2cm$RowMeans2025 <- apply(tmp_df02_haildays_2cm[16:26],1,mean)
tmp_df02_haildays_2cm$RowMeansAllYears <- apply(tmp_df02_haildays_2cm[4:26],1,mean)

tmp_df02_haildays_2cm$sumOfHaildays2013 <- apply(tmp_df02_haildays_2cm[4:14],1,sum) # number of haildays from 2003 to 2013
tmp_df02_haildays_2cm$sumOfHaildays2015 <- apply(tmp_df02_haildays_2cm[6:16],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2017 <- apply(tmp_df02_haildays_2cm[8:18],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2018 <- apply(tmp_df02_haildays_2cm[9:19],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2019 <- apply(tmp_df02_haildays_2cm[10:20],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2020 <- apply(tmp_df02_haildays_2cm[11:21],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2021 <- apply(tmp_df02_haildays_2cm[12:22],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2022 <- apply(tmp_df02_haildays_2cm[13:23],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2023 <- apply(tmp_df02_haildays_2cm[14:24],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2024 <- apply(tmp_df02_haildays_2cm[15:25],1,sum)
tmp_df02_haildays_2cm$sumOfHaildays2025 <- apply(tmp_df02_haildays_2cm[16:26],1,sum)

#replace NA with 0
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2013 = ifelse(is.na(RowMeans2013), 0, RowMeans2013))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2015 = ifelse(is.na(RowMeans2015), 0, RowMeans2015))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2017 = ifelse(is.na(RowMeans2017), 0, RowMeans2017))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2018 = ifelse(is.na(RowMeans2018), 0, RowMeans2018))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2019 = ifelse(is.na(RowMeans2019), 0, RowMeans2019))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2020 = ifelse(is.na(RowMeans2020), 0, RowMeans2020))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2021 = ifelse(is.na(RowMeans2021), 0, RowMeans2021))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2022 = ifelse(is.na(RowMeans2022), 0, RowMeans2022))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2023 = ifelse(is.na(RowMeans2023), 0, RowMeans2023))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2024 = ifelse(is.na(RowMeans2024), 0, RowMeans2024))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(RowMeans2025 = ifelse(is.na(RowMeans2025), 0, RowMeans2025))

tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2013 = ifelse(is.na(sumOfHaildays2013), 0, sumOfHaildays2013))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2015 = ifelse(is.na(sumOfHaildays2015), 0, sumOfHaildays2015))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2017 = ifelse(is.na(sumOfHaildays2017), 0, sumOfHaildays2017))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2018 = ifelse(is.na(sumOfHaildays2018), 0, sumOfHaildays2018))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2019 = ifelse(is.na(sumOfHaildays2019), 0, sumOfHaildays2019))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2020 = ifelse(is.na(sumOfHaildays2020), 0, sumOfHaildays2020))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2021 = ifelse(is.na(sumOfHaildays2021), 0, sumOfHaildays2021))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2022 = ifelse(is.na(sumOfHaildays2022), 0, sumOfHaildays2022))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2023 = ifelse(is.na(sumOfHaildays2023), 0, sumOfHaildays2023))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2024 = ifelse(is.na(sumOfHaildays2024), 0, sumOfHaildays2024))
tmp_df02_haildays_2cm <- tmp_df02_haildays_2cm %>%
  mutate(sumOfHaildays2025 = ifelse(is.na(sumOfHaildays2025), 0, sumOfHaildays2025))

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

#add square buffer around measurements points
head(haildays_2cm_extract_zurich)
haildays_2cm_buffer <- st_buffer(haildays_2cm_extract_zurich, dist = 500, endCapStyle = "SQUARE")

merged_data_all_haildays_2cm <- st_join(merged_data_all, haildays_2cm_buffer)

merged_data_all_haildays_2cm$Weinmerkmal <- ifelse(merged_data_all_haildays_2cm$Rebsorte %in% interspezifisch, 1, 0)
merged_data_all_haildays_2cm$Betrieb <- as.factor(merged_data_all_haildays_2cm$Betrieb)


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

### Get the mean
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

tmp_df02_hailsize$RowMeans2013 <- apply(tmp_df02_hailsize[4:14],1,mean) # average haildays from 2003 to 2013
tmp_df02_hailsize$RowMeans2015 <- apply(tmp_df02_hailsize[6:16],1,mean)
tmp_df02_hailsize$RowMeans2017 <- apply(tmp_df02_hailsize[8:18],1,mean)
tmp_df02_hailsize$RowMeans2018 <- apply(tmp_df02_hailsize[9:19],1,mean)
tmp_df02_hailsize$RowMeans2019 <- apply(tmp_df02_hailsize[10:20],1,mean)
tmp_df02_hailsize$RowMeans2020 <- apply(tmp_df02_hailsize[11:21],1,mean)
tmp_df02_hailsize$RowMeans2021 <- apply(tmp_df02_hailsize[12:22],1,mean)
tmp_df02_hailsize$RowMeans2022 <- apply(tmp_df02_hailsize[13:23],1,mean)
tmp_df02_hailsize$RowMeans2023 <- apply(tmp_df02_hailsize[14:24],1,mean)
tmp_df02_hailsize$RowMeans2024 <- apply(tmp_df02_hailsize[15:25],1,mean)
tmp_df02_hailsize$RowMeans2025 <- apply(tmp_df02_hailsize[16:26],1,mean)
tmp_df02_hailsize$RowMeansAllYears <- apply(tmp_df02_hailsize[4:26],1,mean)

tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2013 = ifelse(is.na(RowMeans2013), 0, RowMeans2013))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2015 = ifelse(is.na(RowMeans2015), 0, RowMeans2015))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2017 = ifelse(is.na(RowMeans2017), 0, RowMeans2017))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2018 = ifelse(is.na(RowMeans2018), 0, RowMeans2018))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2019 = ifelse(is.na(RowMeans2019), 0, RowMeans2019))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2020 = ifelse(is.na(RowMeans2020), 0, RowMeans2020))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2021 = ifelse(is.na(RowMeans2021), 0, RowMeans2021))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2022 = ifelse(is.na(RowMeans2022), 0, RowMeans2022))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2023 = ifelse(is.na(RowMeans2023), 0, RowMeans2023))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2024 = ifelse(is.na(RowMeans2024), 0, RowMeans2024))
tmp_df02_hailsize <- tmp_df02_hailsize %>%
  mutate(RowMeans2025 = ifelse(is.na(RowMeans2025), 0, RowMeans2025))

#If hail > 2cm, add dummy indicator = 1, otherwise 0
tmp_df02_hailsize$Hagelgross2013 <- ifelse(tmp_df02_hailsize$RowMeans2013 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2015 <- ifelse(tmp_df02_hailsize$RowMeans2015 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2017 <- ifelse(tmp_df02_hailsize$RowMeans2017 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2018 <- ifelse(tmp_df02_hailsize$RowMeans2018 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2019 <- ifelse(tmp_df02_hailsize$RowMeans2019 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2020 <- ifelse(tmp_df02_hailsize$RowMeans2020 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2021 <- ifelse(tmp_df02_hailsize$RowMeans2021 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2022 <- ifelse(tmp_df02_hailsize$RowMeans2022 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2023 <- ifelse(tmp_df02_hailsize$RowMeans2023 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2024 <- ifelse(tmp_df02_hailsize$RowMeans2024 > 2, 1, 0)
tmp_df02_hailsize$Hagelgross2025 <- ifelse(tmp_df02_hailsize$RowMeans2025 > 2, 1, 0)

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

#add square buffer around measurements points
head(hailsize_extract_zurich)
hailsize_buffer <- st_buffer(hailsize_extract_zurich, dist = 500, endCapStyle = "SQUARE")

merged_data_all_hailsize <- st_join(merged_data_all, hailsize_buffer)

merged_data_all_hailsize$Weinmerkmal <- ifelse(merged_data_all_hailsize$Rebsorte %in% interspezifisch, 1, 0)
merged_data_all_hailsize$Betrieb <- as.factor(merged_data_all_hailsize$Betrieb)

# 4. Plots #####################################################################
#Sum of haildays over zurich
hageltage2013 <- sum(na.omit(haildays_2cm_extract_zurich$days2013))
hageltage2014 <- sum(na.omit(haildays_2cm_extract_zurich$days2014))
hageltage2015 <- sum(na.omit(haildays_2cm_extract_zurich$days2015))
hageltage2016 <- sum(na.omit(haildays_2cm_extract_zurich$days2016))
hageltage2017 <- sum(na.omit(haildays_2cm_extract_zurich$days2017))
hageltage2018 <- sum(na.omit(haildays_2cm_extract_zurich$days2018))
hageltage2019 <- sum(na.omit(haildays_2cm_extract_zurich$days2019))
hageltage2020 <- sum(na.omit(haildays_2cm_extract_zurich$days2020))
hageltage2021 <- sum(na.omit(haildays_2cm_extract_zurich$days2021))
hageltage2022 <- sum(na.omit(haildays_2cm_extract_zurich$days2022))
hageltage2023 <- sum(na.omit(haildays_2cm_extract_zurich$days2023))
hageltage2024 <- sum(na.omit(haildays_2cm_extract_zurich$days2024))
hageltage2025 <- sum(na.omit(haildays_2cm_extract_zurich$days2025))

jahre <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
numberOfHaildays <- c(hageltage2013, hageltage2014, hageltage2015, hageltage2016,
                      hageltage2017, hageltage2018, hageltage2019, hageltage2020,
                      hageltage2021, hageltage2022, hageltage2023, hageltage2024,
                      hageltage2025)


df <- data.frame(jahre, numberOfHaildays)
ggplot(data = df, aes(x = jahre, y = numberOfHaildays)) +
  geom_line()  +
  labs(title = "Days with hail larger than 2 cm in the canton of Zurich", x = "Years", y =  "Number of days with hail") +
  geom_smooth(method=lm) +
  geom_vline(xintercept = 2021, color = "blue", linetype = "dashed") +
  scale_x_continuous(
    breaks = seq(floor(min(df$jahre)), ceiling(max(df$jahre)), 2) # integers only
  ) 


length(haildays_2cm_extract_zurich$days2025)
sum(na.omit(haildays_2cm_extract_zurich$days2021))
#haildays per year and place
2000 / length(haildays_2cm_extract_zurich$days2025)
#1.15 haildays per year 
summary(haildays_2cm_extract_zurich$RowMeans2013)
summary(haildays_2cm_extract_zurich$RowMeans2025)
#makes sense when comparing to the mean of the years
