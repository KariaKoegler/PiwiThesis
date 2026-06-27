# 0. Administration ############################################################
library(tidyverse)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

# 1. Reading the raw data ######################################################
yield2013 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2013.xlsx")
yield2014 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2014.xls")
yield2015 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2015.xls")
yield2016 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2016.xls")
yield2017 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2017.xlsx")
yield2018 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2018.xlsx")
yield2019 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2019.xlsx")
yield2020 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2020.xlsx")
yield2021 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2021.xlsx")
yield2022 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2022.xlsx")
yield2023 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2023.xlsx")
yield2024 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2024.xlsx")
yield2025 <- read_excel("2_data/1_raw/Ertrag/Weinlese 2025.xlsx")

#2. Clean up ###################################################################
#Bei einigen Jahren beinhaltet die erste Zeile die Spaltennamen
colnames(yield2018) <- yield2018[1, ]
yield2018 <- yield2018[-1, ] # Remove the first row
colnames(yield2019) <- yield2019[1, ]
yield2019 <- yield2019[-1, ]
colnames(yield2020) <- yield2020[1, ]
yield2020 <- yield2020[-1, ]
colnames(yield2021) <- yield2021[1, ]
yield2021 <- yield2021[-1, ]
colnames(yield2022) <- yield2022[1, ]
yield2022 <- yield2022[-1, ]
colnames(yield2023) <- yield2023[1, ]
yield2023 <- yield2023[-1, ]
colnames(yield2024) <- yield2024[1, ]
yield2024 <- yield2024[-1, ]
colnames(yield2025) <- yield2025[1, ]
yield2025 <- yield2025[-1, ]

head(yield2018[,5])
#view(yield2025)
yield2013[yield2013$Gemeinde...3 != yield2013$Gemeinde...5, ]
#sometimes, the original files contain two columns with municipalities. 
#check that the two columns have the same information
yield2013 <- yield2013[yield2013$Gemeinde...3 == yield2013$Gemeinde...5, ]
yield2017 <- yield2017[yield2017$Gemeinde...3 == yield2017$Gemeinde...5, ]
yield2018 <- yield2018[yield2018[,3] == yield2018[,5], ]
yield2019 <- yield2019[yield2019[,3] == yield2019[,5], ]
yield2020 <- yield2020[yield2020[,3] == yield2020[,5], ]
yield2021 <- yield2021[yield2021[,3] == yield2021[,5], ]
yield2022 <- yield2022[yield2022[,3] == yield2022[,5], ]
yield2023 <- yield2023[yield2023[,3] == yield2023[,5], ]
yield2024 <- yield2024[yield2024[,3] == yield2024[,5], ]
yield2025 <- yield2025[yield2025[,3] == yield2025[,5], ]
#delete 5th column as its double
yield2018 <- yield2018[,-5]
yield2019 <- yield2019[,-5]
yield2020 <- yield2020[,-5]
yield2021 <- yield2021[,-5]
yield2022 <- yield2022[,-5]
yield2023 <- yield2023[,-5]
yield2024 <- yield2024[,-5]
yield2025 <- yield2025[,-5]

#3. Erntemenge über die Jahre ##################################################
gesamtertrag13 <- sum(yield2013$`Menge, kg`, na.rm = TRUE)/1000 #tonnen
gesamtertrag14 <- sum(yield2014$`Menge, kg`, na.rm = TRUE)/1000
gesamtertrag15 <- sum(yield2015$`Menge, kg`, na.rm = TRUE)/1000
gesamtertrag16 <- sum(yield2016$`Menge, kg`, na.rm = TRUE)/1000
gesamtertrag17 <- sum(yield2017$`Menge, kg`, na.rm = TRUE)/1000
yield2018$`Menge, kg` <- as.numeric(yield2018$`Menge, kg`)
gesamtertrag18 <- sum(yield2018$`Menge, kg`, na.rm = TRUE)/1000
yield2019$`Menge, kg` <- as.numeric(yield2019$`Menge, kg`)
gesamtertrag19 <- sum(yield2019$`Menge, kg`, na.rm = TRUE)/1000
yield2020$`Menge, kg` <- as.numeric(yield2020$`Menge, kg`)
gesamtertrag20 <- sum(yield2020$`Menge, kg`, na.rm = TRUE)/1000
yield2021$`Menge, kg` <- as.numeric(yield2021$`Menge, kg`)
gesamtertrag21 <- sum(yield2021$`Menge, kg`, na.rm = TRUE)/1000
yield2022$`Menge, kg` <- as.numeric(yield2022$`Menge, kg`)
gesamtertrag22 <- sum(yield2022$`Menge, kg`, na.rm = TRUE)/1000
yield2023$`Menge, kg` <- as.numeric(yield2023$`Menge, kg`)
gesamtertrag23 <- sum(yield2023$`Menge, kg`, na.rm = TRUE)/1000
yield2024$`Menge, kg` <- as.numeric(yield2024$`Menge, kg`)
gesamtertrag24 <- sum(yield2024$`Menge, kg`, na.rm = TRUE)/1000
yield2025$`Menge, kg` <- as.numeric(yield2025$`Menge, kg`)
gesamtertrag25 <- sum(yield2025$`Menge, kg`, na.rm = TRUE)/1000

ertragOverview <- c(gesamtertrag13, gesamtertrag14, gesamtertrag15, gesamtertrag16,
                    gesamtertrag17, gesamtertrag18, gesamtertrag19, gesamtertrag20,
                    gesamtertrag21, gesamtertrag22, gesamtertrag23, gesamtertrag24,
                    gesamtertrag25)
jahre <- c(13:25)
df <- data.frame(jahre, ertragOverview)
plot(df, type = "l")
#strong differences over the years

#4. Erntemenge mit Hagel überlappen ############################################
head(haildays_sf)
#only get the haildays of Zurich
#Grenze von Zürich

kantone <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/1_raw/Kanton/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp")
zürich <- kantone[6,]

#nur die Haildays behalten, dessen Koordinaten sich in Zürich befinden
#mithilfe von GIS gemacht (File mit Kantonsgrenze und File mit Hagel jeweils für die ganze Schweiz
#eingelesen und dann mit intersect die Überlappungen gesucht)
hagelTageZürich <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/hagelTageZürich.shp")

head(hagelTageZürich)
#hier noch die Rebgemeinden hinzufügen
View(muniShapeSimpel)
muniShapeSimpel <- muniShape[, -c(1:15)]
muniShapeSimpel <- muniShapeSimpel[, -c(2:8)]
df <- df[, -c(2, 4)]

st_transform(hagelTageZürich, 2056)
st_transform(muniShapeSimpel, 2056)

st_join(hagelTageZürich, muniShapeSimpel)

#haildays per year 2013
totalhaildays2013 <- sum(na.omit(hagelTageZürich$hldY_14))
totalhaildays2013
totalhaildays2014 <- sum(na.omit(hagelTageZürich$hldY_15))
totalhaildays2015 <- sum(na.omit(hagelTageZürich$hldY_16))
totalhaildays2016 <- sum(na.omit(hagelTageZürich$hldY_17))
totalhaildays2017 <- sum(na.omit(hagelTageZürich$hldY_18))
totalhaildays2018 <- sum(na.omit(hagelTageZürich$hldY_19))
totalhaildays2019 <- sum(na.omit(hagelTageZürich$hldY_20))
totalhaildays2020 <- sum(na.omit(hagelTageZürich$hldY_21))
totalhaildays2021 <- sum(na.omit(hagelTageZürich$hldY_22))
totalhaildays2022 <- sum(na.omit(hagelTageZürich$hldY_23))
totalhaildays2023 <- sum(na.omit(hagelTageZürich$hldY_23))
totalhaildays2024 <- sum(na.omit(hagelTageZürich$hldY_24))
totalhaildays2025 <- sum(na.omit(hagelTageZürich$hldY_25))

haildaysOverview <- c(totalhaildays2013, totalhaildays2014, totalhaildays2015, 
                      totalhaildays2016, totalhaildays2017, totalhaildays2018,
                      totalhaildays2019, totalhaildays2020, totalhaildays2021,
                      totalhaildays2022, totalhaildays2023, totalhaildays2024,
                      totalhaildays2025)
ertragOverview <- c(gesamtertrag13, gesamtertrag14, gesamtertrag15, gesamtertrag16,
                    gesamtertrag17, gesamtertrag18, gesamtertrag19, gesamtertrag20,
                    gesamtertrag21, gesamtertrag22, gesamtertrag23, gesamtertrag24,
                    gesamtertrag25)
jahre <- c(13:23)
df <- data.frame(jahre, ertragOverview, haildaysOverview)
df
hagelundertrag <- ggplot(df, aes(x = jahre)) +
  geom_line(aes(y = haildaysOverview, color = "Hail")) +
  geom_line(aes(y = ertragOverview, color = "Yield")) +
  labs(title = "Hail and Yield", x = "Jahre", y = "Yield in tons and haildays per year") +
  scale_color_manual(values = c("Hail" = "red", "Yield" = "blue"))
hagelundertrag
ggsave(hagelundertrag, file = "3_output/HailandYield.png", width = 25, height = 20, bg = "white", units = "cm")  


#Hageltage sind nicht relevant, da da auch Tage mitgezählt werden, bei denen die
#Körner zu klein sind, um schaden anzurichten. Deshalb nun mit max. average
#hail size
head(na.omit(hailsize_2cm_extract))
head(zürich)
zürich <- st_transform(zürich, 2056)

# #join to only get hail in Zurich
# HailsizeInZurich <- st_join(hailsize_2cm_extract, zürich)
# head(HailsizeInZurich)

#save as shp
hailsize_2cm <- st_as_sf(hailsize_2cm, crs = 2056, coords = c("lon", "lat"))
st_write(hailsize_2cm, "2_data/hailsize_2cm.shp")

hagelSizeZürich <- st_read("Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/2_data/hailsize_2cm_all_years/hailsize_2cm_all_year.shp")
View(hagelSizeZürich)

#clean up
hagelSizeZürich <- hagelSizeZürich[,-c(1, 16:35)]

#Nun will ich für jedes Jahr (= jede Spalte) den Durchschnitt der Hagelgrösse.
#So habe ich dann für jeden Ort, aber zur gleichen Zeit den Durchschnitt.
mean_y11 <- mean(hagelSizeZürich$MZCY_12, na.rm = TRUE)
mean_y12 <- mean(hagelSizeZürich$MZCY_13, na.rm = TRUE)
mean_y13 <- mean(hagelSizeZürich$MZCY_14, na.rm = TRUE)
mean_y14 <- mean(hagelSizeZürich$MZCY_15, na.rm = TRUE)
mean_y15 <- mean(hagelSizeZürich$MZCY_16, na.rm = TRUE)
mean_y16 <- mean(hagelSizeZürich$MZCY_17, na.rm = TRUE)
mean_y17 <- mean(hagelSizeZürich$MZCY_18, na.rm = TRUE)
mean_y18 <- mean(hagelSizeZürich$MZCY_19, na.rm = TRUE)
mean_y19 <- mean(hagelSizeZürich$MZCY_20, na.rm = TRUE)
mean_y20 <- mean(hagelSizeZürich$MZCY_21, na.rm = TRUE)
mean_y21 <- mean(hagelSizeZürich$MZCY_22, na.rm = TRUE)
mean_y22 <- mean(hagelSizeZürich$MZCY_23, na.rm = TRUE)
mean_y23 <- mean(hagelSizeZürich$MZCY_24, na.rm = TRUE)

mean_hailsize_per_year <- c(mean_y13, mean_y14, mean_y15, mean_y16,
                            mean_y17, mean_y18, mean_y19, mean_y20, 
                            mean_y21,
                            mean_y22, mean_y23, mean_y24, mean_y25) #jahr 13 bis 23
head(mean_hailsize_per_year)
ertragOverview <- c(gesamtertrag13, gesamtertrag14, gesamtertrag15, gesamtertrag16,
                    gesamtertrag17, gesamtertrag18, gesamtertrag19, gesamtertrag20,
                    gesamtertrag21, gesamtertrag22, gesamtertrag23, gesamtertrag24,
                    gesamtertrag25)
jahre <- c(13:25)
df <- data.frame(jahre, mean_hailsize_per_year, ertragOverview)


#plot
hagelundertrag2 <- ggplot(df, aes(x = jahre)) +
  geom_line(aes(y = mean_hailsize_per_year*1000, color = "Hailsize")) +
  geom_line(aes(y = ertragOverview, color = "Yield")) +
  labs(title = "Hail and Yield", x = "Years", y = "Yield in tons and average of the maximal hail size per year * 1000") +
  scale_color_manual(values = c("Hailsize" = "red", "Yield" = "blue"))
hagelundertrag2
ggsave(hagelundertrag2, file = "3_output/HailsizeandYield.png", width = 25, height = 20, bg = "white", units = "cm")  

summary(lm(ertragOverview ~ mean_hailsize_per_year, data = df))
