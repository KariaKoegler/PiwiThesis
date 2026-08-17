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
#Some years have the column names in the first row, I delete this row
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

#3. Yield over the years########################################################
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
jahre <- c(2013:2025)
ertragOverview <- data.frame(jahre, ertragOverview)

#4.Overlap yield with hail #####################################################
head(haildays_2cm_extract_zurich$day)
haildays_2cm_extract_zurich <- st_drop_geometry(haildays_2cm_extract_zurich)

#haildays per year 
totalhaildays2013 <- sum(na.omit(haildays_2cm_extract_zurich$days2013))
totalhaildays2014 <- sum(na.omit(haildays_2cm_extract_zurich$days2014))
totalhaildays2015 <- sum(na.omit(haildays_2cm_extract_zurich$days2015))
totalhaildays2016 <- sum(na.omit(haildays_2cm_extract_zurich$days2016))
totalhaildays2017 <- sum(na.omit(haildays_2cm_extract_zurich$days2017))
totalhaildays2018 <- sum(na.omit(haildays_2cm_extract_zurich$days2018))
totalhaildays2019 <- sum(na.omit(haildays_2cm_extract_zurich$days2019))
totalhaildays2020 <- sum(na.omit(haildays_2cm_extract_zurich$days2020))
totalhaildays2021 <- sum(na.omit(haildays_2cm_extract_zurich$days2021))
totalhaildays2022 <- sum(na.omit(haildays_2cm_extract_zurich$days2022))
totalhaildays2023 <- sum(na.omit(haildays_2cm_extract_zurich$days2023))
totalhaildays2024 <- sum(na.omit(haildays_2cm_extract_zurich$days2024))
totalhaildays2025 <- sum(na.omit(haildays_2cm_extract_zurich$days2025))

haildays_summary <- data.frame(totalhaildays2013, totalhaildays2014, totalhaildays2015, 
                               totalhaildays2016, totalhaildays2017, totalhaildays2018,
                               totalhaildays2019, totalhaildays2020, totalhaildays2021, 
                               totalhaildays2022, totalhaildays2023, totalhaildays2024,
                               totalhaildays2025)

haildays_long <- haildays_summary %>%
  pivot_longer(
    cols =  starts_with("totalhaildays"), # columns to reshape
    names_to = "year", # new column for former column names
    values_to = "haildays" # new column for values
  )

haildays_long$year <- sub("totalhaildays", "", haildays_long$year)
haildays_long$year <- as.integer(haildays_long$year)


hagelundertrag <- ggplot() +
  geom_line(data = haildays_long, aes(x = year, y = haildays, color = "Days with hail")) +
  geom_line(data = ertragOverview, aes(x = jahre, y = ertragOverview, color = "Grape yield")) +
  labs(title = "Hail and Yield", x = "Years", y = "Yield in tons and haildays per year") +
  scale_x_continuous(
    breaks = seq(floor(min(haildays_long$year)), ceiling(max(haildays_long$year)), 2) # integers only
  ) +
  scale_color_manual(
    name = NULL,   # Legendentitel ausblenden
    values = c("Days with hail" = "red",
               "Grape yield"   = "blue")
  )

hagelundertrag

colnames(ertragOverview)[colnames(ertragOverview) == "jahre"] <- "year"
df <- full_join(haildays_long, ertragOverview)

summary(lm(ertragOverview ~ haildays, data = df))
#no signficant correlation between yield and hail
