# 1. Admin #########################################################################
library(sf)
library(ggplot2)
library(patchwork)
library(dplyr)

shapefile <- st_read("2_data/Landwirtschaftliche_Kulturflächen_(aktuell)/Landwirtschaftli...achen_-aktuell/LW_NUTZUNGSFLAECHEN_AKTUELL_F.shp")

# 2. Transformation of shapefile #################################################
df <- st_drop_geometry(shapefile)

df$reben <- as.numeric(ifelse(df$BLW_NAME %in% c(
  "Rebflächen mit natürlicher  Artenvielfalt",
  "Reben"
), 1, 0))

df <- df %>%
  dplyr::select(reben)

shapefile <- cbind(shapefile, df)
shapefile <- shapefile[shapefile$reben != 0,]

#deleting unnecessary columns
shapefile <- shapefile[, -c(1, 2, 6:22, 24: 34)]

# 3. Merging of shapefile and vineyard registry ################################
shapefile <- shapefile %>%
  rename(Rebgemeinde = GEMBEZ, Parzellennr. = ESTATE_NR)

#Merge of data_long to shapefile
data_long <- data_long %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  distinct(Parzellennr., Rebgemeinde, .keep_all = TRUE)

merged_data_all_original <- full_join(data_long, shapefile) #joined by Parzellennr. & Rebgemeinde

#4. Overview over the new dataframe ############################################
# Percentage of plots planted before 2000
x <- nrow(merged_data_all_original[merged_data_all_original$plantation_year < 2000,])
y <- nrow(merged_data_all_original)
x/y #50 % were planted before 2000

# Top 3 grape varieties in 2013 vs. in 2025
#2013
merged_data_all_2013 <- merged_data_all_original %>%
  filter(year == 2013) %>%
  group_by(Rebsorte) %>%
  summarise(Gesamtfläche = sum(Fläche), .groups = "drop") %>%
  arrange(desc(Gesamtfläche))
merged_data_all_2013[1,] #1. Pinot noir
merged_data_all_2013[1,2] / 10000 #with 319 ha
merged_data_all_2013[2,] #2. Riesling-Silvaner
merged_data_all_2013[2,2] / 10000 #with 123 ha
merged_data_all_2013[3,] #3. Räuschling
merged_data_all_2013[3,2] / 10000 #with 17 ha

#2025
merged_data_all_2025 <- merged_data_all_original %>%
  filter(year == 2025) %>%
  group_by(Rebsorte) %>%
  summarise(Gesamtfläche = sum(Fläche), .groups = "drop") %>%
  arrange(desc(Gesamtfläche))
merged_data_all_2025[1,] #1. Pinot noir
merged_data_all_2025[1,2] / 10000 #with 250 ha
merged_data_all_2025[2,] #2. Riesling-Silvaner
merged_data_all_2025[2,2] / 10000 #with 105 ha
merged_data_all_2025[3,] #1. Sauvignon blanc
merged_data_all_2025[3,2] / 10000 #with 21 ha

#Has BFF Q2 (= J)
nrow(merged_data_all_original[merged_data_all_original$HAS_BFF_Q2 == "J",]) / nrow(merged_data_all_original)
#38 % have BFF Q2

#What percentage of plots is smaller than 1ha
nrow(merged_data_all_original[merged_data_all_original$Fläche < 10000,]) / nrow(merged_data_all_original)
#nearly all plots are smaller than a ha (over 99 %)
nrow(merged_data_all_original[merged_data_all_original$Fläche < 5000,]) / nrow(merged_data_all_original)
#many are also smaller than 0.5 ha

#how many observations were deleted
# original data (N = 47'710)
x <- nrow(jahr2013large) + nrow(jahr2015large) + nrow(jahr2017large) + nrow(jahr2018large) +
  nrow(jahr2019large) + nrow(jahr2020large) + nrow(jahr2021large) + nrow(jahr2022large) +
   nrow(jahr2023large) + nrow(jahr2024large) + nrow(jahr2025large)

#in the end
y <- nrow(merged_data_all_original)

#number of deleted entries
x - y
# in percent
((x-y)/x)*100

#New problem: 14'000 entries are only available in the Rebkataster, but no 
#corresponding estatenumber in the same municipality can be found
sum(is.na(merged_data_all$BLW_NAME))

#Entries that are in one of the two files but not the others
fehlende_in_shapefile <- anti_join(data_long, shapefile, by = c("Parzellennr.", "Rebgemeinde"))
fehlende_in_data_long <- anti_join(shapefile, data_long, by = c("Parzellennr.", "Rebgemeinde"))
nrow(fehlende_in_shapefile)

nrow(merged_data_all[merged_data_all$Weinmerkmal == 1,]) / nrow(merged_data_all)
nrow(fehlende_in_shapefile[fehlende_in_shapefile$Weinmerkmal == 1,]) / nrow(fehlende_in_shapefile)
#18 % of missing datapoints are PiWis, in the whole dataset its 15 % so PiWis
#are deleted a bit more often than on average. This should be taken into account
#for interpretation
