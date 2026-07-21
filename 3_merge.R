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


# 3. Merging of shapefile and Rebbaukataster ######################################
shapefile <- shapefile %>%
  rename(Rebgemeinde = GEMBEZ, Parzellennr. = ESTATE_NR)

#Merge of data_long to shapefile
data_long <- data_long %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  distinct(Parzellennr., Rebgemeinde, .keep_all = TRUE)

merged_data_all_original <- full_join(data_long, shapefile) 
# View(merged_data_all_original)
