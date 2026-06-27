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
#shapefile <- shapefile[shapefile$reben != 0,]

#Some entries overlap as a part of the vineyard is more steep or has more
#biodiversity as others
#before running this line, check whether the server can handle it (too many
#users at the same time make it crash)
#shapefile <- st_difference(shapefile)

# 3. Merging of shapefile and Rebbaukataster ######################################
data_long[1,]
shapefile[1,]

shapefile <- shapefile %>%
  rename(Rebgemeinde = GEMBEZ, Parzellennr. = ESTATE_NR)

#Excel kann keine Liste in den Koordinaten haben, deshalb neue Spalten mit x und y
# shapefileOhnePolygon <- shapefile
# shapefileOhnePolygon$x <- sub(",.*", "", shapefileOhnePolygon$geometry)
# shapefileOhnePolygon$y <- sub(".*,", "", shapefileOhnePolygon$geometry)
# 
# shapefileOhnePolygon$x <- substring(shapefileOhnePolygon$x, 13)
# shapefileOhnePolygon$y <- substring(shapefileOhnePolygon$y, 1, nchar(shapefileOhnePolygon$y)-3)
# shapefileOhnePolygon <- st_drop_geometry(shapefileOhnePolygon)

#komische Parzellennummern werden korrigiert
# shapefileOhnePolygon <- shapefileOhnePolygon %>%
#  mutate(Parzellennr. = as.numeric(gsub("\\D", "", Parzellennr.)))

#Zusammengefügte Tabelle ohne Polygons aber mit Koordinaten für Excel
# merged_data_ohne_polygon <- data_long %>%
#   full_join(shapefileOhnePolygon, by = c("Parzellennr.", "Rebgemeinde"))

# Als Excel speichern
#library(openxlsx)
#write.xlsx(merged_data_ohne_polygon, file = "MergeErsterVersuch.xlsx")

#Merge of data_long to shapefile
data_long <- data_long %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  distinct(Parzellennr., Rebgemeinde, .keep_all = TRUE)

merged_data_all <- data_long %>%
  full_join(shapefile, by = c("Parzellennr.", "Rebgemeinde"))
