# Load the library
library(sf)
library(ggplot2)
library(patchwork)
library(dplyr)

#convert piwi = 1, convnetional = 0
merged_data_all$Weinmerkmal <- ifelse(
  merged_data_all$Weinmerkmal == "interspezifisch", 1,
  ifelse(merged_data_all$Weinmerkmal == "europäisch", 0, NA)
)

#test
#merged_data_all$Weinmerkmal <- 1

##what is conventional grape and what is piwi
p <- ggplot() +
  geom_sf(data = merged_data_all$geometry, aes(fill = as.factor(merged_data_all$Weinmerkmal)), color = NA) +
  scale_fill_manual(name = "Grape variety", 
                    breaks = c(0,1), 
                    values = c("navyblue", "red"), 
                    labels = c("Conventional grapes", "Piwi")) +
  coord_sf(
    xlim = c(2669087.36213217, 2713875.597690578),
    ylim = c(1227293.256352458, 1276966.884853686),
    expand = FALSE
  ) +
  theme_minimal() 

#p

ggsave("3_output/piwivsconventional.png", plot = p, width = 20, height = 20, dpi = 300, units = "cm", bg = "white")


#nun mit piwi, conventionell und anderen nutzungsflächen
shapefile <- st_read("2_data/Landwirtschaftliche_Kulturflächen_(aktuell)/Landwirtschaftli...achen_-aktuell/LW_NUTZUNGSFLAECHEN_AKTUELL_F.shp")

df <- st_drop_geometry(shapefile)

df$reben <- as.numeric(ifelse(df$BLW_NAME %in% c(
  "Rebflächen mit natürlicher  Artenvielfalt",
  "Reben"
), 1, 0))

df <- df %>%
  select(reben)

shapefile <- cbind(shapefile, df)

shapefile <- shapefile %>%
  rename(Rebgemeinde = GEMBEZ, Parzellennr. = ESTATE_NR)

data_long <- data_long %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))
shapefile <- shapefile %>%
  mutate(Parzellennr. = as.numeric(Parzellennr.))

#hier anders herum mergen
merged_data_all <- shapefile %>%
  full_join(data_long, by = c("Parzellennr.", "Rebgemeinde"))

merged_data_all$Weinmerkmal <- with(merged_data_all, ifelse(
  reben == 0, 0,
  ifelse(Weinmerkmal == "interspezifisch", 2,
         ifelse(Weinmerkmal == "europäisch", 1, NA)))
)

#merged_data_all <- merged_data_all[merged_data_all$reben != 0,]

p <- ggplot() +
  geom_sf(data = merged_data_all$geometry, aes(fill = as.factor(merged_data_all$Weinmerkmal)), color = NA) +
  scale_fill_manual(name = "Grape variety", 
                    breaks = c(0,1,2), 
                    values = c("gray", "navyblue", "red"), 
                    labels = c("Other usage", "Conventional grapes", "PiWi")) +
  coord_sf(
    xlim = c(2669087.36213217, 2713875.597690578),
    ylim = c(1227293.256352458, 1276966.884853686),
    expand = FALSE
  ) +
  theme_minimal()

p

ggsave("3_output/piwioverall.png", plot = p, width = 20, height = 20, dpi = 300, units = "cm", bg = "white")



# Number of grape plots
nrow(subset(shapefile, reben == 1))

# Share from total plots
nrow(subset(shapefile, reben == 1))/nrow(shapefile)*100

# Extract only grape parcels
grapes <- shapefile %>% 
  filter(reben == 1)

# Buffer grapes by 5 meters
grapes_buffered <- st_buffer(grapes, 5)

# Identify parcels within 5m of grapes
intersections <- st_intersects(shapefile, grapes_buffered, sparse = FALSE)

shapefile <- shapefile %>%
  mutate(border_grapes = as.integer(apply(intersections, 1, any)),
         border_grapes = ifelse(reben == 1, 0, border_grapes))

shapefile <- shapefile %>%
  mutate(classifier = ifelse(reben == 1, 1,
                             ifelse(border_grapes == 1, 2, 3)))

p <- ggplot() +
  geom_sf(data = shapefile, aes(fill = as.factor(classifier)), color = NA) +
  scale_fill_manual(name = "Adjacent fields", 
                    breaks = c(1,2,3), 
                    values = c("navyblue","orange","grey"), 
                    labels = c("Grapes","Adjacent field","Not adjacent")) +
  coord_sf(
    xlim = c(2669087.36213217, 2713875.597690578),
    ylim = c(1227293.256352458, 1276966.884853686),
    expand = FALSE
  ) +
  theme_minimal() 

ggsave("3_output/addjacent_plots.png", plot = p, width = 20, height = 20, dpi = 1000, units = "cm", bg = "white")


# Prepare data
df_plot <- shapefile %>%
  st_drop_geometry() %>%
  group_by(border_grapes, BLW_NAME) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(border_grapes) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# Ordering based on bordering parcels (border_grapes == 1)
order_bordering <- df_plot %>%
  filter(border_grapes == 1) %>%
  arrange(desc(share)) %>%
  pull(BLW_NAME)

# Top plot: Non-bordering, fixed ordering
df_top <- df_plot %>%
  filter(border_grapes == 0) %>%
  mutate(BLW_NAME = factor(BLW_NAME, levels = order_bordering))

p_top <- ggplot(df_top, aes(x = BLW_NAME, y = share)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Non-Bordering Parcels",
    x = "", y = "Relative Share"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Bottom plot: Bordering parcels, independent ordering
df_bottom <- df_plot %>%
  filter(border_grapes == 1) %>%
  mutate(BLW_NAME = factor(BLW_NAME, levels = BLW_NAME[order(-share)]))

p_bottom <- ggplot(df_bottom, aes(x = BLW_NAME, y = share)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Bordering Parcels",
    x = "BLW_NAME", y = "Relative Share"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Combine (2 rows, 1 column)
together <- p_top / p_bottom +
  plot_annotation(
    title = "Relative Share of BLW Categories",
    subtitle = "Top: Non-bordering parcels (ordered by bordering ranking) | Bottom: Bordering parcels (independent order)"
  )

ggsave("3_output/BLW_share_requested_combined.png",
       together, width = 28, height = 40, dpi = 600, units = "cm", bg = "white")

#are piwis grown in clusters or are they evenly spread?
# Read the shapefile
shapefile <- #hier muss das gemergde file hin
  
df <- st_drop_geometry(shapefile)


#hagel visualisieren 
library(sf)
hagel2013 <- st_read("shapefolder/hail2013 selection.shp")
testplot <- ggplot() +
  geom_sf(data = hagel2013)

p <- ggplot() +
  geom_sf(data = hagel2013$geometry, fill = "pink") +
  geom_sf(color = "black", fill = "pink") +
  geom_sf(data = merged_data_all$geometry, aes(fill = as.factor(merged_data_all$Weinmerkmal)), color = NA) +
  scale_fill_manual(name = "Grape variety", 
                    breaks = c(0,1,2), 
                    values = c("gray", "navyblue", "red"), 
                    labels = c("Andere Nutzung", "Conventional grapes", "Piwi")) +
  coord_sf(
    xlim = c(2669087.36213217, 2713875.597690578),
    ylim = c(1227293.256352458, 1276966.884853686),
    expand = FALSE
  ) +
  theme_minimal()

ggsave("3_output/hail2013.png", plot = p, width = 20, height = 20, dpi = 1000, units = "cm", bg = "white")
