# 0. Administration ############################################################
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)


# 1. Reading the raw data ######################################################
jahr2013large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2013_mod.xlsx")
jahr2015large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2015_mod.xlsx")
jahr2017large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2017_mod.xlsx")
jahr2018large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2018_mod.xlsx")
jahr2019large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2019_mod.xlsx")
jahr2020large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2020_mod.xlsx")
jahr2021large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2021_mod.xlsx")
jahr2022large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2022_mod.xlsx")
jahr2023large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2023_mod.xlsx")
jahr2024large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2024_mod.xlsx")
jahr2025large <- read_excel("2_data/1_raw/Rebbaukataster/Rebbaukataster_zh_2025_mod.xlsx")

#Number of observations (N = 47'710)
nrow(jahr2013large) + nrow(jahr2015large) + nrow(jahr2017large) + nrow(jahr2018large) +
  nrow(jahr2019large) + nrow(jahr2020large) + nrow(jahr2021large) + nrow(jahr2022large) +
  + nrow(jahr2023large) + nrow(jahr2024large) + nrow(jahr2025large)

#Number of vineyards 
length(unique(jahr2013large$Parzellennr.))

#Number of farms across the years
length(unique(jahr2013large$Betrieb))
length(unique(jahr2025large$Betrieb))

# Share of piwi plots in 2013 and 2023
nrow(subset(jahr2013large, Weinmerkmal == "interspezifisch"))/nrow(subset(jahr2013large, !is.na(Weinmerkmal)))*100
nrow(subset(jahr2025large, Weinmerkmal == "interspezifisch"))/nrow(subset(jahr2023large, !is.na(Weinmerkmal)))*100

#calculate area for original data to check --> to high for all years
sum(jahr2025large$Fläche..m2, na.rm = TRUE)/10000
sum(jahr2013large$Fläche..m2, na.rm = TRUE)/10000

# 2. Clean Up ################################################################
# Clean up the column names to remove special characters and line breaks
colnames(jahr2013large) <- make.names(colnames(jahr2013large), unique = TRUE)
colnames(jahr2015large) <- make.names(colnames(jahr2015large), unique = TRUE)
colnames(jahr2017large) <- make.names(colnames(jahr2017large), unique = TRUE)
colnames(jahr2018large) <- make.names(colnames(jahr2018large), unique = TRUE)
colnames(jahr2019large) <- make.names(colnames(jahr2019large), unique = TRUE)
colnames(jahr2020large) <- make.names(colnames(jahr2020large), unique = TRUE)
colnames(jahr2021large) <- make.names(colnames(jahr2021large), unique = TRUE)
colnames(jahr2022large) <- make.names(colnames(jahr2022large), unique = TRUE)
colnames(jahr2023large) <- make.names(colnames(jahr2023large), unique = TRUE)
colnames(jahr2024large) <- make.names(colnames(jahr2024large), unique = TRUE)
colnames(jahr2025large) <- make.names(colnames(jahr2024large), unique = TRUE)

# Removing spaces
jahr2013large$Betrieb<- gsub(" ","",jahr2013large$Betrieb)
jahr2015large$Betrieb<- gsub(" ","",jahr2015large$Betrieb)
jahr2017large$Betrieb<- gsub(" ","",jahr2017large$Betrieb)
jahr2018large$Betrieb<- gsub(" ","",jahr2018large$Betrieb)
jahr2019large$Betrieb<- gsub(" ","",jahr2019large$Betrieb)
jahr2020large$Betrieb<- gsub(" ","",jahr2020large$Betrieb)
jahr2021large$Betrieb<- gsub(" ","",jahr2021large$Betrieb)
jahr2022large$Betrieb<- gsub(" ","",jahr2022large$Betrieb)
jahr2023large$Betrieb<- gsub(" ","",jahr2023large$Betrieb)
jahr2024large$Betrieb<- gsub(" ","",jahr2024large$Betrieb)
jahr2025large$Betrieb<- gsub(" ","",jahr2025large$Betrieb)

# Standardize the column names
colnames(jahr2019large)[colnames(jahr2019large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2020large)[colnames(jahr2020large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2021large)[colnames(jahr2021large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2022large)[colnames(jahr2022large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2023large)[colnames(jahr2023large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2024large)[colnames(jahr2024large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2025large)[colnames(jahr2025large) == "Fläche...m2"] <- "Fläche..m2"

#Further Clean up
jahr2019large <- jahr2019large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extract the last parenthese and remove the others
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Remove the last parenthese and its content
  )

jahr2020large <- jahr2020large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  
  )

jahr2021large <- jahr2021large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  
  )

jahr2022large <- jahr2022large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  
  )

jahr2023large <- jahr2023large %>%
 mutate(
   Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>%
     str_remove_all("[()]"),  
   Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  
 )

jahr2024large <- jahr2024large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$") 
  )

jahr2025large <- jahr2025large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  
  )

#Only keep intersting columns for easier handling
jahr2013Original <- jahr2013large %>%
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2013 = Rod...jahr,
         Weinmerkmal_2013 = Weinmerkmal)

jahr2015Original <- jahr2015large %>%
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2015 = Rod...jahr,
         Weinmerkmal_2015 = Weinmerkmal)

jahr2017Original <- jahr2017large %>%
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2017 = Rod...jahr,
         Weinmerkmal_2017 = Weinmerkmal)

jahr2018Original <- jahr2018large %>%
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2018 = Rod...jahr,
         Weinmerkmal_2018 = Weinmerkmal)

jahr2019Original <- jahr2019large %>%
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2019 = Rod...jahr,
         Weinmerkmal_2019 = Weinmerkmal)

jahr2020Original <- jahr2020large %>%
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2020 = Rod...jahr,
         Weinmerkmal_2020 = Weinmerkmal)

jahr2021Original <- jahr2021large %>% 
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2021 = Rod...jahr,
         Weinmerkmal_2021 = Weinmerkmal)

jahr2022Original <- jahr2022large %>% 
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2022 = Rod...jahr,
         Weinmerkmal_2022 = Weinmerkmal)

jahr2023Original <- jahr2023large %>% 
  dplyr::select(Betrieb,
         Parzellennr.,
         #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
         Sortennummer,
         Rebgemeinde,
         Rebsorte, 
         Fläche..m2, 
         Pflanz...jahr,
         Rod...jahr,
         Weinmerkmal) %>%
  rename(Rodungsjahr_2023 = Rod...jahr,
         Weinmerkmal_2023 = Weinmerkmal)

jahr2024Original <- jahr2024large %>% 
  dplyr::select(Betrieb,
                Parzellennr.,
                #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
                Sortennummer,
                Rebgemeinde,
                Rebsorte, 
                Fläche..m2, 
                Pflanz...jahr,
                Rod...jahr,
                Weinmerkmal) %>%
  rename(Rodungsjahr_2024 = Rod...jahr,
         Weinmerkmal_2024 = Weinmerkmal)

jahr2025Original <- jahr2025large %>%  
  dplyr::select(Betrieb,
                Parzellennr.,
                #Kategorie...A..bestock..B..unbestockt..C..kein.Wein,
                Sortennummer,
                Rebgemeinde,
                Rebsorte, 
                Fläche..m2, 
                Pflanz...jahr,
                Rod...jahr,
                Weinmerkmal) %>%
  rename(Rodungsjahr_2025 = Rod...jahr,
         Weinmerkmal_2025 = Weinmerkmal)

#Copy to compare data before and after processing
jahr2013 <- jahr2013Original
jahr2015 <- jahr2015Original
jahr2017 <- jahr2017Original
jahr2018 <- jahr2018Original
jahr2019 <- jahr2019Original
jahr2020 <- jahr2020Original
jahr2021 <- jahr2021Original
jahr2022 <- jahr2022Original
jahr2023 <- jahr2023Original
jahr2024 <- jahr2024Original
jahr2025 <- jahr2025Original

#Starting in 2019, the variety number and grape variety were no longer recorded on separate lines.
#As a first step, I'll clean up the code so that the variety number and grape variety are back on separate lines.

#Ensure consistent variety names 
#Sauvignon Soyhières (VB 32-7) --> Sauvignon Soyhières
#Savagnin blanc --> 	Heida
#Cabaret noir (VB 91-26-04) --> Cabernet noir

jahr2019$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2019$Rebsorte)
jahr2019$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2019$Rebsorte)
jahr2019$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2019$Rebsorte)

jahr2020$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2020$Rebsorte)
jahr2020$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2020$Rebsorte)
jahr2020$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2020$Rebsorte)

jahr2021$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2021$Rebsorte)
jahr2021$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2021$Rebsorte)
jahr2021$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2021$Rebsorte)

jahr2022$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2022$Rebsorte)
jahr2022$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2022$Rebsorte)
jahr2022$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2022$Rebsorte)

jahr2023$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2023$Rebsorte)
jahr2023$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2023$Rebsorte)
jahr2023$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2023$Rebsorte)

jahr2024$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2024$Rebsorte)
jahr2024$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2024$Rebsorte)
jahr2024$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2024$Rebsorte)

jahr2025$Rebsorte <- gsub("Sauvignon Soyhières \\(VB 32-7\\)", "Sauvignon Soyhières", jahr2025$Rebsorte)
jahr2025$Rebsorte <- gsub("Cabaret noir \\(VB 91-26-04\\)", "Cabernet noir", jahr2025$Rebsorte)
jahr2025$Rebsorte <- gsub("Savagnin blanc", "Heida", jahr2025$Rebsorte)

#Remove letters out of estate number
jahr2013$Parzellennr. <- gsub("[^0-9]", "", jahr2013$Parzellennr.)
jahr2015$Parzellennr. <- gsub("[^0-9]", "", jahr2015$Parzellennr.)
jahr2017$Parzellennr. <- gsub("[^0-9]", "", jahr2017$Parzellennr.)
jahr2018$Parzellennr. <- gsub("[^0-9]", "", jahr2018$Parzellennr.)
jahr2019$Parzellennr. <- gsub("[^0-9]", "", jahr2019$Parzellennr.)
jahr2020$Parzellennr. <- gsub("[^0-9]", "", jahr2020$Parzellennr.)
jahr2021$Parzellennr. <- gsub("[^0-9]", "", jahr2021$Parzellennr.)
jahr2022$Parzellennr. <- gsub("[^0-9]", "", jahr2022$Parzellennr.)
jahr2023$Parzellennr. <- gsub("[^0-9]", "", jahr2023$Parzellennr.)
jahr2024$Parzellennr. <- gsub("[^0-9]", "", jahr2024$Parzellennr.)
jahr2025$Parzellennr. <- gsub("[^0-9]", "", jahr2025$Parzellennr.)

#Fix unusual estate numbers
jahr2013 <- jahr2013 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% #numbers like 1234+5678 are split in two rows
  separate_rows(Parzellennr., sep = "\\,") %>% #numbers like 1234,5678 are split in two rows
  separate_rows(Parzellennr., sep = "\\/") #numbers like 1234/5678 are split in two rows

jahr2015 <- jahr2015 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2017 <- jahr2017 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2018 <- jahr2018 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2019 <- jahr2019 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2020 <- jahr2020 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2021 <- jahr2021 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2022 <- jahr2022 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/")

jahr2023 <- jahr2023 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2024 <- jahr2024 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

jahr2025 <- jahr2025 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% 
  separate_rows(Parzellennr., sep = "\\,") %>% 
  separate_rows(Parzellennr., sep = "\\/") 

#delete estate number 0
jahr2013 <- jahr2013[jahr2013$Parzellennr. != 0,]
jahr2015 <- jahr2015[jahr2015$Parzellennr. != 0,]
jahr2017 <- jahr2017[jahr2017$Parzellennr. != 0,]
jahr2018 <- jahr2018[jahr2018$Parzellennr. != 0,]
jahr2019 <- jahr2019[jahr2019$Parzellennr. != 0,]
jahr2020 <- jahr2020[jahr2020$Parzellennr. != 0,]
jahr2021 <- jahr2021[jahr2021$Parzellennr. != 0,]
jahr2022 <- jahr2022[jahr2022$Parzellennr. != 0,]
jahr2023 <- jahr2023[jahr2023$Parzellennr. != 0,]
jahr2024 <- jahr2024[jahr2024$Parzellennr. != 0,]
jahr2025 <- jahr2025[jahr2025$Parzellennr. != 0,]


# Renaming, Variety and planting date gets combined
jahr2013 <- jahr2013 %>%
  rename(Fläche_2013 = Fläche..m2, Rebsorte_2013 = Rebsorte) %>%  
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2013, Pflanz...jahr, sep = "_")) 

jahr2015 <- jahr2015 %>%
  rename(Fläche_2015 = Fläche..m2, Rebsorte_2015 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2015, Pflanz...jahr, sep = "_")) 

jahr2017 <- jahr2017 %>%
  rename(Fläche_2017 = Fläche..m2, Rebsorte_2017 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2017, Pflanz...jahr, sep = "_")) 

jahr2018 <- jahr2018 %>%
  rename(Fläche_2018 = Fläche..m2, Rebsorte_2018 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2018, Pflanz...jahr, sep = "_")) 
  
jahr2019 <- jahr2019 %>%
  rename(Fläche_2019 = Fläche..m2, Rebsorte_2019 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2019, Pflanz...jahr, sep = "_")) 

jahr2020 <- jahr2020 %>%
  rename(Fläche_2020 = Fläche..m2, Rebsorte_2020 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2020, Pflanz...jahr, sep = "_")) 

jahr2021 <- jahr2021 %>%
  rename(Fläche_2021 = Fläche..m2, Rebsorte_2021 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2021, Pflanz...jahr, sep = "_")) 

jahr2022 <- jahr2022 %>%
  rename(Fläche_2022 = Fläche..m2, Rebsorte_2022 = Rebsorte) %>%
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2022, Pflanz...jahr, sep = "_")) 

jahr2023 <- jahr2023 %>%
  rename(Fläche_2023 = Fläche..m2, Rebsorte_2023 = Rebsorte) %>%
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2023, Pflanz...jahr, sep = "_"))

jahr2024 <- jahr2024 %>%
  rename(Fläche_2024 = Fläche..m2, Rebsorte_2024 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2024, Pflanz...jahr, sep = "_"))

jahr2025 <- jahr2025 %>%
  rename(Fläche_2025 = Fläche..m2, Rebsorte_2025 = Rebsorte) %>% 
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2025, Pflanz...jahr, sep = "_")) 


#unbestockt instead of unbestockt_NA or unbestockt_0

jahr2013 <- jahr2013 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2015 <- jahr2015 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2017 <- jahr2017 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2018 <- jahr2018 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2019 <- jahr2019 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2020 <- jahr2020 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2021 <- jahr2021 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2022 <- jahr2022 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2023 <- jahr2023 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2024 <- jahr2024 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))

jahr2025 <- jahr2025 %>%
  mutate(Sorte_Pflanzjahr = ifelse(Sorte_Pflanzjahr %in% c("unbestockt_NA", "unbestockt_0"), "unbestockt", Sorte_Pflanzjahr))


# Change "sorte_pflanzjahr", wenn a value in "Rod...jahr" is found
jahr2013<- jahr2013 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2013), paste(Sorte_Pflanzjahr, Rodungsjahr_2013, sep = "_"), Sorte_Pflanzjahr))

jahr2015<- jahr2015 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2015), paste(Sorte_Pflanzjahr, Rodungsjahr_2015, sep = "_"), Sorte_Pflanzjahr))

jahr2017<- jahr2017 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2017), paste(Sorte_Pflanzjahr, Rodungsjahr_2017, sep = "_"), Sorte_Pflanzjahr))

jahr2018<- jahr2018 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2018), paste(Sorte_Pflanzjahr, Rodungsjahr_2018, sep = "_"), Sorte_Pflanzjahr))

jahr2019<- jahr2019 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2019), paste(Sorte_Pflanzjahr, Rodungsjahr_2019, sep = "_"), Sorte_Pflanzjahr))

jahr2020<- jahr2020 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2020), paste(Sorte_Pflanzjahr, Rodungsjahr_2020, sep = "_"), Sorte_Pflanzjahr))

jahr2021<- jahr2021 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2021), paste(Sorte_Pflanzjahr, Rodungsjahr_2021, sep = "_"), Sorte_Pflanzjahr))

jahr2022<- jahr2022 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2022), paste(Sorte_Pflanzjahr, Rodungsjahr_2022, sep = "_"), Sorte_Pflanzjahr))

jahr2023<- jahr2023 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2023), paste(Sorte_Pflanzjahr, Rodungsjahr_2023, sep = "_"), Sorte_Pflanzjahr))

jahr2024<- jahr2024 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2024), paste(Sorte_Pflanzjahr, Rodungsjahr_2024, sep = "_"), Sorte_Pflanzjahr))

jahr2025<- jahr2025 %>%
  mutate(Sorte_Pflanzjahr = ifelse(!is.na(Rodungsjahr_2025), paste(Sorte_Pflanzjahr, Rodungsjahr_2025, sep = "_"), Sorte_Pflanzjahr))


# 3. Merging the files #########################################################
#Some entries only differ in the area, but variety, planting date etc. are the
#same. This causes issues when merging. Duplicates are combined
jahr2013 <- jahr2013 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2013, Weinmerkmal_2013) %>%
  summarise(Fläche_2013 = sum(Fläche_2013), .groups = "drop")
jahr2015 <- jahr2015 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2015, Weinmerkmal_2015) %>%
  summarise(Fläche_2015 = sum(Fläche_2015), .groups = "drop")
jahr2017 <- jahr2017 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2017, Weinmerkmal_2017) %>%
  summarise(Fläche_2017 = sum(Fläche_2017), .groups = "drop")
jahr2018 <- jahr2018 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2018, Weinmerkmal_2018) %>%
  summarise(Fläche_2018 = sum(Fläche_2018), .groups = "drop")
jahr2019 <- jahr2019 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2019, Weinmerkmal_2019) %>%
  summarise(Fläche_2019 = sum(Fläche_2019), .groups = "drop")
jahr2020 <- jahr2020 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2020, Weinmerkmal_2020) %>%
  summarise(Fläche_2020 = sum(Fläche_2020), .groups = "drop")
jahr2021 <- jahr2021 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2021, Weinmerkmal_2021) %>%
  summarise(Fläche_2021 = sum(Fläche_2021), .groups = "drop")
jahr2022 <- jahr2022 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2022, Weinmerkmal_2022) %>%
  summarise(Fläche_2022 = sum(Fläche_2022), .groups = "drop")
jahr2023 <- jahr2023 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2023, Weinmerkmal_2023) %>%
  summarise(Fläche_2023 = sum(Fläche_2023), .groups = "drop")
jahr2024 <- jahr2024 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2024, Weinmerkmal_2024) %>%
  summarise(Fläche_2024 = sum(Fläche_2024), .groups = "drop")
jahr2025 <- jahr2025 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2025, Weinmerkmal_2025) %>%
  summarise(Fläche_2025 = sum(Fläche_2025), .groups = "drop")

#calculate area after clean up as check
sum(jahr2025$Fläche_2025, na.rm = TRUE)/10000
sum(jahr2013$Fläche_2013, na.rm = TRUE)/10000

#Number of observations after clean up (N = 46'762) 
nrow(jahr2013) + nrow(jahr2015) + nrow(jahr2017) + nrow(jahr2018) +
  nrow(jahr2019) + nrow(jahr2020) + nrow(jahr2021) + nrow(jahr2022) +
  + nrow(jahr2023) + nrow(jahr2024) + nrow(jahr2025)

# List with all the data
datensatz_liste <- list(
  unique(jahr2013[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2013","Weinmerkmal_2013","Fläche_2013")]),
  unique(jahr2015[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2015","Weinmerkmal_2015","Fläche_2015")]),
  unique(jahr2017[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2017","Weinmerkmal_2017","Fläche_2017")]),
  unique(jahr2018[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2018","Weinmerkmal_2018","Fläche_2018")]),
  unique(jahr2019[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2019","Weinmerkmal_2019","Fläche_2019")]),
  unique(jahr2020[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2020","Weinmerkmal_2020","Fläche_2020")]),
  unique(jahr2021[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2021","Weinmerkmal_2021","Fläche_2021")]),
  unique(jahr2022[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2022","Weinmerkmal_2022","Fläche_2022")]),
  unique(jahr2023[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2023","Weinmerkmal_2023","Fläche_2023")]),
  unique(jahr2024[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2024","Weinmerkmal_2024","Fläche_2024")]),
  unique(jahr2025[, c("Betrieb", "Parzellennr.", "Rebgemeinde", "Sorte_Pflanzjahr", "Pflanz...jahr", "Rodungsjahr_2025","Weinmerkmal_2025","Fläche_2025")])
)

# Use Reduce, to merge all the data
merged_data_all <- Reduce(function(x, y) merge(x, y, by = c("Betrieb", 
                                                            "Parzellennr.", 
                                                            "Sorte_Pflanzjahr", 
                                                            "Rebgemeinde",
                                                            "Pflanz...jahr"
                                                            ), all = TRUE), datensatz_liste)
                                                            
merged_data_all <- merged_data_all %>%
  distinct()

#New row with only variety names
merged_data_all$Rebsorte <- sub("_.*", "", merged_data_all$Sorte_Pflanzjahr)

#area check
sum(merged_data_all$Fläche_2013, na.rm = TRUE)/10000

# 4. Reshape von wide to long ##############################################
library(tidyr)
library(dplyr)

data_long <- merged_data_all %>%
  pivot_longer(
    cols = matches("^(Rodungsjahr|Weinmerkmal|Fl(ä|a)che)_\\d{4}$"),
    names_to = c(".value", "year"),
    names_sep = "_"
  ) %>%
  mutate(year = as.integer(year)) %>%
  dplyr::select(Betrieb, Parzellennr., Sorte_Pflanzjahr, year, Rodungsjahr, Weinmerkmal,
         Fläche, Rebgemeinde)

data_long$plantation_year <- as.numeric(sub(".*_", "", data_long$Sorte_Pflanzjahr))
data_long$Rebsorte <- sub("_.*", "", data_long$Sorte_Pflanzjahr)

#area check 
sum(data_long$Fläche[which(data_long$year == 2013)], na.rm = TRUE)/10000

#0, when nothing is planted
data_long$Fläche_test <- ifelse((data_long$year <= data_long$plantation_year) & is.na(data_long$Fläche), 0, data_long$Fläche)
data_long$group_id <- paste(data_long$Betrieb, data_long$Parzellennr., data_long$Sorte_Pflanzjahr, sep = "_")
data_long <- data_long %>%
  group_by(group_id) %>%
  mutate(drop_flag = max(Fläche_test))

#Ensure that Weinmerkmal is 1 for PiWis and 0 otherwise
interspezifisch <- c("Übrige Sorten rot \"Piwi\", nicht AOC", "Übrige Sorten weiss \"Piwi\", nicht AOC",
                     "Souvignier gris", "Johanniter", "Muscaris", "Regent", "Buffalo",
                     "Muscat bleu", "Cabernet Jura", "Baco noir", "Bianca", "Birstaler Muskat",
                     "Cabernet blanc", "De Chaunac", "DeChaunac", "Nero", "CAL 1-36",
                     "Divico", "Léon Millot", "Maréchal Foch", "Solaris",
                     "Calardis blanc", "Calardis musqué", "Lac 1/02-11-12", "Lac 1/02-11-17",
                     "Lac 1/02 -05-35", "Sauvitage (WE 88-101-13)", "Seyval blanc",
                     "VB 05-A-100", "WE 86-708-86", "VB CAL 1-28", "Cabernet Carbon",
                     "Cabernet Cortis", "Divico (IRAC 2091)", "Sauvignac (VB CAL 6-04)",
                     "Carminoir", "Divona (IRAC 2060)", "Vidal blanc", "Satin noir (VB 91-26-29)",
                     "Muscatin", "Prior", "Monarch", "Diolinoir", "Siramé", "Chancellor",
                     "Pinot Nova", "Merlotin", "VB CAL 1-22", "Pinotin", "Cabertin",
                     "Piroso", "Cabernet Soyhières", "Sauvignon Soyhières", "VB 32-7 (Sauvignon Soyhières)",
                     "Donauriesling", "Kalina", "Helios",  "Cabernet noir (VB 91-26-04)",
                     "Cabernet noir", "Allegro", "Baron", "Laurot", "Roter Müller-Thurgau",
                     "Cabernet Cantor", "VB 91-26-26", "Direktträger", "CAL 1-22", 
                     "Sauvignon Soyhières (VB 32-7)", "Übrige Sorten rot (Piwi)", 
                     "Millot-Foch", "Voltis", "Bronner", "CAL 1-28", "Mara", "Cabernet VB",
                     "CAL 6-04", "VB CAL 6-04", "VB Cabernet", "Caberneuf ()", 
                     "Caberneuf", "Coliris", "Floreal", "Gamarello (MRAC 1099)",
                     "Merello (MRAC 1087)", "Cabernello (MRAC 40)", "Nerolo (MRAC 1817)",
                     "Cornarello (MRAC 1626)", "Galotta", "Pinot Iskra", "Chambourcin",
                     "RAC 3209", "CabVB rot", "CabVB weiss",  "Aurora", "Excelsior",
                     "Magliasino", "Ontario")
data_long$Weinmerkmal <- ifelse(data_long$Rebsorte %in% interspezifisch, 1, 0)

data_long_unique <- data_long %>%
  distinct()

#delete entries with planting dates in the future
data_long <- data_long[data_long$plantation_year < 2026,]

#area check
sum(data_long$Fläche[which(data_long$year == 2013)], na.rm = TRUE)/10000

#check number of observations
nrow(data_long) /12 #I would expect 4000 to 5000 entries per year, not 7500
#this problem will be fixed now

#When an estate is given from one farm to another, the farm ID changes but the
#rest remains the same. This also causes issues. When the estate changes its
#owner, from now on the farm ID from the first owner is kept. The entries that
#are not needed anymore have an empty Weinmerkmal and can be deleted
#Example: Estate 996599669969 belongs until 2018 to farm ZH 1926, afterwards ZH2149
data_long <- data_long %>%
  group_by(Parzellennr., Sorte_Pflanzjahr, Rebgemeinde, plantation_year, Rebsorte ) %>%
  mutate(Betrieb = first(Betrieb)) %>%
  filter(!is.na(Weinmerkmal)) %>%
  ungroup()

#Differing municipalities (for example seen with estate 3338) lead to NA in
#the area column. When those entries are deleted, the number of observations is
#correct
data_long <- data_long[!is.na(data_long$Fläche),]

#unbestockt --> area = 0
data_long$Fläche <- ifelse(data_long$Rebsorte == "unbestockt", 0, data_long$Fläche)

#area check
sum(data_long$Fläche[which(data_long$year == 2013)], na.rm = TRUE)/10000
#now it finally works!!
sum(na.omit(jahr2013Original$Fläche..m2)) / 10000

#2025 area check
sum(data_long$Fläche[which(data_long$year == 2025)], na.rm = TRUE)/10000
sum(na.omit(jahr2025Original$Fläche..m2)) / 10000

#5. Number of grape varieties over the years ###################################
jahr2013mitRebsorte <- jahr2013
jahr2013mitRebsorte$Rebsorte <-  sub("_.*", "", jahr2013mitRebsorte$Sorte_Pflanzjahr)
length(unique(jahr2013mitRebsorte$Rebsorte)) #99 grape varieties in 2013

jahr2025mitRebsorte <- jahr2025
jahr2025mitRebsorte$Rebsorte <-  sub("_.*", "", jahr2025mitRebsorte$Sorte_Pflanzjahr)
length(unique(jahr2025mitRebsorte$Rebsorte))

#area of grapes compared to canton zurich
sum(jahr2025$Fläche_2025, na.rm = TRUE)/10000 #fläche aller Trauben
#zurich has an area of 1729 km2 (source: statista) --> 172900 ha
#According to the 2023 Agricultural Report, 41% of the canton consists of agricultural land
#area of grapes in relation to agricultural land in %
(sum(jahr2025$Fläche_2025, na.rm = TRUE)/10000) / (172900 * 0.41) * 100 
# --> less than 1 % of the entire area of Zurich is used for grapes

# 6. Area Piwi vs. Area overall ##############################################
x <- sum(data_long$Fläche[which(data_long$Weinmerkmal == 1 & data_long$year == 2013)], na.rm = TRUE)/10000 #piwi
y <- sum(data_long$Fläche[which(data_long$Weinmerkmal == 0 & data_long$year == 2013)], na.rm = TRUE)/10000 #konventionell
x/y #areas of PiWis was 7 % in 2013
x + y #area a bit too high

x <- sum(data_long$Fläche[which(data_long$Weinmerkmal == 1 & data_long$year == 2025)], na.rm = TRUE)/10000 #piwi
y <- sum(data_long$Fläche[which(data_long$Weinmerkmal == 0 & data_long$year == 2025)], na.rm = TRUE)/10000 #konventionell
x/y #in 2025, the PiWi area increased to 19 %
x + y #area too low

#year 2022 to compare with other sources
x <- sum(data_long$Fläche[which(data_long$Weinmerkmal == 1 & data_long$year == 2022)], na.rm = TRUE)/10000 #piwi
y <- sum(data_long$Fläche[which(data_long$Weinmerkmal == 0 & data_long$year == 2022)], na.rm = TRUE)/10000 #konventionell
x/y #in 2022, this code has 14 % while other sources cite 

# 7. Practical examples #######################################################
## 7.1 Area plot ----------------------------------------------------------------------------
yearly_acerage <- data_long %>%
  group_by(year) %>%
  summarise(yearly_acerage = sum(na.omit(Fläche)))

ggplot() +
  geom_line(data = yearly_acerage, aes(x = year, y = yearly_acerage / 10000)) +
  #ylim(0,50000000) +
  labs(y = "Area of grapes in ha", x = "Year") +
  theme_minimal()

sum(data_long$Fläche[which(data_long$year == 2013)], na.rm = TRUE)/10000

#I take the data from each year directly
anbaufläche2013 <- sum(jahr2013Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2015 <- sum(jahr2015Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2017 <- sum(jahr2017Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2018 <- sum(jahr2018Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2019 <- sum(jahr2019Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2020 <- sum(jahr2020Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2021 <- sum(jahr2021Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2022 <- sum(jahr2022Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2023 <- sum(jahr2023Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2024 <- sum(jahr2024Original$Fläche..m2, na.rm = TRUE)/10000
anbaufläche2025 <- sum(jahr2025Original$Fläche..m2, na.rm = TRUE)/10000
#2025 is left out as the area would be much too high
anbaufläche <- c(anbaufläche2013, anbaufläche2015, anbaufläche2017, anbaufläche2018,
                 anbaufläche2019, anbaufläche2020, anbaufläche2021, anbaufläche2022, 
                 anbaufläche2023, anbaufläche2024)

years <- c(2013, 2015, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
yearly_acerage2 <- data.frame(years, anbaufläche)

#yearly_acerage2 --> directly from farmers
#yearly_acerage --> after correction

ggplot() +
  geom_line(data = yearly_acerage, aes(x = year, y = (yearly_acerage/10000), color = "yearly_acerage"), linewidth = 1.2) +
  geom_line(data = yearly_acerage2, aes(x = years, y = anbaufläche, color = "anbaufläche"), linewidth = 1.2) +
  labs(title = "Development of the Area with Planted Grapes", color = "Variable", 
       x = "Years", y = "Area with grapes in ha") +
  scale_color_manual(                
    values = c("yearly_acerage" = "red", "anbaufläche" = "blue"),
    labels = c("anbaufläche" = "Areas provided by farmers", 
               "yearly_acerage" = "Areas after correction")
  ) +
  scale_x_continuous(breaks = seq(min(yearly_acerage$year), max(yearly_acerage$year), by = 2)) +
  theme_minimal()


## 7.2 Export ------------------------------------------------------------------
#export

library(openxlsx)

# Safe as Excel 
write.xlsx(merged_data_all, file = "merged_data_all.xlsx")


## 7.3 Betrieb Example---------------------------------------------------------
library(ggplot2)
library(tidyr)
library(grid)

zh1 <- merged_data_all %>% filter(Betrieb == "ZH1")
print(zh1)

data <- data.frame(
  Betrieb = rep("ZH1", 6),
  Parzellennr. = rep(103, 6),
  Sorte_Pflanzjahr = c(
    "Blauburgunder_1987", "Cabernet Dorsa_2003", "Dornfelder_2007",
    "Pinot gris_2003", "Riesling-Silvaner_1987", "St. Laurent_2012"
  ),
  Weinfarbe = c("rot", "rot", "rot", "weiss", "weiss", NA),
  Fläche_m2_2013 = c(4080, 1220, 500, 1500, 2200, NA),
  Fläche_m2_2015 = c(4080, 1220, 500, 1500, 2200, NA),
  Fläche_m2_2017 = c(3180, 1220, 500, 1500, 2200, 900),
  Fläche_m2_2018 = c(3180, 1220, 500, 1500, 2200, 900),
  Fläche_m2_2019 = c(3180, 1220, 500, 1500, 2200, 900),
  Fläche_m2_2020 = c(3180, 1220, 500, 1500, 2200, 900),
  Fläche_m2_2021 = c(3180, 1220, 500, 1500, 2200, 900),
  Fläche_m2_2022 = c(3180, 1220, 500, 1500, 2200, 900),
  Fläche_m2_2023 = c(3180, 1220, 500, 1500, 2200, 900)
)

#long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Fläche_m2"),
    names_to = "Jahr",
    values_to = "Fläche_m2"
  ) %>%
  mutate(Jahr = as.numeric(gsub("Fläche_m2_", "", Jahr)))  # Jahr aus Spaltennamen extrahieren

data_long <- na.omit(data_long)

ggplot(data_long, aes(x = Jahr, y = Fläche_m2, fill = Sorte_Pflanzjahr)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Veränderung der Anbaufläche der Sorten über die Jahre für Betrieb ZH1",
    x = "Jahr",
    y = "Anbaufläche (m²)",
    fill = "Sorte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.box = "horizontal"                  
  ) +
  guides(fill = guide_legend(nrow = 2))        


## 7.4 Area for each variety over the years-------------------------------------
Blauburgunder <- merged_data_all %>% filter(Rebsorte == "Blauburgunder")


data_filtered <- Blauburgunder[,c("Fläche_2013", "Fläche_2015", "Fläche_2017", 
                                 "Fläche_2018", "Fläche_2019", "Fläche_2020", 
                                 "Fläche_2021", "Fläche_2022", "Fläche_2023")]

#Sum he values for each column and divide with 10000
summed_areas <- colSums(data_filtered, na.rm = TRUE) / 10000

# Convert to data frame
plot_data <- data.frame(
  Jahr = as.numeric(sub("Fläche_", "", names(summed_areas))),
  Fläche_m2 = summed_areas
)

ggplot(plot_data, aes(x = Jahr, y = Fläche_m2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Veränderung der Anbaufläche (Blauburgunder)",
       x = "Jahr",
       y = "Gesamte Anbaufläche (ha)")

#less than 46'000 as unbestockt was deleted
