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

# Share of piwi plots in 2013 and 2023
nrow(subset(jahr2013large, Weinmerkmal == "interspezifisch"))/nrow(subset(jahr2013large, !is.na(Weinmerkmal)))*100
nrow(subset(jahr2025large, Weinmerkmal == "interspezifisch"))/nrow(subset(jahr2023large, !is.na(Weinmerkmal)))*100

# 2. Processing ################################################################
# Bereinige die Spaltennamen, um Sonderzeichen und Zeilenumbrüche zu entfernen
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

# Bereinigen von Leerschlägen 
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

# Vereinheitlichen der Spaltennamen Fläche
colnames(jahr2019large)[colnames(jahr2019large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2020large)[colnames(jahr2020large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2021large)[colnames(jahr2021large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2022large)[colnames(jahr2022large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2023large)[colnames(jahr2023large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2024large)[colnames(jahr2024large) == "Fläche...m2"] <- "Fläche..m2"
colnames(jahr2025large)[colnames(jahr2025large) == "Fläche...m2"] <- "Fläche..m2"

#Für das Jahr 2019:
jahr2019large <- jahr2019large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
  )

#Für alle anderen Jahre: 
jahr2020large <- jahr2020large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
  )

jahr2021large <- jahr2021large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
  )

jahr2022large <- jahr2022large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
  )

#jahr2023 <- jahr2023 %>%
#  mutate(
#    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
#      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
#    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
#  )

jahr2024large <- jahr2024large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
  )

jahr2025large <- jahr2025large %>%
  mutate(
    Sortennummer = str_extract(Rebsorte, "\\(([^()]+)\\)$") %>% 
      str_remove_all("[()]"),  # Extrahiere die letzte Klammer und entferne Klammern
    Rebsorte = str_remove(Rebsorte, "\\s*\\([^()]+\\)$")  # Entferne nur die letzte Klammer und deren Inhalt
  )

##2.1 Datensätze auf die Interessanten Zeilen Reduzieren --> Einfacheres Handling ####
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

jahr2021Original <- jahr2021large %>% #Bei der Fläche gabe es 3 Leerschläge in diesem Datensatz. 
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

jahr2022Original <- jahr2022large %>% #Bei der Fläche gabe es 3 Leerschläge in diesem Datensatz. 
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

jahr2023Original <- jahr2023large %>% #Bei der Fläche gabe es 3 Leerschläge in diesem Datensatz. 
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

jahr2024Original <- jahr2024large %>% #Bei der Fläche gabe es 3 Leerschläge in diesem Datensatz. 
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

jahr2025Original <- jahr2025large %>% #Bei der Fläche gabe es 3 Leerschläge in diesem Datensatz. 
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

#Kopie, um nachher bearbeiten und vergleichen zu können
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

#Ab dem Jahr 2019 wurden Sortennummer und Rebsorte nicht mehr in Seperaten Zeilen erfasst.
#Ich bereinige den Code in einem ersten Schritt mal so, dass Sortennummer und Rebsorte wieder in Seperaten Zeilen sind. 
#Nächstes Problem: ab dem Jahr 2019 gibt es neue Sortennummern. Das aber später. 

#Ab dem Jahr 2019 wurden auch einige Sorten etwas anders benannt. 
#Ich habe mich dazu entschieden die Namen von 2013 zu behalten. 
#Das bedeutet:
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
#Parzellennummern sind manchmal auch mit Buchstaben und nicht nur Zahlen. Ich entferne in diesem 
#Schritt die Buchstaben. 

# Buchstaben aus der Spalte entfernen
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


#komplett leere Parzellennummern wie diese werden gelöscht
jahr2013[3080,]

jahr2013 <- jahr2013 %>%
  filter(Parzellennr. != "") %>%
  separate_rows(Parzellennr., sep = "\\+") %>% ##Parzellennummern wie 1234+5678 werden aufgeteilt
  separate_rows(Parzellennr., sep = "\\,") %>% #Parzellennummern wie 1234,5678 werden aufgeteilt
  separate_rows(Parzellennr., sep = "\\/") #Parzellennummern wie 1234/5678 werden aufgeteilt

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

#parzellennummern sollen nicht grösser als 99999 sein
jahr2013$Parzellennr. <- as.numeric(jahr2013$Parzellennr.)
jahr2013 <- jahr2013 %>%
  filter(Parzellennr. < 99999)
jahr2015$Parzellennr. <- as.numeric(jahr2015$Parzellennr.)
jahr2015 <- jahr2015 %>%
  filter(Parzellennr. < 99999)
jahr2017$Parzellennr. <- as.numeric(jahr2017$Parzellennr.)
jahr2017 <- jahr2017 %>%
  filter(Parzellennr. < 99999)
jahr2018$Parzellennr. <- as.numeric(jahr2018$Parzellennr.)
jahr2018 <- jahr2018 %>%
  filter(Parzellennr. < 99999)
jahr2019$Parzellennr. <- as.numeric(jahr2019$Parzellennr.)
jahr2019 <- jahr2019 %>%
  filter(Parzellennr. < 99099)
jahr2020$Parzellennr. <- as.numeric(jahr2020$Parzellennr.)
jahr2020 <- jahr2020 %>%
  filter(Parzellennr. < 99999)
jahr2021$Parzellennr. <- as.numeric(jahr2021$Parzellennr.)
jahr2021 <- jahr2021 %>%
  filter(Parzellennr. < 99999)
jahr2022$Parzellennr. <- as.numeric(jahr2022$Parzellennr.)
jahr2022 <- jahr2022 %>%
  filter(Parzellennr. < 99999)
jahr2023$Parzellennr. <- as.numeric(jahr2023$Parzellennr.)
jahr2023 <- jahr2023 %>%
  filter(Parzellennr. < 9999)
jahr2024$Parzellennr. <- as.numeric(jahr2024$Parzellennr.)
jahr2024 <- jahr2024 %>%
  filter(Parzellennr. < 99999)
jahr2025$Parzellennr. <- as.numeric(jahr2025$Parzellennr.)
jahr2025 <- jahr2025 %>%
  filter(Parzellennr. < 99999)

#parzellennummer 0 wird gelöscht
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

#unbestockt soll einfach unbestock sein als Eintrag und nicht unbestockt_NA oder unbestockt_0

# Erstelle die Tabelle für 2013 und füge die gewünschten Spalten hinzu
jahr2013 <- jahr2013 %>%
  rename(Fläche_2013 = Fläche..m2, Rebsorte_2013 = Rebsorte) %>%  # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2013, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

# Erstelle die Tabelle für 2015 und füge die gewünschten Spalten hinzu
jahr2015 <- jahr2015 %>%
  rename(Fläche_2015 = Fläche..m2, Rebsorte_2015 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2015, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

#Für die restlichen Tabellen das gleiche: 
jahr2017 <- jahr2017 %>%
  rename(Fläche_2017 = Fläche..m2, Rebsorte_2017 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2017, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2018 <- jahr2018 %>%
  rename(Fläche_2018 = Fläche..m2, Rebsorte_2018 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2018, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr
  
jahr2019 <- jahr2019 %>%
  rename(Fläche_2019 = Fläche..m2, Rebsorte_2019 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2019, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2020 <- jahr2020 %>%
  rename(Fläche_2020 = Fläche..m2, Rebsorte_2020 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2020, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2021 <- jahr2021 %>%
  rename(Fläche_2021 = Fläche..m2, Rebsorte_2021 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2021, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2022 <- jahr2022 %>%
  rename(Fläche_2022 = Fläche..m2, Rebsorte_2022 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2022, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2023 <- jahr2023 %>%
  rename(Fläche_2023 = Fläche..m2, Rebsorte_2023 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2023, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2024 <- jahr2024 %>%
  rename(Fläche_2024 = Fläche..m2, Rebsorte_2024 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2024, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr

jahr2025 <- jahr2025 %>%
  rename(Fläche_2025 = Fläche..m2, Rebsorte_2025 = Rebsorte) %>% # Umbenennen der Fläche-Spalte und Rebsorte
  mutate(Sorte_Pflanzjahr = paste(Rebsorte_2025, Pflanz...jahr, sep = "_")) # Kombinieren von Rebsorte und Pflanzjahr


#unbestockt soll einfach unbestock sein als Eintrag und nicht unbestockt_NA oder unbestockt_0

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

#Ich will nun das Rod...jahr in den Tabellen sinvoll integrieren, dass es mit dem Merge kompatibel ist. 

# Werte in 'Sorte_Pflanzjahr' ändern, wenn ein Wert in 'Rod...jahr' vorhanden ist
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

#bevor alle datensätze zusammengefügt werden, testen wir mal
joinOne <- full_join(jahr2013, jahr2015, by = join_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr,
                                                      Sorte_Pflanzjahr))

# 3. Merging the files ##################################################################
#einige Zeilen unterscheiden sich nur in der Fläche, was im Probelauf
#zu many-to-many relationships geführt hat. Ich kombiniere deshalb gleiche Einträge
jahr2013 <- jahr2013 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2013, Weinmerkmal_2013) %>%
  summarise(Fläche_2013 = sum(Fläche_2013), .groups = "drop")
jahr2015 <- jahr2015 %>%
  group_by(Betrieb, Parzellennr., Sortennummer, Rebgemeinde, Pflanz...jahr, Sorte_Pflanzjahr,
           Rodungsjahr_2015, Weinmerkmal_2015) %>%
  summarise(Fläche_2015 = sum(Fläche_2015), .groups = "drop")
#nun funktioniert das mergen. Deshalb mache ich es für alle jahre
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

#macht man nun eine Probemerge von zwei Jahren, gibt es keine many to many relationships mehr
joinOne <- full_join(jahr2013, jahr2015, by = join_by(Betrieb, Parzellennr., Rebgemeinde, Pflanz...jahr,
                                                      Sorte_Pflanzjahr))

# Problem: Etwas viele NAs, woran liegt das? ######################
#viele NAs, weil die Betriebe unter anderem andere Gemeinden angegeben haben
#Oberstammheim vs. Stammheim etc., das zu beheben wäre aber sehr aufwändig

#Rebsorten dazufügen
joinOne$Rebsorte <- sub("_.*", "", joinOne$Sorte_Pflanzjahr)

length(which(is.na(joinOne$Fläche_2013))) #viele NAs bei Fläche, kann aber auch an unbestockt liegen
length(which(joinOne$Rebsorte=='unbestockt')) #unbestockt macht 293 der 400 Einträge aus

joinOne$unbestockt <- ifelse(joinOne$Rebsorte == "unbestockt", 1, 0)
head(joinOne$unbestockt)

#Mal unbestockt löschen, um andere Einträge mit NA bei Fläche besser sehen zu können
joinOhneUnbestockt <- joinOne[joinOne$unbestockt != 1, ]
View(joinOhneUnbestockt)

#die vielen NAs (ca. 10%) kommen meiner Ansicht nach von den Daten und 
#nicht vom falschen Mergen
#hier einige Beispiele
#Riesling Sylvaner 1982 gepflanzt in Regensberg auf Parzelle 779 gehört
#einmal zu Betrieb 2101 und einmal 1840
#Im Jahr 2013 gibt es auf der Parzelle 4557 Blauburgunder von 1985. 2015 ist es 
#dann zwei Mal Merlot, der neu gepflanzt wurde. Die Fläche Blauburgunder wird
#dann korrekterweise als NA angezeigt.

# Erstelle eine Liste der Datensätze
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

# Verwende Reduce, um alle Datensätze zu mergen
merged_data_all <- Reduce(function(x, y) merge(x, y, by = c("Betrieb", 
                                                            "Parzellennr.", 
                                                            "Sorte_Pflanzjahr", 
                                                            "Rebgemeinde",
                                                            "Pflanz...jahr"
                                                            ), all = TRUE), datensatz_liste)
                                                            
merged_data_all <- merged_data_all %>%
  distinct()

#Nun hat die Fläche und die Weinmerkmale allerdings sehr viele NAs. Fläche
#überprüfen
sum(merged_data_all$Fläche_2013, na.rm = TRUE)/10000
sum(merged_data_all$Fläche_2025, na.rm = TRUE)/10000
#zu wenig und müsste weniger werden über die Jahre

#direkt über die Jahre geht es
(sum(jahr2013$Fläche_2013[which(jahr2013$Weinmerkmal_2013 == "interspezifisch")], na.rm = TRUE) / 10000) / (sum(jahr2013$Fläche_2013, na.rm = TRUE)/10000)
(sum(jahr2025$Fläche_2025[which(jahr2025$Weinmerkmal_2025 == "interspezifisch")], na.rm = TRUE) / 10000) / (sum(jahr2025$Fläche_2025, na.rm = TRUE)/10000)

#Neue Zeile Hinzufügen die nur den Sortennamen enthählt.
merged_data_all$Rebsorte <- sub("_.*", "", merged_data_all$Sorte_Pflanzjahr)

#number of grape varieties over the years
jahr2013mitRebsorte <- jahr2013 
jahr2013mitRebsorte$Rebsorte <-  sub("_.*", "", jahr2013mitRebsorte$Sorte_Pflanzjahr)
length(unique(jahr2013mitRebsorte$Rebsorte))

jahr2025mitRebsorte <- jahr2025 
jahr2025mitRebsorte$Rebsorte <-  sub("_.*", "", jahr2025mitRebsorte$Sorte_Pflanzjahr)
length(unique(jahr2025mitRebsorte$Rebsorte))

#area of grapes compared to canton zurich
sum(jahr2025$Fläche_2025, na.rm = TRUE)/10000
(zürich$KANTONSFLA) #fläche von Zürich in Hektaren
#41 % des Kantons sind laut Agrarbericht 2023 landwirtschaftliche Nutzfläche
(sum(jahr2025$Fläche_2025, na.rm = TRUE)/10000) / (zürich$KANTONSFLA * 0.41)
(zürich$KANTONSFLA * 0.41)

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

#0, wenn noch nichts gepflanzt wurde
data_long$Fläche_test <- ifelse((data_long$year <= data_long$plantation_year) & is.na(data_long$Fläche), 0, data_long$Fläche)
data_long$group_id <- paste(data_long$Betrieb, data_long$Parzellennr., data_long$Sorte_Pflanzjahr, sep = "_")
data_long <- data_long %>%
  group_by(group_id) %>%
  mutate(drop_flag = max(Fläche_test))

data_long_unique <- data_long %>%
  distinct()

#dieser Code habe ich von Lucca übernommen, er reduziert die Fläche aber von
#602 ha auf 380, weshalb ich das dann nicht mehr ausgeführt habe
#data_long <- data_long %>%
# filter(!is.na(drop_flag))

# 5. Further clean up ##############################################################
sum(data_long$Fläche[data_long$year == 2025], na.rm = TRUE)/10000 


#delete entries with planting dates in the future
data_long <- data_long[data_long$plantation_year < 2026,]
merged_data_all <- merged_data_all[merged_data_all$Pflanz...jahr < 2026,]

# 6. Fläche Piwi vs. Gesamtfläche ##############################################
#wieso hat es nun weniger piwis als früher?

x <- sum(merged_data_all$Fläche_2025, na.rm = TRUE)/10000 
y <- sum(merged_data_all$Fläche_2025[which(merged_data_all$Weinmerkmal_2025 == "interspezifisch")], na.rm = TRUE)/10000 
x/y
y

x <- sum(merged_data_all$Fläche_2013, na.rm = TRUE)/10000 
y <- sum(merged_data_all$Fläche_2013[which(merged_data_all$Weinmerkmal_2013 == "interspezifisch")], na.rm = TRUE)/10000 
x/y

fläche2013gesamt <- sum(data_long$Fläche[which(data_long$year == 2013)], na.rm = TRUE)/10000
fläche2013piwi <-sum(data_long$Fläche[which((data_long$Weinmerkmal=='interspezifisch') & 
                                              (data_long$year == 2013))])/10000
fläche2013gesamt / fläche2013piwi

fläche2023gesamt <- sum(data_long$Fläche[which(data_long$year == 2023)], na.rm = TRUE)/10000
fläche2023piwi <- sum(data_long$Fläche[which((data_long$Weinmerkmal=='interspezifisch') &
                                               (data_long$year == 2023))])/10000
fläche2023gesamt / fläche2023piwi

#Ich denke, dass bei Piwis mehr Fehler in den Angaben der landwirte sind
nrow(jahr2013Original) - nrow(jahr2013) #total gelöschte Einträge
#davon piwis
anzahlpiwi2013Original <- which(jahr2013Original$Weinmerkmal_2013 == "interspezifisch")
anzahlpiwi2013Original <- length(anzahlpiwi2013Original)
anzahlpiwi2013Neu <- which(jahr2013$Weinmerkmal_2013 == "interspezifisch")
anzahlpiwi2013Neu <- length(anzahlpiwi2013Neu)
(anzahlpiwi2013Original - anzahlpiwi2013Neu) / ((nrow(jahr2013Original) - nrow(jahr2013)))
#fast 10 % aller gelöschten Einträge aufgrund fehlerhaften Eingaben der Landwirte
#sind Piwis im jahr 2013

nrow(jahr2023Original) - nrow(jahr2023) #total gelöschte Einträge
#davon piwis
anzahlpiwi2023Original <- which(jahr2023Original$Weinmerkmal_2023 == "interspezifisch")
anzahlpiwi2023Original <- length(anzahlpiwi2023Original)
anzahlpiwi2023Neu <- which(jahr2023$Weinmerkmal_2023 == "interspezifisch")
anzahlpiwi2023Neu <- length(anzahlpiwi2023Neu)
(anzahlpiwi2023Original - anzahlpiwi2023Neu) / ((nrow(jahr2023Original) - nrow(jahr2023)))
#hier sind es sogar 20 %, weshalb die Anzahl an Piwis zu sinken scheint.

# 7. Anwendungsbeispiele #######################################################
## 7.1 Area plot ----------------------------------------------------------------------------
yearly_acerage <- data_long %>%
  group_by(year) %>%
  summarise(yearly_acerage = sum(Fläche_test))

ggplot() +
  geom_line(data = yearly_acerage, aes(x = year, y = yearly_acerage)) +
  #ylim(0,50000000) +
  theme_minimal()

# Erste Zeilen des neuen DataFrames anzeigen
head(merged_data_all)


## 7.2 Export --------------------------------------------------------------------------------------------------------------------------------------------------
#export

library(openxlsx)

# Als Excel speichern
write.xlsx(merged_data_all, file = "merged_data_all.xlsx")


## 7.3 Betrieb Example---------------------------------------------------------
#Was kann man nun mit diesem Merge machen?

zh1 <- merged_data_all %>% filter(Betrieb == "ZH1")


print(zh1)


# Erforderliche Bibliotheken laden
library(ggplot2)
library(tidyr)
library(grid)

# Beispieldatensatz erstellen
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

# Datensatz ins lange Format bringen
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Fläche_m2"),
    names_to = "Jahr",
    values_to = "Fläche_m2"
  ) %>%
  mutate(Jahr = as.numeric(gsub("Fläche_m2_", "", Jahr)))  # Jahr aus Spaltennamen extrahieren

# Fehlende Werte entfernen
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
    legend.box = "horizontal"                  # Legende horizontal ausrichten
  ) +
  guides(fill = guide_legend(nrow = 2))        # Legende in 2 Zeilen umbrechen



## 7.4 Fläche pro Sorte über die Jahre----------------------------------------------
Blauburgunder <- merged_data_all %>% filter(Rebsorte == "Blauburgunder")


data_filtered <- Blauburgunder[,c("Fläche_2013", "Fläche_2015", "Fläche_2017", 
                                 "Fläche_2018", "Fläche_2019", "Fläche_2020", 
                                 "Fläche_2021", "Fläche_2022", "Fläche_2023")]

# Summiere die Werte für jede Spalte und teile durch 10.000 (m² -> ha)
summed_areas <- colSums(data_filtered, na.rm = TRUE) / 10000


# Daten in ein DataFrame für ggplot umwandeln
plot_data <- data.frame(
  Jahr = as.numeric(sub("Fläche_", "", names(summed_areas))),
  Fläche_m2 = summed_areas
)

# Säulendiagramm erstellen
ggplot(plot_data, aes(x = Jahr, y = Fläche_m2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Veränderung der Anbaufläche (Blauburgunder)",
       x = "Jahr",
       y = "Gesamte Anbaufläche (ha)")

nrow(data_long)