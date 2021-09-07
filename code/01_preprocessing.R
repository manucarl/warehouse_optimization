library(readxl)
library(tidyverse)
library(vctrs)
library(pryr)
library (dplyr)
library(lubridate)
library(chron)

# rm(list = ls())

# Batch-ID: Spalte D- "AUFTRAGSNR"
# Position im Batch: Spalte E- "AUFTRAGSPOS"
# Picker: Spalte J- "picker_id"
# Start des Pickvorgangs: BEGINN_ZEIT
# Ende des Pickvorgangs: ANFAHR_ZEIT
# Gewicht. GEWICHT
# Strecke wird berechnet über Pickplätze: Q_PLATZ


# 1. agility : modelled by the total number of lines
# 2 driving skill: modeled by the batch travel distance in a tour (m)
# 3 skill in pickking heavy items (strength): modeled by the total batch mass (kg)
# 4 skill in picking at low or high level, which may be influenced by picker height: modeled by the mean pick level of a batch
# 5 skill in picking large volume batches: modeled by the total batch volume (m^3)

# kw_list <- list.files("raw-data", pattern='o_fahr_pos', full.names=T)
# data_list <- lapply(kw_list[10:14], read_excel, na = "(null)")
# data <- bind_rows(data_list, .id="KW")
# save(data, file="processed-data/data_kw14_15.Rdata")



#------------------------------------------------------------------------------------------------------
# data import
#------------------------------------------------------------------------------------------------------

# "." are NAs


# article data
data_articles_raw <- read_xlsx("raw-data/(WFSSW SD01) Artikelstammdaten - Komplett.xlsx") 

data_articles <- data_articles_raw %>% dplyr::select('(A7)·NAN/·Artikel',
                                                     'ELVS·L511:· ·GEBA Länge',
                                                     'ELVS·L513:· ·GEBA Breite',
                                                     'ELVS·L512:· ·GEBA Höhe',
                                                     'ELVS·L514:· ·GEBA Gewicht',
                                                     ) %>% 
  set_names("article", "length", "width", "height", "mass") %>% 
  mutate_all( as.numeric) %>% 
  mutate(volume = length*width*height)





load("processed-data/data_kw11_12_13.Rdata")

# space between racks
rack_distance <- 10



# order picker data for week 11, 12, 13
# data <- read_excel("raw-data/o_fahr_pos KW11_1.xlsx", na = "(null)")
pick_data1 <- data %>% 
  filter(!is.na(ANFAHR_ZEIT)) %>% 
  # dplyr::select("LFDNR", "AUFTRAGSNR", "picker_id", "ARTIKELNR", "BEGINN_ZEIT", "ANFAHR_ZEIT", "GEWICHT_SOLL", "VOLUMEN", "Q_PLATZ") %>% 
  separate("Q_PLATZ", into = c("warehouse", "area", "rack", "place", "pick_level"), sep = c(3,5,7,11,13), convert= F, remove=F) %>% 
  separate("place", into = c("house", "line"), sep = c(2), convert= F, remove=F) %>% 
  
  mutate(AUFTRAGSNR = as.numeric(AUFTRAGSNR),
         ARTIKELNR = as.numeric(ARTIKELNR),
         BEGINN_ZEIT = chron::times(BEGINN_ZEIT),
         ANFAHR_ZEIT = chron::times(ANFAHR_ZEIT),
         # ENDE_ZEIT = hms(ENDE_ZEIT),
         MENGE_IST = `MENGE_IST/SUBSTR(ARTIKELNR,8,4)`,
         warehouse = as.numeric(warehouse)
  ) %>% 
  separate("ARTIKELNR", into = c("article"), sep = -4, convert = TRUE) 

pick_data1 %>% 
  dplyr::select(BEGINN_ZEIT, ANFAHR_ZEIT)

load("processed-data/data_kw14_15.Rdata")


# order picker data for week 14, 15

pick_data2 <- data %>% 
  filter(!is.na(ANFAHR_ZEIT)) %>% 
  # dplyr::select("LFDNR", "AUFTRAGSNR", "picker_id", "ARTIKELNR", "BEGINN_ZEIT", "ANFAHR_ZEIT", "GEWICHT_SOLL", "VOLUMEN", "Q_PLATZ") %>% 
  separate("Q_PLATZ", into = c("warehouse", "area", "rack", "place", "pick_level"), sep = c(3,5,7,11,13), convert= F) %>% 
  separate("place", into = c("house", "line"), sep = c(2), convert= F, remove=F) %>% 
  
  mutate(AUFTRAGSNR = as.numeric(AUFTRAGSNR),
         ARTIKELNR = as.numeric(ARTIKELNR),
         BEGINN_ZEIT = chron::times(BEGINN_ZEIT),
         ANFAHR_ZEIT = chron::times(ANFAHR_ZEIT),
         warehouse = as.numeric(warehouse)
  ) %>% 
  separate("ARTIKELNR", into = c("article"), sep = -4, convert = TRUE) 


# combine all weeks
pick_data <- bind_rows(pick_data1, pick_data2)


print(pick_data %>% dplyr::select(warehouse, area, rack, place, house, line), n =200)
dim(pick_data)


# merge by warehouse and article number
all_data <-  left_join(pick_data, data_articles, keep = F)

# save(all_data, file="processed-data/all_data.Rdata")

dim(all_data)
#[1] 3385131     110


#-------------------------------------------------------------------------------------
# data transformation - from order to batch level
#-------------------------------------------------------------------------------------


source("code/preprocessing_functions.R")

# pipeline going from raw orders to cleaned batch data:
batch_data_final <- all_data %>% gen_order_data %>% gen_batch_data %>% clean_batch_data %>% rescale_rename_data









