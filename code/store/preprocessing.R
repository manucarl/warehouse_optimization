library(readxl)
library(tidyverse)
library(vctrs)
library(pryr)
library (dplyr)

# data <- as_tibble(data)

rm(list = ls())
data_articles01 <- read_xlsx("raw-data/Article number L,B,H_warehouse01.xlsx")
data_articles02 <- read_xlsx("raw-data/Article number L,B,H_warehouse02.xlsx")

data_articles <- bind_rows(data_articles01, data_articles02) %>% 
  rename_with( str_trim) %>% 
  # select(Lager, Artikel, Volumen, Länge, Breite, Höhe, Gang, Platz) %>% 
  rename(warehouse = Lager,
         ARTIKELNR = Artikel) %>% 
  mutate(article = as.numeric(ARTIKELNR),
         volume = Länge*Breite*Höhe/  10e8) %>% 
  select(-c(Volumen)) %>% 
  distinct()



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

library(lubridate)
# kw_list <- list.files("raw-data", pattern='o_fahr_pos', full.names=T)
# data_list <- lapply(kw_list[10:14], read_excel, na = "(null)")
# data <- bind_rows(data_list, .id="KW")
# save(data, file="processed-data/data_kw14_15.Rdata")
# load("processed-data/data_final.Rdata")
load("processed-data/data_kw11_12_13.Rdata")

library(pryr)
library(chron)
# data <- read_excel("raw-data/o_fahr_pos KW11_1.xlsx", na = "(null)")
# object_size(data)
pick_data1 <- data %>% 
  filter(!is.na(ANFAHR_ZEIT)) %>% 
  # select("LFDNR", "AUFTRAGSNR", "picker_id", "ARTIKELNR", "BEGINN_ZEIT", "ANFAHR_ZEIT", "GEWICHT_SOLL", "VOLUMEN", "Q_PLATZ") %>% 
  separate("Q_PLATZ", into = c("warehouse", "area", "rack", "place", "pick_level"), sep = c(3,5,7,11,13), convert= F) %>% 
  separate("place", into = c("house", "line"), sep = c(2), convert= F, remove=F) %>% 
  
  mutate(AUFTRAGSNR = as.numeric(AUFTRAGSNR),
         ARTIKELNR = as.numeric(ARTIKELNR),
         BEGINN_ZEIT = times(BEGINN_ZEIT),
         ANFAHR_ZEIT = times(ANFAHR_ZEIT),
         # ENDE_ZEIT = hms(ENDE_ZEIT),
         MENGE_IST = `MENGE_IST/SUBSTR(ARTIKELNR,8,4)`,
         warehouse = as.numeric(warehouse)
  ) %>% 
  separate("ARTIKELNR", into = c("article"), sep = -4, convert = TRUE) 


load("processed-data/data_kw14_15.Rdata")

pick_data2 <- data %>% 
  filter(!is.na(ANFAHR_ZEIT)) %>% 
  # select("LFDNR", "AUFTRAGSNR", "picker_id", "ARTIKELNR", "BEGINN_ZEIT", "ANFAHR_ZEIT", "GEWICHT_SOLL", "VOLUMEN", "Q_PLATZ") %>% 
  separate("Q_PLATZ", into = c("warehouse", "area", "rack", "place", "pick_level"), sep = c(3,5,7,11,13), convert= F) %>% 
  separate("place", into = c("house", "line"), sep = c(2), convert= F, remove=F) %>% 
  
  mutate(AUFTRAGSNR = as.numeric(AUFTRAGSNR),
         ARTIKELNR = as.numeric(ARTIKELNR),
         BEGINN_ZEIT = times(BEGINN_ZEIT),
         ANFAHR_ZEIT = times(ANFAHR_ZEIT),
         warehouse = as.numeric(warehouse)
  ) %>% 
  separate("ARTIKELNR", into = c("article"), sep = -4, convert = TRUE) 



pick_data <- bind_rows(pick_data1, pick_data2)



print(pick_data %>% select(warehouse, area, rack, place, house, line), n =200)
dim(pick_data)
#[1] 3085279      93

# merge by warehouse and article number
all_data <-  left_join(pick_data, data_articles, keep = F)

dim(all_data)
#[1] 3385131     110

# extract and generate variables we need
all_data_needed <- all_data %>% 
  dplyr::select(AUFTRAGSNR,MDENR, ANFAHR_ZEIT, BEGINN_ZEIT, GEWICHT_SOLL, MENGE_IST, volume, pick_level, area, rack, place, house, line) %>%
  drop_na(volume) %>% 
  group_by(rack) %>%
  rename(picker_id = MDENR) %>% 
  mutate(
    travel_dist_house = 1.4*abs(as.numeric(house) - lag(as.numeric(house), default = NA)),
    ) %>% 
  ungroup %>% 
  mutate(travel_dist_rack = (10)*abs(as.numeric(rack) - lag(as.numeric(rack), default = NA))) # every rack reached hast to be traveled back and forth


all_data_needed %>% dplyr::select(travel_dist_house, travel_dist_rack)
all_data_needed %>% dplyr::select(ANFAHR_ZEIT, BEGINN_ZEIT) %>% summary
summary(all_data_needed)




# summarise data in batches
batch_data <- all_data_needed %>% 
  mutate(pick_end = period_to_seconds(hms(ANFAHR_ZEIT)),
         pick_start = period_to_seconds(hms(BEGINN_ZEIT))) %>% 
  group_by(picker_id, AUFTRAGSNR) %>%  
  # group_by(picker_id, AUFTRAGSNR ) %>% 
  mutate(true_start = ifelse(MENGE_IST != 0, min(pick_start, na.rm=TRUE), Inf)) %>% # set BEGINN_ZEIT to Inf instead of start of shift effectively excluding it
  summarise(
    batch_time_secs = max(pick_end)- min(true_start, na.rm=TRUE),
    batch_time_mins = batch_time_secs/60,
    no_of_lines = length(unique(as.numeric(place))),
    # travel_dist_meter = sum(travel_dist_house+travel_dist_rack),
    travel_dist_meter = sum(travel_dist_rack)+ sum(travel_dist_house),
    
    mean_pick_level = mean(as.numeric(pick_level)),
    total_volume = sum(volume),
    total_mass_kg = sum(GEWICHT_SOLL)#/1000
  )%>% 
  drop_na

length(unique(batch_data$picker_id))
# 179 
nrow(batch_data)
# [1] 43114

# histogram with
p_hist_all <- batch_data  %>%
  ungroup%>%dplyr::select(-picker_id) %>%
  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) + facet_wrap(~name, scales="free")
p_hist_all

# thoughtful
batch_data_clean <- batch_data %>% 
  drop_na %>% 
  filter(
    batch_time_secs >300, 
    batch_time_secs <3600,
    travel_dist_meter > 117.6, # delete too short distance (naive calculation: one rack has 42 places when you pass both directions => 42*1.4 * 2 =  117.6m)
    mean_pick_level > 0, # not clear what pick_level = 0 is
    total_mass_kg <5000, 
    total_volume < 20
  ) %>% 
  group_by(picker_id) %>% 
  filter(n() > 75) %>% # matusiak et al. use only pickers who worked at least 75 batches (for cv)
  ungroup


length(unique(batch_data_clean$picker_id))
# 125
nrow(batch_data_clean)
# [1] 24814
# matusiak et al's clean dataset consists of 24,669 out of 37,841 batches worked by 99 out of 229 pickers 


# further considerations:
# i) only multiple orders in a batch (pick tour)
# 
summary(batch_data_clean)
batch_data_clean  %>%
  ungroup%>%
  dplyr::select(-picker_id) %>%
  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) +
  facet_wrap(~name, scales="free")


summary(batch_data)

library(corrplot)
batch_data_clean %>% ungroup %>% 
  # sample_n(2000) %>% 
  dplyr::select(-c(picker_id, AUFTRAGSNR)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot::corrplot(method="number", type="upper")

# ggsave(filename="figures/vars_hist.png")


# library(corrplot)
# corrplot(cor(batch_data %>% select(-c(AUFTRAGSNR, picker_id))))
# scale predictors

library(caret)
library(lme4)
library(nlme)
# batch_clean <- batch_data %>% 
# filter(total_mass_kg < 6)
# preprocess_values <- preProcess(batch_data %>% select(mean_pick_level, total_volume, total_mass_kg), method = c("center", "scale"))
# batch_train <- predict(preprocess_values, batch_data)

# log transform
batch_data_clean$log_batch_time_secs <- log(batch_data_clean$batch_time_secs)
batch_data_clean$log_batch_time_mins <- log(batch_data_clean$batch_time_mins)

batch_data_clean$log_no_of_lines <- log(batch_data_clean$no_of_lines)
batch_data_clean$log_mean_pick_level <- log(batch_data_clean$mean_pick_level)
batch_data_clean$log_total_mass_kg <- log(batch_data_clean$total_mass_kg)
batch_data_clean$log_travel_dist_meter <- log(batch_data_clean$travel_dist_meter)

batch_data_clean$log_total_volume <- log(batch_data_clean$total_volume)



batch_data_clean$picker_id <- as.factor(batch_data_clean$picker_id)


batch_data_clean$no_of_lines_scaled <- scale(batch_data_clean$no_of_lines)
batch_data_clean$mean_pick_level_scaled <- scale(batch_data_clean$mean_pick_level)
batch_data_clean$total_mass_kg_scaled <- scale(batch_data_clean$total_mass_kg)
batch_data_clean$travel_dist__meter_scaled <- scale(batch_data_clean$travel_dist_meter)
batch_data_clean$total_volume_scaled <- scale(batch_data_clean$total_volume)



# batch_data_clean  %>% ungroup%>%
#   dplyr::select(AUFTRAGSNR,no_of_lines_scaled,mean_pick_level_scaled, total_mass_scaled, travel_dist_scaled,total_volume_scaled ) %>%  pivot_longer(cols=-AUFTRAGSNR) %>% 
#   ggplot(aes(x=value))  + geom_histogram(bins=100)+facet_wrap(~name, scales="free")
# 


batch_data_clean %>% dplyr::select(log_no_of_lines:log_total_volume) %>%  ungroup %>% 
  # sample_n(2000) %>% 
  # dplyr::select(-c(picker_id, AUFTRAGSNR)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot(method="number", type="upper")
# batch_data_clean$total_mass_scaled <- scale(batch_data_clean$total_mass_kg)
# batch_data_clean$total_volume_scaled <- scale(batch_data_clean$total_volume)
# batch_data_clean$mean_pick_level_scaled <- scale(batch_data_clean$mean_pick_level)



ggplot(batch_data_clean, aes(x=mean_pick_level, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_mean_pick_level, y=log_batch_time_secs)) + geom_point( alpha=0.1)


ggplot(batch_data_clean, aes(x=total_volume, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_total_volume, y=log_batch_time_secs)) + geom_point( alpha=0.1, aes(col=picker_id))

ggplot(batch_data_clean, aes(x=total_mass_kg, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_total_mass, y=log_batch_time_secs)) + geom_point( alpha=0.1, aes(col=picker_id))

ggplot(batch_data_clean, aes(x=no_of_lines, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_no_of_lines, y=log_batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=no_of_lines_scaled, y=batch_time_secs)) + geom_point( alpha=0.1)
