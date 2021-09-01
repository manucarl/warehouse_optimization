library(readxl)
library(tidyverse)
library(vctrs)
library(pryr)
library (dplyr)

# data <- as_tibble(data)

rm(list = ls())
data_articles01 <- read_xlsx("raw-data/Article number L,B,H_warehouse01.xlsx", col_types= "numeric")
data_articles02 <- read_xlsx("raw-data/Article number L,B,H_warehouse02.xlsx", col_types= "numeric")

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
# Picker: Spalte J- "MDENR"
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
# data_list <- lapply(kw_list, read_excel, na = "(null)")
# data <- bind_rows(data_list, .id="KW")
# save(data, file="processed-data/data_allxlsx.Rdata")
load("processed-data/data_allxlsx.Rdata")

library(pryr)
library(chron)
# data <- read_excel("raw-data/o_fahr_pos KW11_1.xlsx", na = "(null)")
# object_size(data)
pick_data <- data %>% 
  filter(!is.na(ANFAHR_ZEIT)) %>% 
  # select("LFDNR", "AUFTRAGSNR", "MDENR", "ARTIKELNR", "BEGINN_ZEIT", "ANFAHR_ZEIT", "GEWICHT_SOLL", "VOLUMEN", "Q_PLATZ") %>% 
  separate("Q_PLATZ", into = c("warehouse", "area", "rack", "place", "pick_level"), sep = c(3,5,7,11,13), convert= TRUE) %>% 
  
  mutate(AUFTRAGSNR = as.numeric(AUFTRAGSNR),
         ARTIKELNR = as.numeric(ARTIKELNR),
         BEGINN_ZEIT = times(BEGINN_ZEIT),
         ANFAHR_ZEIT = times(ANFAHR_ZEIT),
         # ENDE_ZEIT = hms(ENDE_ZEIT),
         place = as.numeric(place)
         ) %>% 
  separate("ARTIKELNR", into = c("article"), sep = -4, convert = TRUE) 

print(pick_data ,n =500)
# pick_data %>% filter(GEWICHT_SOLL ==max(GEWICHT_SOLL))

# filter out ovious breaks, exceptionally fast travel speeds
pick_data$place_short <- pick_data$place
str_sub(pick_data$place_short, -2, -2) <- ""
pick_data$place_short <- as.numeric(pick_data$place_short)


# data2 <- pick_data %>% 
#   group_by(rack) %>%
#   mutate(travel_dist_place = 1.4*abs(place_short - lag(place_short, default = NA)),
#          travel_dist_place = cumsum(replace_na(travel_dist_place, 0))) %>% 
#   ungroup %>% 
#   group_by(AUFTRAGSNR, MDENR) %>% 
#   mutate(travel_dist_rack = 10*abs(rack - lag(rack, default = NA)),
#          travel_dist_rack = cumsum(replace_na(travel_dist_rack, 0))) %>% 
#   select(-c(3,4,5,6))

# print(data2, n=150)


# pick_data$place_short <- substr(data$place, 1, nchar())
  # left_join(data_articles,by = c("warehouse", "article"))



all_data <-  left_join(pick_data, data_articles, keep = F)

dim(pick_data)
dim(all_data)
# ~5000 duplicates 
all_data %>%  select(warehouse, rack, place, place_short)

batch_data <- all_data %>% 
  dplyr::select(AUFTRAGSNR,MDENR, ANFAHR_ZEIT, BEGINN_ZEIT, GEWICHT_SOLL,`MENGE_IST/SUBSTR(ARTIKELNR,8,4)`, volume, pick_level, area, rack, place_short) %>%
  drop_na(volume) %>% 
  group_by(rack) %>%
  mutate(travel_dist_place = 1.4*abs(place_short - lag(place_short, default = NA)),
         MENGE_IST = `MENGE_IST/SUBSTR(ARTIKELNR,8,4)`) %>% 
ungroup %>% 
  mutate(travel_dist_rack = 10*abs(rack - lag(rack, default = NA)))
# 
# batch_data %>% 
#   mutate(pick_end = period_to_seconds(hms(ANFAHR_ZEIT)),
#          pick_start = period_to_seconds(hms(BEGINN_ZEIT))
#          ) %>% select(pick_end, pick_start)

# batch_data %>% 
#   group_by(MDENR, AUFTRAGSNR) %>% 
#   summarise(true_start = ifelse(MENGE_IST != 0, min(BEGINN_ZEIT), 999))
# 
# batch_data %>% 
# summarise(estimate = ifelse(MENGE_IST != 0, min(BEGINN_ZEIT), 999))

batch_data <- batch_data %>% 
  mutate(pick_end = period_to_seconds(hms(ANFAHR_ZEIT)),
         pick_start = period_to_seconds(hms(BEGINN_ZEIT))) %>% 

      group_by(MDENR, AUFTRAGSNR) %>% 
        mutate(true_start = ifelse(MENGE_IST != 0, min(pick_start), Inf)) %>% # set BEGINN_ZEIT to Inf instead of start of shift effectively excluding it
      summarise(
            batch_time_secs = max(pick_end)- min(true_start),
            # batch_time = sum(pick_time),
            travel_dist = sum(travel_dist_place+travel_dist_rack),
            mean_pick_level = mean(pick_level),
            no_of_lines = length(unique(place_short)),
            total_volume = sum(volume),
            total_mass = sum(GEWICHT_SOLL)
            )%>% 
  drop_na


# thoughtful 
batch_data <- batch_data %>% 
  drop_na %>% 
  filter(
    batch_time_secs >300,
    # batch_time <3000,
    
    mean_pick_level > 0,
    total_mass <5000,
    total_volume < 20,
    n() > 75) # matusiak et al. use at least 75 batches per worker


library(caret)
set.seed(123)
folds <- groupKFold(subjects, k = 15) 


summary(batch_data)
batch_data  %>% ungroup%>%dplyr::select(-MDENR) %>%  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100)+facet_wrap(~name, scales="free")


batch_data %>%  filter(no_of_lines == 1)
summary(batch_data)





# ggplot(gg_dat) + geom_boxplot(aes(x=1, y=value))+ facet_wrap(~name, scale="free")


library(corrplot)
batch_data %>% ungroup %>% 
  # sample_n(2000) %>% 
  dplyr::select(-c(MDENR, AUFTRAGSNR)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot(method="number", type="upper")

# ggsave(filename="figures/vars_hist.png")


# library(corrplot)
# corrplot(cor(batch_data %>% select(-c(AUFTRAGSNR, MDENR))))
# scale predictors

library(caret)
# library(lme4)
library(nlme)
# batch_clean <- batch_data %>% 
  # filter(total_mass < 6)
# preprocess_values <- preProcess(batch_data %>% select(mean_pick_level, total_volume, total_mass), method = c("center", "scale"))
# batch_train <- predict(preprocess_values, batch_data)

# log transform
batch_data$log_batch_time_secs <- log(batch_data$batch_time_secs)

batch_data$log_no_of_lines <- log(batch_data$no_of_lines)
batch_data$log_mean_pick_level <- log(batch_data$mean_pick_level)
batch_data$log_total_mass <- log(batch_data$total_mass)
batch_data$log_travel_dist <- log(batch_data$travel_dist)

batch_data$log_total_volume <- log(batch_data$total_volume)
batch_data$MDENR <- as.factor(batch_data$MDENR)

# batch_data$total_mass_scaled <- scale(batch_data$total_mass)
# batch_data$total_volume_scaled <- scale(batch_data$total_volume)
# batch_data$mean_pick_level_scaled <- scale(batch_data$mean_pick_level)



ggplot(batch_data, aes(x=mean_pick_level, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data, aes(x=log_mean_pick_level, y=log_batch_time_secs)) + geom_point( alpha=0.1)


ggplot(batch_data, aes(x=total_volume, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data, aes(x=log_total_volume, y=log_batch_time_secs)) + geom_point( alpha=0.1)

ggplot(batch_data, aes(x=total_mass, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data, aes(x=log_total_mass, y=log_batch_time_secs)) + geom_point( alpha=0.1)

ggplot(batch_data, aes(x=no_of_lines, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data, aes(x=log_no_of_lines, y=log_batch_time_secs)) + geom_point( alpha=0.1)

# ggplot(batch_data, aes(x=log_total_mass, y=log_batch_time, colour=MDENR)) + geom_point()
# ggplot(batch_data, aes(x=total_volume, y=batch_time, colour=MDENR)) + geom_point()
# 
# ggplot(batch_data, aes(x=mean_pick_level_scaled, y=batch_time, colour=MDENR)) + geom_point()
# ggplot(batch_data, aes(x=total_volume_scaled, y=batch_time, colour=MDENR)) + geom_point()

m0 <- lm(batch_time_secs ~  mean_pick_level + total_mass + total_volume + no_of_lines + travel_dist, data=batch_data)
AIC(m0)
# m0_lmer <- lmer(batch_time_secs ~  mean_pick_level + total_mass + total_volume + (1|MDENR), data=batch_data)
# AIC(m0_lmer)
lm_model <- lme(fixed = batch_time_secs ~ 1 + mean_pick_level + total_mass + total_volume + no_of_lines,
          data = batch_data,
          random =~ 1|MDENR)
AIC(lm_model)

# 
# m1_lmer <- lmer(log_batch_time_secs ~  mean_pick_level + total_mass + total_volume + (1+ mean_pick_level + total_mass + total_volume|MDENR), data=batch_data)
# AIC(m1_lmer)
############# Null model
null_model <- lme(fixed = batch_time_secs ~ 1 + mean_pick_level + total_mass + total_volume+ travel_dist,
              data = batch_data,
              random =~ 1+ mean_pick_level + total_mass + total_volume + travel_dist|MDENR)
AIC(null_model)


m1 <- lme(fixed = log_batch_time_secs ~ 1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines,
    data = as.data.frame(batch_data),
    random =~ 1+ log_mean_pick_level + log_total_mass + log_total_volume+ log_no_of_lines|MDENR)
AIC(m1)
summary(m1)
library(MASS)
library(RcmdrMisc)
stepwise(m1, direction="forward/backward")


AIC(m1)

library(buildmer)
model <- buildlme(Reaction ~ Days + (Days|Subject),data=lme4::sleepstudy)

stepm1 <- buildlme(batch_time_secs ~ 1 + log_mean_pick_level + log_total_mass + log_total_volume +
                     (1+ log_mean_pick_level + log_total_mass + log_total_volume|MDENR), data=batch_data)
         

AIC(m2)

m3<- lmer(log_batch_time ~ log_mean_pick_level  + log_total_mass + total_volume +
            (1+log_mean_pick_level +log_total_mass+ total_volume  | MDENR), data=batch_data)



# library(rstanarm)


# M1_stanlmer <- stan_lmer(log_batch_time ~  mean_pick_level + total_mass + total_volume + (1+ mean_pick_level + total_mass + total_volume|MDENR), data=batch_data)




AIC(m3)

gg_dat <- batch_data %>% 
  select(AUFTRAGSNR, log_batch_time, log_mean_pick_level, log_total_volume, log_total_mass) %>% 
  pivot_longer(cols=-AUFTRAGSNR)

ggplot(gg_dat) + geom_histogram(aes(value), bins=100) + facet_wrap(~name, scales="free")
# ggplot(gg_dat) + geom_boxplot(aes(x=1, y=value))+ facet_wrap(~name, scale="free")

gg_dat <- batch_data %>% 
  select(AUFTRAGSNR, batch_time, mean_pick_level, total_volume, total_mass, log_mean_pick_level, log_total_volume, log_total_mass) %>% 
  pivot_longer(cols=-c(AUFTRAGSNR, batch_time))

ggplot(gg_dat) + geom_point(aes(y=value, x=batch_time)) + facet_wrap(~name, scales="free")


gg_dat <- batch_data %>% 
  select(AUFTRAGSNR, log_batch_time, log_mean_pick_level, log_total_volume, log_total_mass) %>% 
  pivot_longer(cols=-c(AUFTRAGSNR, log_batch_time))

ggplot(gg_dat) + geom_point(aes(y=value, x=log_batch_time)) + facet_wrap(~name, scales="free")

# divide data 80/20 for cv


gg_dat <- batch_data %>% 
  
  select(AUFTRAGSNR,batch_time, mean_pick_level, total_volume, total_mass) %>% 
  pivot_longer(cols=-AUFTRAGSNR)

ggplot(gg_dat) + geom_histogram(aes(value), bins=100) + facet_wrap(~name, scales="free")


print(batch_data, n= 200)
