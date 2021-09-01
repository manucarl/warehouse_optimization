library(tidyverse)
library(nlme)
library(caret)
library(lme4)

#rm(list = ls())

set.seed(42) 

scale_this <- function(x) as.vector(scale(x))

load("processed-data/all_data_prep.Rdata")
load("processed-data/batch_data_clean.Rdata")



load("models/full_model_all_log.RData")


batch_data <- batch_data_clean %>% 
  mutate(total_volume = total_volume * 10e-10,
         travel_dist_km = travel_dist_meter * 10e-4) %>% 
  rename(batch_id = AUFTRAGSNR,
         batch_time = batch_time_secs,
         nlines = no_of_lines,
         distance = travel_dist_km,
         plevel = mean_pick_level,
         volume = total_volume,
         mass = total_mass_kg) %>% 
  mutate(log_batch_time = log(batch_time),
         log_nlines = log(nlines),
         log_distance = log(distance),
         log_plevel = log(plevel),
         log_volume = log(volume),
         log_mass = log(mass)
  ) %>% 
  dplyr::select(batch_id, picker_id, batch_time:mass, -travel_dist_meter, distance, log_batch_time:log_mass)


base_model <- lmer(log_batch_time_secs ~ 1 +
                     (1 |picker_id),
                   data = batch_data_clean,
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5))
                   
)
ranova(base_model)


full_model <- lmer(log_batch_time ~ 1+ log_nlines + log_plevel + log_volume + log_mass + log_distance + 
                     (1+ log_nlines + log_plevel + log_volume + log_mass + log_distance|picker_id),
                   data = batch_data,
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5))
                   
)


AIC(gen_full_model)
diagnostic_plot <- sjPlot::plot_model(full_model, type = "diag")

diagnostic_plot[[2]]
diagnostic_plot[[3]]
diagnostic_plot[[4]]

sjPlot::plot_model(full_model, type = "pred", show.data = T)

model <- full_model
data <- batch_data

library(merTools)

# predictInterval(model)   # for various model predictions, possibly with new data

# reEx <- REsim(model)

# plotREsim(reEx)
# plotFEsim(model)
# df  %>% mutate_if(is.factor, function(.) as.numeric(as.character(.)))
real_times <- data$batch_time
pred_times <- (predict(model, newdata=data)%>% exp) 

sum(real_times) /
  sum(pred_times)

mse <- mean((real_times- pred_times)^2)
rmse <- sqrt(mse)
# matusiak et al: real batch execution times for the original batching are 1% smaller than the forecast in this case

hist_dat <- bind_rows(true = real_times , predicted = pred_times, .id=name) %>% pivot_longer(everything())
hist_dat %>% ggplot() + geom_histogram(aes(x=value), binwidth =100) + facet_grid(~name)

hist_dat %>% ggplot() + geom_violin(aes(x=name, y=value)) 


real_times %>%  summary
unname(pred_times) %>% summary


## Calculate picker productivity 

# total batch execution time and productivity of each picker
picker_productivity <- data %>% 
  group_by(picker_id) %>% 
  summarise(productivity = n()/sum(batch_time)) %>% 
  arrange(desc(productivity))

picker_productivity2 <- data %>% 
  mutate(pred = predict(model, batch_data)) %>% 
  group_by(picker_id) %>% 
  summarise(lines = n(),
            total_time = sum(pred),
            productivity = lines/total_time) %>% 
  arrange(desc(productivity))


# worked_batches <- all_data_needed %>% 
#   group_by(picker_id) %>% 
#   summarise(worked_batches = n_distinct(AUFTRAGSNR))
# 
# picker_productivity <- left_join(picker_times, worked_batches) %>% 
#   mutate(productivity = worked_batches/(total_batch_time/3600),
#          picker_id = as.factor(picker_id)) %>% 
#   arrange(desc(productivity))



# number of partitions/ virtual days
n_part <- 12
# construct virtual days
partitions <- caret::createFolds(data$batch_id, k = n_part)

sapply(partitions, length)

# picker qualifies for inclusion in the workforce of the virtual day if both 
# 1)the sum of real execution times of the batches he or she performed and
# 2)the sum of the forecast batch execution times 
#exceed the minimum threshold

M_min <- 7*3600#7.40
M_max <- 9.5*3600

# M_min <- 7.4*3600
# M_max <- 7.75*3600
d <- 2

# get eligible pickers for each day
picker_days <- sapply(1:n_part, function(d) {
  
  day <- data %>% 
    slice(partitions[[d]]) # filter picks of each day
  
  
  pred <- predict(model, day)
  
  # resids <- day$log_batch_time - pred
  
  # pred_corr <- exp(pred)* mean(exp(resids))
  
  day <- tibble(day, pred= pred %>% exp)
  
  day_agg <- day %>% 
    group_by(picker_id) %>% 
    summarise(total_secs = sum(batch_time), 
              total_pred_secs = sum(pred))# %>%
  
  day_keep <- day_agg %>% filter(total_secs > M_min, total_secs < M_max,
                                 total_pred_secs > M_min, total_pred_secs < M_max)
  
  # left_join(day_keep, picker_productivity) %>% 
  #   arrange(desc(productivity))
  day_keep$picker_id
  
  
})


smearing_pred <- function(model, data) {
  pred <- predict(model, data)
  resids <- pred - data$log_batch_time_secs
  
  exp(pred)*mean(exp(resids))
}








picker_days

options(scipen=999)


# fastest first assumes that there are as many pickers available as needed

res_ff <-  vector("list", length=n_part)
for(day in seq_along(partitions)){
  
  
  pickers_of_the_day <- picker_days[[day]]
  pickers_of_the_day_sorted <- left_join(tibble(picker_id =pickers_of_the_day), picker_productivity2) %>%
    arrange(desc(productivity))
  
  batch_data_day <- data %>% 
    slice(partitions[[day]]) %>% 
    filter(picker_id %in% pickers_of_the_day) %>% 
    arrange(desc(nlines))
  
  
  
  # picker <- picker_productivity$picker_id[p] # fastest available picker
  
  
  
  p <- 1 # start with first picker (fastest picker)
  # i <- 1
  ind <-1 # start with first row (batch with largest no of lines)
  
  sum_batches <- 0
  preds <- vector("list", length(pickers_of_the_day))
  
  # loop over pickers from fastest to slowest      
  for(p in seq_along(pickers_of_the_day)){
    pred_time_accumulated <- pred_time_accumulated_ff <- 0
    
    # 
    while(pred_time_accumulated_ff < (M_max) && ind <= nrow(batch_data_day)){
      # p <- 1
      picker <-  as.numeric(as.character(pickers_of_the_day_sorted[p,1] %>%  unlist)) # fastest available picker
      
      # picker <-  pickers_of_the_day_sorted[p,1] %>%  unlist # fastest available picker
      
      # pred_time_ff <- smearing_pred(mmodel, batch_data_day[ind,] %>%  mutate(picker_id = picker))
      # pred_time <- smearing_pred(mmodel, batch_data_day[ind,])
      
      pred_time_ff <- predict(model, batch_data_day[ind,] %>%  mutate(picker_id = picker)) %>% exp
      # pred_time <- predict(mmodel, batch_data_day[ind,]) %>%  exp
      # 
      # print(c(pred_time=pred_time, pred_time_ff = pred_time_ff, true = batch_data_day[ind,]$batch_time_secs))
      pred_time_accumulated_ff <- pred_time_accumulated_ff + pred_time_ff
      # pred_time_accumulated <- pred_time_accumulated + pred_time
      
      sum_batches <- sum_batches + 1
      ind <- ind +1
      # print(pred_time_accumulated)
      
      
      
    }
    
    preds[[p]] <- c(time_acc_ff=pred_time_accumulated_ff, picker_id=picker, sum_batches = sum_batches)
    
    sum_batches <- 0
    
  }
  res_ff[[day]] <-   do.call(rbind, preds) #%>% colSums
  
  
}


ratio_ff <- do.call(rbind, res_ff) %>% colSums 





res <-  vector("list", length=n_part)
for(day in seq_along(partitions)){
  
  
  pickers_of_the_day <- picker_days[[day]]
  pickers_of_the_day_sorted <- left_join(tibble(picker_id =pickers_of_the_day), picker_productivity2) %>%
    arrange(desc(productivity))
  
  batch_data_day <- data %>% 
    slice(partitions[[day]]) %>% 
    filter(picker_id %in% pickers_of_the_day) #%>% 
  #arrange(desc(no_of_lines))
  
  
  
  # picker <- picker_productivity$picker_id[p] # fastest available picker
  
  
  
  p <- 1 # start with first picker (fastest picker)
  # i <- 1
  ind <-1 # start with first row (batch with largest no of lines)
  
  sum_batches <- 0
  preds <- vector("list", length(pickers_of_the_day))
  
  # loop over pickers from fastest to slowest      
  for(p in seq_along(pickers_of_the_day)){
    pred_time_accumulated <- 0
    
    # 
    while(pred_time_accumulated < (M_max) && ind <= nrow(batch_data_day)){
      # p <- 1
      # picker <-  pickers_of_the_day_sorted[p,1] %>%  unlist# fastest available picker
      
      
      # pred_time_ff <- smearing_pred(mmodel, batch_data_day[ind,] %>%  mutate(picker_id = picker))
      pred_time <- predict(model, batch_data_day[ind,]) %>%  exp
      
      # pred_time_ff <- predict(mmodel, batch_data_day[ind,] %>%  mutate(picker_id = picker)) %>% exp
      # pred_time <- predict(mmodel, batch_data_day[ind,]) %>%  exp
      # 
      # print(c(pred_time=pred_time, pred_time_ff = pred_time_ff, true = batch_data_day[ind,]$batch_time_secs))
      # pred_time_accumulated_ff <- pred_time_accumulated_ff + pred_time_ff
      pred_time_accumulated <- pred_time_accumulated + pred_time
      
      sum_batches <- sum_batches + 1
      ind <- ind +1
      # print(pred_time_accumulated)
      
      
      
    }
    
    preds[[p]] <- c(time_acc = pred_time_accumulated, picker_id=picker, sum_batches = sum_batches)
    
    pred_time_accumulated <- 0
    sum_batches <- 0
    
  }
  res[[day]] <-   do.call(rbind, preds) #%>% colSums
  
  
}      



ratio <- do.call(rbind, res) %>% colSums 

ratio_ff[1]/ratio[1]

