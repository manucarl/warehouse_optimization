library(tidyverse)


remotes::install_github("https://github.com/kavetinaveen/CWVRP")



#-------------------
# CWRP example
#-------------------

library(CWVRP)
knitr::kable(head(An32k5demand))
knitr::kable(head(An32k5locations))


g <- ggplot(An32k5locations, aes(x = X, y = Y))

g + annotate("text", x = An32k5locations[, 2], y = An32k5locations[, 3], label = An32k5locations[, 1])

DMat <- DistMat(An32k5locations)
row.names(DMat) <- NULL
knitr::kable(DMat[1:5, 1:5])


DMat <- DistMat(An32k5locations)
SMat <- SavingMat(DMat)
knitr::kable(SMat[1:5, 1:5])


DMat <- DistMat(An32k5locations)
Sort_Edges <- Sorted_Edges(DMat)
row.names(Sort_Edges) <- NULL
knitr::kable(head(Sort_Edges))


Greedy_routes <- CW_VRP(demand = An32k5demand, locations = An32k5locations, Vehicle_Capacity = 10, method = "manhattan")

Greedy_routes <- CW_VRP(demand = An32k5demand, DMat = DMat, Vehicle_Capacity = 100)

#-----------------------------
# For us
#-----------------------------

load("processed-data/all_data_prep.Rdata")

# number of partitions/ virtual days
n_part <- 12
# construct virtual days
partitions <- caret::createFolds(batch_data_final$batch_id, k = n_part)

sapply(partitions, length)

# picker qualifies for inclusion in the workforce of the virtual day if both 
# 1)the sum of real execution times of the batches he or she performed and
# 2)the sum of the forecast batch execution times 
#exceed the minimum threshold

M_min <- 7*60#7.40
M_max <- 9.5*60

# M_min <- 7.4*3600
# M_max <- 7.75*3600
d <- 2

# get eligible pickers for each day
picker_days <- sapply(1:n_part, function(d) {
  
  day <- batch_data_final  %>% 
    slice(partitions[[d]]) # filter picks of each day
  
  
  predictions <- predict(full_model, day)
  # pred <- 0
  # resids <- day$log_batch_time - pred
  
  # pred_corr <- exp(pred)* mean(exp(resids))
  
  day <- tibble(day, pred= predictions %>% exp)
  
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

order_data <- left_join(
  batch_data_final %>% 
    slice(partitions[[1]]) %>% 
    select(batch_id),
  all_data_needed %>% 
    filter(house <46) %>% 
    rename(
      batch_id = AUFTRAGSNR
    )
)

batch <- order_data %>% 
  filter(batch_id == first(batch_id) ) %>% 
  select(LFDNR, batch_id,rack:line) %>% 
  arrange(rack)


order_sample <- order_data %>% 
  slice_sample(n=100)


calculate_distance <- function(racks, rack_dist = 10, return = TRUE){
  
  racks <- racks %>% as.numeric
  travel_dist_rack = rack_dist*abs(as.numeric(racks) - lag(as.numeric(racks), default = NA))
  
  travel_start_end_dist <- (min(racks, na.rm=T) - 1) * rack_dist + (max(racks, na.rm=T) - 1) * rack_dist # travel from depot and back to depot
  
  sum(travel_dist_rack, na.rm=T) + sum(travel_start_end_dist) + 50*length(unique(racks))
  
}


racks <- batch$rack

calculate_distance(batch$rack)

calculate_savings <- function(racks){
  calculate_distance(racks[1]) + calculate_distance(racks[2]) - calculate_distance(c(racks[1], racks[2]))
}

rack_dist <- 10

n_racks <- 20


savings_mat <- outer(1:99, 1:99, FUN= function(X, Y) apply(cbind(X,Y), 1, calculate_savings ))



demand <- data.frame(ID = 1:n_racks, demand = 1) 

combinations <- combn(as.character(1:n_racks), 2) %>% t
savings_df <- tibble(combinations, savings=apply(combinations, 1, calculate_savings)) %>% 
  arrange(desc(savings))


dist_mat <- outer(1:n_racks, 1:n_racks, FUN= function(X, Y) (Y - X)*rack_dist )
dist_mat[lower.tri(dist_mat)] <- t(dist_mat)[lower.tri(t(dist_mat))] 
colnames(dist_mat) <- rownames(dist_mat) <- 1:n_racks

batches <- CW_VRP(demand = demand, DMat = dist_mat, Vehicle_Capacity = 2)


library(ggplot2)
all_data_needed %>% group_by(AUFTRAGSNR) %>% summarise(sum_mass =sum(mass/1000)) %>% ggplot() + geom_histogram(aes(sum_mass))


Greedy_routes <- CW_VRP(demand = demand, DMat = dist_mat, Vehicle_Capacity = 100)

# group over houses

data %>% group_by(rack, house) %>% summarise(mass= sum(mass))



library(lme4)
load("processed-data/batch_data_final.RData")
full_model <- lmer(log_batch_time ~ 1+ log_nlines + log_plevel + log_volume + log_mass + log_distance + 
                     (1+ log_nlines + log_plevel + log_volume + log_mass + log_distance |picker_id),
                   data = batch_data_final,
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5))
                   
)
full_model2 <- lmer(log_batch_time ~ 1+ log_nlines + log_plevel + volume + log_mass + log_distance + 
                     (1+ log_nlines + log_plevel + volume + log_mass + log_distance|picker_id),
                   data = batch_data_final,
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5))
                   
)
# number of partitions/ virtual days
n_part <- 12
# construct virtual days
partitions <- caret::createFolds(batch_data_final$batch_id, k = n_part)

sapply(partitions, length)

# picker qualifies for inclusion in the workforce of the virtual day if both 
# 1)the sum of real execution times of the batches he or she performed and
# 2)the sum of the forecast batch execution times 
#exceed the minimum threshold

M_min <- 7*60#7.40
M_max <- 9.5*60

# M_min <- 7.4*3600
# M_max <- 7.75*3600
d <- 2

# get eligible pickers for each day
picker_days <- sapply(1:n_part, function(d) {
  
  day <- batch_data_final  %>% 
    slice(partitions[[d]]) # filter picks of each day
  
  
  predictions <- predict(full_model, day)
  # pred <- 0
  # resids <- day$log_batch_time - pred
  
  # pred_corr <- exp(pred)* mean(exp(resids))
  
  day <- tibble(day, pred= predictions %>% exp)
  
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

order_data <- left_join(
batch_data_final %>% 
  slice(partitions[[1]]) %>% 
  select(batch_id),
all_data_needed %>% 
  filter(house <46) %>% 
  rename(
    batch_id = AUFTRAGSNR
  )
)


demand_data <- order_data %>% 
  # mutate(volume = mas %>% 
  group_by(rack) %>% 
  summarise(total_mass_per_rack = sum(mass/1000))

savings_mat <- outer(1:99, 1:99, FUN= function(X, Y) apply(cbind(X,Y), 1, calculate_savings ))



# demand <- data.frame(ID = 1:n_racks, demand = 1) 

combinations <- combn(as.character(1:n_racks), 2) %>% t
savings_df <- tibble(combinations, savings=apply(combinations, 1, calculate_savings)) %>% 
  arrange(desc(savings))

n_racks <- 80
dist_mat <- outer(1:n_racks, 1:n_racks, FUN= function(X, Y) (Y - X)*rack_dist )
dist_mat[lower.tri(dist_mat)] <- t(dist_mat)[lower.tri(t(dist_mat))] 
colnames(dist_mat) <- rownames(dist_mat) <- 1:n_racks

demand_data <- demand_data %>% mutate(ID=as.integer(rack)) %>%  rename(demand = total_mass_per_rack) %>%  select(ID, demand) %>% as.data.frame
 
n_racks <- 30
batches <- CW_VRP(demand = demand_data[1:n_racks,], DMat = dist_mat[1:n_racks,1:n_racks], Vehicle_Capacity = 3600 )


rack_length = 21 * 2.4
# example: 3 racks
n_racks <- 3

# dist_mat <- matrix(cbind(c(0,10,20,50,60,70), c(0, 0, 10, 60, 50, 60), c(0,0,0, 70,60,50), c(0,0,0,0, 10, 20), c(0,0,0,0,0, 10), c(0,0,0,0,0,0)), ncol=6)
dist_mat <- matrix(cbind(c(0, 50, 60, 70), c(0,0,10, 20), c(0,0,0,10), c(0,0,0,0)), ncol =4)
dist_mat[t(lower.tri(dist_mat))] <- dist_mat[(lower.tri(dist_mat))] 

order_data_sample <- order_data %>% 
  filter(rack %>% as.numeric < 4) %>% 
  mutate(vertex = as.numeric(rack)+1) %>% 
  group_by(vertex) %>% 
  summarise(total_mass = sum(mass))



demand_data <- data.frame(ID = 2:4, demand)

print(all_data_needed  %>% group_by(rack) %>%  summarise(max_house_per_rack = max(house)), n=100)

print(all_data_needed %>% group_by(house) %>%  filter(house == max(house)) %>%  summarise(sum_picks_in_max_house = n()), n= 100)


print(all_data_needed %>% group_by(house) %>%  filter(house == max(house)) %>%  summarise(sum_picks_in_max_house = n()), n = 100)


d