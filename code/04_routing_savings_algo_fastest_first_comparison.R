## ---------------------------
##
## Script name: 04_routing_savings_algo_fastest_first_comparison.R
##
## Purpose of script: SIMPLE routing heuristic, SIMPLE Clark & Wright savings implementation and comparison of fastest first vs. random assignment
##
## Author: Manuel Carlan
##
## Date Created: 2021-07-20
##
## Copyright (c) Manuel Carlan, 2021
## Email: mcarlan@uni-goettingen.de


library(tidyverse)
library(lme4)

source("code/preprocessing_functions.R")

load("processed-data/batch_data_final.RData")
load("processed-data/all_data.RData")


load("processed-data/full_model.RData")

set.seed(42)
# number of partitions/ virtual days
n_part <- 12
# construct virtual days
partitions <- caret::createFolds(batch_data_final$batch_id, k = n_part)

sapply(partitions, length)


# picker qualifies for inclusion in the workforce of the virtual day if both 
# 1)the sum of real execution times of the batches he or she performed and
# 2)the sum of the forecast batch execution times 
#exceed the minimum threshold

# minimum work time per day in mins
M_min <- 7*60#7.40

# maximum work time per day in mins
M_max <- 9.5*60


# get eligible pickers for each day
picker_days <- sapply(1:n_part, function(d) {
  
  day <- batch_data_final  %>% 
    slice(partitions[[d]]) # filter picks of each day
  
  
  predictions <- predict(full_model, day)


  day <- tibble(day, pred= predictions %>% exp)
  
  day_agg <- day %>% 
    group_by(picker_id) %>% 
    summarise(total_secs = sum(batch_time), 
              total_pred_secs = sum(pred))# %>%
  
  # keep only pickers whose predicted and real total batch execution time is within the limits
  day_keep <- day_agg %>% filter(total_secs > M_min, total_secs < M_max,
                                 total_pred_secs > M_min, total_pred_secs < M_max)
  

  day_keep$picker_id
  
  
})


# orders of a day
# e.g. first day:
day <- 1

batches_of_the_day <- batch_data_final %>%
  slice(partitions[[day]]) %>% 
  filter(picker_id %in% picker_days[[day]])

orders_of_the_day <- all_data %>% 
  dplyr::select(LFDNR,AUFTRAGSNR,MDENR, ANFAHR_ZEIT, BEGINN_ZEIT, mass, MENGE_IST, volume, pick_level, area, rack, line, place, house, line) %>%
  drop_na(volume) %>%
  mutate(pick_end = lubridate::period_to_seconds(lubridate::hms(ANFAHR_ZEIT)),
         pick_start = lubridate::period_to_seconds(lubridate::hms(BEGINN_ZEIT)),
         rack = as.numeric(rack)) %>% 
  rename(picker_id = MDENR, 
         batch_id = AUFTRAGSNR,
         order_id = LFDNR) %>% 

  filter(batch_id %in% batches_of_the_day$batch_id) %>% 
  rename(plevel = pick_level) %>% 
  select(batch_id, order_id, picker_id, volume, mass, plevel, rack, line, place)


# randomly sample n orders
order_sample <- orders_of_the_day %>% 
  sample_n(200) %>% 
  select(order_id, batch_id, rack:line, mass, place)




#----------------------------------------------------------------------------------------------------------------------
# simple routing heuristic for depot at rack = 01 and universal drop off locations (the whole left part of the warehouse)
# so that Dijkstra is no needed
#----------------------------------------------------------------------------------------------------------------------


# odd rack numbers are up - down, even rack numbers are down - up
find_route <- function(order_sample, start = "01", end = "01") {
  
  # input: df with orders and corresponding racks as columns
  # output: route
vs <- lapply(unique(order_sample$rack), function(i){
  
  ifelse(as.numeric(i) %% 2 == 0, sort_fun <- desc, sort_fun <-  function(x) x) # rack 1 up - down, rack 2: down - - up etc.
  order_sample <- order_sample %>% 
    arrange(rack)
  v <-  order_sample %>% filter(rack == i) %>%
    dplyr::select(order_id, batch_id, rack, place) %>%
    arrange(sort_fun(place)) # 3)sort each aisle vector v according to the travel direction of the aisle
  v
}
)

ysmall <- do.call(rbind, vs) %>% arrange(rack) %>% select(-batch_id)

start_end <- c("order_id" = NA, "rack" = "01", "place" = NA, "house" = NA)
ysmall_tour <- rbind(start_end, ysmall, start_end) %>% select(order_id, rack)

ysmall_tour %>% mutate(rack = as.numeric(rack))
}

# condition: routes start and end at rack 01
route <- find_route(order_sample) 

print(route, n= 202)

# route for one order
route1 <- find_route(order_sample[1,])

# route for another order
route2 <- find_route(order_sample[2,])

# combined route of two orders
route12 <- find_route(order_sample[1:2,])




calculate_distance <- function(route, rack_distance=10){
  
  rack_distance * abs(route$rack - lag(route$rack)) %>% sum(na.rm=T)
}

calculate_distance(route1)
calculate_distance(route2)
calculate_distance(route12)

# savings: sum of distances when taking both routes individually minus distance of sequential tour
calculate_distance(route1) + calculate_distance(route2) - calculate_distance(route12)

# savings are always distance of shorter route, i.e. the route with the smaller rack index (as you pass the rack anyway)
calculate_distance(route1)


# calculates savings for two routes as input
calculate_savings <- function(route1, route2, start="01", end = "01"){

  # input: 2 route dfs with 3 rows (start, rack, end)
  # output: savings by combining the two routes
  route <- route1
  route$rack[2] <-  min(c(route1$rack[2], route2$rack[2]))
  calculate_distance(route)
}

# calculates savings for a vector of two rack indices
calculate_savings_rack <- function(racks, start=1, end = 1){
  

  route <- data.frame(rack=c(start, min(c(racks[1], racks[2])), end))
  # route$rack[2] <-  min(c(route1$rack[2], route2$rack[2]))
  calculate_distance(route)
}

calculate_savings_rack(c(route1$rack[2], route2$rack[2]))


find_route(order_sample)
find_route(orders_of_the_day)


# savings matrix
savings_matrix <- outer(2:99, 2:99, FUN = function(X, Y) apply(cbind(X, Y), 1, calculate_savings_rack))
savings_matrix[t(lower.tri(savings_matrix, diag=T))] <- 0
savings_matrix




#--------------------------------------------------------------------------------------------------------
# savings algo
#--------------------------------------------------------------------------------------------------------
# savings algorithm: sort savings and combine with biggest savings. for us, this means that
# 1. start with orders in aisle with biggest number (farest away from depot) and combine them into batch until capacity is reached.
# 2. If capacity is not reached, merge with orders in clostes aisle etc.

capacity <- 2000


ind <- 1
acc_mass <- 0

breaks <- NULL
for(i in 1:nrow(orders_of_the_day)){
  
  acc_mass <- acc_mass + orders_of_the_day[i,]$mass/1000
  if(acc_mass >= capacity){
    breaks <- c(breaks, i)
    acc_mass <- 0
  }
}

# use lagged indices
diffs <- diff(c(1, breaks, nrow(orders_of_the_day)+1))

# attribute new batch ids
new_orders_of_the_day <- orders_of_the_day
new_orders_of_the_day$batch_id <- rep(1:(length(breaks) + 1), times=diffs)

# aggregate new batches
rack_distance <- 10

new_batches_day <- new_orders_of_the_day %>% 
  group_by(batch_id) %>%
  summarise(
    nlines =n(),
    distance = 2 * rack_distance * max(rack %>% as.numeric),
    plevel = mean(as.numeric(plevel)),
    volume = sum(volume/10e8),
    mass = sum(mass/1000)
  ) %>% 
  ungroup %>% 
  arrange(desc(nlines)) %>% 
  mutate(
         log_nlines = log(nlines),
         log_distance = log(distance ),
         log_plevel = log(plevel),
         log_volume = log(volume),
         log_mass = log(mass)
  ) %>% 
  drop_na 
  
# get fastest pickers from data
picker_productivity <- batch_data_final %>% 
  group_by(picker_id) %>% 
  summarise(
  
    productivity = mean(nlines/batch_time) # average number of units per time picked
    )%>% 
  arrange(desc(productivity))

# now that new batches are attributed, we need to associate pickers

# 1) randomly
random_batch <- new_batches_day
random_batch$picker_id <- sample(picker_productivity$picker_id, nrow(new_batches_day), replace=T)

random_batch$batch_times <- predict(full_model, random_batch) %>%  exp

random_batch$batch_times %>% sum


# batch time if specific picker handled all batches (sorted by productivity)
times_ff <- sapply(picker_productivity$picker_id, function(x) predict(full_model, new_batches_day %>%  mutate(picker_id = x)) %>%  exp %>% sum)
times_random <- sapply(sample(picker_productivity$picker_id), function(x) predict(full_model, new_batches_day %>%  mutate(picker_id = x)) %>%  exp %>% sum)


tibble("fastest first" = times_ff, "random" = times_random, picker = 1:length(times_ff)) %>% 
  pivot_longer(cols=-picker) %>% 
  ggplot(aes(x=picker, y=value)) +
  facet_wrap(~name)+
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  ylab("total execution time (if respective picker executed all batches)") +
  ggtitle("batch execution times: fastest first vs. random assignment (pickers are sorted according to productivity in the left panel)")

# ggsave(filename="figures/fastest_first_vs_random_assignment.png")
