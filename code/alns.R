library(tidyverse)
library(lme4)

source("code/preprocessing_functions.R")
set.seed(42)

load("processed-data/batch_data_final.RData")
load("processed-data/all_data.RData")

full_model <- lmer(log_batch_time ~ 1+ log_nlines + log_plevel + log_volume + log_mass + log_distance +
                     (1+ log_nlines + log_plevel + log_volume + log_mass |picker_id),
                   data = batch_data_final,
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5))
                   
)


summary(full_model)

batch_data_final %>% 
  ggplot + geom_boxplot(aes(y=batch_time, group=picker_id %>% as.numeric))

batch_data_final %>% 
  filter(picker_id %in% sample(picker_id, 20)) %>% 
  group_by(picker_id) %>% 
  # slice_sample(n=5) %>% 
  ggplot(aes(x = log_mass , y = log_batch_time, group = picker_id)) +
  geom_point(color = "cadetblue4", alpha = 0.80) +
  geom_smooth(method = 'lm', se = FALSE, color = "black") +
  facet_wrap(~picker_id)

batch_data_final %>% group_by(picker_id) %>% summarize(mean = mean(batch_time))

# save(full_model, file="models/full_model_all_log.RData")

# load("models/full_model_all_log.RData")
# number of partitions/ virtual days
n_part <- 12
# construct virtual days
partitions <- caret::createFolds(batch_data_final$batch_id, k = n_part)

sapply(partitions, length)

rack_distance <- 10
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


# orders of a day
# first day:
day <- 1

orders_day <- left_join(
  batch_data_final %>% 
    slice(partitions[[day]]) %>% 
    select(batch_id),
  all_data %>% 
    gen_order_data %>% 
    filter(house <46)
) %>% 
  arrange(desc(rack)) %>% 
  filter(rack > 0) %>% 
  mutate(pick_end = lubridate::period_to_seconds(lubridate::hms(ANFAHR_ZEIT)),
         pick_start = lubridate::period_to_seconds(lubridate::hms(BEGINN_ZEIT)),
         rack = as.numeric(rack))


# randomly sample n orders
order_sample <- orders_day %>% 
  sample_n(200) %>% 
  select(order_id, batch_id, rack:line, mass)




batch_day <- orders_day %>% 
  group_by(batch_id) %>%
  summarise(
    no_of_lines =n(),
    travel_dist_meter = 2 * rack_distance * max(rack %>% as.numeric),
    mean_pick_level = mean(as.numeric(pick_level)),
    total_volume = sum(volume/10e8),
    total_mass_kg = sum(mass/1000)
  ) %>% 
  ungroup %>% 
  arrange(desc(no_of_lines)) %>% 
  mutate(
    log_nlines = log(no_of_lines),
    log_distance = log(travel_dist_meter ),
    log_plevel = log(mean_pick_level),
    log_volume = log(total_volume),
    log_mass = log(total_mass_kg)
  ) %>% 
  drop_na 



# 5 different heuristics
# each heuristics is composed of a destroy and repair method

# destroy method breaks down a solution in a predefined manner
# repair methods try to find good alternatives to s by constructing another feasible solution
# heuristics are selected with each iteration with a weighted random seleciton criterion called roulette wheel method


x <- new_batch_day

### heuristics

# 3 move batches
## move one batch to another picker

# 4 empty picker and greedy repair
## move all batches from a single picker to other workers
### select picker randomly from all pickers with batches
### --> each of the selected picker's batches is assigned to other pickers with a greedy heuristic

# 5 move all batches from a picker to another
## move all batches from one random picker to the picker that executes them the fastest

# # 1st option
# rho1 <- 33
# rho2 <- 13
# rho3 <- 9  
# lambda <- 0.95 # decay parameter, controls rate of change in the weights
# delta <- 200 # every delta iterations scores are updated, weights are set to zero
# phi <- 0.999995
# 

# 2nd option
rho1 <- 65
rho2 <- 13
rho3 <- 9
lambda <- 0.95 # decay parameter, controls rate of change in the weights
delta <- 91 # every delta iterations scores are updated, weights are set to zero
phi <- 0.999995



source("code/heuristics.R")

heuristics <- c(heuristic3, heuristic4, heuristic5)
# no of heuristics used
n_heuristics <- length(heuristics)

# weights of heuristics w
# every heuristic has a weight that is influences the probability of being chosen in each iteration
# as algo runs, good performance of a heuristic will increase its weight and thus the probability of it getting selected again
weights_mat <- matrix(1, n_iter, n_heuristics)
ws <- weights_mat[1,]



# score phi
# every heuristic has a score phi which is updated whenever the heuristics is selected based on the heuristics's search success
# every score is updated every delta iterations by adding score phi to it
# at the beginning and after an update of the scores, i.e. every delta iterations, the values phi are set to zero
phi_mat <- matrix(0, n_iter, n_heuristics) 
phis <- phi_mat[1,]

# at each iteration temperature T is updated by T <- phi * T, where T is the temperature variable and phi the cooling coefficient

# first free at the beginning
new_batch_day <- batch_day

new_batch_day$picker_id <- sample(picker_days[[1]], nrow(new_batch_day), replace=T)


#at the beginning the current solution is the best solution
s_star <- s <- new_batch_day

#cost of best solution
f_s_star <- predict(full_model, s_star) %>% exp %>% sum

# starting tempeerature

# cost of current solution
f_s <- predict(full_model, s) %>%  exp %>% sum
temp <- -0.03 * f_s/log(0.5)



i <-1
#######################
# the algo
####################

n_iter <- 200


for(it in 1:n_iter){
  
print(it)

  f_s_star <- predict(full_model, s_star) %>% exp %>% sum
  
  f_s <- predict(full_model, s) %>%  exp %>% sum
  
# probabilities of choosing each heuristic
probs <-  ws/sum(ws)

# draw one of the heuristics for current iteration
h <- sample(1:n_heuristics, size=1, prob=probs)

print(paste("heuristic ", h+ 2))

h_fun <- heuristics[[h]]


print(nrow(s))
# new solution via application of chosen heuristic
s_prime <- h_fun(s)

# cost of new solution
f_s_prime <- predict(full_model, s_prime) %>% exp %>% sum


print(c("f_s_prime" = f_s_prime, "f_s" = f_s, "f_s_star"= f_s_star))

Q <- runif(1)

if(f_s_prime < f_s_star){
    
  print("case 1")
  s_star <- s <-  s_prime

  rho <- rho1
  
} else if(f_s_prime < f_s){
  
  print("case 2")
  
  s <- s_prime
  
  rho <- rho2
  
}else if(Q <= exp((f_s_prime - f_s)/ temp)){
  
  print("case 3")
  
  s <- s_prime

  rho <- rho3
}

if(it %% delta == 0)  {
  phis[h] <- phis[h] + rho


phis[h] <- 0 
}
ws[h] <- lambda*ws[h] + (1-lambda)*phis[h]

temp <- phi*temp


}
# heuristic3(batch_day)
# 
# sum(predict(mmodel, heuristic3(batch_day)))
# sum(predict(mmodel, batch_day))
# 
# 
# 
# 
# heuristic4(batch_day)
# 
# sum(predict(mmodel, heuristic4(batch_day)))
# sum(predict(mmodel, batch_day))



