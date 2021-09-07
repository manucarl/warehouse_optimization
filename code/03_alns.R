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

# save(full_model, file="models/full_model_all_log.RData")
set.seed(42)

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
partition_index <- 1

# get eligible pickers for each day
picker_days <- sapply(1:n_part, function(partition_index) {
  
  # batches of the day
  batches_of_the_day <- batch_data_final  %>% 
    slice(partitions[[partition_index]]) # filter picks of each day
  
  predictions <- predict(full_model, batches_of_the_day)

  batches_of_the_day <- tibble(batches_of_the_day, pred= predictions %>% exp)
  
  # aggregate predicted and real execution times for each picker involved in the day
  day_agg <- batches_of_the_day %>% 
    group_by(picker_id) %>% 
    summarise(total_secs = sum(batch_time), 
              total_pred_secs = sum(pred))
  
  # keep only those pickers that satisfy the time conditions
  day_keep <- day_agg %>% filter(total_secs > M_min, total_secs < M_max,
                                 total_pred_secs > M_min, total_pred_secs < M_max)
  
  day_keep$picker_id
  
  
})

order_data <- all_data %>%
  gen_order_data %>% 
  rename()

day <- 1


# use only batches of the day and ??? pickers that are eligible
batches_of_the_day <- batch_data_final %>%
  slice(partitions[[day]])
# %>% 
#   filter(picker_id %in% picker_days[[day]])

orders_of_the_day <- order_data %>% 
  filter(batch_id %in% batches_of_the_day$batch_id) %>% 
  rename(plevel = pick_level) %>% 
  select(batch_id, order_id, picker_id, volume, mass, rack)

pickers_of_the_day <- picker_days[[day]]
# maximum number of orders per batch
N <- 50

# orders_of_the_day <- orders_of_the_day %>% 
#   group_by(batch_id) %>% 
#   sample_n(sample( 2:N , size=1)) 

# 5 different heuristics
# each heuristics is composed of a destroy and repair method

# destroy method breaks down a solution in a predefined manner
# repair methods try to find good alternatives to s by constructing another feasible solution
# heuristics are selected with each iteration with a weighted random seleciton criterion called roulette wheel method



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

heuristics <- c(heuristic1, heuristic3, heuristic4, heuristic5)


heuristics <- c(heuristic3)

# no of heuristics used
n_heuristics <- length(heuristics)



n_iter <- 200

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
# new_batch_day$picker_id <- sample(picker_days[[day]], nrow(new_batch_day), replace=T)

allow_new_levels <- F
#at the beginning the current solution is the best solution
s_star <- s <- batches_of_the_day

#cost of best solution
f_s_star <- predict(full_model, s_star) %>% exp %>% sum

# starting tempeerature

# cost of current solution
f_s <- predict(full_model, s) %>%  exp %>% sum
temp <- -0.03 * f_s/log(0.5)


it <- 1
#######################
# the algo
####################

new_batch_counter <- 0

for(it in 1:n_iter){
  

  f_s_star <- predict(full_model, s_star, allow.new.levels = allow_new_levels) %>% exp %>% sum
  
  f_s <- predict(full_model, s, allow.new.levels = allow_new_levels) %>%  exp %>% sum
  
# probabilities of choosing each heuristic
probs <-  ws/sum(ws)

# draw one of the heuristics for current iteration
h <- sample(1:n_heuristics, size=1, prob=probs)
# 
# print(paste("heuristic ", h ))

h_fun <- heuristics[[h]]

# new solution via application of chosen heuristic
s_prime <- h_fun(s)

# cost of new solution
f_s_prime <- predict(full_model, s_prime, allow.new.levels = allow_new_levels) %>% exp %>% sum


# print(c("f_s_prime" = f_s_prime, "f_s" = f_s, "f_s_star"= f_s_star))

Q <- runif(1)

if(f_s_prime < f_s_star){
    
  # print("case 1")
  s_star <- s <-  s_prime

  rho <- rho1
  
} else if(f_s_prime < f_s){
  
  # print("case 2")
  
  s <- s_prime
  
  rho <- rho2
  
}else if(Q <= exp((f_s_prime - f_s)/ temp)){
  
  # print("case 3")
  
  s <- s_prime

  rho <- rho3
}

phis[h] <- phis[h] + rho

if(it %% delta == 0)  {
  

phis[h] <- 0 
}
ws[h] <- lambda*ws[h] + (1-lambda)*phis[h]

temp <- phi*temp

}

f_s_star /
predict(full_model, batches_of_the_day) %>% exp %>% sum
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
(batch_data_final$picker_id %>%  unique) %in%
(s_prime$picker_id %>%  unique)



