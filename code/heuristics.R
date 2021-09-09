## ---------------------------
##
## Script name: heuristics.R
##
## Purpose of script: heuristics 1 - 5 for ALNS from matusiak et al. (2017)
##
## Author: Manuel Carlan
##
## Date Created: 2021-07-12
##
## Copyright (c) Manuel Carlan, 2021
## Email: mcarlan@uni-goettingen.de

# BatchGAP
# goal minimize total execution time (cost) under the constraints
# 1. each batch is picked at most once
# 2. each picker's maximum working time is enforced
# 3. no more than N orders are contained in any one batch (not enforced?)
# 4. each order must be allocated once to any chosen batch



# heuristic 1
# 1. randomly choose Q ~ U(2,7) batches to be destroyed from any of the pickers
# 2. form set of pickers currently assigned to the batches in 1)
# 3. randomly choose a number of orders up to the maximum batch size of N orders from set in 1) Form a new batch from the chosen orders and assign it to a random worker
#    from 2) if time constraint can be upheld
# 4. repeat step 4 until there are no orders left to assign
# 5. calculate tour costs (distance) using routing algorithm for the new batches and form completition time forecasts for each new picker-batch pair
 
# x <- batches_of_the_day

rack_distance  <- 10


sample.vec <- function(x, ...) x[sample(length(x), ...)]

# x$batch_id <- x%>% group_by(batch_id) %>%  group_indices()

heuristic1 <- function(x){
  
  batch_sample <- sample(2:7, size = 1)
  # print(paste0("Q: ", Q))
  # batches to be destroyed
  batches_to_be_destroyed <- sample(x$batch_id, size = batch_sample)
  
  # print(paste0("batches destroyed: ", batches_to_be_destroyed$batch_id))
  # associated orders
  
  # problem starts probably when only new batches are there
  Od <- orders_of_the_day %>% filter(batch_id %in% batches_to_be_destroyed) %>% ungroup

  # pickers currently assigned to Od
  Wd <- Od$picker_id %>%  unique 
  new_batches <- list()
  
  i <- 1
  # randomly choose orders (size and orders are random)
  # sampleOd$order_id
  while(nrow(Od) >= 1){
    new_batch_counter <<- new_batch_counter + 1
    # randomly choose orders (size and orders are random) from Od
    new_batches[[i]] <- Od %>% sample_n(size=sample(1:nrow(Od), 1))  %>% mutate(batch_id = new_batch_counter)
    Od <- Od[!Od$order_id %in% new_batches[[i]]$order_id,]

    i <- i + 1
  }
  
  new_orders <- bind_rows(new_batches)  
  
  # orders_of_the_day %>% distinct() %>% 
  #   rows_update(new_orders, by = "order_id") %>% 
  #   right_join(orders_of_the_day %>% select(order_id), by = "order_id")
    
  orders_of_the_day <<- orders_of_the_day %>% filter(batch_id %in% batches_to_be_destroyed) 

  # aggregate new batches
  new_part <- new_orders %>%
    group_by(batch_id) %>%
    summarise(
      nlines =n(),
      plevel = mean(as.numeric(pick_level)),
      volume =  sum(volume) * 10e-10,
      mass = sum(mass / 1000),
      distance = 2 * rack_distance * max(rack %>% as.numeric) *  10e-4
    ) %>% 
    ungroup %>% 
    mutate(
           log_nlines = log(nlines),
           log_distance = log(distance),
           log_plevel = log(plevel),
           log_volume = log(volume),
           log_mass = log(mass)
    )
      
  new_part$picker_id <- sample.vec(Wd, size=nrow(new_part), replace = T)   
  x <- x %>% filter(!batch_id %in% batches_to_be_destroyed$batch_id) %>% select(colnames(new_part)) %>% bind_rows(new_part)
  x
}

# heuristic 3
# tries to move a set of batches between pickers while leaving the order composition of the batches intact
# probably one batch per picker is moved to another picker
heuristic3 <- function(x) {
  
  # sample random set of batches from various pickers
  xnew <- x %>%
    group_by(picker_id) %>% 
    sample_n(1) %>% 
    ungroup 
    
  shuffled_pickers <-  sample(pickers_of_the_day, size=nrow(xnew), replace = T)
  x[x$batch_id %in% xnew$batch_id,]$picker_id <- shuffled_pickers
  x
 
  
  # x %>% mutate(picker_id = replace(picker_id, batch_id %in% xnew$batch_id, shuffled_pickers))
  
   # x %>% distinct() %>% 
   #  rows_update(xnew, by = "batch_id") %>% 
   #  right_join(x %>% select(x), by = "batch_id")
}




# move all batches from a single picker to other workers
# first, the picker is selected randomly from all pickers with batches, then each of the selected picker's batches is assigned
# to other pickers with a greedy heuristic (???)
heuristic4 <- function(x) {
  
  pickers <- x$picker_id %>% unique
  chosen_picker <- sample.vec(pickers, size=1) # choose one picker randomly
  
  # use pickers currently involved 
  # remainin_pickers <- pickers[!pickers == chosen_picker]
  #or pickers of the day?
  remaining_pickers <- pickers_of_the_day[pickers_of_the_day != chosen_picker]
  
  #day_pickers <-  day_pickers[!day_pickers %in% empty_picker_batches$picker_id[1]] # delete picker from picker pool
  
  # 1. forecast each pick above by all remaining pickers
  # 2. sort according to forecast time
  # 3. associate pick with fastest picker continue
  batches <- x %>% filter(picker_id == chosen_picker)

  print(nrow(batches))
  # problem: the more batches a picker gets, the longer the for loop takes
  # greedy heuristic (i understand greedy here as looping through batches and assigning each batch to fastest picker)
  for(i in 1:nrow(batches)){
    
    # xnew <- data.frame(subset(x[i,], select= -picker_id))
    batch_preds <- tibble(picker_id= remaining_pickers, pred = predict(full_model, cbind(subset(x[i,], select= -picker_id), picker_id = remaining_pickers))) %>% 
      left_join(time_check) %>% 
      mutate(new_time = pred + total_pred_time) %>% 
      arrange(pred) %>% 
      suppressMessages()
   # print(batch_preds)
    
    j <- 1
    new_time <- batch_preds[1,4]
    while(new_time < M_max){
      
      new_time <- batch_preds[j, 4]
      
      j <- j + 1
      
      print(j)
    }
    time_check[time_check$picker_id == chosen_picker,2] <- new_time
    
    assign('time_check',time_check, envir=.GlobalEnv)
    
    # print(time_check[time_check$picker_id == picker,2])
    # print(time_check, n = 20)
    batches$picker_id <-batch_preds[j,]$picker_id
    
    # give to the next ELIGIBLE picker:
  }
  
  # only solutions that 
 
  # print(batches, n= nrow(batches))
  rows_update(x, batches)
  
  # x %>% distinct() %>% 
  #   rows_update(batches, by = "batch_id") %>% 
  #   right_join(x %>% select(x), by = "batch_id")
  # tibble(new_batch, forecast= predict(mmodel, newdata=new_batch))
}




# moves all batches from one random picker to the picker that executes them the fastest (what happens to batches of this picker?)
heuristic5 <- function(x) {
  
  # picker of which batches are reassigned
  chosen_picker <- sample.vec(x$picker_id, size = 1)
  

  # forecast execution times for every remaining picker
  fastest <- sapply(pickers_of_the_day, function(p) predict(full_model, x %>% mutate(picker_id = p))) %>% 
    colSums %>% as_tibble %>% 
    mutate(picker_id = pickers_of_the_day) %>% 
    arrange((value))
  
  #give batches of random picker to fastest picker
  x$picker_id[x$picker_id %in% chosen_picker] <- as.numeric(fastest[1,2])
  x
 }
 
  
  
    
  
  
