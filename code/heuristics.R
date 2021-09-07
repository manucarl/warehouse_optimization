
# currently only heuristics 3-5 that leave batches intact!
rack_distance  <- 10
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
 
# x <- batch_day


sample.vec <- function(x, ...) x[sample(length(x), ...)]

# x$batch_id <- x%>% group_by(batch_id) %>%  group_indices()

heuristic1 <- function(x){
  
  Q <- sample(2:7, size = 1)
  # print(paste0("Q: ", Q))
  # batches to be destroyed
  batches_to_be_destroyed <- x %>% sample_n(size = Q)
  
  # print(paste0("batches destroyed: ", batches_to_be_destroyed$batch_id))
  # associated orders
  
  # problem starts probably when only new batches are there
  Od <- orders_day %>% filter(batch_id %in% batches_to_be_destroyed$batch_id) %>% ungroup

  print(paste0("Od: ", Od$order_id))
  # pickers currently assigned to Od
  Wd <- Od$picker_id %>%  unique 
  Wd <-
  print(paste0("currently assigned picker", Wd))
  new_batches <- list()
  i <- 1
  # randomly choose orders (size and orders are random)
  # sampleOd$order_id
  while(nrow(Od) >= 1){
    new_batch_counter <<- new_batch_counter + 1
    new_batches[[i]] <- Od %>% sample_n(size=sample(1:nrow(Od), 1))  %>% mutate(batch_id = new_batch_counter)
    Od <- Od[!Od$order_id %in% new_batches[[i]]$order_id,]

    i <- i + 1
  }
  
  print(new_batches)
  # aggregate new batches
  new_part <- bind_rows(new_batches) %>%
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
  
  xnew <- x %>%
    group_by(picker_id) %>% 
    sample_n(1) %>% 
    ungroup
  
  xnew$picker_id <- xnew$picker_id[sample(nrow(xnew))]


  rows_update(x, xnew)

}




# move all batches from a single picker to other workers
# first, the picker is selected randomly from all pickers with batches, then each of the selected picker's batches is assigned
# to other pickers with a greedy heuristic (???)
heuristic4 <- function(x) {
  
  chosen_picker <- sample(x$picker_id, 1) # choose one picker randomly
  day_pickers <- pickers[!pickers %in% chosen_picker]

  #day_pickers <-  day_pickers[!day_pickers %in% empty_picker_batches$picker_id[1]] # delete picker from picker pool
  
  # 1. forecast each pick above by all remaining pickers
  # 2. sort according to forecast time
  # 3. associate pick with fastest picker continue
  batches <- x %>% filter(picker_id == chosen_picker)
  # print(batches, n= nrow(batches))
  
  
  # greedy heuristic (i understand greedy here as looping through batches and assigning each batch to fastest picker)
  for(i in 1:nrow(batches)){
    
    # xnew <- data.frame(subset(x[i,], select= -picker_id))
    batch_preds <- tibble(day_pickers, pred = predict(full_model, cbind(subset(x[i,], select= -picker_id), picker_id = day_pickers))) %>%  arrange(pred)
   # print(batch_preds)
    batches[i,]$picker_id <- batch_preds[1,]$day_pickers
    
  }
  # print(batches, n= nrow(batches))
  rows_update(x, batches)
  # tibble(new_batch, forecast= predict(mmodel, newdata=new_batch))
}




# moves all batches from one random picker to the picker that executes them the fastest (what happens to batches of this picker?)
heuristic5 <- function(x) {
  random_picker_batches <- filter(x, picker_id == sample(picker_id, size=1)) # choose one picker randomly
  
  # forecast execution times for every remaining picker
  day_pickers <- unique(x$picker_id)   
  fastest <- sapply(day_pickers, function(p) predict(full_model, x %>% mutate(picker_id = p))) %>% 
    colSums %>% as_tibble %>% 
    mutate(picker_id = day_pickers) %>% 
    arrange((value))
  
  #give batches of random picker to fastest picker
 x_new <-  random_picker_batches %>% 
    mutate(picker_id = fastest$picker_id[1])
 
 rows_update(x, x_new)
 
 
 }
 
  
  
    
  
  
