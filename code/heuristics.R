
# currently only heuristics 3-5 that leave batches intact!




# heuristic 3
# tries to move a set of batches between pickers while leaving the order composition of the batches intact
# probably one batch per picker is moved to another picker
heuristic3 <- function(x) {
  
  
  
  
  xnew <- x %>%
    group_by(picker_id) %>% 
    sample_n(1) %>% 
    ungroup %>% 
    mutate(picker_id = sample(picker_id))

  rows_update(x, xnew)

}





# move all batches from a single picker to other workers
# first, the picker is selected randomly from all pickers with batches, then each of the selected picker's batches is assigned
# to other pickers with a greedy heuristic (???)
heuristic4 <- function(x) {
  
  empty_picker_batches <- filter(x, picker_id == sample(picker_id, size=1)) # choose one picker randomly
  day_pickers <- unique(x$picker_id) 
  #day_pickers <-  day_pickers[!day_pickers %in% empty_picker_batches$picker_id[1]] # delete picker from picker pool
  
  # 1. forecast each pick above by all remaining pickers
  # 2. sort according to forecast time
  # 3. associate pick with fastest picker continue
  new_allocations <- as.data.frame(matrix(0, nrow(empty_picker_batches), 3))
  colnames(new_allocations) <- c("batch_id", "picker_id", "forecast")
  
  # greed heuristic 
  for(i in seq_along(empty_picker_batches$batch_id)){
    batch_picker_data <- full_join(empty_picker_batches[i,-1], tibble(picker_id=day_pickers), by=character()) # cross join to get batch combination with all remaining pickers
    new_allocations[i,] <- (tibble(batch_picker_data %>%# forecast batch execution time for all pickers, sort acc. to execution time and take the fastest
                                     select(batch_id, picker_id),
                                   log_batch_time_secs = predict(mmodel,batch_picker_data)) %>%
                              arrange(log_batch_time_secs))[1,]
    # day_pickers <- day_pickers[!day_pickers %in% new_allocations[i,]$picker_id] # delete current fastest picker from pool for next iteration
    
  }
  
  new_batch <- rows_update(x, new_allocations %>% as_tibble %>%  select(-forecast), by = c("batch_id"))
  # tibble(new_batch, forecast= predict(mmodel, newdata=new_batch))
  new_batch
}


# moves all batches from one random picker to the picker that executes them the fastest (what happens to batches of this picker?)
heuristic5 <- function(x) {
  random_picker_batches <- filter(x, picker_id == sample(picker_id, size=1)) # choose one picker randomly
  
  # forecast execution times for every remaining picker
  day_pickers <- unique(x$picker_id)   
  fastest <- sapply(day_pickers, function(p) predict(mmodel, x %>% mutate(picker_id = p))) %>% 
    colSums %>% as_tibble %>% 
    mutate(picker_id = day_pickers) %>% 
    arrange(value)
  
  #give batches of random picker to fastest picker
  random_picker_batches %>% 
    mutate(picker_id = fastest$picker_id[1])
}
 
  
  
    
  
  
