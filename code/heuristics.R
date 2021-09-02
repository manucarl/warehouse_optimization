
# currently only heuristics 3-5 that leave batches intact!

pickers <- picker_days[[1]]
# BatchGAP
# goal minimize total execution time (cost) under the constraints
# 1. each batch is picked at most once
# 2. each picker's maximum working time is enforced
# 3. no more than N orders are contained in any one batch (not enforced?)
# 4. each order must be allocated once to any chosen batch


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
 
  
  
    
  
  
