
# select, prepare, generate and rename variables on order level
gen_order_data <- function(data){
  data %>% 
    dplyr::select(LFDNR,AUFTRAGSNR,MDENR, ANFAHR_ZEIT, BEGINN_ZEIT, mass, MENGE_IST, volume, pick_level, area, rack, place, house, line) %>%
    drop_na(volume) %>%
    mutate(pick_end = lubridate::period_to_seconds(lubridate::hms(ANFAHR_ZEIT)),
           pick_start = lubridate::period_to_seconds(lubridate::hms(BEGINN_ZEIT)),
           rack = as.numeric(rack)) %>% 
    rename(picker_id = MDENR, 
           batch_id = AUFTRAGSNR,
           order_id = LFDNR)
}



# summarise orders on batch level 
gen_batch_data <- function(data, rack_distance = 10){
  
  data %>% 
    # pick_level = ifelse(pick_level %in% c("01", "03", "05"), 1, 2)) %>%
    group_by(picker_id, batch_id) %>%
    # group_by(picker_id, AUFTRAGSNR ) %>% 
    # mutate(true_start = ifelse(MENGE_IST != 0, min(pick_start, na.rm=TRUE), Inf)) %>% # set BEGINN_ZEIT to Inf instead of start of shift effectively excluding it
    # mutate(
    #   batch_time_secs = (pick_end)- (pick_start),
    #   batch_time_mins = batch_time_secs/60,
    #   # no_of_lines = length(unique(as.numeric(place))),
    #   ) %>%
    summarise(
      batch_time_secs = (max(pick_end)-min(pick_start)),
      # batch_time_secs = sum(pick_end-pick_start),
      
      # no_of_lines =n_distinct(house),
      no_of_lines =n(),
      # travel_dist_meter = sum(travel_dist_house+travel_dist_rack),
      travel_dist_meter = 2 * rack_distance * max(rack %>% as.numeric),
      mean_pick_level = mean(as.numeric(pick_level)),
      total_volume = sum(volume),
      total_mass_kg = sum(mass/1000)
    ) %>% 
    ungroup
}



# filter peculiar observations
clean_batch_data <- function(data) {
  data %>% 
    filter(
      batch_time_secs > 300, 
      batch_time_secs <7200,
      travel_dist_meter >= 10, # delete too short distance (naive calculation: one rack has 42 places when you pass both directions => 42*1.4 * 2 =  117.6m)
      mean_pick_level > 0, # not clear what pick_level = 0 is
      total_mass_kg <3000, 
      no_of_lines > 20) %>% 
    # total_volume < 15  ) %>% 
    group_by(picker_id) %>% 
    filter(n() > 75) %>% # matusiak et al. use only pickers who worked at least 75 batches (for cv)
    ungroup %>% 
    drop_na
}


# rescaling and renaming
rescale_rename_data <- function(data) {
  
  data %>% 
    mutate(total_volume = total_volume * 10e-10,
           travel_dist_km = travel_dist_meter * 10e-4,
           batch_time = batch_time_secs/60) %>% 
    rename(         nlines = no_of_lines,
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
    dplyr::select(batch_id, picker_id, batch_time, nlines, plevel:mass, -travel_dist_meter, distance, log_batch_time:log_mass)
}


get_batch_data <- function(data) data %>% gen_order_data %>% clean_batch_data %>% rescale_rename_data

