
#-------------------------------------------------------------------------------------
# data transformation - from order to batch level
#-------------------------------------------------------------------------------------

# 1) select, generate and mutate variables into desired form
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

# order_data <- all_data %>% gen_order_data

# 2) summarise orders on batch level and generate 
gen_batch_data <- function(data, rack_distance = rack_distance){
  data %>% 
    group_by(picker_id, batch_id) %>%
    summarise(
      batch_time_secs = (max(pick_end)-min(pick_start)),
      
      no_of_lines =n(),
      travel_dist_meter = 2 * rack_distance * max(rack %>% as.numeric), # !!!! uses simplest heuristic for routing / distance calculation !!!!!
      mean_pick_level = mean(as.numeric(pick_level)),
      total_volume = sum(volume),
      total_mass_kg = sum(mass/1000)
    ) %>% 
    ungroup
}

# batch_data <- order_data %>% gen_batch_data


# 3) filter peculiar observations
clean_batch_data <- function(data, min_order_number_per_picker = 75) {
  data %>% 
    filter(
      batch_time_secs > 300, 
      batch_time_secs <7200,
      mean_pick_level > 0, # not clear what pick_level = 0 is
      total_mass_kg <3000, 
      no_of_lines > 20
    ) %>% 
    group_by(picker_id) %>% 
    filter(n() > min_order_number_per_picker) %>% # matusiak et al. use only pickers who worked at least 75 batches (for cv)
    ungroup %>% 
    drop_na
}

# batch_data_clean <- batch_data %>% clean_batch_data

# 4) rescaling and renaming

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


