## ---------------------------
##
## Script name: 02_eda.R
##
## Purpose of script: little bit of exploratory data analysis
##
## Author: Manuel Carlan
##
## Date Created: 2021-04-11
##
## Copyright (c) Manuel Carlan, 2021
## Email: mcarlan@uni-goettingen.de


library(tidyverse)

# load(file="processed-data/batch_data_final.RData")
load(file="processed-data/all_data.RData")
load("processed-data/batch_data_final.RData")

# write plots as .png on drive?
write_plots <-T



sd(batch_data_final$batch_time)
summary(batch_data_final)


# histogram with
p_hist_all <- batch_data_final  %>%
  ungroup%>%dplyr::select(-picker_id) %>%
  pivot_longer(cols=-batch_id) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) + facet_wrap(~factor(name, levels=c("batch_time", "log_batch_time",
                                                                                                              "distance", "log_distance",
                                                                                                              "mass", "log_mass",
                                                                                                              "nlines", "log_nlines",
                                                                                                              "plevel", "log_plevel",
                                                                                                              "volume", "log_volume"))
                                                                                       , scales="free", ncol= 2)
p_hist_all

if(write_plots) ggsave(filename = "figures/all_histograms_cleaned_data.png", plot = p_hist_all)


length(unique(batch_data_final$picker_id))
# 135
nrow(batch_data_clean)
# [1] 24814
# matusiak et al's clean dataset consists of 24,669 out of 37,841 batches worked by 99 out of 229 pickers 



summary(batch_data_clean)
batch_data_clean  %>%
  ungroup%>%
  dplyr::select(-picker_id) %>%
  pivot_longer(cols=-batch_id) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) +
  facet_wrap(~name, scales="free")


summary(batch_data)

library(corrplot)
batch_data_clean %>% ungroup %>% 
  # sample_n(2000) %>% 
  dplyr::select(-c(picker_id, batch_id)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot::corrplot(method="number", type="upper")




load("processed-data/full_model.RData")

summary(full_model)
pickers <- batch_data_final$picker_id %>% unique



# boxplots of execution time per picker
batch_data_final %>% 
  ggplot + geom_boxplot(aes(y=batch_time, group=picker_id %>% as.factor)) + 
  xlab("picker_id") +
  ggtitle("batch execution times of each picker")


if(write_plots) ggsave(filename = "figures/boxplots_times_per_picker.png")

i <- 1
for(picker_set in split(pickers, ceiling(seq_along(pickers)/20))){
  batch_data_final %>% 
    filter(picker_id %in% picker_set) %>% 
    group_by(picker_id) %>% 
    # slice_sample(n=5) %>% 
    ggplot(aes(x = log_mass , y = log_batch_time, group = picker_id)) +
    geom_point(color = "cadetblue4", alpha = 0.80) +
    geom_smooth(method = 'lm', se = FALSE, color = "black") +
    facet_wrap(~picker_id)
  
  if(write_plots) ggsave(filename = paste("figures/lm_fit_for_effect_of_pickers", i, ".png"))
  i <- i +1
  
}

