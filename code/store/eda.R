# histogram with
p_hist_all <- batch_data  %>%
  ungroup%>%dplyr::select(-picker_id) %>%
  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) + facet_wrap(~name, scales="free")
p_hist_all


length(unique(batch_data_clean$picker_id))
# 125
nrow(batch_data_clean)
# [1] 24814
# matusiak et al's clean dataset consists of 24,669 out of 37,841 batches worked by 99 out of 229 pickers 


# further considerations:
# i) only multiple orders in a batch (pick tour)
# 
summary(batch_data_clean)
p_hist_clean <- batch_data_clean  %>%
  ungroup%>%
  dplyr::select(-picker_id) %>%
  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) +
  facet_wrap(~name, scales="free")
p_hist_clean

summary(batch_data)

library(corrplot)
batch_data_clean %>% ungroup %>% 
  # sample_n(2000) %>% 
  dplyr::select(-c(picker_id, AUFTRAGSNR)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot::corrplot(method="number", type="upper")

# ggsave(filename="figures/vars_hist.png")


# library(corrplot)
# corrplot(cor(batch_data %>% select(-c(AUFTRAGSNR, picker_id))))
# scale predictors

library(caret)
library(lme4)
library(nlme)
# batch_clean <- batch_data %>% 
# filter(total_mass_kg < 6)
# preprocess_values <- preProcess(batch_data %>% select(mean_pick_level, total_volume, total_mass_kg), method = c("center", "scale"))
# batch_train <- predict(preprocess_values, batch_data)

# log transform




# batch_data_clean  %>% ungroup%>%
#   dplyr::select(AUFTRAGSNR,no_of_lines_scaled,mean_pick_level_scaled, total_mass_scaled, travel_dist_scaled,total_volume_scaled ) %>%  pivot_longer(cols=-AUFTRAGSNR) %>% 
#   ggplot(aes(x=value))  + geom_histogram(bins=100)+facet_wrap(~name, scales="free")
# 
summary(batch_data_clean)
batch_data_clean  %>%
  ungroup%>%
  dplyr::select(-picker_id) %>%
  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) +
  facet_wrap(~name, scales="free")

batch_data_clean %>% dplyr::select(log_no_of_lines:log_total_volume) %>%  ungroup %>% 
  # sample_n(2000) %>% 
  # dplyr::select(-c(picker_id, AUFTRAGSNR)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot(method="number", type="upper")
# batch_data_clean$total_mass_scaled <- scale(batch_data_clean$total_mass_kg)
# batch_data_clean$total_volume_scaled <- scale(batch_data_clean$total_volume)
# batch_data_clean$mean_pick_level_scaled <- scale(batch_data_clean$mean_pick_level)



ggplot(batch_data_clean, aes(x=mean_pick_level, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_mean_pick_level, y=log_batch_time_secs)) + geom_point( alpha=0.1)


ggplot(batch_data_clean, aes(x=total_volume, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_total_volume, y=log_batch_time_secs)) + geom_point( alpha=0.1, aes(col=picker_id))

ggplot(batch_data_clean, aes(x=total_mass_kg, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_total_mass, y=log_batch_time_secs)) + geom_point( alpha=0.1, aes(col=picker_id))

ggplot(batch_data_clean, aes(x=no_of_lines, y=batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=log_no_of_lines, y=log_batch_time_secs)) + geom_point( alpha=0.1)
ggplot(batch_data_clean, aes(x=no_of_lines_scaled, y=batch_time_secs)) + geom_point( alpha=0.1)