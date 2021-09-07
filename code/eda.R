library(tidyverse)

# load(file="processed-data/batch_data_final.RData")
load(file="processed-data/all_data.RData")

batch_data <- all_data %>% gen_order_data %>% gen_batch_data %>% clean_batch_data %>% rescale_rename_data


sd(batch_data$batch_time_secs)
summary(batch_data$batch_time_secs)

summary(batch_data$batch_time_secs)
batch_data %>% 
  ggplot(aes(batch_time_secs))+
  geom_histogram(bins=100)

batch_data %>% 
  ggplot(aes(batch_time_secs))+
  geom_histogram(bins=100) +
  scale_x_log10()

batch_data %>% 
  ggplot(aes(log(batch_time_secs)))+
  geom_histogram(bins=100) 

batch_data %>% 
  ggplot(aes(no_of_lines, batch_time_secs))+
  geom_point() +
  scale_y_log10()



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
batch_data_clean  %>%
  ungroup%>%
  dplyr::select(-picker_id) %>%
  pivot_longer(cols=-AUFTRAGSNR) %>% 
  ggplot(aes(x=value))  + geom_histogram(bins=100, color="black",fill=NA) +
  facet_wrap(~name, scales="free")


summary(batch_data)

library(corrplot)
batch_data_clean %>% ungroup %>% 
  # sample_n(2000) %>% 
  dplyr::select(-c(picker_id, AUFTRAGSNR)) %>%
  mutate_if(is.factor, as.numeric) %>%
  cor %>% 
  corrplot::corrplot(method="number", type="upper")





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
