library(nlme)
library(tidyverse)
set.seed(1234)

# load("processed-data/batch_data_clean.Rdata")
# write_delim(batch_data_clean, file="processed-data/batch_data_clean.raw")

load("processed-data/all_data_prep.RData")

# summarise data in batches
batch_data <- all_data_needed %>% 
  mutate(pick_end = period_to_seconds(hms(ANFAHR_ZEIT)),
         pick_start = period_to_seconds(hms(BEGINN_ZEIT)),
         pick_level = ifelse(pick_level %in% c("01", "03", "05"), 1, 2)) %>% 
  group_by(picker_id, AUFTRAGSNR) %>%
  # group_by(picker_id, AUFTRAGSNR ) %>% 
  mutate(true_start = ifelse(MENGE_IST != 0, min(pick_start, na.rm=TRUE), Inf)) %>% # set BEGINN_ZEIT to Inf instead of start of shift effectively excluding it
  summarise(
    batch_time_secs = max(pick_end)- min(true_start, na.rm=TRUE),
    batch_time_mins = batch_time_secs/60,
    # no_of_lines = length(unique(as.numeric(place))),
    no_of_lines =n_distinct(house),
    
    # travel_dist_meter = sum(travel_dist_house+travel_dist_rack),
    travel_dist_meter = sum(travel_dist_rack), #+ sum(travel_dist_house),
    
    mean_pick_level = mean(as.numeric(pick_level)),
    total_volume = sum(volume),
    total_mass_kg = sum(mass/1000)#/1000
  )


length(unique(batch_data$picker_id))
# 179 
nrow(batch_data)
# [1] 43114



# thoughtful
batch_data_clean <- batch_data %>% 
  filter(
    batch_time_secs >300, 
    batch_time_secs <3600,
    # travel_dist_meter > 117.6, # delete too short distance (naive calculation: one rack has 42 places when you pass both directions => 42*1.4 * 2 =  117.6m)
    mean_pick_level > 0, # not clear what pick_level = 0 is
    total_mass_kg <3000, 
    # total_volume < 15
  ) %>% 
  group_by(picker_id) %>% 
  filter(n() > 75) %>% # matusiak et al. use only pickers who worked at least 75 batches (for cv)
  ungroup %>% 
  drop_na %>% 
  mutate(log_no_of_lines = log(no_of_lines),
         log_travel_dist_meter = log(travel_dist_meter),
         log_mean_pick_level = log(mean_pick_level),
         log_volume = log(total_volume),
         log_total_mass_kg = log(total_mass_kg),
         log_batch_time_secs = log(batch_time_secs)
  )

# save(batch_data_clean, file="processed-data/batch_data_clean.RData")


batch_data_train <- batch_data_clean %>% 
  group_by(picker_id) %>% 
  sample_frac(size = 0.8, replace = FALSE)


batch_data_test <- batch_data_clean[!(batch_data_clean$AUFTRAGSNR %in% batch_data_train$AUFTRAGSNR), ]





# 
# library(sjstats)
# performance::icc(model0)

model0  <- lm(log_batch_time_secs ~ 1 ,
              data = batch_data_train)

sum((batch_data_test$log_batch_time_secs -  predict(model0, newdata=batch_data_test))^2)
AIC(model0)

model0re  <- lme(fixed = log_batch_time_secs ~ 1 ,
                 random =~ 1|picker_id,
                 data = batch_data_train
                 
)
anova(model0re, model0) # include RE
AIC(model0re)

SSR0re <- sum((batch_data_test$log_batch_time_secs -  predict(model0re, newdata=batch_data_test))^2)

anova(model0re, model0)




# Model 1: Lines
model1X  <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines,
                random =~ 1|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)
AIC(model1X)

model1Xre  <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines,
                random =~ no_of_lines|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)
AIC(model1Xre) 


anova(model1X, model1Xre) # include RE
SSR1Xre <- sum((batch_data_test$log_batch_time_secs -  predict(model1Xre, newdata=batch_data_test))^2)




model1L  <- lme(fixed = log_batch_time_secs ~ 1 + log_no_of_lines,
                random =~ 1|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)
AIC(model1L)


model1Lre  <- lme(fixed = log_batch_time_secs ~ 1 + log_no_of_lines,
                  random =~ 1+ log_no_of_lines|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
anova(model1L, model1Lre) # include RE


SSR1Lre <- sum((batch_data_test$log_batch_time_secs -  predict(model1Lre, newdata=batch_data_test))^2)


### ->  include no_of_lines with random slope
# Model 2: Travel

model2XX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + travel_dist_meter,
                random =~ 1+ no_of_lines|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)

AIC(model2XX)

model2XXre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + travel_dist_meter,
                random =~ 1+ no_of_lines + travel_dist_meter|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)
AIC(model2XXre)

anova(model2XX, model2XXre) # include RE

SSR2XXre <- sum((batch_data_test$log_batch_time_secs -  predict(model2XXre, newdata=batch_data_test))^2)





model2XL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter,
                random =~ 1+ no_of_lines|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)

AIC(model2XL)

model2XLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model2XLre)

anova(model2XL, model2XLre) # include RE



SSR2XLre <- sum((batch_data_test$log_batch_time_secs -  predict(model2XLre, newdata=batch_data_test))^2)


# Model 3: Mass
model3XLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg,
                 random =~ 1+ no_of_lines+ log_travel_dist_meter|picker_id,
                 data = batch_data_train,
                 control=lmeControl(opt='optim')
)

AIC(model3XLX)

model3XLXre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg,
                 random =~ 1+ no_of_lines+ log_travel_dist_meter + total_mass_kg|picker_id,
                 data = batch_data_train,
                 control=lmeControl(opt='optim')
)

AIC(model3XLXre)

anova(model3XLX, model3XLXre) # include RE

SSR3XLXre <- sum((batch_data_test$log_batch_time_secs -  predict(model3XLXre, newdata=batch_data_test))^2)




model3XLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg,
                 random =~ 1+ no_of_lines+ log_travel_dist_meter|picker_id,
                 data = batch_data_train,
                 control=lmeControl(opt='optim')
)

AIC(model3XLL)

model3XLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg,
                   random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim')
)

anova(model3XLL, model3XLLre) # include RE

SSR3XLLre <- sum((batch_data_test$log_batch_time_secs -  predict(model3XLLre, newdata=batch_data_test))^2)




# Model 4: mean_pick_evel

model4XLLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + mean_pick_level,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model4XLLX)

model4XLLXre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + mean_pick_level,
                  random =~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + mean_pick_level|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model4XLLXre)
anova(model4XLLX, model4XLLXre) # include RE

SSR4XLLXre <- sum((batch_data_test$log_batch_time_secs -  predict(model4XLLXre, newdata=batch_data_test))^2)




model4XLLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model4XLLL)

model4XLLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

anova(model4XLLL, model4XLLLre) # include RE

SSR4XLLLre <- sum((batch_data_test$log_batch_time_secs -  predict(model4XLLLre, newdata=batch_data_test))^2)





## Danger zone
model5XLLLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level + total_volume,
                    random =~ 1+ no_of_lines+ log_travel_dist_meter + total_mass_kg + log_mean_pick_level|picker_id,
                    data = batch_data_train,
                    control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)
AIC(model5XLLLX)

model5XLLLXre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level + total_volume,
                     random =~ 1+ no_of_lines+ log_travel_dist_meter + total_mass_kg + log_mean_pick_level + total_volume|picker_id,
                     data = batch_data_train,
                     control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

anova(model5XLLLX, model5XLLLXre) # include RE

SSR5XLLLXre <- sum((batch_data_test$log_batch_time_secs -  predict(model5XLLLXre, newdata=batch_data_test))^2)



model5XLLLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level + log_total_volume,
                   random =~ 1+ no_of_lines+ no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

AIC(model5XLLLL)

model5XLLLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level + log_total_volume,
                   random =~ 1+no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level + log_total_volume|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

AIC(model5XLLLLre)

anova(model5XLLLL, model5XLLLLre) # include RE

SSR5XLLLL <- sum((batch_data_test$log_batch_time_secs -  predict(model5XLLLL, newdata=batch_data_test))^2)



OLS <- lm(log_batch_time_secs ~ no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level, data=batch_data_train)
AIC(OLS)

SSROLS <- sum((batch_data_test$log_batch_time_secs -  predict(OLS, newdata=batch_data_test))^2)

models <- list(model0 = model0,
               model0re = model0re,
               model1X = model1X,
               model1L = model1L,
               model1Xre = model1Xre,
               model1Lre = model1Lre,
               model2XX = model2XX,
               model2XL = model2XL,
               model2XXre = model2XXre,
               model2XLre = model2XLre,
               model3XLX = model3XLX,
               model3XLL = model3XLL,
               model3XLXre = model3XLXre,
               model3XLLre = model3XLLre,
               model4XLLX = model4XLLX,
               model4XLLL = model4XLLL,
               model4XLLXre = model4XLLXre,
               model4XLLLre = model4XLLLre,
               model5XLLLX = model5XLLLX,
               model5XLLLL = model5XLLLL,
               model5XLLLXre = model5XLLLXre,
               model5XLLLLre = model5XLLLLre,
               OLS = OLS)

getSSR <- function(model, data){
  sum((data$log_batch_time_secs -  predict(model, newdata=data))^2)
}
# save(models, file="processed-data/model_selection.RData")



table_models[1, ] <- list("0", "***", NA, NA, NA, NA, NA, AIC(model0), getSSR(model0re, batch_data_test), anova(model0re, model0)[2,9]  )
table_models[2, ] <- list("1X", "***", "***", NA, NA, NA, NA, AIC(model1Xre), SSR1Xre, anova(model1X, model1Xre)[2,9]  )
table_models[3, ] <- list("1L", "***", "***", NA, NA, NA, NA, AIC(model1Lre), SSR1Lre, anova(model1L, model1Lre)[2,9]  )
table_models[4, ] <- list("2XX", "***", "***", "***", NA, NA, NA, AIC(model2XXre), SSR2XXre, anova(model2XX, model2XXre)[2,9]  )
table_models[5, ] <- list("2XL", "***", "***", "***", NA, NA, NA, AIC(model2XLre), SSR2XLre, anova(model2XL, model2XLre)[2,9]  )
table_models[6, ] <- list("3XLX", "***", "***", "***", "***", NA, NA, AIC(model3XLXre), SSR3XLXre, anova(model3XLX, model3XLXre)[2,9]  )
table_models[7, ] <- list("3XLL", "***", "***", "***", "***", NA, NA, AIC(model3XLLre), SSR3XLLre, anova(model3XLL, model3XLLre)[2,9]  )
table_models[8, ] <- list("4XLLX", "***", "***", "***", "***", NA, NA, AIC(model4XLLXre), SSR4XLLXre,anova(model4XLLX, model4XLLXre)[2,9]  )
table_models[9, ] <- list("4XLLL", "***", "***", "***", "***", NA, NA, AIC(model4XLLLre), SSR4XLLLre,anova(model4XLLL, model4XLLLre)[2,9]  )
table_models[10, ] <- list("5XLLLX", "***", "***", "***", "***",  "***","***", AIC(model5XLLLXre), SSR5XLLLXre,anova(model5XLLLX, model5XLLLXre)[2,9]  )
table_models[11, ] <- list("5XLLLL", "***", "***", "***", "***",  "***","***", AIC(model5XLLLL), SSR5XLLLL,anova(model5XLLLL, model5XLLLLre)[2,9]  )
table_models[12, ] <- list("OLS", "***", "***", "***", "***",  "***","***", AIC(OLS), SSROLS, NA )

table_models$`p(RE)`<- round(as.numeric(table_models$`p(RE)`), 4)


library(knitr)
kable(table_models)

summary(model4XLLLre)



modeltest <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level + log_total_volume,
                   random =~ 1+ no_of_lines+ no_of_lines + log_travel_dist_meter + total_mass_kg + log_mean_pick_level|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

AIC(model5XLLLL)