
# folds <- group_vfold_cv(batch_data_clean, group = picker_id)
# folds <- initial_split(batch_data_clean, prop = 0.8, strata = "picker_id")

batch_data_recipe <- batch_data_clean %>% 
  recipe(log_batch_time_secs ~ mean_pick_level + total_mass_kg + total_volume + no_of_lines + travel_dist_meter) %>% 
  step_corr(all_predictors()) %>% 
  step_log(all_predictors()) %>% 
  prep()
  
  juice(batch_data_recipe)
# Simple 
# kable
m0 <- lm((log_batch_time_mins) ~  mean_pick_level + total_mass_kg + total_volume + no_of_lines + travel_dist_meter, data=batch_data_clean)
AIC(m0)
m0 <- lm((log_batch_time_secs) ~  log(mean_pick_level) + total_mass_kg + total_volume + no_of_lines + travel_dist_meter, data=batch_data_clean)
AIC(m0)

bacth_data_clean %>% 
  mutate()
batch_data_train <- batch_data_clean %>% 
  group_by(picker_id) %>% 
  sample_frac(size = 0.8, replace = FALSE)


batch_data_test <- batch_data_clean[!(batch_data_clean$AUFTRAGSNR %in% batch_data_train$AUFTRAGSNR), ]
#‘lmerTest’


full_model <- lme(fixed = log_batch_time_secs ~ 1 + log(mean_pick_level) + log(total_mass_kg) + log(total_volume) + log(no_of_lines) + log(travel_dist_meter),
                  data = batch_data_clean,
                  random =~ 1+ log(mean_pick_level) + log(total_mass_kg) + log(total_volume) + log(no_of_lines) + log(travel_dist_meter)|picker_id
              
                  )


model0  <- lme(fixed = log_batch_time_secs ~ 1 ,
                random =~ 1|picker_id,
                data = batch_data_train,
               control=lmeControl(opt='optim')
               
)

 sum((batch_data_test$log_batch_time_secs -  predict(model0, newdata=batch_data_test))^2)
AIC(model0)

library(sjstats)
performance::icc(model0)

install.packages("sjstats")
# 1. no_of_lines
model1X  <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines,
                  random =~ 1|picker_id,
                  data = batch_data_train
)
AIC(model1X)
sum((batch_data_test$log_batch_time_secs -  predict(model1X, newdata=batch_data_test))^2)

model1L  <- lme(fixed = log_batch_time_secs ~ 1 + log_no_of_lines,
                random =~ 1|picker_id,
                data = batch_data_clean
)
AIC(model1L)


model1Xre  <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines,
                random =~ 1+ no_of_lines|picker_id,
                data = batch_data_clean
)
anova(model1X, model1Xre) # include RE



# 2. travel_dist

model2XX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + travel_dist_meter,
                random =~ 1+ no_of_lines|picker_id,
                data = batch_data_clean
)

AIC(model2XX)

model2XL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter,
                random =~ 1+ no_of_lines|picker_id,
                data = batch_data_clean
)

AIC(model2XL)

model2XLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter,
                random =~ 1+ no_of_lines+ log_travel_dist_meter|picker_id,
                data = batch_data_clean
)
anova(model2XL, model2XLre) # include RE

# 3. total_mass

model3XLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + total_mass_kg,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter|picker_id,
                  data = batch_data_clean
)

AIC(model3XLX)

model3XLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg,
                 random =~ 1+ no_of_lines+ log_travel_dist_meter|picker_id,
                 data = batch_data_clean
)

AIC(model3XLL)

model3XLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg,
                 random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                 data = batch_data_clean
)

anova(model3XLL, model3XLLre) # include RE

# 4. total_volume
model4XLLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + total_volume,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                  data = batch_data_clean
)
AIC(model4XLLX)


model4XLLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_total_volume,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                  data = batch_data_clean
)
AIC(model4XLLL)

model4XLLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_total_volume,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg+ log_total_volume|picker_id,
                  data = batch_data_clean,
                  control=lmeControl(opt='optim')
)
anova(model4XLLL, model4XLLLre) # include RE


# 5. mean_pick_level
model5XLLLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_total_volume + mean_pick_level,
                    random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg+ log_total_volume|picker_id,
                    data = batch_data_clean,
                    control=lmeControl(opt='optim')
)
AIC(model5XLLLX)

model5XLLLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_total_volume + log_mean_pick_level,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg+ log_total_volume|picker_id,
                  data = batch_data_clean,
                  control=lmeControl(opt='optim')
)
AIC(model5XLLLL)


model5XLLLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_total_volume + log_mean_pick_level,
                   random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg+ log_total_volume+ log_mean_pick_level|picker_id,
                   data = batch_data_clean,
                   control=lmeControl(opt='optim')
)

anova(model5XLLLL, model5XLLLLre) # include RE

# model5XLLLLre is the best model

modelOLS <- lm(log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_total_volume + log_mean_pick_level,
               data = batch_data_clean) 
AIC(modelOLS)



# 4. mean_pick_level
model4XLLX <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + mean_pick_level,
                   random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                   data = batch_data_clean
)

AIC(model4XLLX)

model4XLLL <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg|picker_id,
                  data = batch_data_clean
)
AIC(model4XLLL)

model4XLLLre <- lme(fixed = log_batch_time_secs ~ 1 + no_of_lines + log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level,
                  random =~ 1+ no_of_lines+ log_travel_dist_meter + log_total_mass_kg + log_mean_pick_level|picker_id,
                  data = batch_data_clean,
                  control = lmerControl(
                    optimizer ='optimx', optCtrl=list(method='L-BFGS-B'))
)

anova(model4XLLL, model4XLLLre) # include RE


model2LX <- lme(fixed = log_batch_time_secs ~ 1 + log_no_of_lines + travel_dist_meter,
    random =~ 1+ log_no_of_lines|picker_id,
    data = batch_data_clean
)

AIC(model2LX)

model2LXre <- lme(fixed = log_batch_time_secs ~ 1 + log_no_of_lines + travel_dist_meter,
                random =~ 1+ log_no_of_lines + travel_dist_meter|picker_id,
                data = batch_data_clean
)
anova(model2LX, model2LXre)




zero_model <- lmer(log_batch_time_secs ~ 1 +
                 (1 |picker_id),
               data=batch_data_clean,
)

AIC(zero_model)
model1X <- lmer(log_batch_time_mins ~ 1 + no_of_lines + 
                  (1 |picker_id),
                data=batch_data_clean,
)
AIC(model1X)

model1Xre <- lmer(log_batch_time_mins ~ 1 + no_of_lines + 
                  (1 +no_of_lines|picker_id),
                data=batch_data_clean,
)
anova(model1X, model1Xre)

model1L <- lmer(log_batch_time_mins ~ 1 + no_of_lines_scaled + 
                  (1 |picker_id),
                data=batch_data_clean,
)
AIC(model1L)

model1Lre <- lmer(log_batch_time_mins ~ 1 + no_of_lines_scaled + 
                    (1 +no_of_lines_scaled|picker_id),
                  data=batch_data_clean,
)
anova(model1X, model1Xre)

AIC(model1X)




m0_lmer <- lmer(log_batch_time_secs ~  mean_pick_level + total_mass_kg + total_volume + no_of_lines+(1|picker_id), data=batch_data_clean)
AIC(m0_lmer)

m0_lmer <- lmer(log_batch_time_secs ~  mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+(1|picker_id), data=batch_data_clean)
AIC(m0_lmer)
m1_lmer <- lmer(log_batch_time_secs ~  mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+
                  (mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled|picker_id), data=batch_data_clean)
AIC(m1_lmer)

m2_lmer <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines+
                  (log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines|picker_id), data=batch_data_clean,
                control = lmerControl(optimizer ="Nelder_Mead"))
AIC(m2_lmer)

library(optimx)
m2_lmer <- lmer(log_batch_time_secs ~   mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+
                  (log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines|picker_id), data=batch_data_clean,
                control = lmerControl(
                  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

# AIC(m0_lmer)
lm_model <- lme(fixed = batch_time_secs ~ 1 + mean_pick_level + total_mass_kg + total_volume + no_of_lines,
                data = batch_data_clean,
                random =~ 1|picker_id,
                method="ML")
AIC(lm_model)

# 
# m1_lmer <- lmer(log_batch_time_secs ~  mean_pick_level + total_mass_kg + total_volume + (1+ mean_pick_level + total_mass_kg + total_volume|picker_id), data=batch_data_clean)
# AIC(m1_lmer)
############# Null model
full_model <- lme(fixed = log_batch_time_secs ~ 1 + mean_pick_level + total_mass_kg + total_volume+ no_of_lines,
                  data = batch_data_clean,
                  random =~ 1+ mean_pick_level + total_mass_kg + total_volume + no_of_lines|picker_id,
                  method="ML")
AIC(full_model)

full_model_scaled <- lme(fixed = log_batch_time_secs ~ 1 + mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled,
                  data = batch_data_clean,
                  random =~ 1+ mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled|picker_id,
                  method="ML")
AIC(full_model_scaled)

full_model_log <- lme(fixed = log_batch_time_secs ~1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines,
                         data = batch_data_clean,
                         random =~ 1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines|picker_id)
AIC(full_model_log)

# mlasso <- glmmLasso(fix=log_batch_time_secs ~ mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled,
#          rnd =list(picker_id =~1 + mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled),
#          data=batch_data_clean %>% as.data.frame,
#          lambda=10)
# summary(mlasso)
# library(rstanarm)
# 
# stan_lmer(formula = course ~ 1 + (1 | school), 
#           data = GCSE,
#           seed = 349)
# 
# library(groupdata2)

# m1_lmer <- stan_lmer(log_batch_time_secs ~  mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+
                  # (mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled|picker_id), data=batch_data_clean)
# 
# library(caret)
# folds <- groupKFold(batch_data_clean$picker_id, k = 10) 
# folds <- partition(batch_data_clean, p = 0.8, id_col = "picker_id") 
#   
#   
# i <- 1
# m0 <- lme(fixed = log_batch_time_secs ~ 1,
#           random =~ 1|picker_id,
#           data = batch_data_clean[folds[[i]],]
# )
#          mean(batch_data_clean[-folds[[i]],"batch_time_secs"] - predict(m0, data=batch_data_clean[-folds[[i]],]))
# 
# 
#          cbind(batch_data_clean[-folds[[i]],"batch_time_secs"],   predict(m0, newdata=batch_data_clean[-folds[[i]],]))

# every fold should contain every picker and 80% of every batch they worked
# picker_groups <- batch_data_clean$picker_id
# batches_per_picker <- table(picker_groups)
# 
# lapply(batches_per_picker, function(x) createDataPartition(x, p=.8))
# 
# folds <- groupKFold(picker_groups, k =10)
# lapply(folds, function(x, y) table(y[x]), y = picker_groups)
# 
# length(unique(picker_groups))
# set.seed(131)
# groups <- sort(sample(letters[1:4], size = 20, replace = TRUE))
# table(groups)
# folds <- groupKFold(groups)
# lapply(folds, function(x, y) table(y[x]), y = groups)
# 
# table(batch_data_clean %>%  select(picker_id, AUFTRAGSNR))
# 
# set.seed(3527)
# subjects <- sample(1:20, size = 80, replace = TRUE)
# table(subjects)
# 
# 
# library(loo)
# ?kfold_split_random
# folds <- groupKFold(subjects, k = 15) 
# 
# kfold_split_grouped(K = 10, x = batch_data_clean$picker_id)
# 
# 
# groupKFold(batch_data_clean$picker_id, k = 10) 

library(lme4)
lmer.0 <- lmer(log_batch_time_secs ~  mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+
                  (1|picker_id), data=batch_data_clean)

AIC(lmer.0)

lmer.1 <- lmer(log_batch_time_secs ~  mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+
                 (1+mean_pick_level_scaled|picker_id), data=batch_data_clean)
AIC(lmer.1)

lmer.10 <- lmer(log_batch_time_secs ~  mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled+
                  (mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled|picker_id), data=batch_data_clean)


library(cAIC4)
lmer_null <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines+
                 (1|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)
cAIC(lmer_null)

lmer.a <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines+
                 (1|picker_id)+(0+log_mean_pick_level|picker_id) +  (0+log_total_mass|picker_id) +(0+log_total_volume|picker_id) + (0+log_no_of_lines|picker_id),
               data=batch_data_clean)

library(optimx)
lmer.b <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines+
                 (1 +log_mean_pick_level+log_total_mass+log_total_volume+log_no_of_lines|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)
lmer.c <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + log_total_volume + log+
                 (1 +log_mean_pick_level+log_total_mass+log_total_volume+log_travel_dist|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)

library(optimx)
lmer0 <- lmer(log_batch_time_secs ~  1+
                 (1|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)
cAIC(lmer0)

lmer1x <- lmer(log_batch_time_secs ~  1+mean_pick_level+
                (1+mean_pick_level|picker_id),
              data=batch_data_clean,
              REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)
caic_1X <- cAIC(lmer1x)

lmer1L <- lmer(log_batch_time_secs ~  1+log_mean_pick_level+
                 (1+log_mean_pick_level|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)
caic_1L <- cAIC(lmer1L)


lmer1LX <- lmer(log_batch_time_secs ~  1+log_mean_pick_level+total_mass_kg+
                 (1+log_mean_pick_level + total_mass_kg|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)
caic_1L <- cAIC(lmer1L)


lmer.c <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines+
                 (1 +log_mean_pick_level+log_total_mass+log_total_volume+log_no_of_lines|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)


lmer_matusiak <- lmer(log_batch_time_secs ~  log_mean_pick_level + log_total_mass + total_volume + log_no_of_lines+
                 (1 +log_mean_pick_level+log_total_mass+log_total_volume+log_no_of_lines|picker_id),
               data=batch_data_clean,
               REML = FALSE, lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
)


best_model <- stepcAIC(lmer.c, numberOfSavedModels = 3, direction = "backward")

residuals <- residuals(best_model$finalModel) #extracts the residuals and places them in a new column in our original data table

abs(residuals) #creates a new column with the absolute value of the residuals
abs_res2 <- abs(residuals) ^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(abs_res2 ~ picker_id, data=batch_data_clean) #ANOVA of the squared residuals
anova(Levene.Model.F) 




hclust(sd(lmer.b))

hclust(

?hclust

































library(lmtest)

#perform Breusch-Pagan Test
bptest(lmer.b)


lambda <- BoxCoxLambda(AirPassengers, lower=0)

REML = FALSE, glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')

AIC(lmer.a)
best_model <- stepcAIC(lmer.b, numberOfSavedModels = 3)

library(glmmLasso)
mlasso_null <- glmmLasso(fix=log_batch_time_secs ~ mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled,
                    rnd =list(picker_id =~mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled),
                    data=batch_data_clean,
                    lambda=10000)
mlasso_null$aic


mlasso <- glmmLasso(fix=log_batch_time_secs ~ mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled,
         rnd =list(picker_id =~1 + mean_pick_level_scaled + total_mass_scaled + total_volume_scaled + no_of_lines_scaled),
         data=batch_data_clean %>% as.data.frame,
         lambda=1)
mlasso$aic

mlasso$aic

AIC(mlasso)

summary(bic_best.11)


m0 <- lm(log_batch_time_secs ~  mean_pick_level + total_mass_kg + total_volume + no_of_lines + travel_dist_meter, data=batch_data_clean)
AIC(m0)

m1 <- lme(fixed = log_batch_time_secs ~ 1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines,
          data = as.data.frame(batch_data_clean),
          random =~ 1+ log_mean_pick_level + log_total_mass + log_total_volume+ log_no_of_lines|picker_id)

m1_lmer <- lmer(log_batch_time_secs ~ 1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines + 
                  (1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines|picker_id), data=batch_data_clean, REML=F)


m1_lmer <- lmer(log_batch_time_secs ~ 1 + log_total_mass + log_total_volume + 
                  (1 |picker_id), data=batch_data_clean)
AIC(m1_lmer)
library(cAIC4)
stepcAIC(m1_lmer, data=batch_data_clean)
summary(m1_lmer)
library(MASS)
library(RcmdrMisc)
stepwise(m1, direction="forward/backward")


AIC(m1)

library(buildmer)
model <- buildlme(Reaction ~ Days + (Days|Subject),data=lme4::sleepstudy)

stepm1 <- buildlme(log_batch_time_secs ~ 1 + log_mean_pick_level + log_total_mass + log_total_volume + log_no_of_lines +
                     (1+ log_mean_pick_level + log_total_mass + log_total_volume+ log_no_of_lines|picker_id),
                   data=batch_data_clean)

m0_lmer <- lmer(batch_time_secs ~  mean_pick_level + total_mass_kg + total_volume + (1|picker_id), data=batch_data_clean)
AIC(m0_lmer)
AIC(m2)

m1 <- lme(fixed = log_batch_time_secs ~ .,
          random =~ .|picker_id, 
          data = as.data.frame(batch_data_clean))


m3<- lmer(log_batch_time ~ .
            (. | picker_id), data=batch_data_clean)



# library(rstanarm)


# M1_stanlmer <- stan_lmer(log_batch_time ~  mean_pick_level + total_mass_kg + total_volume + (1+ mean_pick_level + total_mass_kg + total_volume|picker_id), data=batch_data_clean)




AIC(m3)

gg_dat <- batch_data_clean %>% 
  select(AUFTRAGSNR, log_batch_time, log_mean_pick_level, log_total_volume, log_total_mass) %>% 
  pivot_longer(cols=-AUFTRAGSNR)

ggplot(gg_dat) + geom_histogram(aes(value), bins=100) + facet_wrap(~name, scales="free")
# ggplot(gg_dat) + geom_boxplot(aes(x=1, y=value))+ facet_wrap(~name, scale="free")

gg_dat <- batch_data_clean %>% 
  select(AUFTRAGSNR, batch_time, mean_pick_level, total_volume, total_mass_kg, log_mean_pick_level, log_total_volume, log_total_mass) %>% 
  pivot_longer(cols=-c(AUFTRAGSNR, batch_time))

ggplot(gg_dat) + geom_point(aes(y=value, x=batch_time)) + facet_wrap(~name, scales="free")


gg_dat <- batch_data_clean %>% 
  select(AUFTRAGSNR, log_batch_time, log_mean_pick_level, log_total_volume, log_total_mass) %>% 
  pivot_longer(cols=-c(AUFTRAGSNR, log_batch_time))

ggplot(gg_dat) + geom_point(aes(y=value, x=log_batch_time)) + facet_wrap(~name, scales="free")

# divide data 80/20 for cv


gg_dat <- batch_data_clean %>% 
  
  select(AUFTRAGSNR,batch_time, mean_pick_level, total_volume, total_mass_kg) %>% 
  pivot_longer(cols=-AUFTRAGSNR)

ggplot(gg_dat) + geom_histogram(aes(value), bins=100) + facet_wrap(~name, scales="free")


print(batch_data_clean, n= 200)
