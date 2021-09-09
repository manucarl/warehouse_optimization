## ---------------------------
##
## Script name: 02_model_selection_nlme.R
##
## Purpose of script: performs model selection based on AIC and prediction SSR
##
## Author: Manuel Carlan
##
## Date Created: 2021-04-12
##
## Copyright (c) Manuel Carlan, 2021
## Email: mcarlan@uni-goettingen.de


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###  Not the most current version. Results may change slightly. Needs to run   
###  again as variables have changed (e.g. distance) + Switched to lme4.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Tasks:

# 1. Estimate model with variable as fixed effect
# 2. use aic to determine whether fixed effect should be included
# 3. Estimate model with added variable random effect
# 4. use anova to determine whether random effect should be included
# 5. Repeat 1-4 with log variable
# 6. Next variable
# 7. Summarize in table



library(nlme)
library(tidyverse)
set.seed(1234)

load("processed-data/batch_data_final.Rdata")
# write_delim(batch_data_clean, file="processed-data/batch_data_clean.raw")

load("processed-data/all_data.RData")



scale2 <- function(x, na.rm = FALSE) (x) / sd(x, na.rm)


# get train data
batch_data_train <- batch_data_final %>% 
  group_by(picker_id) %>% 
  sample_frac(size = 0.8, replace = FALSE)

batch_data_train %>% summary

# get test data
batch_data_test <- batch_data_final %>% filter(!(batch_data_final$batch_id %in% batch_data_train$batch_id))




# library(sjstats)
# performance::icc(model0)


# base model
model0  <- lm(log_batch_time ~ 1 ,
              data = batch_data_train)

sum((batch_data_test$log_batch_time -  predict(model0, newdata=batch_data_test))^2)
AIC(model0)

model0re  <- lme(fixed = log_batch_time ~ 1 ,
                 random =~ 1|picker_id,
                 data = batch_data_train
                 
)
anova(model0re, model0) # include RE
AIC(model0re)

SSR0re <- sum((batch_data_test$log_batch_time -  predict(model0re, newdata=batch_data_test))^2)

anova(model0re, model0)


# Model 1: Lines
model1X  <- lme(fixed = log_batch_time ~ 1 + nlines,
                random =~ 1|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)
AIC(model1X)

model1Xre  <- lme(fixed = log_batch_time ~ 1 + nlines,
                  random =~ nlines|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model1Xre) 


anova(model1X, model1Xre) # include RE
SSR1Xre <- sum((batch_data_test$log_batch_time -  predict(model1Xre, newdata=batch_data_test))^2)




model1L  <- lme(fixed = log_batch_time ~ 1 + log_nlines,
                random =~ 1|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)
AIC(model1L)

AIC(model1L) > AIC(model1X) # 1X


model1Lre  <- lme(fixed = log_batch_time ~ 1 + log_nlines,
                  random =~ 1+ log_nlines|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
anova(model1L, model1Lre) # include RE

AIC(model1Lre)

SSR1Lre <- sum((batch_data_test$log_batch_time -  predict(model1Lre, newdata=batch_data_test))^2)


### ->  include nlines with random slope
# Model 2: Travel

model2XX <- lme(fixed = log_batch_time ~ 1 + nlines + distance,
                random =~ 1+ nlines|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)

AIC(model2XX)

model2XXre <- lme(fixed = log_batch_time ~ 1 + nlines + distance,
                  random =~ 1+ nlines + distance|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model2XXre)

anova(model2XX, model2XXre) # include RE

SSR2XXre <- sum((batch_data_test$log_batch_time -  predict(model2XXre, newdata=batch_data_test))^2)





model2XL <- lme(fixed = log_batch_time ~ 1 + nlines + log_distance,
                random =~ 1+ nlines|picker_id,
                data = batch_data_train,
                control=lmeControl(opt='optim')
)

AIC(model2XL) > AIC(model2XX)

model2XLre <- lme(fixed = log_batch_time ~ 1 + nlines + log_distance,
                  random =~ 1+ nlines + log_distance|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model2XLre)

anova(model2XL, model2XLre) # include RE



SSR2XXre <- sum((batch_data_test$log_batch_time -  predict(model2XXre, newdata=batch_data_test))^2)


# Model 3: Mass
model3XXX <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass,
                 random =~ 1+ nlines+ distance|picker_id,
                 data = batch_data_train,
                 control=lmeControl(opt='optim')
)

AIC(model3XXX)

model3XXXre <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass,
                   random =~ 1+ nlines+ distance + mass|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim')
)

AIC(model3XXXre)

anova(model3XXX, model3XXXre) # include RE

SSR3XXXre <- sum((batch_data_test$log_batch_time -  predict(model3XXXre, newdata=batch_data_test))^2)




model3XXL <- lme(fixed = log_batch_time ~ 1 + nlines + distance + log_mass,
                 random =~ 1+ nlines+ distance|picker_id,
                 data = batch_data_train,
                 control=lmeControl(opt='optim')
)

AIC(model3XXL) > AIC(model3XXX) 

model3XXLre <- lme(fixed = log_batch_time ~ 1 + nlines + distance + log_mass,
                   random =~ 1+ nlines+ distance + log_mass|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim')
)

anova(model3XXL, model3XXLre) # include RE

SSR3XXLre <- sum((batch_data_test$log_batch_time -  predict(model3XXLre, newdata=batch_data_test))^2)




# Model 4: mean_pick_evel

model4XXXX <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + plevel,
                  random =~ 1+ nlines+ distance + mass|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model4XXXX)

model4XXXXre <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + plevel,
                    random =~ 1 + nlines + distance + mass + plevel|picker_id,
                    data = batch_data_train,
                    control=lmeControl(opt='optim')
)
AIC(model4XXXXre)

anova(model4XXXX, model4XXXXre) # include RE

SSR4XXXre <- sum((batch_data_test$log_batch_time -  predict(model4XXXXre, newdata=batch_data_test))^2)




model4XXXL <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + log_plevel,
                  random =~ 1+ nlines+ distance + mass|picker_id,
                  data = batch_data_train,
                  control=lmeControl(opt='optim')
)
AIC(model4XXXL) 


AIC(model4XXXL) >AIC(model4XXXX)

model4XXXLre <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + log_plevel,
                    random =~ 1+ nlines+ distance + mass + log_plevel|picker_id,
                    data = batch_data_train,
                    control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

anova(model4XXXL, model4XXXLre) # include RE

SSR4XXXLre <- sum((batch_data_test$log_batch_time -  predict(model4XXXLre, newdata=batch_data_test))^2)





## Danger zone
model5XXXLX <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + log_plevel + volume,
                   random =~ 1+ nlines+ distance + mass + log_plevel|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)
AIC(model5XXXLX)

model5XXXLXre <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + log_plevel + volume,
                     random =~ 1+ nlines+ distance + mass + log_plevel + volume|picker_id,
                     data = batch_data_train,
                     control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

anova(model5XXXLX, model5XXXLXre) # include RE

SSR5XLLLXre <- sum((batch_data_test$log_batch_time -  predict(model5XLLLXre, newdata=batch_data_test))^2)



model5XXXLL <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + log_plevel + log_volume,
                   random =~ 1+ nlines+ nlines + distance + mass + log_plevel|picker_id,
                   data = batch_data_train,
                   control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

AIC(model5XXXLL) >AIC(model5XXXLX)

model5XXXLLre <- lme(fixed = log_batch_time ~ 1 + nlines + distance + mass + log_plevel + log_volume,
                     random =~ 1+nlines + distance + mass + log_plevel + log_volume|picker_id,
                     data = batch_data_train,
                     control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

AIC(model5XXXLLre)

anova(model5XXXLL, model5XXXLLre) # do not include RE

SSR5XLLLL <- sum((batch_data_test$log_batch_time -  predict(model5XLLLL, newdata=batch_data_test))^2)



OLS <- lm(log_batch_time ~ nlines + log_distance + log_mass + log_plevel, data=batch_data_train)
AIC(OLS)

SSROLS <- sum((batch_data_test$log_batch_time -  predict(OLS, newdata=batch_data_test))^2)

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
  sum((data$log_batch_time -  predict(model, newdata=data))^2)
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



modeltest <- lme(fixed = log_batch_time ~ 1 + nlines + log_distance + mass + log_plevel + log_volume,
                 random =~ 1+ nlines+ nlines + log_distance + mass + log_plevel|picker_id,
                 data = batch_data_train,
                 control=lmeControl(opt='optim', maxIter = 100, msMaxIter=100)
)

AIC(model5XLLLL)