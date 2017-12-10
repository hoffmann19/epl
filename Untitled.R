#change wd and csv name
setwd("C:/Users/hoffmads/OneDrive - Maritz LLC/Scrum/In Progress/churn_model/csvs_from_sql")
df = read.csv("keybank_5mo_10_24_2017.csv")
# df = rbind(df, read.csv("wecom_6mo_04_03_2017.csv")) #unions another timeframe to get bigger sample size

## packages
library("xgboost")  # the main algorithm
library("archdata") # for the sample dataset
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("e1071")
library("doSNOW")
library("kernlab")
library("ranger")
library("C50")
library("caTools")
library("plyr")
library("caretEnsemble")
library("dplyr")

#converting all factors to numeric
str(df)
indx = sapply(df, is.factor)
df[indx] = lapply(df[indx], function(x) as.numeric(as.character(x)))
str(df)

#converting Y to factor
df$left_in_time_frame1 = as.factor(df$left_in_time_frame1)

#taking care of NA
df[, 4:ncol(df)][is.na(df[, 4:ncol(df)])] <- 0

##using h2o
# install.packages("h2o")
library(h2o)
h2o.init()

##splits in h2o
h2o_df = as.h2o(df, destination_frame = "final2_h2o")
splits = h2o.splitFrame(
  h2o_df,
  c(.6,0.2), #creates splits of 60% and 20%
  seed = 1234
)
h2o_train = h2o.assign(splits[[1]], "train.hex")
h2o_valid = h2o.assign(splits[[2]], "valid.hex")
h2o_test = h2o.assign(splits[[3]], "test.hex")

#putting actual restults from test into data.frame format
df_test = as.data.frame(h2o_test)

#dropping certain columns
drops <- c("pax_id",
           "currently_active_flag",
           "ecards_received_tf1",
           "recs_received_tf1",
           "PB_amount_received_tf1",
           "amount_received_tf1",
           "PB_rec_received_tf1",
           "recs_given_tf1",
           "tf2_redeemed_points",
           "tf3_redeemed_points",
           "tf2_thru_tf3_redeemed_points",
           "days_since_last_redemption",
           "frequency_redemption_points_per_month_tf2_and_tf3"
)

h2o_train = h2o_train[ , !(names(h2o_train) %in% drops)]
h2o_valid = h2o_valid[ , !(names(h2o_valid) %in% drops)]
h2o_test = h2o_test[ , !(names(h2o_test) %in% drops)]
h2o_automl_frame = h2o_df[ , !(names(h2o_df) %in% drops)]

#h2o randomForest model
h2o_start = Sys.time()
h2o_rf_model = h2o.randomForest(y = c("left_in_time_frame1"), 
                                training_frame = h2o_train,
                                validation_frame = h2o_valid,
                                # stopping_rounds = 2, #stop fitting new trees when the 2-tree average is with .001(default)
                                # of the prior two 2-tree averages
                                ntrees = 200,
                                score_each_iteration = T, #predict against training and validation for each tree. 
                                # default will skip several
                                seed = 1000
)
h2o_end = Sys.time()
h2o_time = h2o_end - h2o_start
h2o_time  

##evaluating rf model
summary(h2o_rf_model)

#importance for RF Model
h2o.varimp_plot(h2o_rf_model)
importance = h2o.varimp(h2o_rf_model)
importance = importance[order(-importance$scaled_importance),]

#predicting using rf model
pred_rf = h2o.predict(object = h2o_rf_model, 
                      newdata = h2o_test)
pred_rf_frame = as.data.frame(pred_rf)
write.csv(pred_rf_frame, file = "pred_rf.csv")


##h20 gbm
h2o_gbm_start = Sys.time()
gbm1 = h2o.gbm(
  training_frame = h2o_train,
  validation_frame = h2o_valid,
  y = c("left_in_time_frame1"),
  seed = 1000,
  max_depth = 50
)
summary(gbm1)
h2o_gbm_end = Sys.time()
gbm_duration = h2o_gbm_end - h2o_gbm_start
gbm_duration
h2o.varimp_plot(gbm1)

#predicting using gbm model
pred_gbm = h2o.predict(object = gbm1, 
                       newdata = h2o_test)
pred_gbm_frame = as.data.frame(pred_gbm)
write.csv(pred_gbm_frame, file = "pred_gb.csv")


##h20 Automated Machine Learning
h2o_autoML_start = Sys.time()
auto_ml = h2o.automl(
  training_frame = h2o_automl_frame,
  # validation_frame = h2o_valid,
  # leaderboard_frame = h2o_test,
  y = c("left_in_time_frame1"),
  seed = 1000,
  max_runtime_secs = 1000,
  max_models =  20
)
lb = auto_ml@leaderboard
lb
auto_ml@leader

pred_aml = h2o.predict(auto_ml@leader,h2o_test)
pred_aml = as.data.frame(pred_aml)

test_preds = data.frame(df_test$pax_id, pred_rf_frame, pred_gbm_frame, pred_aml)

summary(auto_ml)
h2o_autoML_end = Sys.time()
autoML_duration = h2o_autoML_end - h2o_autoML_start
autoML_duration

h2o.varimp_plot(auto_ml)

pred_aml2 = h2o.predict(auto_ml@leader, h2o_automl_frame)
pred_aml2 = as.data.frame(pred_aml2)
pred_aml2 = data.frame(df$pax_id, pred_aml2)
test = subset(test_preds, test_preds$predict.2 == 1)

#done
library(beepr)
beep(sound = 5)
h2o.shutdown(prompt = TRUE)


