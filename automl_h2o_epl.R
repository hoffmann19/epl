# #install packages
# install.packages("xgboost")  # the main algorithm
# install.packages("archdata") # for the sample dataset
# install.packages("caret")    # for the confusionmatrix() function (also needs e1071 package)
# install.packages("e1071")
# install.packages("doSNOW")
# install.packages("kernlab")
# install.packages("ranger")
# install.packages("C50")
# install.packages("caTools")
# install.packages("plyr")
# install.packages("caretEnsemble")
# install.packages("dplyr")
# install.packages("h2o")


  #packages
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
  library("h2o")
  
##using h2o
  h2o.init()
  
##splits in h2o
  h2o_df = as.h2o(combineddb, destination_frame = "final2_h2o")
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
  
  colnames(combineddb)
  
  #dropping certain columns
  drops <- c("team",
             "date",
             "GameID",
             "Opponent",
             "goal_difference",
             "game_week",
             "team_gameweek_id",
             "opp_team_gameweek_id",
             "cumu_goal_difference")
  
  h2o_train = h2o_train[ , !(names(h2o_train) %in% drops)]
  h2o_valid = h2o_valid[ , !(names(h2o_valid) %in% drops)]
  h2o_test = h2o_test[ , !(names(h2o_test) %in% drops)]
  h2o_automl_frame = h2o_df[ , !(names(h2o_df) %in% drops)]

##h20 Automated Machine Learning
h2o_autoML_start = Sys.time()
auto_ml = h2o.automl(
  training_frame = h2o_automl_frame,
  # validation_frame = h2o_valid,
  # leaderboard_frame = h2o_test,
  y = c("win_flag"),
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


