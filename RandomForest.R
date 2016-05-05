install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
View(flight_test)
fligt_select_param <- flight_test[,c(3,4,6,14,21,25)]
View(fligt_select_param)
fligt_select_param <-na.omit(fligt_select_param)
fligt_select_param$day=ifelse(fligt_select_param$DAY_OF_WEEK==1,"MONDAY",
                              ifelse(fligt_select_param$DAY_OF_WEEK==2,"TUESDAY",
                                     ifelse(fligt_select_param$DAY_OF_WEEK==3,"WEDNESDAY",
                                            ifelse(fligt_select_param$DAY_OF_WEEK==4,"THURSDAY",
                                                   ifelse(fligt_select_param$DAY_OF_WEEK==5,"FRIDAY",
                                                          ifelse(fligt_select_param$DAY_OF_WEEK==6,"SATURDAY",
                                                                 ifelse(fligt_select_param$DAY_OF_WEEK==7,"SUNDAY","NA")))))))
fligt_select_param$day = as.factor(fligt_select_param$day)
fligt_select_param$distt = ifelse(fligt_select_param$DISTANCE<1118,"SHORT","LONG")
fligt_select_param$distt= as.factor(fligt_select_param$distt)
fligt_select_param$dayofmonth = fligt_select_param$DAY_OF_MONTH
fligt_select_param$dayofmonth  = as.factor(fligt_select_param$dayofmonth )
fligt_select_param$depttime = fligt_select_param$CRS_DEP_TIME
fligt_select_param$timeslot = ifelse(fligt_select_param$CRS_DEP_TIME<700,"EARLY MORNING",
                                     ifelse(fligt_select_param$CRS_DEP_TIME>700 && fligt_select_param$CRS_DEP_TIME<1200,"MORNING",
                                            ifelse(fligt_select_param$CRS_DEP_TIME>1200 && fligt_select_param$CRS_DEP_TIME<1600,"AFTERNOON",
                                                   ifelse(fligt_select_param$CRS_DEP_TIME>1600 && fligt_select_param$CRS_DEP_TIME<2400,"EVENING"))))
fligt_select_param$timeslot = as.factor(fligt_select_param$timeslot )
randomize_flight_data = fligt_select_param[order(runif(nrow(fligt_select_param))),] 
View(randomize_flight_data)
randomize_flight_data = randomize_flight_data[,c(-1,-2,-3,-4 , -6 ,-10)]
trainIndex=createDataPartition(fligt_select_param$ARR_DEL15,p=0.8,list=FALSE)
flight_training_set=randomize_flight_data[trainIndex,]
flight_test_set = randomize_flight_data[- trainIndex,]
View(flight_training_set)
?randomForest
random_forest_model <-randomForest(as.factor(ARR_DEL15) ~ day + dayofmonth + timeslot + distt , data = flight_training_set, importance = TRUE, ntree = 50, mtry = 2,  na.action = na.omit) 
random_forest_model
prediction_rf <- predict(random_forest_model, flight_test_set)
random_forest_model$err.rate
random_forest_model$confusion
head(prediction_rf)
T=table(Observed=flight_test_set$ARR_DEL15,Predicted=prediction_rf)
head(T)
T
Err=1-sum(prediction_rf==flight_test_set$ARR_DEL15)/length(flight_test_set$ARR_DEL15)
Err