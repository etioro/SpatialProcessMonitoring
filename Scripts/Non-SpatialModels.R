library(caret)

# Run SimulateSpatialData to create the training set for non-spatial data

#training set for non-spatial model
training_data1 <- filtered_log[2001:140056,] 

# Train model to determine optimal model
# gbm
set.seed(42)
# Train model to determine hyperparameters.
gbm_model <- caret::train(RemTime ~ event.concept.name+ElapsedTime+count+cdate+doy, 
                          data = training_data1, 
                          method = "gbm", 
                          distribution = "gaussian",
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                          na.action = na.pass,
                          verbose = FALSE,
                          tuneLength=4)

pred_gbm <- predict(gbm_model,newdata = testing_data,type="raw")


#multi-layer perceptron

# Train model to determine hyperparameters.
mlp_model <- caret::train(RemTime ~ activity_id+ElapsedTime+count+cdate+doy, 
                          data = training_data1, 
                          method = "mlp", 
                          distribution = "gaussian",
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                          na.action = na.pass,
                          verbose = FALSE,
                          tuneLength=4)


pred_mlp <- predict(mlp_model,newdata = testing_data,type="raw")
