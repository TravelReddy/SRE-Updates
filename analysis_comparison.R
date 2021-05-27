# Analysis comparison
library(glmnet)
library(ncvreg)
source('updated_class.R')


df = read.csv('../clean_pls_predictors.csv')
train_split = round(dim(df)[1] * .9)

set.seed('12345')
train_set = sample(1:dim(df)[1], train_split, replace = F)
test_set = (1:dim(df)[1])[-train_set]

# 
# build_sets = function(df, train_set, response_vec = 1, to_matrix = T){
# 
#   test_set = (1:dim(df)[1])[-train_set]
# 
#   y_train = df[train_set, response_vec]
#   y_test = df[test_set, response_vec]
#   
#   x_train = df[train_set, -response_vec]
#   x_test = df[test_set, -response_vec]
#   
#   if(length(response_vec) == 1){
#     y_train_mean = mean(y_train)
#     y_train_sd = sd(y_train)
#     y_train_scaled = (y_train - y_train_mean) / y_train_sd
# 
#     y_test_scaled = (y_test - y_train_mean) / y_train_sd
#   }else{
#     y_train_mean = apply(y_train, 2, mean)
#     y_train_sd = apply(y_train, 2, sd)
#     y_train_scaled = scale(y_train)
# 
#     y_test_scaled = scale(y_test, center = y_train_mean, scale = y_train_sd)
#   }
# 
#   x_train_mean = apply(x_train, 2, mean)
#   x_train_sd = apply(x_train, 2, sd)
#   x_train_scaled = scale(x_train)
# 
#   x_test_scaled = scale(x_test, center = x_train_mean, scale = x_train_sd)
#   
#   #temp_result = scale_data(x_train, y_train, x_test, y_test)
# 
#   result = list(
#     'x_train' = temp_result$x_train,
#     'y_train' = temp_result$y_train,
#     'x_test' = temp_result$x_test,
#     'y_test' = temp_result$y_test,
#     'x_mean' = temp_result$x_mean,
#     'x_sd' = temp_result$x_sd,
#     'y_mean' = temp_result$y_mean,
#     'y_sd' = temp_result$y_sd,
#     'y_test_no_scale' = y_test,
#     'response_vec' = response_vec
#   )
#   return(result)
# }

# 
# 
# 
# update_prediction_scale = function(train_info, pred_val){
# 
#   if(length(train_info$response_vec) == 1){
#     pred_scaled = pred_val * train_info$y_sd + train_info$y_mean
#   }else{
#     pred_scaled = sweep(pred_val, 2, train_info$y_sd, '*')
#     pred_scaled = sweep(pred_scaled, 2, train_info$y_mean, '+')
#   }
# 
#   return(pred_scaled)
# }




##########################################
########################################## Group Structure
########################################## 

## RR = 30.38
## ENET = 25.42
## LASSO = 26.11


train_info = build_sets(df, .9, 1:8)

# ridge
pred_val = mod_mgaussian(train_info, 0)
pred_val = update_prediction_scale(train_info, pred_val)

plot(train_info$y_test_no_scale[,1])
lines(pred_val[,1], col = 'red')

norm(as.matrix(pred_val - train_info$y_test_no_scale))

# lasso
pred_val = mod_mgaussian(train_info, 1)
pred_val = update_prediction_scale(train_info, pred_val)

plot(train_info$y_test_no_scale[,1])
lines(pred_val[,1], col = 'red')

norm(as.matrix(pred_val - train_info$y_test_no_scale))

# enet
pred_val = mod_mgaussian(train_info, .5)
pred_val = update_prediction_scale(train_info, pred_val)

plot(train_info$y_test_no_scale[,1])
lines(pred_val[,1], col = 'red')

norm(as.matrix(pred_val - train_info$y_test_no_scale))



pred_val = mod_mgaussian_ada(train_info, 1)
pred_val = update_prediction_scale(train_info, pred_val)

plot(train_info$y_test_no_scale[,1])
lines(pred_val[,1], col = 'red')

norm(as.matrix(pred_val - train_info$y_test_no_scale))

##########################################
########################################## With prior information
########################################## 

## RR = 18.15
## ENET = 16.05
## LASSO = 16.12
## SCAD = 17.12
## ADA = 15.08

response_vec = 1:8

pred_scaled_ridge = matrix(rep(0, length(test_set) * length(response_vec)),
                           length(test_set), length(response_vec))
y_test = pred_scaled_ridge

pred_scaled_lasso = pred_scaled_ridge
pred_scaled_ada = pred_scaled_ridge
pred_scaled_scad = pred_scaled_ridge
pred_scaled_enet = pred_scaled_ridge


# train_info = build_sets(df, train_set, response_vec = 1)
# 
# 
# fit = cv.glmnet(train_info$x_train, train_info$y_train, alpha = 0)
# pred_val = predict(fit, train_info$x_test, s='lambda.min')
# pred_val = update_prediction_scale(train_info, pred_val)
# 
# plot(train_info$y_test_no_scale)
# lines(pred_val, col = 'red')

for(i in 1:length(response_vec)){
  
  train_info = build_sets(df, .9, response_vec = i)
  y_test[,i] = train_info$y_test_no_scale
  
  pred_val = mod_ridge(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_ridge[,i] = pred_val
  
  pred_val = mod_lasso(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_lasso[,i] = pred_val

  pred_val = mod_elastic_net(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_enet[,i] = pred_val


  pred_val = mod_scad(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_scad[,i] = pred_val

  pred_val = mod_ada_lasso(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_ada[,i] = pred_val
}


plot(y_test[,1])
lines(pred_scaled_ridge[,1], col = 'red')

norm(as.matrix(pred_scaled_ridge - y_test))
norm(as.matrix(pred_scaled_enet - y_test))
norm(as.matrix(pred_scaled_lasso - y_test))
norm(as.matrix(pred_scaled_scad - y_test))
norm(as.matrix(pred_scaled_ada - y_test))


##########################################
########################################## Iterative Approach
########################################## 
## RR = 27.28
## ENET = 23.11
## LASSO = 22.50
## SCAD = 21.05
## ADA = 26.45

response_vec = 1:8

pred_scaled_ridge = matrix(rep(0, length(test_set) * length(response_vec)),
                           length(test_set), length(response_vec))
y_test = pred_scaled_ridge

pred_scaled_lasso = pred_scaled_ridge
pred_scaled_ada = pred_scaled_ridge
pred_scaled_scad = pred_scaled_ridge
pred_scaled_enet = pred_scaled_ridge

for(i in 1:length(response_vec)){
  
  other_responses = c(1:8)[-i]
  
  train_info = build_sets(df[,-other_responses], .9, response_vec = 1)
  y_test[,i] = train_info$y_test_no_scale
  
  pred_val = mod_ridge(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_ridge[,i] = pred_val
  
  pred_val = mod_lasso(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_lasso[,i] = pred_val
  
  pred_val = mod_elastic_net(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_enet[,i] = pred_val
  
  
  pred_val = mod_scad(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_scad[,i] = pred_val
  
  pred_val = mod_ada_lasso(train_info)
  pred_val = update_prediction_scale(train_info, pred_val)
  pred_scaled_ada[,i] = pred_val
}


plot(y_test[,1])
lines(pred_scaled_ridge[,1], col = 'red')

norm(as.matrix(pred_scaled_ridge - y_test))
norm(as.matrix(pred_scaled_enet - y_test))
norm(as.matrix(pred_scaled_lasso - y_test))
norm(as.matrix(pred_scaled_scad - y_test))
norm(as.matrix(pred_scaled_ada - y_test))


# 
# 
# ##### for the linear model
# 
# pred_scaled = matrix(rep(0, dim(y_test_scaled)[1] * dim(y_test_scaled)[2]),
#                      dim(y_test_scaled)[1], dim(y_test_scaled)[2])
# 
# for(i in 1:dim(y_train_scaled)[2]){
#   temp_response = y_train_scaled[,i]
#   df = as.data.frame(x_train_scaled)
#   df$response = temp_response
#   
#   fit = lm(response~., data = df)
#   df_test = as.data.frame(x_test_scaled)
#   
#   temp_pred = predict(fit, df_test)
#   pred_scaled[,i] = temp_pred * y_train_sd[i] + y_train_mean[i]
# }
# 
# norm(as.matrix(pred_scaled - y_test))
# 

##########################################
########################################## MICE implementation
########################################## 

## SCAD = 21.05


response_vec = 1:8
data_impute = df
y_test = data_impute[test_set, response_vec]

data_impute[test_set, response_vec] = NA

fit = mouse$new(X = data_impute, prediction_algorithm = mod_scad, max_iterations = 5)

pred_scaled = matrix(rep(0, length(test_set) * length(response_vec)),
                     length(test_set), length(response_vec))

for(i in 1:length(response_vec)){
  pred_scaled[, i] = fit$missing_data[[i]]$value
}

plot(df[test_set, 1])
lines(fit$missing_data[[1]]$value, col = 'red')


norm(as.matrix(pred_scaled - y_test))



library(mice)

# error = 34.02
fit = mice(data_impute, ls.meth = 'ridge', m = 1, maxit = 5)
temp_glucan = apply(fit$imp$glucan,1,mean)



pred_scaled = matrix(rep(0, length(test_set) * length(response_vec)),
                     length(test_set), length(response_vec))

for(i in 1:length(response_vec)){
  pred_scaled[, i] = apply(fit$imp[[i]],1,mean)
}

plot(df[test_set, 1])
lines(temp_glucan, col = 'red')

norm(as.matrix(pred_scaled - y_test))




##### TASKS
# 1. verify that everything is working with mouse from r to python
# 2. on line 291, there is a data set call data_impute
#   a. Remove all NA values from that data set
#   b. Perform Hierarchical clustering on the row space with n clusters
#   c. Take each different cluster, add back all of the NA values (something very similar to data_impute)
#       (make sure that each cluster has at least one NA row included)
#   d. Run multiple imputations: MICE
#       (run mouse on each different cluster)
#   e. Average the different cluster results where the NA rows are overlapping based on the cut of the dendrogram

