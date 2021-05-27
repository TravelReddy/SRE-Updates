library(R6)


mod_ada_lasso = function(scaled_df){
  require('glmnet')
  
  # apply ridge regression to determine the penalty weights
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0)
  ridge_coefs = fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.min)]
  # apply lasso with penalization
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 1, penalty.factor = 1 / abs(ridge_coefs))
  temp_lambda = fit$lambda
  while(fit$lambda.1se == fit$lambda.min){
    temp_lambda = temp_lambda / 2
    fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 1, penalty.factor = 1 / abs(ridge_coefs), lambda = temp_lambda)
  }
  
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.min')
  
  # best_coefs = which(fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.min)] != 0)
  # sub_x_train = scaled_df$x_train[, best_coefs]
  # sub_x_test = scaled_df$x_test[, best_coefs]
  # fit = cv.glmnet(sub_x_train, scaled_df$y_train, alpha = 0)
  # 
  # impute_values = predict(fit, newx = sub_x_test, s = 'lambda.min')
  
  return(impute_values)
}

mod_ada_enet = function(scaled_df){
  require('glmnet')
  
  # apply ridge regression to determine the penalty weights
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0)
  ridge_coefs = fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.min)]
  # apply lasso with penalization
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = .5, penalty.factor = 1 / abs(ridge_coefs))
  temp_lambda = fit$lambda
  while(fit$lambda.1se == fit$lambda.min){
    temp_lambda = temp_lambda / 2
    fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = .5, penalty.factor = 1 / abs(ridge_coefs), lambda = temp_lambda)
  }
  
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.1se')
  
  return(impute_values)
}


mod_elastic_net = function(scaled_df){
  require('glmnet')
  
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0.5)
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.1se')
  
  return(impute_values)
}


mod_scad = function(scaled_df){
  require('ncvreg')
  
  fit = cv.ncvreg(scaled_df$x_train, scaled_df$y_train, penalty = 'SCAD')
  impute_values = predict(fit, scaled_df$x_test, lambda = fit$lambda.min)
  
  return(impute_values)
}

mod_ridge = function(scaled_df){
  require('glmnet')
  
  #set.seed('12345')
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0) #
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.1se')
  
  return(impute_values)
}

mod_lasso = function(scaled_df){
  require('glmnet')
  
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 1)
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.1se')
  
  return(impute_values)
}

mod_mgaussian = function(scaled_df, alpha = 0){
  require('glmnet')
  
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = alpha, family = 'mgaussian')
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.1se')[,,1]
  
  return(impute_values)
}

mod_mgaussian_ada = function(scaled_df, index = 1){
  require('glmnet')
  
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0, family = 'mgaussian')
  temp_coefs = fit$glmnet.fit$beta[[index]][, which(fit$lambda == fit$lambda.min)]
  
  
  fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0, family = 'mgaussian', penalty.factor = 1/abs(temp_coefs))
  temp_lambda = fit$lambda
  
  while(fit$lambda.min == fit$lambda.1se){
    temp_lambda = temp_lambda / 2
    fit = cv.glmnet(scaled_df$x_train, scaled_df$y_train, alpha = 0, family = 'mgaussian', penalty.factor = 1/abs(temp_coefs), lambda = temp_lambda)
  }
  
  impute_values = predict(fit, newx = scaled_df$x_test, s = 'lambda.1se')[,,1]
  
  return(impute_values)
}





scale_data = function(x_train, y_train, x_test, y_test){
  y_dim = dim(y_train)
  
  if(is.null(y_dim)){
    y_train_mean = mean(y_train)
    y_train_sd = sd(y_train)
    y_train_scaled = (y_train - y_train_mean) / y_train_sd
    
    y_test_scaled = (y_test - y_train_mean) / y_train_sd
  }else{
    y_train_mean = apply(y_train, 2, mean)
    y_train_sd = apply(y_train, 2, sd)
    y_train_scaled = scale(y_train)
    
    y_test_scaled = scale(y_test, center = y_train_mean, scale = y_train_sd)
  }
  
  x_train_mean = apply(x_train, 2, mean)
  x_train_sd = apply(x_train, 2, sd)
  x_train_scaled = scale(x_train)
  
  x_test_scaled = scale(x_test, center = x_train_mean, scale = x_train_sd)
  
  result = list(
    'x_train' = x_train_scaled,
    'y_train' = y_train_scaled,
    'x_test' = x_test_scaled,
    'y_test' = y_test_scaled,
    'x_mean' = x_train_mean,
    'x_sd' = x_train_sd,
    'y_mean' = y_train_mean,
    'y_sd' = y_train_sd
  )
  return(result)
}

update_prediction_scale = function(train_info, pred_val){
  
  if(is.null(dim(pred_val))){
    pred_scaled = pred_val * train_info$y_sd + train_info$y_mean
  }else{
    pred_scaled = sweep(pred_val, 2, train_info$y_sd, '*')
    pred_scaled = sweep(pred_scaled, 2, train_info$y_mean, '+')
  }
  
  return(pred_scaled)
}

build_sets = function(df, split_percent = .8, response_vec = 1, to_matrix = T, seed_val = '12345', scale_all = T){
  
  train_split = round(dim(df)[1] * split_percent)
  
  set.seed(seed_val)
  train_set = sample(1:dim(df)[1], train_split, replace = F)
  
  test_set = (1:dim(df)[1])[-train_set]

  y_train = df[train_set, response_vec]
  y_test = df[test_set, response_vec]
  
  x_train = df[train_set, -response_vec]
  x_test = df[test_set, -response_vec]
  
  df_scaled = scale_data(x_train, y_train, x_test, y_test)
  
  result = df_scaled
  result$y_test_no_scale = y_test
  result$response_vec = response_vec
  
  if(!scale_all){
    result$x_train = x_train
    result$y_train = y_train
    result$x_test = x_test
    result$y_test = y_test
  }
  
  return(result)
}

mouse = R6::R6Class('mouse', list(
  X = NULL,
  n = NULL,
  p = NULL,
  q = NULL,
  X_scaled = NULL,
  y_test = NULL,
  missing_data = NULL,
  current_column_name = NULL,
  current_column_index = NULL,
  current_row_index = NULL,
  X_old = NULL, 
  max_iterations = NULL,
  stopping_criterion = NULL,
  iterations_performed = 0,
  prediction_algorithm = NULL,
  initialize = function(X, prediction_algorithm, max_iterations = 10, stopping_criterion = 1e-5){
    self$prediction_algorithm = prediction_algorithm
    self$max_iterations = max_iterations
    self$stopping_criterion = stopping_criterion
    
    temp_X = as.matrix(X)
    self$n = dim(temp_X)[1]
    self$p = dim(temp_X)[2]
    
    temp_missing_data = list()
    for(j in 1:self$p){
      NAs_found = which(is.na(temp_X[,j]) == T)
      if(length(NAs_found) != 0){
        
        current_mean = mean(temp_X[, j], na.rm = T)
        temp_list = list(
          'index' = NAs_found, 
          'value' = rep(current_mean, length(NAs_found))
        )
        temp_X[NAs_found, j] = current_mean
        temp_missing_data[[as.character(j)]] = temp_list
      }
    }
    
    # setting initial values using ridge

    for(i in names(temp_missing_data)){
      temp_column_index = as.numeric(i)
      temp_row_index = temp_missing_data[[i]]$index
      
      temp_x_test = temp_X[temp_row_index, -temp_column_index]
      column_nas = apply(temp_x_test, 2, sum)
      column_nas = is.finite(column_nas)
      temp_x_test = temp_x_test[, column_nas]

      temp_x_train = temp_X[-temp_row_index,-temp_column_index]
      temp_x_train = temp_x_train[, column_nas]

      temp_y_train = temp_X[-temp_row_index, temp_column_index]

      fit = cv.glmnet(temp_x_train, temp_y_train, alpha = 0)
      pred_val = predict(fit, temp_x_test, s = 'lambda.min')
      temp_missing_data[[i]]$value = pred_val
    }

    for(i in names(temp_missing_data)){
      temp_column_index = as.numeric(i)
      temp_row_index = temp_missing_data[[i]]$index

      temp_X[temp_row_index, temp_column_index] = temp_missing_data[[i]]$value
    }

      
    self$X = temp_X
    self$q = length(temp_missing_data)
    
    self$missing_data = temp_missing_data
    self$X_old = temp_X * 100
    self$run()
    invisible(self)
  },
  set_column_response = function(input_index){
    if(is.numeric(input_index)){
      self$current_column_name = as.character(input_index)
      self$current_column_index = input_index
      
    }else{
      self$current_column_name = input_index
      self$current_column_index = as.numeric(input_index)
    }
    self$current_row_index = self$missing_data[[self$current_column_name]]$index
    invisible(self)
  },
  set_data_split = function(){
    # 
    # current_test_index = self$current_row_index
    # original_num_samples = dim(self$X)[1]
    # current_train_index = (1:original_num_samples)[-current_test_index]
    # len_train_index = length(current_train_index)
    # 
    # new_sample = sample(current_train_index, round(.2 * len_train_index))
    # 
    # new_test_index = c(current_test_index, new_sample)
    # new_train_index = (1:original_num_samples)[-new_test_index]
    # 
    # temp_X_train = self$X[new_train_index, -self$current_column_index]
    # temp_X_test = self$X[new_test_index, -self$current_column_index]
    # 
    # temp_y_train = self$X[new_train_index, self$current_column_index]
    # temp_y_test = self$X[new_test_index, self$current_column_index]
    
    temp_X_train = self$X[-self$current_row_index, -self$current_column_index] #self$current_column_index
    temp_X_test = self$X[self$current_row_index, -self$current_column_index] #self$current_column_index

    temp_y_train = self$X[-self$current_row_index, self$current_column_index]
    temp_y_test = self$X[self$current_row_index, self$current_column_index]
    
    self$X_scaled = scale_data(temp_X_train, temp_y_train, temp_X_test, temp_y_test)
    invisible(self)
  },
  update_X = function(){
    # this will always be included in the prediction algorithm
    # rescale the predictions
    
    updated_y_hat = update_prediction_scale(self$X_scaled, self$y_test)
    
    self$missing_data[[self$current_column_name]]$value = updated_y_hat

    # set new imputations
    self$X[self$current_row_index, self$current_column_index] = updated_y_hat
    invisible(self)
  },
  run = function(){
    for(iter in 1:self$max_iterations){
      self$iterations_performed = iter
      # 2. Regressing the missing values
      # random starting point for the missing data column

      random_columns_select = sample(names(self$missing_data))
      for(k in random_columns_select){
        self$set_column_response(k)
        self$set_data_split()
      
        prev_val = self$y_test
        self$y_test = self$prediction_algorithm(self$X_scaled)
      
      
        # if(k == '1'){
        #   #print(mean(self$X_scaled$x_test))
        #   #print(mean(self$y_test))
        # }
      
        # print(mean(self$X_scaled$y_mean))
        # print(mean(self$X_scaled$x_sd))
        # print(mean(self$X_scaled$y_sd))
      
        #print(as.character(self$current_row_index))
        # 
        # if(k == '1'){
        #   actual_y = c(40.46008, 37.83425, 43.06487, 41.83948, 41.76306, 40.61271, 41.56093,
        #                37.42656, 38.33466, 36.69829, 40.37133, 41.70115,
        #                40.98595, 38.40379, 36.27574, 38.80000, 40.97116,
        #                38.85117, 41.48655, 40.69707, 41.67805, 37.15873,
        #                37.01069, 36.91855, 35.86296, 36.67976, 36.09924,
        #                37.45337, 34.86377, 37.38489, 35.99122, 35.64997,
        #                31.84535, 36.10292, 35.58554, 41.48070, 39.51883,
        #                38.11953, 41.05300, 38.10000, 39.05000, 39.80000, 41.45000)
        #   print(sqrt(mean((actual_y - self$y_test)^2)))
        # }
      
        self$update_X()
      }
      
      #print(norm(as.matrix(self$X - self$X_old)))
      # breaking criterion if the values have converged before reaching the max number of iterations
      if(norm(self$X - self$X_old) < self$stopping_criterion){
        break
      }else{
        self$X_old = self$X
      }
    }
  }
))



mouse_basic = R6::R6Class('mouse', list(
  X = NULL,
  n = NULL,
  p = NULL,
  q = NULL,
  X_scaled = NULL,
  y_test = NULL,
  missing_data = NULL,
  current_column_name = NULL,
  current_column_index = NULL,
  current_row_index = NULL,
  X_old = NULL, 
  max_iterations = NULL,
  stopping_criterion = NULL,
  iterations_performed = 0,
  prediction_algorithm = NULL,
  initialize = function(X, prediction_algorithm, max_iterations = 30, stopping_criterion = 1e-5){
    self$prediction_algorithm = prediction_algorithm
    self$max_iterations = max_iterations
    self$stopping_criterion = stopping_criterion
    
    temp_X = as.matrix(X)
    self$n = dim(temp_X)[1]
    self$p = dim(temp_X)[2]
    
    temp_missing_data = list()
    for(j in 1:self$p){
      NAs_found = which(is.na(temp_X[,j]) == T)
      if(length(NAs_found) != 0){
        current_mean = mean(temp_X[, j], na.rm = T)
        temp_list = list(
          'index' = NAs_found, 
          'value' = rep(current_mean, length(NAs_found))
        )
        temp_X[NAs_found, j] = current_mean
        temp_missing_data[[as.character(j)]] = temp_list
      }
    }
    self$X = temp_X
    self$q = length(temp_missing_data)
    self$missing_data = temp_missing_data
    self$X_old = temp_X * 100
    self$run()
    invisible(self)
  },
  set_column_response = function(input_index){
    if(is.numeric(input_index)){
      self$current_column_name = as.character(input_index)
      self$current_column_index = input_index
      
    }else{
      self$current_column_name = input_index
      self$current_column_index = as.numeric(input_index)
    }
    self$current_row_index = self$missing_data[[self$current_column_name]]$index
    invisible(self)
  },
  set_data_split = function(){
    temp_X_train = self$X[-self$current_row_index, -self$current_column_index]
    temp_X_test = self$X[self$current_row_index, -self$current_column_index]
    temp_y_train = self$X[-self$current_row_index, self$current_column_index]
    temp_y_test = self$X[self$current_row_index, self$current_column_index]
    
    self$X_scaled = scale_data(temp_X_train, temp_y_train, temp_X_test, temp_y_test)
    invisible(self)
  },
  update_X = function(){
    # this will always be included in the prediction algorithm
    # rescale the predictions
    #updated_y_hat = update_prediction_scale(self$X_scaled, self$y_test)
    
    updated_y_hat = self$y_test

    self$missing_data[[self$current_column_name]]$value = updated_y_hat
    
    # set new imputations
    self$X[self$current_row_index, self$current_column_index] = updated_y_hat
    invisible(self)
  },
  run = function(){
    for(iter in 1:self$max_iterations){
      self$iterations_performed = iter
      # 2. Regressing the missing values
      # random starting point for the missing data column
      random_columns_select = names(self$missing_data)
      for(k in random_columns_select){
        self$set_column_response(k)
        #print(k)
        current_row_index = self$missing_data[[k]]$index
        
        current_column_index = as.numeric(k)
        
        x_train = self$X[-current_row_index, -current_column_index]
        
        x_test = self$X[current_row_index, -current_column_index]
        
        
        y_train = self$X[-current_row_index, current_column_index]
        
        
        
        y_test = self$X[current_row_index, current_column_index]
        
        # print(dim(x_train))
        # print(dim(x_test))
        # print(dim(y_train))
        # print(dim(x_test))
        
        temp_scaled = list(
          'x_train' = as.matrix(x_train),
          'y_train' = as.matrix(y_train),
          'x_test' = as.matrix(x_test)
        )
        
        #self$set_column_response(k)
        #self$set_data_split()
        self$y_test = self$prediction_algorithm(temp_scaled)
        
        self$missing_data[[self$current_column_name]]$value = self$y_test
        
        self$X[current_row_index, current_column_index] = self$y_test
        
        if(k == '1'){
          actual_y = c(40.46008, 37.83425, 43.06487, 41.83948, 41.76306, 40.61271, 41.56093,
                       37.42656, 38.33466, 36.69829, 40.37133, 41.70115,
                       40.98595, 38.40379, 36.27574, 38.80000, 40.97116,
                       38.85117, 41.48655, 40.69707, 41.67805, 37.15873,
                       37.01069, 36.91855, 35.86296, 36.67976, 36.09924,
                       37.45337, 34.86377, 37.38489, 35.99122, 35.64997,
                       31.84535, 36.10292, 35.58554, 41.48070, 39.51883,
                       38.11953, 41.05300, 38.10000, 39.05000, 39.80000, 41.45000)
          #print(sqrt(mean((actual_y - self$y_test)^2)))
          print(sum(y_train))
        }
        
        #self$update_X()
      }
      
      #print(norm(as.matrix(self$X - self$X_old)))
      # breaking criterion if the values have converged before reaching the max number of iterations
      if(norm(self$X - self$X_old) < self$stopping_criterion){
        break
      }else{
        self$X_old = self$X
      }
    }
  }
))







mice = R6::R6Class('mice',list(
  X = NULL,
  prediction_algorithm = NULL,
  num_replicates = 10,
  max_iterations = 30,
  stopping_criterion = 1e-5,
  list_of_replicates = list(), 
  imputation_results = list(),
  imputation_metrics = NULL,
  initialize = function(X, prediction_algorithm){
    self$X = X
    self$prediction_algorithm = prediction_algorithm
  },
  run = function(){
    for(m in 1:self$num_replicates){
      self$list_of_replicates[[m]] = mouse$new(self$X, self$prediction_algorithm, self$max_iterations, self$stopping_criterion)
    }
  },
  collect = function(){
    for(i in 1:self$num_replicates){
      for(j in names(self$list_of_replicates[[i]]$missing_data)){
        if(is.null(self$imputation_results[[j]])){
          temp_index = self$list_of_replicates[[i]]$missing_data[[j]]$index
          temp_matrix = matrix(rep(0, length(temp_index) * num_replicates),num_replicates, length(temp_index))
          temp_matrix = as.data.frame(temp_matrix)
          names(temp_matrix) = temp_index
          self$imputation_results[[j]] = temp_matrix
        }
        temp_value = self$list_of_replicates[[i]]$missing_data[[j]]$value
        self$imputation_results[[j]][i, ] = temp_value
      }
    }
  },
  set_summary_stats = function(){
    if(length(self$imputation_results) == 0){
      self$collect()
    }
    
    for(j in 1:self$imputation_results){
      temp_result = list()
      num_samples = dim(self$imputation_results[[j]])[1]
      temp_result$mean = apply(self$imputation_results[[j]], 2, mean)
      temp_result$sd = apply(self$imputation_results[[j]], 2, sd)
      # assuming 95% CI, z = 1.96
      standard_error = 1.96 * temp_result$sd / sqrt(num_samples)
      temp_result$upper_ci = temp_result$mean + standard_error
      temp_result$lower_ci = temp_result$mean - standard_error
      
      # this will be a dictionary in python
      self$imputation_metrics[[j]] = temp_result
    }
  }
))


# test = mouse$new(X = data_impute, prediction_algorithm = mod_elastic_net)
# test$run()
# test$missing_data[[test$current_column_name]]$index
# 
# #Error in self$X[self$missing_data[[self$current_column_name]]$index, self$current_column_index] = y_test : 
# #  number of items to replace is not a multiple of replacement length
# 
# test$X[test$missing_data[[test$current_column_name]]$index, test$current_column_index]

