# 1. write the equation that governs the SRE
# M - 2M (1 / (1 + exp((a * x)^b)))
SRE = function(cycles, a, b, m){
  f = m - 2 * m * (1 / (1 + exp((a * cycles)^b)))
  return(f)
}

rmse = function(x, y){
  return(sqrt(mean((x - y)^2)))
}

r2 = function(actual, predicted){
  SS_tot = sum((actual - mean(actual))^2)
  SS_reg = sum((actual - predicted)^2)
  return(1 - SS_reg / SS_tot)
}

# 2. create a range of values for a, b, and m
num_a = 100
num_b = 1
a_range = seq(.02, .03, length.out = num_a)
b_range = 0.6
#b_range = seq(.5, .7, length.out = num_b)
m_range = 20.5 # fixed

all_combinations = expand.grid(a_range, b_range)
names(all_combinations) = c('a', 'b')
num_combinations = dim(all_combinations)[1]

# estiamted SRE through cell sage: a = 0.0219, b = 0.600, m = 20.5

# 3. Create the data required for regression
# read in the data
cells = read.csv('xcel_cells.csv')

# selecting cell number 4
response = cells[which(cells$cell == 4), ]

# create data required for elastic net as a 
# series of different potential values of a, b, and m
cycles = response$cycles
capacity = response$capacity
num_cycles = length(cycles)
pred_matrix = matrix(
  rep(0,num_cycles * num_combinations),
  num_cycles,
  num_combinations
)

# applying the SRE for each different combination
for(i in 1:num_combinations){
  pred_matrix[, i] = SRE(
    cycles = cycles,
    a = all_combinations$a[i], 
    b = all_combinations$b[i],
    m = m_range
    )
}

#4.  elastic-net regression analysis

library(glmnet)

fit = glmnet(pred_matrix, capacity, alpha = .5)
temp_lambda = fit$lambda
new_lambda = exp(seq(log(temp_lambda[1]), log(temp_lambda[3]), length.out = 100))
fit = glmnet(pred_matrix, capacity, alpha = .5, lambda = new_lambda)
plot(fit)
coefs = fit$beta[, 1]
coefs[-which(coefs == 0)]

fit = lm(capacity ~ pred_matrix[,20])
fit$coefficients
pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')

rmse(capacity, pred_val)
r2(capacity, pred_val)


# adaptive lasso for consistent variable selection

fit = cv.glmnet(pred_matrix, capacity, alpha = 0)
temp_coefs = fit$glmnet.fit$beta[,which(fit$lambda == fit$lambda.min)]
fit = cv.glmnet(pred_matrix, capacity, alpha = 1, penalty.factor = 1/abs(temp_coefs))
plot(fit)
coefs = fit$glmnet.fit$beta[,which(fit$lambda == fit$lambda.1se)]
coefs[-which(coefs == 0)]

fit = lm(capacity ~ pred_matrix[,1])
fit$coefficients

pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)


# LASSO

fit = cv.glmnet(pred_matrix, capacity, alpha = 1)
temp_lambda = fit$lambda

temp_lambda = exp(seq(log(temp_lambda[1]), log(temp_lambda[5]), length.out = 100))
fit = cv.glmnet(pred_matrix, capacity, alpha = 1, lambda = temp_lambda)
coefs = fit$glmnet.fit$beta[,which(fit$lambda == fit$lambda.min)]
non_zeros = coefs[-which(coefs == 0)]
non_zeros

fit = lm(capacity ~ pred_matrix[,18])
fit$coefficients

pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)


# SCAD

library(ncvreg)
fit = cv.ncvreg(pred_matrix, capacity, penalty = 'SCAD')
coefs = fit$fit$beta[,which(fit$lambda == fit$lambda.min)]
non_zeros = coefs[-which(coefs == 0)]
non_zeros
pred_val = predict(fit, pred_matrix)

plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)

fit = lm(capacity ~ pred_matrix[,12])
fit$coefficients

pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)




fit = lm(capacity ~ pred_matrix[,30])
fit$coefficients

pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)


# fused lasso to deal with the block structure

library(genlasso)
fit = fusedlasso2d(capacity, pred_matrix, 10, 10)
plot(fit$beta[, 50])
#which(fit$beta[,50] > .02)


fit = lm(capacity ~ pred_matrix[,22] + pred_matrix[,2] + pred_matrix[,98] )
fit$coefficients
pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)


fit = lm(capacity ~ pred_matrix[,20])
fit$coefficients
pred_val = fit$fitted.values
plot(capacity)
lines(pred_val, col = 'red')
rmse(capacity, pred_val)
r2(capacity, pred_val)

