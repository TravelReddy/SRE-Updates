# normalized capacity

norm_capacity = capacity
m = max(norm_capacity)
norm_capacity =( norm_capacity - m )/ (- 2 * m)

# M in the equation is our capacity term - 
# b range has 100 values between 0.4 and 0.7 
# a is our cycles and a to the power of b is cycles for each of the b matrix 
#norm_capacity = norm_capacity / (max(norm_capacity) + 1e-3)
plot(norm_capacity)

b_range = seq(0.4,0.7, length.out = 100)
b_matrix = matrix(rep(0, num_cycles * length(b_range)), num_cycles, length(b_range))

for(i in 1:length(b_range)){
  b_matrix[,i] = cycles^b_range[i]
}

# fit glm net  .. 
fit = cv.glmnet(b_matrix, norm_capacity, family = quasibinomial(), alpha = .5)
fit$glmnet.fit$beta[,which(fit$lambda == fit$lambda.min)]
pred_val = predict(fit, b_matrix, s = 'lambda.min', type = 'response')

fit = glm(norm_capacity ~ b_matrix[,95] - 1, family = 'quasibinomial')

plot(norm_capacity)
lines(fit$fitted.values, col = 'red')

a = .072
b = b_range[95]
m


