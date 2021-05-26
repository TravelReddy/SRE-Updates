test = read.csv('../xcel_cells.csv')

capacity = test$capacity[which(test$cell == 16)]
cycles = test$cycles[which(test$cell == 16)]


#cycles = c(0,25,50,75,100,125,175,225,275,325,375,450)
#capacity = c(0,12.78135439,17.1789985,18.85630151,19.61333722,20.0294139,20.4976147,20.97086963,21.36114805,21.73976311,22.03629028,22.47788751)

plot(cycles, capacity)

sre = function(abm, cycles){
  a = abm[1]
  b = abm[2]
  m = abm[3]
  return(2 * m * (.5 - 1/(1 + exp((a * cycles)^b))))
}

sre_fit = function(parm, capacity, cycles){
  pred_val = sre(parm, cycles)
  result = sqrt(mean((capacity - pred_val)^2))
  return(result)
}

parm = c(0.2, .3, max(capacity))

plot(sre(parm, cycles))
fit = optim(parm, sre_fit, capacity = capacity, cycles = cycles)

plot(capacity)
lines(sre(fit$par, cycles), col = 'red')
fit$par

