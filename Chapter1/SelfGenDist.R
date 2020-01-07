# 生成正态分布的衍生分布
seq10000 <- seq(from = -10, to = 10, length.out = 10000)
plot(x = seq10000, y = dnorm(x = seq10000), type = 'l',
     main = '正态分布衍生分布', xlab = 'x', ylab = 'density')

a <- 0.5
b <- 0.5
d <- a * dnorm(x = seq10000, mean = -3, sd = 1) +
  b * dnorm(x = seq10000, mean = 3, sd = 1)
lines(x = seq10000, y = d, type = 'l', col = 'red')

a <- 0.5
b <- 1.5
d <- a * dnorm(x = seq10000, mean = -3, sd = 1) +
  b * dnorm(x = seq10000, mean = 3, sd = 1)
lines(x = seq10000, y = d/(a+b), type = 'l', col = 'blue')
legend("topright",
       legend = c("N(0,1)", 
                  '0.5N(-3,1)+0.5N(3,1)',
                  '(0.5N(-3,1)+1.5N(3,1))/2'),
       lty = 1,
       col = c('black','red', 'blue'))





