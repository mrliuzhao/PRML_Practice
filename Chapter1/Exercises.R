# 1.4 复合函数概率密度关系
# x ~ N(6,1)
xSeq <- seq(from = 6 - 8, to = 6 + 8, length.out = 10000)
x <- dnorm(x = xSeq, mean = 6, sd = 1)
xSample <- rnorm(50000, mean = 6, sd = 1)
hist(x = xSample, breaks = 100, freq = FALSE)
lines(x = xSeq, y = x, col = 'red')

# x = g(y) = lny - ln(1-y) +5
# y = 1 / (1+exp(-x+5))
ySeq <- 1 / (1 + exp(-xSeq + 5))
# f(x) = N(6,1) = f[g(y)]
y <- x
ySample <- 1 / (1 + exp(-xSample + 5))
hist(x = ySample, breaks = 100, freq = FALSE)
lines(x = ySeq, y = y, col = 'blue')
# h(y) = f[g(y)]*g'(y)
y <- (1/ySeq + 1/(1-ySeq)) * x
lines(x = ySeq, y = y, col = 'red')

legend("topleft",
       legend = c("f[g(y)]", 
                  'f[g(y)]g\'(y)'),
       lty = 1,
       col = c('blue','red'))





