# 生成带有正态抖动的线性关系
n <- 100
x <- runif(n, min = 0, max = 10)
deltas <- rnorm(n = n, mean = 0, sd = 5)
y <- 3*x + 4 + deltas
plot(x = x, y = y)
lines(x = c(0,10), y = c(4,34))

# 进行一元线性拟合
mod <- lm(y ~ x)
a <- as.vector(mod$coefficients)[1]
b <- as.vector(mod$coefficients)[2]
yFit <- a + b*x
lines(x = sort(x),y = yFit[order(x)], col = 'red')

# 查看线性拟合的95%置信区间
conf <- confint(mod, level = 0.95)
yFit.low <- conf[1,1] + conf[2,1]*x
yFit.high <- conf[1,2] + conf[2,2]*x
lines(x = sort(x),y = yFit.low[order(x)], col = 'green')
lines(x = sort(x),y = yFit.high[order(x)], col = 'green')


# 估计均方误差
res <- (yFit - y)^2
Qe <- sum(res)
sigmaFit <- sqrt(Qe / (n-2))
Sxx <- var(x)*(n-1)

# 绘制预测区间
confLev <- 0.7
alpha <- 1-confLev
tReg <- qt(p = c(alpha/2), df = n-2, lower.tail = TRUE)
xSeq <- seq(from = 5-100,to = 5+100, length.out = 10000)
temp <- (xSeq - mean(x))^2 / Sxx
addOn <- tReg * sigmaFit * sqrt(1 + (1/n) + temp)
yFit.low <- a + b*xSeq + addOn
yFit.high <- a + b*xSeq - addOn
plot(x = xSeq, y = yFit.low, type = 'l',
     ylim = c(min(yFit.low),max(yFit.high)), col = 'green')
lines(x = xSeq, y = yFit.high, col = 'green')
lines(x = xSeq, y = (3*xSeq + 4))
lines(x = xSeq, y = (a + b*xSeq), col = 'red')

lines(x = xSeq, y = yFit.low, col = 'blue')
lines(x = xSeq, y = yFit.high, col = 'blue')

plot(x = xSeq, y = addOn)
plot(x = xSeq, y = yFit.low, type = 'l')
lines(x = xSeq, y = yFit.high)


