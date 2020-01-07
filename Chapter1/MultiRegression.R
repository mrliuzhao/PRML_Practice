# 生成10个 sin(2 pi x)，加上服从正态分布的随机抖动
x <- runif(10)
deltas <- rnorm(n = length(x), mean = 0, sd = 0.1)
y <- sin(2 * pi * x) + deltas
seq1000 <- seq(from = 0, to = 1, length.out = 1000)
plot(x = sort(x),y = y[order(x)],type = 'p', 
     xlab = 'x', ylab = 'y',
     xlim = c(0,1), ylim = c(min(y,-1.0), max(y,1.0)))
lines(x = seq1000, y = sin(2 * pi * seq1000),col='red')


# 通过多项式拟合
# 注意poly函数使用的是正交多项式！
mod <- lm(y~poly(x,3))  
mod2 <- lm(y~x + I(x^2) + I(x^3))
co <- as.vector(mod$coefficients)
co2 <- as.vector(mod2$coefficients)
yFit <- mod$fitted.values
yFit2 <- mod2$fitted.values
points(x = x,y = yFit, col = 'green')
lines(x = seq1000, y = predict(mod, data.frame(x=seq1000)), col = 'green')
lines(x = seq1000, col = 'blue',
      y = co2[1] + (co2[2] * seq1000) + 
        (co2[3] * (seq1000^2)) + (co2[4] * (seq1000^3)))
rms <- sum(mod2$residuals ^ 2)

# 查看过拟合
mod <- lm(y~poly(x,9))  
yFit <- mod$fitted.values
plot(x = sort(x),y = y[order(x)],type = 'p', 
     xlab = 'x', ylab = 'y',
     xlim = c(0,1), ylim = c(min(y,yFit), max(y,yFit)))
points(x = x,y = yFit, col = 'green')
lines(x = seq1000, y = predict(mod, data.frame(x=seq1000)), col = 'green')
rms <- sum(mod$residuals ^ 2)






