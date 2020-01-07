# 查看最大似然估计法计算出正态总体方差的差异
n <- 100
muTrue <- 0
sigmaTrue <- 2
x <- rnorm(n = n, mean = muTrue, sd = sqrt(sigmaTrue))
muML <- sum(x) / n
sigmaML <- sum((x - muML)^2)/n
# 真实曲线为黑色，拟合曲线为红色
seq1000 <- seq(from = muTrue - 3, to = muTrue + 3, 
               length.out = 1000)
plot(x = seq1000, col = 'black', type = 'l', ylim = c(0,0.5),
     y = dnorm(x = seq1000, mean = muTrue, sd = sqrt(sigmaTrue)))
lines(x = seq1000, col = 'red',
      y = dnorm(x = seq1000, mean = muML, sd = sqrt(sigmaML)))


