library(fpp)
install.packages('fpp2')
library(fpp2)
library(forecast)


par(mfrow=c(1,2))
Acf(goog, lag.max = 60)
Acf(ausbeer,lag.max=60)

autoplot(goog)
autoplot(ausbeer)



s_st=rnorm(100)
par(mfrow=c(1,2))
Acf(elec,lag.max=60)
Acf(s_st, lag.max = 60)


autoplot(elec)
plot.ts(s_st)


set.seed(123)
n <- 120                     # 10 years of monthly data
time <- 1:n

# Simulate strong seasonality: large sine wave
seasonal_pattern <- 20 * sin(2 * pi * time / 12)  # 12-month cycle
noise <- rnorm(n, mean = 0, sd = 2)               # small random noise

# Final series: seasonal + noise
s_s <- seasonal_pattern + noise


n <- 100
time <- 1:n
# Create a decreasing linear trend with noise
s_t <- -0.5 * time + rnorm(n, mean = 0, sd = 5)

par(mfrow=c(1,2))
Acf(s_t,lag.max = 60)
Acf(s_s, lag.max=60)

plot.ts(s_t)
plot.ts(s_s)
