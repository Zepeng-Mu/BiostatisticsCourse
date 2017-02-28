obs = c(7,5,6,6,7,5,3,4,5,8,2,4,5,6,7,6,4,5,9,3,6,4)

hist(obs, freq = F)

mean = mean(obs)
SD = sd(obs)

x = seq(min(obs), max(obs), by = .1)
norm = dnorm(x, mean = mean, sd = SD)
lines(x, norm, type = 'l', col = 'red')
abline(v = c(qnorm(.025, mean, SD), qnorm(.975, mean, SD)), col = 'brown')
abline(v=c(mean - 2*SD, mean + 2*SD), col = 'brown')

