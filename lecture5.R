sample = rnorm(300, mean = 1, sd = 7)
hist(sample)
m = mean(sample)
s = sd(sample)
n = length(sample)
diff <- qt(0.975, df = n-1)*s/sqrt(n)
left <- m-diff ; right <- m+diff
assumed <- m + 2
tleft <- (assumed - right)/(s/sqrt(n))
p <- pt(-tleft, df = n-1)
power = 1-p