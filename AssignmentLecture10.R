a = 2

xValue1 = unique(sample(seq(5, 20, by = 0.2), 60, rep = TRUE))
b1 = 17
y1 = a * xValue1 + b1
yErr1 = sample(seq(-6.5, 5, by = 0.2), length(y1), rep = TRUE)
yValue1 = y1 + yErr1
intercept1 = rep(b1, length(yValue1))

xValue2 = unique(sample(seq(5, 20, by = 0.2), 60, rep = TRUE))
b2 = 2
y2 = a * xValue2 + b2
yErr2 = sample(seq(-5, 6.5, by = 0.2), length(y2), rep = TRUE)
yValue2 = y2 + yErr2
intercept2 = rep(b2, length(yValue2))

xValue = c(xValue1, xValue2)
yValue = c(yValue1, yValue2)
intercept = c(intercept1, intercept2)
intercept = as.factor(intercept)
regData = data.frame(yValue, xValue, intercept)

plot(xValue, yValue)
reg = lm(yValue~xValue*intercept, data = regData)
summary(reg)
plot(residuals(reg))
abline(lm(yValue[intercept==b1]~xValue[intercept==b1]), lty=1, col='blue')
abline(lm(yValue[intercept==b2]~xValue[intercept==b2]), lty=1, col='red')
step(reg)
