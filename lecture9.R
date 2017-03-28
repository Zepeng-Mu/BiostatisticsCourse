x = sample(seq(15, 30, by = 0.2), 60, rep = TRUE); x
xValue = unique(x); xValue

y = 3 * xValue + 7
yErr = sample(seq(-5, 5, by = 0.2), length(y), rep = TRUE); yErr
yValue = y + yErr; yValue

regData = data.frame(yValue, xValue)

reg = lm(yValue~xValue, data = regData)
s = summary(reg)
plot(xValue, yValue)
abline(reg)
plot(reg$fitted, reg$resid)
shapiro.test(reg$resid)
s$r.squared
