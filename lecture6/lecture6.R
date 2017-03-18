ctrl = c(8.3811, 7.3764, 7.8559)
h = c(9.7472, 9.6442, 9.5276)
m = c(8.338, 7.7673)
hm = c(9.5393, 9.5088, 9.6644)

bcMicroArray = data.frame(expressionLevel = c(ctrl, h, m, hm),
                          Sample = factor(c(rep('Control', 3), rep('Her2', 3), rep('Myc', 2), rep('Her2+Myc', 3)))
                          )

options(digits = 3)
tapply(bcMicroArray$expressionLevel, bcMicroArray$Sample, mean)
tapply(bcMicroArray$expressionLevel, bcMicroArray$Sample, var)

boxplot(expressionLevel~Sample, data = bcMicroArray, xlab = 'Samples', ylab = 'Expression Level')

bartlett.test (expressionLevel~Sample, data=bcMicroArray)

fit = aov(expressionLevel~Sample, data = bcMicroArray)
summary(fit)

par(mfrow=c(2,2))
plot(fit)