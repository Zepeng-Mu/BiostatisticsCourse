machine = c(rep(1, 4), rep(2, 4), rep(3, 4))
machine = as.factor(machine)

workerList = c('Amy', 'Bob', 'Cooper', 'David')
workers = c(rep(workerList, 3))
workers = as.factor(workers)

quality = c(50, 47, 47, 53, 63, 54, 57, 58, 52, 42, 41, 48)

productQuality = data.frame(quality, machine, workers)
print(productQuality)

plot(productQuality$quality[1 : 11], productQuality$quality[2 : 12])

fit1 = aov(quality ~ machine + workers, data = productQuality)
summary(fit1)
par(mfrow = c(2, 2))
plot(fit1)

bartlett.test(quality ~ machine, data = productQuality)
bartlett.test(quality ~ workers, data = productQuality)

machine2 = rep(machine, 3)

workers2 = rep(workers, 3)

#qualityRep1 = quality + c(sample(-1:1, 4, replace = TRUE), sample(-5:0, 4, replace = TRUE), sample(0:5, 4, replace = TRUE))
#qualityRep2 = quality + c(sample(-1:1, 4, replace = TRUE), sample(-5:0, 4, replace = TRUE), sample(0:5, 4, replace = TRUE))
#quality2 = c(quality, qualityRep1, qualityRep2)

quality2 = c(50, 47, 47, 53, 63, 54, 57, 58, 52, 42, 41, 48, 51, 47, 47, 52, 59, 53, 56, 55, 55, 42, 43, 50, 50, 46, 46, 54, 58, 50, 54, 55, 54, 45, 45, 51)

productQuality2 = data.frame(quality2, machine2, workers2)
print(productQuality2)

plot(productQuality2$quality2[1 : 35], productQuality2$quality2[2 : 36])

fit2 = aov(quality2 ~ machine2 + workers2 + machine2 : workers2, data = productQuality2)
summary(fit2)
par(mfrow = c(2, 2))
plot(fit2)

bartlett.test(quality2 ~ machine2, data = productQuality)
bartlett.test(quality2 ~ workers2, data = productQuality)

TukeyHSD(fit2)$machine2

fit2m = aov(quality2 ~ machine2, data = productQuality2)
summary(fit2m)
par(mfrow = c(2, 2))
plot(fit2m)

fit2w = aov(quality2 ~ workers2, data = productQuality2)
summary(fit2w)
par(mfrow = c(2, 2))
plot(fit2w)
