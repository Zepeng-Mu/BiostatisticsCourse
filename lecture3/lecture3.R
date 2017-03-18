Lines <-
  "ID x y
1 208 197
2 202 150
3 203 255
4 200 134
5 205 266
6 206 200
7 207 189
8 208 186
9 203 215
10 210 199"

weight <- read.table(con <- textConnection(Lines), header = TRUE)
close(con)

weight
head(weight)

t1 = t.test(weight$x, weight$y, var.equal = FALSE)
t2 = t.test(weight$x, mu = 200)
sx = shapiro.test(weight$x)
sy = shapiro.test(weight$y)

print(t1)
print(t2)
print(sx)
print(sy)

setwd('/Users/apple/Documents/GitHub/BiostatisticsCourse')
hist(weight$y, col = 'lightblue', border = 'pink', main = 'Histogram of y', xlab = 'Weight')
jpeg(file = "lecture3.jpeg")
dev.off()
