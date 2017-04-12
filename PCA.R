Dir = '/Users/apple/Documents/Study/Biology/courses/ComputationalGenomics/assignments/pharmacogenomics'
setwd(Dir)

total = read.table('total.csv', header = TRUE, sep = ',', colClasses = c('character', rep('double', 23)))
PTDown = read.table('GSE31432_Pertuzumab&TrastuzumabDown.csv', header = TRUE, sep = ',' ,colClasses = 'character')
PTUp = read.table('GSE31432_Pertuzumab&TrastuzumabUp.csv', header = TRUE, sep = ',' ,colClasses = 'character')

rownames(total) = total$X
total = total[, -1]

varied = unique(c(PTDown$X, PTUp$X))
varied = unique(c(PTDown$X))

Deg = total[varied, ]

untreated = apply(as.matrix(Deg[, c(1:3, 13:15)]), 1, mean)
TrastuzumabPertuzumab = apply(rbind(Deg[, c(10:12, 22:23)]), 1, mean)

expMtrx = cbind(untreated, TrastuzumabPertuzumab)

pca = princomp(expMtrx, cor = TRUE)

pca$loadings
pca$scores

plot(pca)
biplot(pca)
