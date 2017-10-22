nshow <- read_csv("C:/Users/Francisco/Dropbox/IST-MEIC/2ºano/1ºsemestre/SAD/Projecto/noshows.csv")
library(dplyr)
nsData = nshow[, 3:13]
nsData$ScheduledDay <- NULL
nsData = mutate_if(nsData, is.character, as.factor)
View(nsData)
Gen_dummies = dummy(nsData$Gender)
nsData$AppointmentDay <- as.factor(nsData$AppointmentDay)
Neigh_dummies = dummy(nsData$Neighbourhood)
Apoi_dummies = dummy(nsData$AppointmentDay)
nsData$Neighbourhood <- NULL
nsData$AppointmentDay <- NULL
nsData$Gender <- NULL
nsData = cbind(nsData, Gen_dummies)
nsData = cbind(nsData, Apoi_dummies)
nsData = cbind(nsData, Neigh_dummies)
dim(nsData)
str(nsData)
View(nsData)
pca1ns <- princomp(nsData, cor=TRUE)
#plot(pca1ns)
#plot(pca1ns, type='l')
summary(pca1ns)



pcaFns <- prcomp(nsData)
PCAcomps <- data.frame(pcaFns$x[,1:5])
#plot(PCAcomps, pch=25)

probClusters <- (nrow(nsData)-1)*sum(apply(nsData,2,var))
for (i in 2:15) probClusters[i] <- sum(kmeans(nsData, centers=i)$withinss)
#plot(1:15, probClusters, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

clusterNS <- kmeans(PCAcomps, 4, nstart=20, iter.max=2000)
View(clusterNS)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(PCAcomps, col=clusterNS$clust, pch=16)


