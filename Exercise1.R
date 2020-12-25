####EXERCISE1
library(pdfCluster)
data(oliveoil)
olive <- oliveoil[,3:10]
str(olive)
###unscaled k=3
set.seed(665544)
olivek3 <- kmeans(olive,centers=3,nstart=100)
olivek3$centers
table(olivek3$cluster,oliveoil$macro.area)

###scaled k=3
set.seed(665544)
solive=scale(olive)
solivek3 <- kmeans(solive,centers=3,nstart=100)
solivek3$centers
table(solivek3$cluster,oliveoil$macro.area)
table(olivek3$cluster,oliveoil$macro.area)

### unscaled k=9
set.seed(665544)
olivek9 <- kmeans(olive,centers=9,nstart=100)
olivek9$centers



#scaled k=9
set.seed(665544)
solivek9 <- kmeans(solive,centers=9,nstart=100)
solivek9$centers

table(olivek9$cluster,oliveoil$region)
table(solivek9$cluster,oliveoil$region)

#########Exercise 3
boston<-read.table("Boston.dat.txt",header = T,sep="")
str(boston)

bostonk=boston[,-4]
str(bostonk) 
sboston<-scale(bostonk)

sprboston<- princomp(sboston) # PCA
plot(sprboston,main="Standardised boston  principal components")
biplot(sprboston,cex=0.7) # Biplot with variable axes




k_sboston<-kmeans(sboston,centers=2,nstart=100)

table(k_sboston$cluster,boston$chas)

unique(boston$rad)
k_sboston<-kmeans(sboston,centers=9,nstart=100)
table(k_sboston$cluster,boston$rad)

k.max<-15

wss <- sapply(1:k.max, 
              function(k){kmeans(sboston, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = T, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
set.seed(123466)

k_sboston<-kmeans(sboston,centers=6,nstart=100)
k_sboston$centers

library(cluster)
set.seed(123456)
cg1 <- clusGap(sboston,kmeans,K.max = 10,B=100,d.power=2,spaceH0="scaledPCA",nstart=100)
print(cg1,method="globalSEmax",SE.factor=2)

plot(cg1,main="")

#EXERCISE 4

library(pracma)
kmpp <- function(X, k) {
  n <- nrow(X)
  C <- numeric(k)
  C[1] <- sample(1:n, 1)
  for (i in 2:k) {
    dm <- distmat(X, X[C, ])
    pr <- apply(dm, 1, min); pr[C] <- 0
    C[i] <- sample(1:n, 1, prob = pr)
  }
  kmeans(X, X[C, ])
}
set.seed(665544)

kmpp.sboston<-kmpp(sboston,6)
kmpp.sboston$tot.withinss
k_sboston$tot.withinss
