---
title: "Homework 1"
output: pdf_document
---

# EXERCISE 1

```r
library(pdfCluster)
```

```
## pdfCluster 1.0-3
```

```r
data(oliveoil)
olive <- oliveoil[,3:10]
str(olive)
###unscaled k=3
set.seed(665544)
olivek3 <- kmeans(olive,centers=3,nstart=100)
olivek3$centers
table(olivek3$cluster,oliveoil$macro.area)

###scaled k=3

solive=scale(olive)
set.seed(665544)
solivek3 <- kmeans(solive,centers=3,nstart=100)
solivek3$centers
```

```r
table(olivek3$cluster,oliveoil$macro.area)
```

```
##    
##     South Sardinia Centre.North
##   1    42        0          134
##   2   190       22            0
##   3    91       76           17
```

```r
table(solivek3$cluster,oliveoil$macro.area)
```

```
##    
##     South Sardinia Centre.North
##   1     2        0          121
##   2   102       98           30
##   3   219        0            0
```

Analyzing the output of the second table (the one concerning the scaled data), we can see that we have a pretty good clustering result in terms of matching the macro-areas, in fact the first cluster represents the Centre-North area, the second one Sardinia and the South and the third one the South. We can't say the same for the unscaled data, looking at the first table we can state that the clusters are more impure in terms of macro-area, so we can conclude that the clustering on scaled data looks better.

```r
### unscaled k=9
set.seed(665544)
olivek9 <- kmeans(olive,centers=9,nstart=100)
olivek9$centers



#scaled k=9
set.seed(665544)
solivek9 <- kmeans(solive,centers=9,nstart=100)
solivek9$centers
```


```r
table(olivek9$cluster,oliveoil$region)
```

```
##    
##     Apulia.north Calabria Apulia.south Sicily Sardinia.inland Sardinia.coast Liguria.east
##   1            2        0            0      5               0              0            0
##   2            0        0           75      2               0              0            0
##   3            0        0            3      1              65              0            0
##   4           13        0            0      2               0              0           16
##   5            0        3           77      7               0              0            0
##   6            1       48            5     11               0              0            2
##   7            9        5            0      7               0              0           32
##   8            0        0           30      1               0              0            0
##   9            0        0           16      0               0             33            0
##    
##     Liguria.west Umbria
##   1           39      0
##   2            0      0
##   3            3      0
##   4            2     51
##   5            0      0
##   6            1      0
##   7            5      0
##   8            0      0
##   9            0      0
```

```r
table(solivek9$cluster,oliveoil$region)
```

```
##    
##     Apulia.north Calabria Apulia.south Sicily Sardinia.inland Sardinia.coast Liguria.east
##   1            0        1            0      0               0              0           33
##   2            0        0          144      0               0              0            0
##   3            2       32           12     16               0              0            0
##   4            1        0            0      0               0              0            7
##   5            0        0           49      2               0              0            0
##   6            0        0            0      0               0              0           10
##   7            0        0            0      0              65             33            0
##   8            0       23            1     12               0              0            0
##   9           22        0            0      6               0              0            0
##    
##     Liguria.west Umbria
##   1            0      1
##   2            0      0
##   3            0      0
##   4            0     50
##   5            0      0
##   6           50      0
##   7            0      0
##   8            0      0
##   9            0      0
```

Again the results of the scaled data seem to produce a better result in terms of matching the clusters to the regions. Just to make some examples the second and fifth clusters represent south Apulia, the fourth one Umbria and so on.


# EXERCISE 2

```r
boston<-read.table("Boston.dat.txt",header = T,sep="")
```

```
## Warning in file(file, "rt"): cannot open file 'Boston.dat.txt': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
str(boston)
```

```
## Error in str(boston): object 'boston' not found
```

```r
bostonk=boston[,-4]
```

```
## Error in eval(expr, envir, enclos): object 'boston' not found
```

```r
str(bostonk) 
```

```
## Error in str(bostonk): object 'bostonk' not found
```

```r
sboston<-scale(bostonk)
```

```
## Error in scale(bostonk): object 'bostonk' not found
```

```r
sprboston<- princomp(sboston)
```

```
## Error in princomp(sboston): object 'sboston' not found
```


```r
plot(sprboston,main="Standardised boston principal components")
```

```
## Error in plot(sprboston, main = "Standardised boston principal components"): object 'sprboston' not found
```

It seems like most of the variance is explained by the first two components.


```r
biplot(sprboston,cex=0.7) 
```

```
## Error in biplot(sprboston, cex = 0.7): object 'sprboston' not found
```
From this grapgh it's very hard to infer the number of clusters, we squeezed 10 dimensions in just two, so useful information can be hidden by this representation.



```r
set.seed(665544)
k_sboston<-kmeans(sboston,centers=2,nstart=100)
```

```
## Error in as.matrix(x): object 'sboston' not found
```

```r
table(k_sboston$cluster,boston$chas)
```

```
## Error in table(k_sboston$cluster, boston$chas): object 'k_sboston' not found
```

It's clear that the clusters don't match the position with respect to the Charles River.


```r
unique(boston$rad)
```

```
## Error in unique(boston$rad): object 'boston' not found
```

```r
set.seed(665544)
k_sboston<-kmeans(sboston,centers=9,nstart=100)
```

```
## Error in as.matrix(x): object 'sboston' not found
```

```r
table(k_sboston$cluster,boston$rad)
```

```
## Error in table(k_sboston$cluster, boston$rad): object 'k_sboston' not found
```
Again the clusters do't match the index of accessibility to radial highways.



```r
k.max<-15

set.seed(665544)
wss <- sapply(1:k.max, 
              function(k){kmeans(sboston, k, nstart=50,iter.max = 15 )$tot.withinss})
```

```
## Error in as.matrix(x): object 'sboston' not found
```

```r
plot(1:k.max, wss,
     type="b", pch = 19, frame = T, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
```

```
## Error in plot(1:k.max, wss, type = "b", pch = 19, frame = T, xlab = "Number of clusters K", : object 'wss' not found
```

```r
set.seed(665544)

k_sboston<-kmeans(sboston,centers=6,nstart=100)
```

```
## Error in as.matrix(x): object 'sboston' not found
```

```r
k_sboston$centers
```

```
## Error in eval(expr, envir, enclos): object 'k_sboston' not found
```

Now I tried to use the elbow method in order to find a reasonoble number of clusters such that adding another one doesn't give a much better modeling to the data. Here the cutoff point it's not very evident so I opted for 6 as number of clusters. The reason for this decision is that after that point the total within sum of squares decreases linearly with respect to the number of clusters. The important consideration I need to make is that this method doesn't provide us a clear solution to the problem of finding a reasonable number of clusters.


```r
library(cluster)
set.seed(123)
cg1 <- clusGap(sboston,kmeans,K.max = 10,B=100,d.power=2,spaceH0="scaledPCA",nstart=100)
```

```
## Error in stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= : object 'sboston' not found
```

```r
print(cg1,method="globalSEmax",SE.factor=2)
```

```
## Error in print(cg1, method = "globalSEmax", SE.factor = 2): object 'cg1' not found
```

```r
print(cg1,method="Tibs2001SEmax",SE.factor=2)
```

```
## Error in print(cg1, method = "Tibs2001SEmax", SE.factor = 2): object 'cg1' not found
```

```r
plot(cg1,main="")
```

```
## Error in plot(cg1, main = ""): object 'cg1' not found
```
Again this method doesn't let us make a clear interpretation about the number of clusters. The "globalSEmax" and  "Tibs2001SEmax" criterion indicate that the number of clusters should be ten, but simply they are suggesting the solution with most clusters. 


# EXERCISE 2

The "kmeans++" method is an improvement of the basic one because it entroduces a smarter initialization of the centroids. By definition this are the steps of the algorithm:

1)Randomly select the first centroid from the data points.

2)For each data point compute its distance from the nearest, previously chosen centroid.

3)Select the next centroid from the data points such that the probability of choosing a point as centroid is directly proportional to its distance from the nearest, previously chosen centroid. (i.e. the point having maximum distance from the nearest centroid is most likely to be selected next as a centroid)

4)Repeat steps 2 and 3 untill k centroids have been sampled 

The main idea of the algorithm is to initiliaze the centroids such that they are as far as possible from each other and this increases the chance that they will lie in different clusters. Using "kmeans++" makes the start of the algorithm more computationally demanding, but it also makes the convergence faster.




```r
set.seed(123466)
k_sboston<-kmeans(sboston,centers=6,nstart=100)
```

```
## Error in as.matrix(x): object 'sboston' not found
```

```r
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
```

```
## Error in nrow(X): object 'sboston' not found
```

```r
kmpp.sboston$tot.withinss
```

```
## Error in eval(expr, envir, enclos): object 'kmpp.sboston' not found
```

```r
k_sboston$tot.withinss
```

```
## Error in eval(expr, envir, enclos): object 'k_sboston' not found
```

As we can see the Total within-cluster sum of squares obtained from kmeans is slightly less then the one of kmeans++. This can be interpreted as an unexpected result but the reason of this lies in the fact that we set "nstart"=100 so the randomness of the initialization of the algorithm and the resulting problems are downsized. Let's try with "nstart=1"

```r
set.seed(123466)
k_sboston<-kmeans(sboston,centers=6,nstart=1)
```

```
## Error in as.matrix(x): object 'sboston' not found
```

```r
kmpp.sboston$tot.withinss
```

```
## Error in eval(expr, envir, enclos): object 'kmpp.sboston' not found
```

```r
k_sboston$tot.withinss
```

```
## Error in eval(expr, envir, enclos): object 'k_sboston' not found
```

Now it's clear that the Total within-cluster sum of squares is 
substantially greater in the case of kmeans
