dta <- read.table( "DATA/fromage.txt", header=TRUE, row.names=1)

# HAC
n <- nrow(dta)
dta_standard <- as.data.frame(scale(dta, center=TRUE,scale=TRUE)*sqrt(n/(n-1)))
d <- dist(dta_standard)
tree <- hclust(d^2/2*n, method="ward.D")
plot(rev(tree$height), type="h")

# cutting tree in classes
K <- 4 # number of classes
h.clust <- cutree(tree,k=K)
h.clust <- as.factor(h.clust)

# dta + classes
dta_hac <- data.frame(class=h.clust, dta)

library(FactoMineR)
catdes(dta_hac, num.var=1)
