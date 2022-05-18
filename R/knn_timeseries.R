library(FNN)
X <- read.table(file="https://www.cs.ucr.edu/~eamonn/discords/qtdbsel102.txt")
dim(X)
head(X)
plot(x=1:length(X$V2), y=X$V2, xlim=c(4000,5000), type='l')

w <- 100 # 窓幅
nk <- 1 # k近傍探す
Xtr <- X[1:3000, 2] # 2列目の1~3000行目
Dtr <- embed(Xtr, w) # 1時点ずらして(len(data)-w+1)行作る
Xva <- X[3001:6000, 2]
D <- embed(Xva, w) # (2901, 100)
# 
# ベクトルDに一番近いベクトルをDtrから最近傍法でnk個探す
d <- knnx.dist(Dtr, D, k=nk) # (2901, 1)
d[1:3]
a <- d[, 1]
plot(a, ylab="anomaly score", type='l')
plot(a, ylab="anomaly score", xlim=c(1000, 1500), type='l')


# 窓幅1000
w <- 1000
Dtr <- embed(Xtr, w)
D <- embed(Xva, w)
dim(Dtr)
dim(D)
d <- knnx.dist(Dtr, D, k=nk) 
a <- d[, 1]
plot(a, ylab="anomaly score", type='l')

plot(x=1:length(X$V2), y=X$V2, xlim=c(0,500), type='l')



