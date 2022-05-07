library(car)
data(Davis)
Davis

### 単変量の場合
hist(Davis$weight)
plot(density(Davis$weight))

mu <- mean(Davis$weight) # 標本平均の計算
s2 <- mean((Davis$weight -  mu)^2) # 標本分散の計算
c(mu, s2)

a <- (Davis$weight - mu)^2 / s2 # 異常度の計算
# ある観測値x'の異常度aはサンプルサイズが十分大きい時、自由度1のカイ二乗分布に従う
th <- qchisq(0.99, 1) # 自由度1のカイ二乗分布の1%水準
plot(a, xlab="index", ylab="anomaly score") # 異常度プロット
lines(0:200, rep(th, length(0:200)), col="red", lty=2) # 閾値の線を書く



### 多変量の場合
X <- cbind(Davis$weight, Davis$height)
plot(X[, 1], X[, 2], pch=16, xlab="weight", ylab="height")

mx <- colMeans(X) # 標本平均ベクトル
mx
Xc <- X - matrix(1, nrow(X), 1) %*% mx # 各データと標本平均の差
Sx <- t(Xc) %*% Xc / nrow(X) # 標本共分散行列
a <- rowSums((Xc %*% solve(Sx)) * Xc) # 異常度(マハラノビス距離)
plot(a, xlab="index", ylab="anomaly score")
lines(0:200, rep(th, length(0:200)), col="red", lty=2)

a # (200, )





