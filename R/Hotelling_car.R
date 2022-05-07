library(car)
data(Davis)
Davis

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

