N <- 1000 # サンプル1000個
# 正常なデータ
mu0 = 3 
sig0 = 0.5
pi0 = 0.6
# 異常なデータ
mu1 = 0
sig1 = 3
pi1 = 0.4
attr <- sample(0:1, N, replace=T, prob=c(pi0,pi1)) # 各サンプルの真の出身(正常か異常か)
attr
x <- rep(-99, N) # -99で初期化した長さNの配列を用意
# 0出身は0の正規分布からデータ生成
x[which(attr == 0)] <- rnorm(length(which(attr == 0)), mu0, sig0)
# 1出身は1の正規分布からデータ生成
x[which(attr == 1)] <- rnorm(length(which(attr == 1)), mu1, sig1)

# パラメータの初期値は分からんので、よしなに設定
pi0=0.5; pi1=0.5
mu0=5.0; mu1=-5.0
sigma0=1.0; sigma1=5.0

# 期待値最大化法
for(iteration in 1:10){
  piN0 <- pi0*dnorm(x, mu0, sig0) # 正常データを観測する確率*正常分布でのx起きる確率
  piN1 <- pi1*dnorm(x, mu1, sig1) # 異常データを観測する確率*異常分布でのx起きる確率
  qn0 <- piN0/(piN0+piN1) # xが0の出身の確率
  qn1 <- piN1/(piN0+piN1) # xが1の出身の確率
  pi0 <- sum(qn0)/N # pi0の推定値(正常データを観測する確率)
  pi1 <- sum(qn1)/N # pi1の推定値(異常データを観測する確率)
  mu0 <- sum(qn0*x)/(N*pi0)
  mu1 <- sum(qn1*x)/(N*pi1)
  sig0 <- sqrt(sum(qn0*(x-mu0)*(x-mu0))/(N*pi0))
  sig1 <- sqrt(sum(qn1*(x-mu1)*(x-mu1))/(N*pi1))
}

# 正常なデータはnormal(mu0, sig0**2)に従っているとわかる
mu0
sig0









