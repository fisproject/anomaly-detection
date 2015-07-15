require(MASS)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

X <- UScrime[,-c(2,16)]
M <- ncol(X)
y <- UScrime[,16]
N <- length(y)
head(cbind(X,y))
#     M  Ed Po1 Po2  LF  M.F Pop  NW  U1 U2 GDP Ineq     Prob    Time    y
# 1 151  91  58  56 510  950  33 301 108 41 394  261 0.084602 26.2011  791
# 2 143 113 103  95 583 1012  13 102  96 36 557  194 0.029599 25.2999 1635
# 3 142  89  45  44 533  969  18 219  94 33 318  250 0.083401 24.3006  578
# 4 136 121 149 141 577  994 157  80 102 39 673  167 0.015801 29.9012 1969
# 5 141 121 109 101 591  985  18  30  91 20 578  174 0.041399 21.2998 1234
# 6 121 110 118 115 547  964  25  44  84 29 689  126 0.034201 20.9995  682

model <- lm.ridge(
        y ~ .,
        cbind(X,y),
        lambda=seq(0,5,length=50)
      )

summary(model)
#        Length Class  Mode
# coef   700    -none- numeric
# scales  14    -none- numeric
# Inter    1    -none- numeric
# lambda  50    -none- numeric
# ym       1    -none- numeric
# xm      14    -none- numeric
# GCV     50    -none- numeric
# kHKB     1    -none- numeric
# kLW      1    -none- numeric

bestIndex <- which.min(model$GCV)
coefs <- coef(model)[bestIndex,]
lam <- model$lambda[bestIndex]
ypred <- as.matrix(X) %*% as.matrix(coefs[2:15]) + coefs[1]
yy <- data.frame(x=y, y=ypred)

p <- ggplot(yy,
      aes(
        x=x,
        y=y
      )
    )
p <- p + geom_point() + labs(title="", x="observed value", y="predictive value")
plot(p)
ggsave("./img/lm-ridge.png", p)

sig2 <- (lam*sum(coefs[2:15]^2) + sum(as.numeric(ypred)-y)^2) / N
X_ <- t(scale(X, scale=F))
H <- t(X_) %*% solve(X_ %*% t(X_) + lam * diag(M),X_)
TrHN <- sum(diag(H)) / N
a <- (as.numeric(ypred)-y)^2 / ((1-TrHN)*sig2) # anomaly
th <- sort(a)[N*(1-0.05)] # threshold
anomaly <- data.frame(a=a, th=th)

p <- ggplot(anomaly,
      aes(
        x=c(1:47),
        y=a
      )
    )
p <- p + geom_point() + geom_hline(yintercept=th, linetype="dashed", colour="blue") + labs(title="", x="sample index", y="anomaly")
plot(p)
ggsave("./img/anomary.png", p)
