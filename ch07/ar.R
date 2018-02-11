require(ggplot2)
require(reshape2)
require(stats)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

head(nottem, 50)
# [1] 40.6 40.8 44.4 46.7 54.1 58.5 57.7 56.4 54.3 50.5 42.9 39.8 44.2 39.8 45.1 47.0 54.1 58.7
# [19] 66.3 59.9 57.0 54.2 39.7 42.8 37.5 38.7 39.5 42.1 55.7 57.8 56.8 54.3 54.3 47.1 41.8 41.7
# [37] 41.8 40.1 42.9 45.8 49.2 52.7 64.2 59.6 54.4 49.2 36.3 37.6 39.3 37.5

D.train <- nottem[1:120]
xi <- nottem[121:240]

ar.model <- ar(D.train)
# Call:
# ar(x = D.train)
#
# Coefficients:
#       1        2        3        4        5        6        7        8
#  0.5880  -0.0234  -0.1972  -0.2599  -0.0802  -0.0617  -0.1332  -0.1593
#
# Order selected 8  sigma^2 estimated as  10.65

r <- ar.model$order
alpha <- ar.model$ar # AIC
xmean <- ar.model$x.mean # AIC
sig2 <- ar.model$var.pred
N <- length(xi) - r
X <- t(embed(xi-xmean, r))[,1:N] # slide window
ypred <- t(X) %*% alpha + xmean # prediction
y <- xi[(1+r):length(xi)]
anomaly <- (y - as.numeric(ypred))^2 / sig2

d <- melt(
      data.frame(x = c(1:length(anomaly)),
                 anomaly = anomaly,
                 temp_fahrenheit = nottem[1:length(anomaly)]),
      measure = c("anomaly", "temp_fahrenheit")
    )

p <- ggplot(d,
      aes(
        x = x,
        y = value,
        group = variable,
        colour = variable
      )
    )
p <- p + geom_line() +
    labs(title = "nottem", x = "time index", y = "")
plot(p)
ggsave("./img/ar-nottem.png", p)
