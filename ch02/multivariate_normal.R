require(ggplot2)
require(car)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

summary(Davis)
# sex         weight          height          repwt            repht
# F:112   Min.   : 39.0   Min.   : 57.0   Min.   : 41.00   Min.   :148.0
# M: 88   1st Qu.: 55.0   1st Qu.:164.0   1st Qu.: 55.00   1st Qu.:160.5
#         Median : 63.0   Median :169.5   Median : 63.00   Median :168.0
#         Mean   : 65.8   Mean   :170.0   Mean   : 65.62   Mean   :168.5
#         3rd Qu.: 74.0   3rd Qu.:177.2   3rd Qu.: 73.50   3rd Qu.:175.0
#         Max.   :166.0   Max.   :197.0   Max.   :124.00   Max.   :200.0
#                                         NA's   :17       NA's   :17

d <- data.frame(h=Davis$height, w=Davis$weight)

p <- ggplot(d,
      aes(
        x=d$h,
        y=d$w
      )
    )

p <- p + geom_point() + labs(title="", x="height", y="weight")
plot(p)

# hotelling
X <- cbind(Davis$height, Davis$weight)
mx <- colMeans(X) # sample mean
Xc <- X - matrix(1, nrow(X), 1) %*% mx # centration
Sx <- t(Xc) %*% Xc / nrow(X) # sample Variance-covariance matrix
a <- rowSums((Xc %*% solve(Sx)) * Xc) # anomaly
th <-  qchisq(0.99, 1) # threshold

dd <- data.frame(a)

p <- ggplot(dd,
      aes(
        x=c(1:200),
        y=dd$a
      )
    )
p <- p + geom_point() + geom_hline(yintercept=th, linetype="dashed", colour="blue") + labs(title="", x="sample number", y="anomaly")
plot(p)
ggsave("multivariate-normal-anomaly.png", p)
