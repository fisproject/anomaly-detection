require(car)
require(mclust)

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

X <- Davis[-12, c("weight", "height")]
res <- Mclust(X)
summary(res, parameters = T)
# ----------------------------------------------------
# Gaussian finite mixture model fitted by EM algorithm
# ----------------------------------------------------
#
# Mclust VEV (ellipsoidal, equal shape) model with 2 components:
#
#  log.likelihood   n df       BIC       ICL
#       -1402.653 199 10 -2858.238 -2921.798
#
# Clustering table:
#   1   2
#  78 121
#
# Mixing probabilities:
#         1         2
# 0.4313847 0.5686153
#
# Means:
#             [,1]      [,2]
# weight  75.80214  57.32628
# height 177.21662 165.55903
#
# Variances:
# [,,1]
#           weight   height
# weight 154.01612 49.82309
# height  49.82309 53.08884
# [,,2]
#          weight   height
# weight 45.44897 29.80516
# height 29.80516 41.76247

plot(res)

pi <- res$parameters$pro # mix rete
X <- Davis[, c("weight", "height")]
XX <- cdens(
      modelName = res$modelName,
      X,
      parameters = res$parameters
  )

anomaly <- -log(as.matrix(XX) %*% as.matrix(pi))

d <- data.frame(x = c(1:200), y = anomaly)

p <- ggplot(d,
      aes(
        x = x,
        y = y
      )
    )
p <- p + geom_point() +
    labs(title = "", x = "index", y = "anomaly")
plot(p)
ggsave("./img/mclust-anomaly.png", p)
