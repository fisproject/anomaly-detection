require(car)
require(KernSmooth)

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

N <- length(Davis$weight)
mu <- mean(Davis$weight)
x <- Davis[,c("weight", "height")]
h <- c(dpik(x$weight), dpik(x$height)) # esitame kernel-bandwidth
est <- bkde2D(x, bandwidth=h, gridsize=c(10^3, 10^3)) # gridsize 1000
d <- list(x=est$x1, y=est$x2, z=est$fhat)

image(d, xlim=c(35, 110), ylim=c(145, 200), xlab="weight", ylab="height")
contour(d, add=T)
