require(MASS)
require(kernlab)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

cc <- c("Min.Price", "Price", "Max.Price", "MPG.city", "MPG.highway", "EngineSize",
 "Horsepower", "RPM", "Rev.per.mile", "Fuel.tank.capacity","Length", "Wheelbase",
 "Width", "Turn.circle", "Weight")

mask <- is.element(colnames(Cars93), cc)
head(Cars93[,mask])
# Min.Price Price Max.Price MPG.city MPG.highway EngineSize Horsepower  RPM Rev.per.mile
# 1      12.9  15.9      18.8       25          31        1.8        140 6300         2890
# 2      29.2  33.9      38.7       18          25        3.2        200 5500         2335
# 3      25.9  29.1      32.3       20          26        2.8        172 5500         2280
# 4      30.8  37.7      44.6       19          26        2.8        172 5500         2535
# 5      23.7  30.0      36.2       22          30        3.5        208 5700         2545
# 6      14.2  15.7      17.3       22          31        2.2        110 5200         2565
# Fuel.tank.capacity Length Wheelbase Width Turn.circle Weight
# 1               13.2    177       102    68          37   2705
# 2               18.0    195       115    71          38   3560
# 3               16.9    180       102    67          37   3375
# 4               21.1    193       106    70          37   3405
# 5               21.1    186       109    69          39   3640
# 6               16.4    189       105    69          41   2880

Xc <- t(scale(Cars93[,mask])) # centralized

kpc <- kpca(
    t(Xc),
    kernel="rbfdot", # RBF kernel
    kpar=list(sigma=0.01),
    features=2
  )

Zt <- rotated(kpc) # subspace

d <- data.frame(x=Zt[,1], y=Zt[,2])
p <- ggplot(d,
      aes(
        x=x,
        y=y,
        label=c(1:93)
      )
    )
p <- p + geom_point() + geom_text() + labs(title="", x="1st principal component", y="2nd principal component")
plot(p)
ggsave("./img/KPCA-sigma-0.01.png", p)


kpc <- kpca(
    t(Xc),
    kernel="rbfdot", # RBF kernel
    kpar=list(sigma=0.1),
    features=2
  )

Zt <- rotated(kpc) # subspace

d <- data.frame(x=Zt[,1], y=Zt[,2])
p <- ggplot(d,
      aes(
        x=x,
        y=y,
        label=c(1:93)
      )
    )
p <- p + geom_point() + geom_text() + labs(title="", x="1st principal component", y="2nd principal component")
plot(p)
ggsave("./img/KPCA-sigma-0.1.png", p)
