require(MASS)
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
colnames(Xc) <- t(Cars93[,"Make"]) # car type
S <- Xc %*% t(Xc) #
evd <- eigen(S) # eigenvalue decomposition

d <- data.frame(x=c(1:15), y=evd$values)
p <- ggplot(d,
      aes(
        x=x,
        y=y
      )
    )
p <- p + geom_point() + geom_line() + labs(title="", x="index", y="eigen value")
plot(p)
ggsave("./img/PCA.png", p)

m <- 2
x2 <- t(evd$vectors[,1:m]) %*% Xc
a1 <- colSums(Xc*Xc) - colSums(x2*x2) # calculate anomalies

print(a1[order(a1, decreasing=T)[1:6]])
# Chevrolet Corvette        Honda Civic          Geo Metro Mercedes-Benz 300E
#          13.595830          11.829742          11.156367          10.586025
# Volkswagen Eurovan      Dodge Stealth
#           9.971148           8.727322

G <- t(Xc) %*% Xc # Gram matrix
evd <- eigen(G)
Lam_12 <- diag(evd$values[1:m]^{-1/2})
xx2 <- Lam_12 %*% t(evd$vectors[,1:m]) %*% t(Xc) %*% Xc # normal
aa1 <- colSums(Xc*Xc) - colSums(xx2*xx2) # calculate anomalies

print(a1[order(a1, decreasing=T)[1:3]])
# Chevrolet Corvette        Honda Civic          Geo Metro
#           13.59583           11.82974           11.15637
