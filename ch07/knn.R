require(FNN)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# wget http://www.cs.ucr.edu/~eamonn/discords/qtdbsel102.txt
dt <- read.table("data/qtdbsel102.txt")
width <- 100
nk <- 1

D.train <- embed(dt[1:3000, 2], width)
D.test <- embed(dt[3001:6000, 2], width)
d <- knnx.dist(D.train, D.test, k=nk) # k-nearest neighbor

df <- data.frame(x=c(1:(3000 - width + 1)), y=d[,1])

p <- ggplot(df,
      aes(
        x=x,
        y=y
      )
    )
p <- p + geom_line() + labs(title="", x="index", y="anomaly")
plot(p)
ggsave("./img/knn.png", p)
