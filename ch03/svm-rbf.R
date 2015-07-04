require(kernlab)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

x <- rbind(
    matrix(rnorm(120), ncol=2),
    matrix(rnorm(120, mean=3), ncol=2)
  )

x <- scale(x)

model <- ksvm(
          x,
          type="one-svc",
          kernel=rbfdot(sigma=0.5),
          nu=0.1
        )

colorcode <- rep(0, nrow(x))
colorcode[model@alphaindex] <- 1

x <- as.data.frame(cbind(x, colorcode))

p <- ggplot(
  x,
  aes (
    x = V1,
    y = V2,
    colour=colorcode
  )
)
p <- p + geom_point()
plot(p)
ggsave("./img/svm-rbf.png", p)
