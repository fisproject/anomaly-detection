require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# wget http://www.cs.ucr.edu/~eamonn/discords/qtdbsel102.txt
dt <- read.table("data/qtdbsel102.txt")

# singular spectrum transformation
xi <- dt[3001:6000, 2]
w <- 50 # width
m <- 2
k <- w/2
L <- k/2 # lag
Tt <- length(xi)
score <- rep(0, Tt)

for(t in (w+k):(Tt-L+1)) {
  t.start <- t-w-k+1
  t.end <- t-1
  X1 <- t(embed(xi[t.start:t.end], w))[w:1,] # trajectory matrix
  X2 <- t(embed(xi[(t.start+L):(t.end+L)], w))[w:1,] # test matrix

  U1 <- svd(X1)$u[,1:m] # X11 SVD
  U2 <- svd(X2)$u[,1:m] # X12 SVD
  sig1 <- svd(t(U1) %*% U2)$d[1] # overlap
  score[t] <- 1 - sig1^2 # change score
}

df <- data.frame(x=c(1:3000), y=score)

p <- ggplot(df,
      aes(
        x=x,
        y=y
      )
    )
p <- p + geom_line() + labs(title="", x="time index", y="change")
plot(p)
ggsave("./img/stt.png", p)
