require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# μ
mu0 <- 3
mu1 <- 0
# σ
sig0 <- 0.5
sig1 <- 3
# π : mix rate
pi0 <- 0.6
pi1 <- 0.4

N <- 1000 # sample size

# Gaussian mixture distribution
attr <- sample(
          0:1,
          N,
          replace=T,
          prob=c(pi0, pi1)
        )

x <- rep(-99, N)
x[which(attr==0)] <- rnorm(length(which(attr==0)), mu0, sig0) # normal
x[which(attr==1)] <- rnorm(length(which(attr==1)), mu1, sig1) # noise

# μ
mu0 <- 5.0
mu1 <- -5.0
# σ
sig0 <- 1.0
sig1 <- 5.0
# π
pi0 <- 0.5
pi1 <- 0.5

for (iteration in 1:10) {
  piN0 <- pi0 * dnorm(x, mu0, sig0)
  piN1 <- pi1 * dnorm(x, mu1, sig1)
  qn0 <- piN0 / (piN0+piN1)
  qn1 <- piN1 / (piN0+piN1)
  pi0 <- sum(qn0) / N
  pi1 <- sum(qn1) / N
  mu0 <- sum(qn0*x) / (N*pi0)
  mu1 <- sum(qn1*x) / (N*pi1)
  sig0 <- sqrt(sum(qn0*(x-mu0)*(x-mu0)) / (N*pi0))
  sig1 <- sqrt(sum(qn1*(x-mu1)*(x-mu1)) / (N*pi1))
}

p <- qplot(x, geom="density")
p <- p + stat_function(fun=dnorm, colour="red", arg=list(mean=mu0, sd=sig0)) +
      stat_function(fun=dnorm, colour="blue", arg=list(mean=mu1, sd=sig1))
plot(p)
ggsave("./img/em.png", p)
