require(ggplot2)

# change working directory
theme_set(theme_gray(base_family="HiraMaruProN-W4"))
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/score.csv", header = T)
head(d)
# name score anomaly
# 1   x1  0.19   FALSE
# 2   x2  0.86    TRUE
# 3   x3  0.17   FALSE
# 4   x4  0.12   FALSE
# 5   x5  0.04   FALSE
# 6   x6  0.78    TRUE

p <- ggplot(d,
      aes(
        x = name,
        y = score,
        fill = anomaly,
      )
    )
p <- p + geom_bar(stat = "identity") +
    labs(title = "", x = "index", y = "anomaly score")
plot(p)
ggsave("./img/score.png", p)

sorted <- d[order(d$score, decreasing=T),]

p <- ggplot(sorted,
      aes(
        x=c(1:nrow(sorted)),
        y=score,
        fill=anomaly,
      )
    )
p <- p + geom_bar(stat = "identity") +
    labs(title = "sorted", x = "", y = "score")
plot(p)
ggsave("./img/sorted-score.png", p)

# ROC
anomaly <- d$anomaly
anomaly_sorted <- sorted$anomaly

n_total <- length(anomaly)
n_anom <- sum(anomaly)
n_norm <- n_total - n_anom

coverage <- rep(0, n_total)
detection <- rep(1, n_total)

for(i in c(1:n_total)) {
  n_detecedAnom <- sum(anomaly_sorted[1:i])
  n_detecedNorm <- (n_total-i) - sum (anomaly_sorted[-(1:i)])
  coverage[i] <- n_detecedAnom / n_anom
  detection[i] <- n_detecedNorm / n_norm
}

roc <- data.frame(x = detection, y = coverage)

p <- ggplot(roc,
      aes(
        x=1-x,
        y=y
      )
    )
p <- p + geom_point() +
    geom_line() +
    labs(title = "ROC", x = "誤報率", y = "異常網羅率")
plot(p)
ggsave("./img/ROC.png", p)
