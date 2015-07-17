require(CCA)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# Canonical Correspondence Analysis: CCA
data(nutrimouse)
head(nutrimouse$gene, 3)
# X36b4 ACAT1 ACAT2  ACBP  ACC1  ACC2 ACOTH ADISP ADSS1 ALDH3  AM2R
# 1 -0.42 -0.65 -0.84 -0.34 -1.29 -1.13 -0.93 -0.98 -1.19 -0.68 -0.59
# 2 -0.44 -0.68 -0.91 -0.32 -1.23 -1.06 -0.99 -0.97 -1.00 -0.62 -0.58
# 3 -0.48 -0.74 -1.10 -0.46 -1.30 -1.09 -1.06 -1.08 -1.18 -0.75 -0.66
#   AOX  BACT  BIEN  BSEP Bcl.3 C16SR  CACP  CAR1   CBS CIDEA  COX1
# 1 -0.16 -0.22 -0.89 -0.69 -1.18  1.66 -0.92 -0.97 -0.26 -1.21 -1.11
# 2 -0.12 -0.32 -0.88 -0.60 -1.07  1.65 -0.87 -0.92 -0.36 -1.17 -1.06
# 3 -0.16 -0.32 -0.89 -0.70 -1.17  1.57 -1.02 -0.98 -0.40 -1.29 -1.17
#  COX2  CPT2 CYP24 CYP26 CYP27a1 CYP27b1 CYP2b10 CYP2b13 CYP2c29
# 1 -1.18 -0.87 -1.37 -1.21   -0.71   -1.31   -1.23   -1.19   -0.06
# 2 -1.06 -0.87 -1.14 -1.12   -0.62   -1.14   -1.20   -1.06   -0.20
# 3 -1.14 -0.95 -1.30 -1.22   -0.78   -1.29   -1.32   -1.25   -0.30
# CYP3A11 CYP4A10 CYP4A14 CYP7a CYP8b1   FAS   FAT  FDFT   FXR G6PDH
# 1   -0.09   -0.81   -0.81 -0.77  -0.77 -0.41 -1.03 -0.98 -0.93 -1.22
# 2   -0.34   -0.88   -0.84 -0.71  -0.63 -0.37 -0.98 -0.92 -0.87 -1.09
# 3   -0.45   -0.71   -0.98 -0.93  -0.53 -0.30 -1.03 -1.04 -1.00 -1.28
# ...

head(nutrimouse$lipid, 3)
# C14.0 C16.0 C18.0 C16.1n.9 C16.1n.7 C18.1n.9 C18.1n.7 C20.1n.9
# 1  0.34 26.45 10.22     0.35     3.10    16.98     2.41     0.26
# 2  0.38 24.04  9.93     0.55     2.54    20.07     3.92     0.23
# 3  0.36 23.70  8.96     0.55     2.65    22.89     3.96     0.26
# C20.3n.9 C18.2n.6 C18.3n.6 C20.2n.6 C20.3n.6 C20.4n.6 C22.4n.6
# 1     0.00     8.93     0.00     0.00     0.78     3.07     0.00
# 2     0.00    14.98     0.30     0.30     1.64    15.34     0.58
# 3     0.19    16.06     0.27     0.33     1.51    13.27     0.54
# C22.5n.6 C18.3n.3 C20.3n.3 C20.5n.3 C22.5n.3 C22.6n.3
# 1     0.00     5.97     0.37     8.62     1.75    10.39
# 2     2.10     0.00     0.00     0.00     0.48     2.61
# 3     1.77     0.00     0.00     0.00     0.22     2.51

X <- as.matrix(nutrimouse$gene[,1:10])
Y <- as.matrix(nutrimouse$lipid)

res.cc <- cc(X,Y) # highlight correlations between two data matrices

df <- data.frame(x=c(1:10), y=res.cc$cor)

p <- ggplot(df,
      aes(
        x=x,
        y=y
      )
    )
p <- p + geom_line() + geom_point() + labs(title="", x="index", y="canonical correlations")
plot(p)
ggsave("./img/cca.png", p)

plt.cc(res.cc)
