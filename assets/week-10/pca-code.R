# Slides from last time: http://andrewpbray.github.io/math-243/assets/week-10/PCA.Rmd

d <- read.csv("http://andrewpbray.github.io/math-243/assets/data/handwritten.csv")

plot_letter <- function(x) { # x should be a row from the handwritten data set
  a <- as.numeric(x[, -1])
  m <- matrix(a, nrow = 8, byrow = TRUE)
  m <- t(apply(m, 2, rev)) # rotate matrix
  image(m, axes = FALSE, col = rev(grey(seq(0, 1, length = 256))))
  box()
}