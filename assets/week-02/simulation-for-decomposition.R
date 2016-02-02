## ------------------------------------------------------------------------
runif(3)

## ------------------------------------------------------------------------
rnorm(3)

## ------------------------------------------------------------------------
rnorm(3, mean = 4, sd = .5)

## ------------------------------------------------------------------------
rnorm(3)
rnorm(3)
set.seed(485)
rnorm(3)
set.seed(485)
rnorm(3)

## ------------------------------------------------------------------------
x <- seq(5, 35, .1)
f <- function(x) {
  23 + 11 * x - .4 * x^2
}
y <- f(x) + rnorm(length(x), mean = 0, sd = 11)

## ----echo = FALSE--------------------------------------------------------
df <- data.frame(x = x, f = f(x))
library(ggplot2)
p1 <- ggplot(df, aes(x = x, y = f)) + 
  ylim(c(-110, 130)) +
  ylab("y")
p1 + geom_line(col = I("goldenrod"), lwd = 1)
  

## ----echo = FALSE--------------------------------------------------------
x <- runif(20, 5, 35)
df_pt <- data.frame(x = x, y = f(x) + rnorm(length(x), mean = 0, sd = 11))
p1 + geom_point(data = df_pt, aes(x = x, y = y)) +
  geom_line(col = I("goldenrod"), lwd = 1)

## ----echo = FALSE, warning = FALSE---------------------------------------
x <- runif(20000, 5, 35)
df_pt <- data.frame(x = x, y = f(x) + rnorm(length(x), mean = 0, sd = 11))
p1 + geom_point(data = df_pt, aes(x = x, y = y), alpha = .1) +
  geom_line(col = I("goldenrod"), lwd = 1)

## ---- echo = FALSE-------------------------------------------------------
set.seed(34)
x <- runif(20, 5, 35)
df_pt <- data.frame(x = x, y = f(x) + rnorm(length(x), mean = 0, sd = 11))
fhat <- lm(y ~ x, data = df_pt)
df_fhat <- data.frame(x = x, f = fhat$fit)
(p2 <- p1 + geom_point(data = df_pt, aes(x = x, y = y), col = I("steelblue")) +
  geom_line(data = df_fhat, aes(x = x, y = f), col = I("steelblue")))

## ---- echo = FALSE-------------------------------------------------------
set.seed(28)
x <- runif(20, 5, 35)
df_pt <- data.frame(x = x, y = f(x) + rnorm(length(x), mean = 0, sd = 11))
fhat <- lm(y ~ x, data = df_pt)
df_fhat <- data.frame(x = x, f = fhat$fit)
p2 + geom_point(data = df_pt, aes(x = x, y = y), col = I("tomato")) +
  geom_line(data = df_fhat, aes(x = x, y = f), col = I("tomato"))

## ----cache = TRUE, warning = FALSE, echo = FALSE-------------------------
df_fhat <- cbind(df_fhat, group = rep(0, nrow(df_fhat)))
for(i in 1:1000) {
  x <- runif(20, 5, 35)
  df_pt <- data.frame(x = x, y = f(x) + rnorm(length(x), mean = 0, sd = 11))
  fhat <- lm(y ~ x, data = df_pt)$fit
  df_fhat_new <- data.frame(x = x, f = fhat, group = rep(i, length(x)))
  df_fhat <- rbind(df_fhat, df_fhat_new)
}
p1 + geom_line(data = df_fhat, aes(x = x, y = f, group = group),
               col = I("steelblue"), alpha = .05)

## ----cache = TRUE, warning = FALSE, echo = FALSE-------------------------
p1 + geom_line(data = df_fhat, aes(x = x, y = f, group = group),
               col = I("steelblue"), alpha = .05) +
  geom_line(col = I("goldenrod"), lwd = 1)

