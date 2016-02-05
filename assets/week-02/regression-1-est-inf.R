## ---- echo=FALSE,eval=TRUE-----------------------------------------------
n <- 60
beta_0 <- 12
beta_1 <- .7
sigma <- 2
x <- rnorm(n, mean = 20, sd = 3) # generate arbitrary x's

plot(20, 25, xlim = c(12, 28), ylim = c(17, 35), ylab = "y", xlab = "x", type = "n") # set up an empty plot
abline(a = beta_0, b = beta_1, col = "goldenrod", lwd = 2) # add mean function

## ---- eval=TRUE, echo=FALSE----------------------------------------------
plot(20, 25, xlim = c(12, 28), ylim = c(17, 35), ylab = "y", xlab = "x", type = "n") # set up an empty plot

# generate y
f_mean <- beta_0 + beta_1 * x # mean function
f_data <- f_mean + rnorm(n, mean = 0, sd = sigma) # data generating function

points(x, f_data, pch = 16, col = "steelblue") # add generated data
# try to recover the true mean function
m1 <- lm(f_data ~ x)
abline(m1, lwd = 1.5)
abline(a = beta_0, b = beta_1, col = "goldenrod", lwd = 2) # add mean function

## ---- echo=FALSE,eval=TRUE, cache=TRUE-----------------------------------
it <- 5000
coef_mat <- matrix(rep(NA, it * 2), ncol = 2)
for(i in 1:it) {
  x <- rnorm(n, mean = 20, sd = 3)
  f_mean <- beta_0 + beta_1 * x
  f_data <- f_mean + rnorm(n, mean = 0, sd = sigma)
  coef_mat[i, ] <- lm(f_data ~ x)$coef
}

## ---- eval=TRUE, echo=FALSE----------------------------------------------
plot(20, 25, xlim = c(12, 28), ylim = c(17, 35), ylab = "y", xlab = "x", type = "n") # set up an empty plot

points(x, f_data, pch = 16, col = "steelblue") # add generated data
for(i in 1:it) {
  abline(coef_mat[i, 1], coef_mat[i, 2], col = rgb(0, 0, 0, 0.02))
}
abline(a = beta_0, b = beta_1, col = "goldenrod", lwd = 2) # add mean function

## ---- echo=FALSE---------------------------------------------------------
beta_1s <- coef_mat[, 2]
hist(beta_1s, breaks = 20, 
     main = expression(paste("Sampling distribution of ", hat(beta)[1])),
     xlab = expression(hat(beta)[1]), prob = TRUE)
# mean(beta_1s)
# sd(beta_1s)

## ---- echo=FALSE---------------------------------------------------------
beta_1s <- coef_mat[, 2]
hist(beta_1s, breaks = 20, 
     main = expression(paste("Sampling distribution of ", hat(beta)[1])),
     xlab = expression(hat(beta)[1]), prob = TRUE)
abline(v = beta_1, col = "orange", lwd = 2)
text(beta_1+.02, .5, expression(beta[1]))

## ---- echo=FALSE---------------------------------------------------------
beta_1s <- coef_mat[, 2]
hist(beta_1s, breaks = 20, 
     main = expression(paste("Sampling distribution of ", hat(beta)[1])),
     xlab = expression(hat(beta)[1]), prob = TRUE)
abline(v = beta_1, col = "orange", lwd = 2)
text(beta_1+.02, .5, expression(beta[1]))
xl <- seq(min(beta_1s), max(beta_1s), length.out = 100)
lines(xl, dnorm(xl, mean = beta_1, sd = sqrt(sigma^2/sum((x-mean(x))^2))), 
      lwd = 2, col = "thistle3")

## ---- eval=FALSE, echo = FALSE-------------------------------------------
## ## Constructing a CI for $\hat{\beta}_1$
## 
## \[ \hat{\beta}_1 \pm t_{\alpha/2, n-2} * SE(\hat{\beta}_1) \]
## 
## beta_1 <- m1$coef[2]
## alpha <- .05
## t_stat <- qt(1-alpha/2, n - 2)
## SE <- summary(m1)$coef[[4]]
## moe <- t_stat * SE
## c(beta_1 - moe, beta_1 + moe)
## 
## confint(m1, "x") # to double check

