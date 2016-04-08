set.seed(495)
mt <- mtcars[sample(1:nrow(mtcars), 8), ]
mt$disp <- mt$disp + rnorm(nrow(mt), sd = 30)
mt$wt <- mt$wt + rnorm(nrow(mt), sd = 1.1)
mt[1, "disp"] <- 270
mt[1, "mpg"] <- 15
mt$mpg <- round(mt$mpg, digits = 0)

# Single tree
t1 <- tree(mpg ~ disp + wt, data=mt, control = tree.control(nrow(mt),mincut=0, minsize=2))
# condition is tunned with the function tree.control(nobs,mincut,minsize,mindev)

plot(t1)
text(t1, pretty=0)

library(gbm)
library(dplyr)
library(scatterplot3d)

# Boosted Model
b1 <- gbm(mpg ~ disp + wt, distribution="gaussian",
          data=mt, n.trees=3, interaction.depth=1, shrinkage = 0.1,
          n.minobsinnode = 1)

summary(b1)

p <- plot(b1, i.var=c(1,2), return.grid = TRUE)

with(p, {scatterplot3d(disp, wt, y,        # x y and z axis
              color="goldenrod", pch=19, # filled blue circles
              type="p",             # lines to the horizontal plane
              xlab = expression("X"[1]),
              ylab = expression("X"[2]),
              zlab = "y")}
)
