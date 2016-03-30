library(scatterplot3d)
set.seed(495)
mt <- mtcars[sample(1:nrow(mtcars), 8), ]
mt$disp <- mt$disp + rnorm(nrow(mt), sd = 30)
mt$wt <- mt$wt + rnorm(nrow(mt), sd = 1.1)
mt[1, "disp"] <- 270
mt[1, "mpg"] <- 15.1
mt$mpg <- round(mt$mpg, digits = 0)
pdf("scatterA.pdf", height = 4, width = 5)
with(mt, {
  scatterplot3d(disp, wt, mpg,        # x y and z axis
                color="blue", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                xlab = expression("X"[1]),
                ylab = expression("X"[2]),
                zlab = "Y",
                zlim = c(0, 30))
})
dev.off()

mean(mt$mpg[mt$disp <  254.1])
mean(mt$mpg[mt$disp > 254.1])

