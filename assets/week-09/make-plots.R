library(scatterplot3d)
set.seed(495)
mt <- mtcars[sample(1:nrow(mtcars), 8), ]
mt$disp <- mt$disp + rnorm(nrow(mt), sd = 30)
mt$wt <- mt$wt + rnorm(nrow(mt), sd = 1.1)
mt[1, "disp"] <- 270
mt[1, "mpg"] <- 15
mt$mpg <- round(mt$mpg, digits = 0)
pdf("scatterA.pdf", height = 4.5, width = 5)
with(mt, {
  scatterplot3d(disp, wt, mpg,        # x y and z axis
                color="goldenrod", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                xlab = expression("X"[1]),
                ylab = expression("X"[2]),
                zlab = "Y",
                zlim = c(0, 30))
})
dev.off()

mt$f1 <- mean(mt$mpg[mt$disp <  254.1])
mt$f1[mt$disp > 254.1] <- mean(mt$mpg[mt$disp > 254.1])
mt$r <- mt$mpg - mt$f1

pdf("scatterB.pdf", height = 4.5, width = 5)
with(mt, {
  scatterplot3d(disp, wt, r,        # x y and z axis
                color="goldenrod", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                xlab = expression("X"[1]),
                ylab = expression("X"[2]),
                zlab = "r-old")
})
dev.off()

mt$f2 <- mean(mt$r[mt$wt <  4.1])
mt$f2[mt$wt > 4.1] <- mean(mt$r[mt$wt > 4.1])
mt$r2 <- mt$r - mt$f2

pdf("scatterC.pdf", height = 4.5, width = 5)
with(mt, {
  scatterplot3d(disp, wt, r2,        # x y and z axis
                color="goldenrod", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                xlab = expression("X"[1]),
                ylab = expression("X"[2]),
                zlab = "r-old")
})
dev.off()

pdf("scatterD.pdf", height = 4.5 * 1.3, width = 5 * 1.3)
with(mt, {
  scatterplot3d(disp, wt, mpg,        # x y and z axis
                color="goldenrod", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                xlab = expression("X"[1]),
                ylab = expression("X"[2]),
                zlab = "Y",
                zlim = c(0, 30))
})
dev.off()