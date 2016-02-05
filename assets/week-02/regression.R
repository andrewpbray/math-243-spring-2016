
























Laura, Alex B.
Revant, Alex G-L
Will, Philip
Dean, Vikram
Hans, Natalie
Evan, Hanna
Aurora, Julie
Miguel, Reilly
Kaitlyn, Alicia
Michael, Sarah
Julia, Taylor






data(mtcars)
str(mtcars)
plot(mpg ~ disp, data = mtcars)
m1 <- lm(mpg ~ disp, data = mtcars)
attributes(m1)
m1$coefficients
summary(m1)
