# what would the correlation look like with overlapping variables?
# TO DO: try with more pillars

n <- 13
main <- runif(n)
main.A <- 3*main + rnorm(n)
main.B <- 3*main + rnorm(n)
main.C <- 2*main + rnorm(n)

a <- 3*main.A + rnorm(n)
b <- 3.5*main.A + rnorm(n)
c <- 4*main.A + rnorm(n)
d <- 3*main.A + rnorm(n)
e <- 2*main.B + rnorm(n)
f <- 3.2*main.B + rnorm(n)
g <- 2.5*main.B + rnorm(n)
h <- 3.2*main.C + rnorm(n)
i <- 2.5*main.C + rnorm(n)

I.A <- a+c+d
I.B <- e+f
I.C <- g+h+i
II.A <- a+b
II.B <- d+e+g
II.C <- h+i

cor(I.B, I.A)

I <- data.frame(I.A, I.B, I.C)
I$Score <- rowMeans(I)

II <- data.frame(II.A, II.B, II.C)
II$Score <- rowMeans(II)

raw <- data.frame(cbind(I,II))
cor(raw)

Res <- data.frame(Nr = 1:n)
Res$I.A <- lm(I.A ~ Score, I)$residuals
Res$I.B <- lm(I.B ~ Score, I)$residuals
Res$I.C <- lm(I.C ~ Score, I)$residuals
Res$II.A <- lm(II.A ~ Score, II)$residuals
Res$II.B <- lm(II.B ~ Score, II)$residuals
Res$II.C <- lm(II.C ~ Score, II)$residuals
corrplot(cor(Res))
