
n <- 10000

a <- rnorm(n, 0, 1)

# Constant cov(xt, a)
x1 <- 0.5*a + rnorm(n, 0, 1)
x2 <- 0.5*a + rnorm(n, 0, 1)
x3 <- 0.5*a + rnorm(n, 0, 1)

# Time-varying cov(xt, a)
x1 <- 0.5*a + rnorm(n, 0, 1)
x2 <- 0.7*a + rnorm(n, 0, 1)
x3 <- 0.9*a + rnorm(n, 0, 1)

y1 <- -0.2*x1 + a + rnorm(n, 0, 1)
y2 <- -0.2*x2 + a + rnorm(n, 0, 1)
y3 <- -0.2*x3 + a + rnorm(n, 0, 1)

df <- data.frame(x1, x2, x3, y1, y2, y3)

cor(df)

m1 <- '
alpha =~ 1*y1 + 1*y2 + 1*y3
y1 ~ beta*x1
y2 ~ beta*x2 
y3 ~ beta*x3
alpha ~~ x1 + x2 + x3
x1 ~~ x2 + x3
x2 ~~ x3
y1 ~~ u*y1
y2 ~~ u*y2
y3 ~~ u*y3
'
m1.fit <- sem(m1, df)
summary(m1.fit, nd = 6)

m2 <- '
alpha =~ 1*y1 + 1*y2 + 1*y3
eta   =~ 1*x1 + 1*x2 + 1*x3
y1 ~ beta*x1 
y2 ~ beta*x2 
y3 ~ beta*x3
alpha ~~ eta 
y1 ~~ u*y1
y2 ~~ u*y2
y3 ~~ u*y3
'
m2.fit <- sem(m2, df)
summary(m2.fit, nd = 6)
