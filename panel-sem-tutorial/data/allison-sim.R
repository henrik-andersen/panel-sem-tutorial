
# ========================================== # 
# Simulated example, two-sided Allison model
# Henrik Andersen 
# 03.12.2020
# ========================================== # 

rm(list = ls())


# --- Simulate data 

# Set parameters 
phi = rho = 0.2
gamma = beta = 0.5

# Individual effects 
alpha <- rnorm(1000, 0, 1)
eta   <- 0.25*alpha + rnorm(1000, 0, 1)

# True underlying constructs
xt1 <- alpha + rnorm(1000, 0, 1)
yt1 <- eta   + rnorm(1000, 0, 1)

xt2 <- alpha + phi*xt1 + gamma*yt1 + rnorm(1000, 0, 1)
yt2 <- eta   + rho*yt1 + beta*xt1  + rnorm(1000, 0, 1)

xt3 <- alpha + phi*xt2 + gamma*yt2 + rnorm(1000, 0, 1)
yt3 <- eta   + rho*yt2 + beta*xt2  + rnorm(1000, 0, 1)

xt4 <- alpha + phi*xt3 + gamma*yt3 + rnorm(1000, 0, 1)
yt4 <- eta   + rho*yt3 + beta*xt3  + rnorm(1000, 0, 1)

# Indicators
# For x 
x11 <- 1.25*xt1 + rnorm(1000, 0, 1)
x21 <- 1.50*xt1 + rnorm(1000, 0, 1)
x31 <- 0.85*xt1 + rnorm(1000, 0, 1)

x12 <- 1.25*xt2 + rnorm(1000, 0, 1)
x22 <- 1.50*xt2 + rnorm(1000, 0, 1)
x32 <- 0.85*xt2 + rnorm(1000, 0, 1)

x13 <- 1.25*xt3 + rnorm(1000, 0, 1)
x23 <- 1.50*xt3 + rnorm(1000, 0, 1)
x33 <- 0.85*xt3 + rnorm(1000, 0, 1)

x14 <- 1.25*xt4 + rnorm(1000, 0, 1)
x24 <- 1.50*xt4 + rnorm(1000, 0, 1)
x34 <- 0.85*xt4 + rnorm(1000, 0, 1)

# For y 
y11 <- 1.40*yt1 + rnorm(1000, 0, 1)
y21 <- 1.25*yt1 + rnorm(1000, 0, 1)
y31 <- 0.90*yt1 + rnorm(1000, 0, 1)

y12 <- 1.40*yt2 + rnorm(1000, 0, 1)
y22 <- 1.25*yt2 + rnorm(1000, 0, 1)
y32 <- 0.90*yt2 + rnorm(1000, 0, 1)

y13 <- 1.40*yt3 + rnorm(1000, 0, 1)
y23 <- 1.25*yt3 + rnorm(1000, 0, 1)
y33 <- 0.90*yt3 + rnorm(1000, 0, 1)

y14 <- 1.40*yt4 + rnorm(1000, 0, 1)
y24 <- 1.25*yt4 + rnorm(1000, 0, 1)
y34 <- 0.90*yt4 + rnorm(1000, 0, 1)

df <- data.frame(x11, x21, x31, 
                 x12, x22, x32, 
                 x13, x23, x33, 
                 x14, x24, x34, 
                 y11, y21, y31, 
                 y12, y22, y32, 
                 y13, y23, y33, 
                 y14, y24, y34)


# --- Run model 

library(lavaan)

# Configural 
m1 <- '
# Measurement models 
xt1 =~ x11 + x21 + x31
xt2 =~ x12 + x22 + x32
xt3 =~ x13 + x23 + x33
xt4 =~ x14 + x24 + x34
yt1 =~ y11 + y21 + y31
yt2 =~ y12 + y22 + y32
yt3 =~ y13 + y23 + y33
yt4 =~ y14 + y24 + y34
# Individual effects
alpha =~ 1*xt2 + 1*xt3 + 1*xt4 
eta   =~ 1*yt2 + 1*yt3 + 1*yt4
# Regressions, time-invariant effects
xt2 ~ phi*xt1 + gamma*yt1 
xt3 ~ phi*xt2 + gamma*yt2
xt4 ~ phi*xt3 + gamma*yt3
yt2 ~ rho*yt1 + beta*xt1 
yt3 ~ rho*yt2 + beta*xt2
yt4 ~ rho*yt3 + beta*xt3
# Correlations
alpha ~~ eta + xt1 + yt1
eta   ~~ xt1 + yt1 
xt1   ~~ yt1
# Contemporary residual correlations
xt2 ~~ yt2
xt3 ~~ yt3
xt4 ~~ yt4
'
m1.fit <- sem(model = m1, data = df, meanstructure = TRUE, estimator = "ML")
summary(m1.fit, fit.measures = TRUE, standardized = TRUE)

# Metric 
m2 <- '
# Measurement models 
xt1 =~ lx1*x11 + lx2*x21 + lx3*x31 
xt2 =~ lx1*x12 + lx2*x22 + lx3*x32
xt3 =~ lx1*x13 + lx2*x23 + lx3*x33
xt4 =~ lx1*x14 + lx2*x24 + lx3*x34
yt1 =~ ly1*y11 + ly2*y21 + ly3*y31
yt2 =~ ly1*y12 + ly2*y22 + ly3*y32
yt3 =~ ly1*y13 + ly2*y23 + ly3*y33
yt4 =~ ly1*y14 + ly2*y24 + ly3*y34
# Individual effects
alpha =~ 1*xt2 + 1*xt3 + 1*xt4 
eta   =~ 1*yt2 + 1*yt3 + 1*yt4
# Regressions, time-invariant effects
xt2 ~ phi*xt1 + gamma*yt1 
xt3 ~ phi*xt2 + gamma*yt2
xt4 ~ phi*xt3 + gamma*yt3
yt2 ~ rho*yt1 + beta*xt1 
yt3 ~ rho*yt2 + beta*xt2
yt4 ~ rho*yt3 + beta*xt3
# Correlations
alpha ~~ eta + xt1 + yt1
eta   ~~ xt1 + yt1 
xt1   ~~ yt1
# Contemporary residual correlations
xt2 ~~ yt2
xt3 ~~ yt3
xt4 ~~ yt4
'
m2.fit <- sem(model = m2, data = df, meanstructure = TRUE, estimator = "ML")
summary(m2.fit, fit.measures = TRUE, standardized = TRUE)

# Configural 
m3 <- '
# Measurement models 
xt1 =~ lx1*x11 + lx2*x21 + lx3*x31 
xt2 =~ lx1*x12 + lx2*x22 + lx3*x32
xt3 =~ lx1*x13 + lx2*x23 + lx3*x33
xt4 =~ lx1*x14 + lx2*x24 + lx3*x34
yt1 =~ ly1*y11 + ly2*y21 + ly3*y31
yt2 =~ ly1*y12 + ly2*y22 + ly3*y32
yt3 =~ ly1*y13 + ly2*y23 + ly3*y33
yt4 =~ ly1*y14 + ly2*y24 + ly3*y34
# Individual effects
alpha =~ 1*xt2 + 1*xt3 + 1*xt4 
eta   =~ 1*yt2 + 1*yt3 + 1*yt4
# Regressions, time-invariant effects
xt2 ~ phi*xt1 + gamma*yt1 
xt3 ~ phi*xt2 + gamma*yt2
xt4 ~ phi*xt3 + gamma*yt3
yt2 ~ rho*yt1 + beta*xt1 
yt3 ~ rho*yt2 + beta*xt2
yt4 ~ rho*yt3 + beta*xt3
# Correlations
alpha ~~ eta + xt1 + yt1
eta   ~~ xt1 + yt1 
xt1   ~~ yt1
# Contemporary residual correlations
xt2 ~~ yt2
xt3 ~~ yt3
xt4 ~~ yt4
# Intercepts 
x11 + x12 + x13 + x14 ~ tx1*1
x21 + x22 + x23 + x24 ~ tx2*1
x31 + x32 + x33 + x34 ~ tx3*1
y11 + y12 + y13 + y14 ~ ty1*1
y21 + y22 + y23 + y24 ~ ty2*1
y31 + y32 + y33 + y34 ~ ty3*1
'
m3.fit <- sem(model = m3, data = df, meanstructure = TRUE, estimator = "ML")
summary(m3.fit, fit.measures = TRUE, standardized = TRUE)


# --- Likelihood ratio test
anova(m1.fit, m2.fit, m3.fit)

# --- Export data 

# Write as R object
saveRDS(df, file = "D:/Henrik/Seafile/Meine Bibliothek/df-allison-sim.rda")

# Write as csv for Mplus 
write.table(df, file = "D:/Henrik/Seafile/Meine Bibliothek/df-allison-sim.csv", 
          col.names = FALSE, row.names = FALSE, sep = ",")


