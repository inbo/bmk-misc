source("packages.R")
set.seed(2)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
summary(b)
plot(b, pages = 1, residuals = TRUE)  ## show partial residuals
plot(b, pages = 1, seWithMean = TRUE) ## `with intercept' CIs
