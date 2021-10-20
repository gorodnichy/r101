# File name: stat101_a.R
# Visualizing various probability density functions
# See also: stat101_a.Rmd (the same code inside an automatically generated report)


# The Normal Distribution
# See: 

# Read about rnorm and rlnorm functions
?rnorm
?Distributions

pnorm(0) # 0.5
pnorm(1) # 0.8413447
pnorm(2) # 0.9772499
pnorm(3) # 0.9986501

# Visualize probability density functions (PDF) and cumulative distribution function (CDF), which is integral under curve from -inf to PDF(x), 
# with different sample points for different distributions
xx = -5:5; 
xx = c(-3,-2,-1,-0.5, -0.25, 0, 0.25, 0.5, 1, 2, 3)
set.seed(111)  # Seed for random number generator. Run `?set.seed` to learn about any function you don't know
xx = rnorm(11)
xx = rlnorm(11)
xx

# with base graphics
plot(xx, dnorm(xx), main = "Normal PDF = f(x) ") 

# with ggplot2 graphics
library(ggplot2)
qplot(xx, dnorm(xx), geom = c("point", "line"), main = "Normal: PDF = f(x) ") 
qplot(xx, dnorm(xx, log=T), geom = c("point", "line"), main = "Normal: PDF = f(x) ")
qplot(xx, pnorm(xx), geom = c("point", "line"), main = "Normal: CDF = f(x) ")
qplot(xx, pnorm(xx, log.p=T), geom = c("point", "line"), main = "Normal: CDF = f(x) ")


qplot(xx, dlnorm(xx), geom = c("point", "line"), main = "Log Normal PDF = f(x) ")
qplot(xx, dlnorm(xx, log=T), geom = c("point", "line"), main = "Log Normal: PDF = f(x) ")
qplot(xx, plnorm(xx), geom = c("point", "line"), main = "Log Normal CDF = f(x) ")
qplot(xx, plnorm(xx, log.p=T), geom = c("point", "line"), main = "Log Normal: CDF = f(x) ")


# Fun distributions

qbirthday(prob = 0.5, classes = 365, coincident = 2)
pbirthday(n <- 36, classes = 365, coincident = 2)
