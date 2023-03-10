library(gamlss)
library(ggplot2)
# Generate some example data
set.seed(123)
x <- seq(0, 10, length.out = 100)
y <- 2*sin(x) + rnorm(100, sd = 0.5)

# Fit a piecewise cubic spline model with 3 knots
model1 <- gamlss(y ~ pb(x, df = c(3), bs = "cs"), family = NO)
model2 <- gamlss(y ~ pb(x, knots = c(2.5, 5.0, 7.5), bs = "cs"), family = NO)
#model3 <- gamlss(y ~ pb(x, knots = c(2, 3), bs = "cs"), family = "Gaussian")
model3 <- gamlss(y ~ pb(x, knots = c(2,6), bs = "cs"), family = NO)
model4 <- gamlss(y ~ pb(x))

# Plot the data and model fit
#plot(x, y, ylim = c(-3, 3))
#lines(x, fitted(model), col = "red")
raw_data<-data.frame(x, y)
data_trend1<-data.frame(x, fitted(model1))
#data_trend2<-data.frame(x, fitted(model2))
col_name <- "x"
data_trend2<-data.frame(raw_data[[col_name]], fitted(model2))
data_trend3<-data.frame(x, fitted(model3))
data_trend4<-data.frame(x, fitted(model4))

u <- ggplot(data=raw_data, aes(x=x, y=y)) + geom_point(size=0.3, color="red")

u <- u + geom_line(data=data_trend1, aes(x=x, y=fitted.model1.), color="blue", linewidth=0.5) 
#u <- u + geom_line(data=data_trend2, aes(x=x, y=fitted.model2.), color="green", linewidth=1) 
#u <- u + geom_line(data=data_trend3, aes(x=x, y=fitted.model3.), color="purple", linewidth=0.5) 
u <- u + geom_line(data=data_trend4, aes(x=x, y=fitted.model4.), color="orange", linewidth=0.5) 
u