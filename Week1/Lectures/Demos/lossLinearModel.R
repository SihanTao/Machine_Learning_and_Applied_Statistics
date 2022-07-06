## An interactive demo on loss functions and empirical risk minimization in R

## Generate some synthetic data
n = 50
x = rnorm(n) # generate input values
error = rnorm(n) * .5 # generate error
y = 1.5 * x + error # generate output values
plot(x,y) # plot the data
u = seq(-5,5,.1) 
lines(u,1.5*u,col="blue") # plot the line y=1.5*x

## Create a function that returns beta*x i.e. model prediction for a value of beta and an input value of x
yhat = function(beta,x) {
  return(beta * x)
}
lines(u,yhat(1,u),col="red")
lines(u,yhat(2,u),col="red") # QUIZ: why do they all intersect at (0,0)?

## How should we measure error? Which line is better?

## The residual is the difference between y and yhat: try varying beta 
draw.errors = function(beta) {
  plot(x,y,main=sprintf("beta = %.02f",beta))
  yhat1 = yhat(beta,x)
  lines(u,yhat(beta,u),col="red")
  residuals = y - yhat1
  for(i in 1:length(x))
    lines(c(x[i],x[i]),c(y[i],yhat1[i]))
}
draw.errors(1)
draw.errors(0)
draw.errors(1.5)

## Notice that there is no way to get the errors to be zero. QUIZ: why not?

## We will use the square of the residuals as our loss function
squared.loss = function(residuals) {
  sum(residuals^2)
}
loss = NULL
# Consider a grid of values of beta and compute the loss for each of them
betas = seq(-1,3,.01)
for(beta in betas) {
  loss = c(loss,squared.loss(y - yhat(beta,x)))  
}
## a more efficient one liner to accomplish the same thing as the for loop:
# loss = vapply(betas, function(beta) squared.loss(y - yhat(beta,x)),0)

plot(betas,loss,ty="l",xlab = expression(beta),ylab="Sum of squared errors") # where is the minimum?

# let us pick a non-minimum point--ask students for a choice of beta, e.g. beta = -0.5
plot(x,y)
draw.errors(1.4)

# now let's consider the minimum found from this search and compare it to the true minimum
betahat.grid = betas[which.min(loss)] # remember, we want to minimize loss! here is the minimum found by a grid search
plot(betas,loss,ty="l",xlab = expression(beta),ylab="Sum of squared errors",
     main=sprintf("betahat = %.02f vs true beta = %.02f",betahat.grid,1.5)) # where is the minimum?
abline(v=betahat.grid,col="red")
abline(v=1.5,col="blue")
legend("topright",c(expression(hat(beta)),expression(beta[0])),fill=c("red","blue"))

# rerun this code from the top a few times and see how \hat beta changes for different datasets!
# check lossLinearModel_unbiased-demo.R.
# also try increasing or decreasing n (this tells us about consistency, see https://en.wikipedia.org/wiki/Consistent_estimator)

# we can compare the empirical estimate from the data to the true estimate
plot(x,y)
lines(u,yhat(betahat.grid,u),col="red")
lines(u,yhat(1.5,u),col="blue")
legend("topleft",c(expression(hat(beta)),expression(beta[0])),fill=c("red","blue"))

draw.errors(betahat.grid)
draw.errors(1.5)
## What if we used optimize instead of a grid search?

betahat = optimize(function(beta) squared.loss(y - yhat(beta,x)),interval=c(-1,3))$minimum
plot(x,y,main=sprintf("betahat = %.02f vs true beta = %.02f",betahat,1.5)) 
lines(u,yhat(betahat,u),col="red")
lines(u,yhat(1.5,u),col="blue")
legend("topleft",c(expression(hat(beta)),expression(beta[0])),fill=c("red","blue"))

# QUIZ. Why use optimize insetad of a grid search?

data.frame(beta.grid = betahat.grid, beta.optim = betahat, true.beta = 1.5)
# QUIZ. how many significant figures are meaningful?

#### STOP HERE A PRIORI

## Mention that this is an unbiased estimator
source("lossLinearModel_unbiased-demo.R")

## Another example in two dimensions
x1 = rnorm(n)
x2 = rnorm(n)
error = rnorm(n) * .1
y = 1.5 * x1 + 1 * x2 + error
plot(x1,y)
plot(x2,y)
library(scatterplot3d)
plt = scatterplot3d(x1, x2, y, highlight.3d = TRUE, angle = 30,
              col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
              cex.lab = 1.1, main = "Hemisphere", pch = 20)
plt$plane3d(0,1.5,1)
summary(lm(y ~ x1 + x2))
# Define yhat using matrix-vector notation:
yhat = function(beta,x) {
  return(x %*% beta)
}

for(i in 1:5) {
  beta = matrix(rnorm(2))
  x = cbind(x1,x2)
  yhat.random = yhat(beta,x)
  plot(y,yhat.random)
  cor(y,yhat.random)
  plt = scatterplot3d(x1, x2, y, highlight.3d = TRUE, angle = 30,
                      col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
                      cex.lab = 1.1, main = "Hemisphere", pch = 20)
  plt$plane3d(0,beta[1],beta[2])
}

u = expand.grid(seq(-3,3,.1),seq(-3,3,.1))
loss = apply(u, 1,function(beta) squared.loss(y - yhat(matrix(beta),x)))
plt = scatterplot3d(u[,1], u[,2], loss, highlight.3d = TRUE, angle = 60,
                    col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
                    cex.lab = 1.1, main = "Hemisphere", pch = 20)
betahat = u[which.min(loss),]
plt$points3d(c(betahat[1],betahat[1]),c(betahat[2],betahat[2]),c(0,5000),ty="l",lwd=5,col="red")
plt$points3d(c(1.5,1.5),c(1,1),c(0,5000),ty="l",lwd=5,col="blue")

##
n = 100
x1 = rnorm(n)
x2 = rnorm(n)
error = rnorm(n) * .1
y = 1.5 * x1 + 1 * x2 - .5 * x1 * x2 + error
plot(x1,y)
plot(x2,y)
plt = scatterplot3d(x1, x2, y, highlight.3d = TRUE, angle = 30,
                    col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
                    cex.lab = 1.1, main = "Hemisphere", pch = 20)
summary(lm(y ~ x1 + x2))
summary(lm(y ~ x1 + x2 + I(x1 * x2)))

