result = NULL
for(i in 1:1000) {
  n = 50
  x = rnorm(n)
  u = seq(-5,5,.1)
  error = rt(n,df = 2)*5
  y = 1.5 * x + error
  yhat = function(beta,x) {
    return(beta * x)
  }
  squared.loss = function(residuals) {
    sum(residuals^2)
  }
  
 # betahat = optimize(function(beta) squared.loss(y - yhat(beta,x)),interval=c(-1,3))$minimum # change to -10, 10
  betahat = coef(lm(y ~ -1 + x)) #<-- more efficient, no need for restriction
  result = rbind(result,data.frame(beta.optim = betahat, true.beta = 1.5))
}
draw.errors(1)

hist(result$beta.optim,breaks=25)
mean(result$beta.optim)
var(result$beta.optim)
abline(v=mean(result$beta.optim),col="red",lwd=5)
abline(v=1.5,col="blue",lwd=5)

result = NULL
for(i in 1:1000) {
  n = 50
  x = rnorm(n)
  u = seq(-5,5,.1)
  error = rt(n,df = 2)*5
  y = 1.5 * x + error
  yhat = function(beta,x) {
    return(beta * x)
  }
  absolute.loss = function(residuals) {
    sum(abs(residuals))
  }
  
  betahat = optimize(function(beta) absolute.loss(y - yhat(beta,x)),interval=c(-5,5))$minimum
  result = rbind(result,data.frame(beta.optim = betahat, true.beta = 1.5))
}
hist(result$beta.optim,breaks=25)

abline(v=mean(result$beta.optim),col="red",lwd=5)
abline(v=1.5,col="blue",lwd=5)

mean(result$beta.optim)
var(result$beta.optim)
