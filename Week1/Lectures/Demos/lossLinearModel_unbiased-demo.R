result = NULL
for(i in 1:1000) {
  n = 50
  x = rnorm(n)
  u = seq(-5,5,.1)
  error = rnorm(n) * .5
  y = 1.5 * x + error
  yhat = function(beta,x) {
    return(beta * x)
  }
  squared.loss = function(residuals) {
    sum(residuals^2)
  }
  
  betahat = optimize(function(beta) squared.loss(y - yhat(beta,x)),interval=c(-1,3))$minimum
  
  result = rbind(result,data.frame(beta.optim = betahat, true.beta = 1.5))
}
hist(result$beta.optim,breaks=25)
abline(v=1.5,col="red",lwd=5)
mean(result$beta.optim)
var(result$beta.optim)
