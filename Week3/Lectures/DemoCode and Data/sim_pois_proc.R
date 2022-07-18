# slide 47
lambda = 2
TT = 10
N = rpois(1,lambda*TT)
E = runif(N,min=0,max=TT)
E = sort(E)
plot(E,rep(0,length(E)),type='p',pch=19,cex=0.3)

