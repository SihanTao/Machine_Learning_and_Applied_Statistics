# Slide 139
# load point pattern from csv
N = read.table('test_data_dense.csv',sep=',')
#convert it into the ppp data format for SpatStat
N = as.ppp(N,c(0,1,0,1))
Fenv = envelope(N,Fest)
plot(Fenv)

# CSR
N = rpoispp(100)
Fenv = envelope(N,Fest)
plot(Fenv)

#clustered process
N = rThomas(10,0.02,10)
Fenv = envelope(N,Fest)
plot(Fenv)