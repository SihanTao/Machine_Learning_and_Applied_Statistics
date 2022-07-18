# slide 92

# load point pattern from csv
N = read.table('test_data_dense.csv',sep=',')
# convert into ppp data format for spat stat
N = as.ppp(N,c(0,1,0,1))
plot(N)
# estimate K(r) using Ripley’s edge correction 
K = Kest(N,correction='Ripley')
# plot K(r) and L(r)-r 
plot(K)
plot(K,sqrt(./pi)-r ~ r)