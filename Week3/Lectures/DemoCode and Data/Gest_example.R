
# load point pattern from csv
N = read.table('test_data_sparse.csv',sep=',')
#convert it into the ppp data format for SpatStat
N = as.ppp(N,c(0,1,0,1))
#estimate G(d)
G = Gest(N)
# plot G(d)
plot(G)

Genv = envelope(N,Gest)
plot(Genv)


# load point pattern from csv
N = read.table('test_data_dense.csv',sep=',')
#convert it into the ppp data format for SpatStat
N = as.ppp(N,c(0,1,0,1))
#estimate G(d)
G = Gest(N)
# plot G(d)
plot(G)

Genv = envelope(N,Gest)
plot(Genv)


# CSR
N = rpoispp(200)
#estimate G(d)
G = Gest(N)
# plot G(d)
plot(G)

Genv = envelope(N,Gest)
plot(Genv)