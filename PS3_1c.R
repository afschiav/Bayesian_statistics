

#create P matrix
P <- matrix(c(0, 0.15, 0.3, 0.5, 0 , 0.7, 0.5, 0.85, 0), 3, 3, byrow=TRUE)

#calculate eigenvalues/vectors
ev <- eigen(P)
values<-ev$values
vectors <-ev$vectors

#calculate scalar vector for eigenvector that corresponds to maximum eigenvalue
scale_vector = rep(sum(vectors[,which.max(values)]), times=3)

#calculate stationary ergodic distribution of system
stationary_dist = vectors[,which.max(values)]/scale_vector
