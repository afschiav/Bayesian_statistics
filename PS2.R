

###########################################
############  Problem set 2  ##############
###########################################

#a)
r <- 0:100
pA <-as.list(rep(1/101, 101)) #define prior A
plot(r,pA) #plot prior A

#b)
pB <- choose(100,r)/2^100  #define prior A
plot(r,pB) #plot prior A


#d)
theta <- seq(0,1, 0.01)
l<- dbinom(7,10,theta) #define likelihood function 
plot(theta, l)

#e)
postA <- (l*as.numeric(pA))/sum(l*as.numeric(pA)) #compute normalized posterior probability w/ prior A
postB <- (l*pB)/sum(l*as.numeric(pB))  #compute normalized posterior probability w/ prior A
plot(theta, postA, ylim=c(0,0.09), col="blue", ylab ="PostA (blue), PostB (red)", xlab =expression(theta))+points(theta, postB, col = "red")

#f)
theta[which.max(postA)] #calculate max posterior value from posterior A
theta[which.max(postB)]  #calculate max posterior value from posterior B
