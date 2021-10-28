library(TeachingDemos)


#define the binomial kernal function that represents the 
#scenario at hand: mortgages either default or do not (binary)
binom.kern <- function(theta,n,k){
  (theta^k)*(1-theta)^(n-k)
}

#define the binomial PDF that represents the 
#scenario at hand: mortgages either default or do not (binary)
binom<-function(theta,n,k){
  choose(n,k)*(theta)^(k)*(1-theta)^(n-k)
  }

log.post<-function(p,n,k){
  log(choose(n,k)*(p^k)*(1-p)^(n-k))
} 

#specify sample size and number of defaults
n<-100
k<-5


#calculate the maximum (log) likelihood:
mle<-optimize(log.post,interval=c(0,1),n=n,k=k,maximum=T)
ci <- qbeta(c(0.025,0.975),k+1,(n-k)+1)

#plot the binomial distribution with maximum likelihood value
curve(binom(x,100,5), from=0, to=1, xlab=expression(theta),ylab="p[default| existing mortgages]", main="Posterior Distribution")
abline(h=max(binom(mle$maximum,n,k)), lty = 2, col = "red" )
abline(v=mle$maximum,n,k, lty = 2, col = "red" )
text(mle$maximum,0, expression(paste("E[",theta,"]:0.05")), pos=4)
points(mle$maximum,binom(mle$maximum,n,k),pch=20,col="red",cex=1)
text(0.2,max(binom(mle$maximum,n,k)), "MLE:0.18", pos = 4)
# Add vertical CI lines
abline(v=ci[1],lty=2)
abline(v=ci[2],lty=2)

#Calculate the shortest interval with 95% probability:
best_ci <- hpd(qbeta, shape1=k+1, shape2=n-k+1, conf = 0.95)
abline(v=best_ci[1], lty=2, col = "blue")
abline(v=best_ci[2], lty=2, col = "blue")

