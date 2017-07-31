EulerPriceSeries <- function(s_0,tau,dt,sigma,r,K){
  
  N =  tau/dt;
  s = matrix(, nrow = K, ncol = N);
  s[,1] = s_0;
  for (i in 1:K){
    for (j in 2:N){
          phi = rnorm(1);
          s[i,j] = s[i,j-1] * (1+ r*dt + sigma * phi * sqrt(dt));
    }
    if(i==1)
    plot(s[i,],type ='l',xlab = "Time",ylab = "Price",ylim=c(50, 200),main ="Asset Price Dynamics")
      
    else lines(s[i,],type ='l')
  }
  
  #plot(s[round(runif(1, 1, 100)),],type ='l',xlab = "Time",ylab = "Price",main ="Sample Asset Price Dynamics")
  return(s);
}