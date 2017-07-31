MD3 <- function(){
  
  s_0 = 100;
  tau = 1;
  dt = 1/252;
  sigma = 0.2;
  r = 0.05;
  E= 100;
  K = c(MD3 <- function(){
    
    s_0 = 100;
    tau = 1;
    dt = 1/252;
    sigma = 0.2;
    r = 0.05;
    E= 100;
    K = c(100,200,750,1000,2500,5000,7500,
          10000,20000,30000,40000,50000,60000,70000,80000,90000,10000);
    k = 1;
    
    P= rep(0,5,8);
    Title <- c("arithmetic_fixed_call","geometric_fixed_call","arithmetic_fixed_put","geometric_fixed_put",
               "arithmetic_float_call","geometric_float_call","arithmetic_float_put","geometric_float_put");
    
    
    for(i in 1:length(K)){
      s = EulerPriceSeries(s_0,tau,dt,sigma,r,K[i]);
      if (i == 1) P = AsianOptionPricing(s,tau, E, r, k)
      else P=rbind(P,AsianOptionPricing(s,tau, E, r, k));
      print(P);
    }
    
    plot(log(x),a,type = 'l',ylab = "Option Value Error in Percentage",xlab ="Number of Paths(log scale)",ylim=c(-0.1,0.15),main='Fixed Strike Asian Option Payoff Error vs Log(N)')
    lines(log(x),c,type = 'l',ylab = "Option Value",xlab ="Number of Paths(log scale)",col="blue")
    lines(log(x),d,type = 'l',ylab = "Option Value",xlab ="Number of Paths(log scale)",col="green",main="a")
    lines(log(x),b,type = 'l',ylab = "Option Value",xlab ="Number of Paths(log scale)",col="red")
    legend(9,0.12,c("Arithmetic Call","Arithmetic Put","Geometric Call","Geometric Put"),
           lty=c(1,1),
           lwd=c(2.5,2.5),col=c("black","red","blue","green"))
    
    
    return(P);
    
  });
  k = 1;
  
  P= rep(0,5,8);
  Title <- c("arithmetic_fixed_call","geometric_fixed_call","arithmetic_fixed_put","geometric_fixed_put",
             "arithmetic_float_call","geometric_float_call","arithmetic_float_put","geometric_float_put");
  
  
  for(i in 1:length(K)){
    s = EulerPriceSeries(s_0,tau,dt,sigma,r,K[i]);
      if (i == 1) P = AsianOptionPricing(s,tau, E, r, k)
      else P=rbind(P,AsianOptionPricing(s,tau, E, r, k));
      print(P);
  }
  # 
  # plot(log(x),a,type = 'l',ylab = "Option Value Error in Percentage",xlab ="Number of Paths(log scale)",ylim=c(-0.1,0.15),main='Fixed Strike Asian Option Payoff Error vs Log(N)')
  # lines(log(x),c,type = 'l',ylab = "Option Value",xlab ="Number of Paths(log scale)",col="blue")
  # lines(log(x),d,type = 'l',ylab = "Option Value",xlab ="Number of Paths(log scale)",col="green",main="a")
  # lines(log(x),b,type = 'l',ylab = "Option Value",xlab ="Number of Paths(log scale)",col="red")
  # legend(9,0.12,c("Arithmetic Call","Arithmetic Put","Geometric Call","Geometric Put"),
  # lty=c(1,1),
  # lwd=c(2.5,2.5),col=c("black","red","blue","green"))
  # 
  return(P);
}