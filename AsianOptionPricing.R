AsianOptionPricing <- function (s,tau, E, r, k){

  Title <- c("arithmetic_fixed_call","geometric_fixed_call","arithmetic_fixed_put","geometric_fixed_put",
             "arithmetic_float_call","geometric_float_call","arithmetic_float_put","geometric_float_put");
  P <- NA;
  Std <- 0;
  arithmetic_mean = rowMeans(s);
  geometric_mean = exp(rowMeans(log(s)));
   
  func = function(x) max(x,0);
  P[1] = mean(exp(-r*tau)*sapply((arithmetic_mean-E),func));
  P[2]  = mean(exp(-r*tau)*sapply((geometric_mean-E), func));
  P[3]  = mean(exp(-r*tau)*sapply((E-arithmetic_mean),func));
  P[4]   = mean(exp(-r*tau)*sapply((E-geometric_mean), func));
  
  P[5] = mean(exp(-r*tau)*sapply((s[,ncol(s)]-k*arithmetic_mean),func));
  P[6]  = mean(exp(-r*tau)*sapply((s[,ncol(s)]-k*geometric_mean),func));
  P[7]  = mean(exp(-r*tau)*sapply((k*arithmetic_mean-s[,ncol(s)]),func));
  P[8]   = mean(exp(-r*tau)*sapply((k*geometric_mean-s[,ncol(s)]),func));
  
  Std[1] = sd(exp(-r*tau)*sapply((arithmetic_mean-E),func));
  Std[2]  = sd(exp(-r*tau)*sapply((geometric_mean-E), func));
  Std[3]  = sd(exp(-r*tau)*sapply((E-arithmetic_mean),func));
  Std[4]   = sd(exp(-r*tau)*sapply((E-geometric_mean), func));
  
  Std[5] = sd(exp(-r*tau)*sapply((s[,ncol(s)]-k*arithmetic_mean),func));
  Std[6]  = sd(exp(-r*tau)*sapply((s[,ncol(s)]-k*geometric_mean),func));
  Std[7]  = sd(exp(-r*tau)*sapply((k*arithmetic_mean-s[,ncol(s)]),func));
  Std[8]   = sd(exp(-r*tau)*sapply((k*geometric_mean-s[,ncol(s)]),func));
  
  print(Std)

  return(P);
  
}