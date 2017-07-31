

AsianOptionAnalyticalSol <- function(){
#Kenma frost

t= 0
b= 0.5 * (r-(sigma^2)/6);
sigmaA = sigma/sqrt(3);

d1 = (log(S0/E)+(b+0.5*(sigmaA^2))*(T-t))/(sigmaA*sqrt(T-t))
d2 = d1 - (sigmaA*sqrt(T-t))

call_geom = S0*exp((b-r)*(T-t))*pnorm(d1)-E*exp(-r*(T-t))*pnorm(d2);
put_geom = -S0*exp((b-r)*(T-t))*pnorm(-d1)+E*exp(-r*(T-t))*pnorm(-d2);


#Levy
T=1
T2=T
D=0
M1 = 2*S0^2
M2 = r-D + sigma^2;
M3 = ((2*(r-D)+sigma^2)*T2);
M4 = 2*(r-D) + sigma^2;
M5 = (r-D) * T2;
M = (M1/M2) * ((exp(M3)-1)/M4 -((exp(M5)-1)/(r-D)));
L = M / (T^2);
S_Z = S0 * (exp(-D*T2)-exp(-r*T2))/((r-D)*T);
X=log(L)-2*(r*T2+log(S_Z));

EZ = E;

d1 = (0.5 * log(L)-log(EZ))/sqrt(X);
d2 = d1 - sqrt(X);

call_arith = S_Z*pnorm(d1) - EZ*(exp(-r*T2)) * pnorm(d2);
put_arith = call_arith - S_Z + EZ * exp(-r*T2);

AsianOptionAnalyticalSol <- c(call_arith,put_arith,call_geom,put_geom)

}

