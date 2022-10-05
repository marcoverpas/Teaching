#A simple neoclassical-Keynesian model (flexible prices but sticky nominal wages)

#Made by mvp, 4/10/2022

# Clear Environment
rm(list=ls(all=TRUE))

# Clear Plots
#if(!is.null(dev.list())) dev.off()

# Clear Console
cat("\014")

#Time span
nPeriods = 50

#Scenarios
nScenarios = 2

#Parameters
c0 = 10        #Autonomous consumption
c1=0.8         #Marginal propensity to consume out of income
i0 = 0.04      #Interest rate
b0 = 14        #Autonomous investment
b1 = 100       #Sensitivity of investment to interest rate
prod = 5       #Labour productivity
g0 = 0         #Government spending
l0 = 45        #Labour demand independent of wages
l1 = 10        #Sensitivity of labour demand to real wage rate
l2 = 8         #Sensitivity of labour supply to real wage rate
s0 = 6         #Saving independent of interest rate
s1 = 100       #Sensitivity of saving to interest rate
p1 = 1         #Initial price level
gamma_w = 0.2  #Speed of adjustment of nominal wage rate
w1 = 2.5       #Initial level of nominal wage rate  
L_f = 20       #Full employment level

#Variables
#Aggregate demand
Z=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Aggregate output (supply) in nominal terms
Y=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Aggregate output (supply) in real terms
X=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Aggregate consumption
C=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Aggregate investment
I=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Government spending
G=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Labour demand 
Ld=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Labour supply
Ls=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Price level
p=matrix(data=p1,nrow=nScenarios,ncol=nPeriods)
#Aggregate saving
S=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Interest rate
i=matrix(data=i0,nrow=nScenarios,ncol=nPeriods)
#Real wage rate
wr=matrix(data=2.5,nrow=nScenarios,ncol=nPeriods)
#Full-employment real wage rate
wr_f=matrix(data=2.5,nrow=nScenarios,ncol=nPeriods)
#Nominal wage rate
w=matrix(data=w1,nrow=nScenarios,ncol=nPeriods)

#Scenarios loop
for (j in 1:nScenarios){

#Time loop
for (t in 2:nPeriods){

  #Iterations  
  for (iter in 1:20){
  
  #Negative shock to investment after period 25 in scenario 2
  if (t >= 25 & j==2) {b0 = 8}  
  
  #############################  SHARED EQUATIONS
    
  Z[j,t] = C[j,t] + I[j,t] + G[j,t] #Aggregate demand
  
  Y[j,t] = Z[j,t]                   #Nominal output 
  
  C[j,t] = c0 + c1 * Y[j,t-1]       #Consumption
  
  I[j,t] = b0 - b1 * i[j,t-1]       #Investment                 
  
  G[j,t] = g0                       #Government spending
  
  ############################# MODEL-SPECIFIC EQUATIONS
  
  S[j,t] = s0 + s1 * i[j,t-1]                         #Private saving (as a positive function of i)
  
  i[j,t] = (b0 - s0)/(s1 + b1)                        #Interest rate (such that S = I)
  
  p[j,t] = (Y[j,t] + prod * l1 * w1)/(prod * l0)      #Price level (market clearing): p[j,t] = Y[j,t]/X[j,t]
  
  X[j,t] = Ls[j,t]*prod                               #Real output (defined by production function)
  
  Ld[j,t] = l0 - l1*wr[j,t]                           #Labour demand (as a negative function of the real wage)
  
  Ls[j,t] = Ld[j,t]                                   #Labour supply (adjusting to labour demand)
  
  wr_f[j,t] = l0/(l2 + l1)                            #Full-employment real wage rate (such that: Ld = Ls)  
  
  wr[j,t] = w[j,t]/p[j,t]                             #Real wage rate (as ratio of nominal wage rate to price level)

  w[j,t] = w[j,t-1] - gamma_w*(wr[j,t-1] - wr_f[j,t]) #Nominal wage rate (sticky, adjusting slowly to new labour market conditions)
  
  #############################
  
}}}


#Set plot layout
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

#Plot demand components
plot(Z[1,23:nPeriods]/p[1,23:nPeriods], type="l",col=1,lty=2,ylim=range(0,100),main="a) Demand components (real)",ylab = '$',xlab = 'Time')
lines(C[1,23:nPeriods]/p[1,23:nPeriods],col=2,lty=2)
lines(I[1,23:nPeriods]/p[1,23:nPeriods],col=3,lty=2)
lines(G[1,23:nPeriods]/p[1,23:nPeriods],col=4,lty=2)
lines(Z[2,23:nPeriods]/p[2,23:nPeriods],col=1)
lines(C[2,23:nPeriods]/p[2,23:nPeriods],col=2)
lines(I[2,23:nPeriods]/p[2,23:nPeriods],col=3)
lines(G[2,23:nPeriods]/p[2,23:nPeriods],col=4)
#abline(v=5,col="gray30",lty=3)
legend("right",c("Demand","Consumption","Investment","Gov. spending"),  bty = 'n', lty=c(1,1,1,1), col = c(1,2,3,4), box.lty=0)

#Plot price level and nominal wage
plot(p[2,23:nPeriods]/p[1,23:nPeriods], type="l",col=5,main="b) Prices and wages after shock",ylab = 'Ratio to baseline',xlab = 'Time')
lines(w[2,23:nPeriods]/w[1,23:nPeriods], col=6, lty=2)
legend("topright",c("Price","Nominal wage"),  bty = 'n', lty=c(1,2), col = c(5,6), box.lty=0)

#Plot employment
plot(Ld[2,23:nPeriods]/Ld[1,23:nPeriods], type="l",col="orange",main="c) Employment level after shock",ylab = 'Ratio to baseline',xlab = 'Time')
abline(h=1,col="gray20",lty=3)

#Plot investment and saving
plot(I[2,23:nPeriods]/I[1,23:nPeriods], type="l",col=3,main="d) Investment and saving after shock",ylab = 'Ratio to baseline',xlab = 'Time')
lines(S[2,23:nPeriods]/S[1,23:nPeriods], col=1, lty=2)
legend("topright",c("Investent","Saving"),  bty = 'n', lty=c(1,2), col = c(3,1), box.lty=0)
