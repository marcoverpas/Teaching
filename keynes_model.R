#A simple Keynes model (fixed prices)

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
c0 = 10     #Autonomous consumption
c1 = 0.8    #Marginal propensity to consume out of income
i0 = 0.04   #Interest rate
b0 = 14     #Autonomous investment
b1 = 100    #Sensitivity of investment to interest rate
prod = 5    #Labour productivity
w1 = 2.5    #Initial value of nominal wage rate
mu = 1      #Mark-up
g0 = 0      #Government spending

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
p=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Aggregate saving
S=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Interest rate
i=matrix(data=i0,nrow=nScenarios,ncol=nPeriods)
#Nominal wage rate
w=matrix(data=w1,nrow=nScenarios,ncol=nPeriods)
#Real wage rate
wr=matrix(data=2.5,nrow=nScenarios,ncol=nPeriods)

#Scenarios loop
for (j in 1:nScenarios){

#Time loop
for (t in 2:nPeriods){
  
  #Iterations
  for (iter in 2:20){
  
  #Negative shock to investment after period 25 in scenario 2
  if (t >= 25 & j==2) {b0 = 8}  
    
  ############################# SHARED EQUATIONS 
  
  Z[j,t] = C[j,t] + I[j,t] + G[j,t] #Aggregate demand
  
  Y[j,t] = Z[j,t]                   #Nominal output
  
  C[j,t] = c0 + c1 * Y[j,t-1]       #Consumption
   
  I[j,t] = b0 - b1 * i[j,t-1]       #Investment
  
  G[j,t] = g0                       #Government spending
  
  ############################# MODEL-SPECIFIC EQUATIONS
  
  S[j,t] = Y[j,t] - C[j,t]         #Private saving (income that is not consumed)
  
  i[j,t] = i0                      #Interest rate (set by central bank either directly or through changes in money supply)
  
  p[j,t] = (w[j,t]/prod)*(1 + mu)  #Price level (mark-up rule)
  
  X[j,t] = Y[j,t]/p[j,t]           #Real output (demand-driven)
  
  Ld[j,t] = X[j,t]/prod            #Labour demand (determined on goods market)
  
  Ls[j,t] = Ld[j,t]                #Labour supply (adjusting to labour demand)
  
  wr[j,t] = w[j,t]/p[j,t]          #Real wage rate (as ratio of nominal wage rate to price level)
  
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
