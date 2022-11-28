#A simple 3-equation model

#Made by mvp, 24/11/2021

# Clear Environment
rm(list=ls(all=TRUE))

# Clear Plots
#if(!is.null(dev.list())) dev.off()

# Clear Console
cat("\014")

#Number of periods
nPeriods = 100

#Number of scenarios
nScenarios=7 

#Set coefficient values
a1 = 0.5
a2 = 0.5
a3 = 0.5

#Create shock components (including autonomous consumption)
a0=matrix(data=0.025,nrow=nScenarios,ncol=nPeriods)
pi0=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
r0=matrix(data=0,nrow=nScenarios,ncol=nPeriods)

#Create variables
yn=matrix(data=1,nrow=nScenarios,ncol=nPeriods)
pit=matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)
rn=matrix(data=a0/a1,nrow=nScenarios,ncol=nPeriods)
y=matrix(data=yn,nrow=nScenarios,ncol=nPeriods)
pi=matrix(data=pit,nrow=nScenarios,ncol=nPeriods)
pis=matrix(data=pit,nrow=nScenarios,ncol=nPeriods)
r=matrix(data=rn,nrow=nScenarios,ncol=nPeriods)


#Create and solve model

#Choose scenario
for (j in 1:nScenarios){
  
  #Define time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:100){
      
      #Demand shock (permanent)
      if (i>=10 && j==2){
        a0[j,i]=0.03}  
      
      #Inflation shock (temporary, meaning permanent increase in price level)
      if (i==10 && j==3){
        pi0[j,i]=0.010}
      
      #Target inflation change 
      if (i>=10 && j==4){
        pit[j,i]=0.01}
      
      #Demand shock (permanent) --- non-anchored expectations
      if (i>=10 && j==5){
        a0[j,i]=0.03}  
      
      #Inflation shock (temporary, meaning permanent increase in price level) --- non-anchored expectations
      if (i==10 && j==6){
        pi0[j,i]=0.010}
      
      #Target inflation change --- non-anchored expectations
      if (i>=10 && j==7){
        pit[j,i]=0.01}
      
      #3-equation model
      y[j,i] = yn[j,i] - a1*r[j,i-1] + a0[j,i]                        #Output level  
      
      if (j<=4)                                                       #Inflation rate
      {pi[j,i] = pit[j,i-1] + a2*(y[j,i-1] - yn[j,i-1]) + pi0[j,i]}   # - Anchored expectations
      else
      {pi[j,i] = pi[j,i-1] + a2*(y[j,i-1] - yn[j,i-1]) + pi0[j,i]}    # - Non-anchored expectations
      
      r[j,i] = rn[j,i] + a3*(pi[j,i] - pit[j,i]) + r0[j,i]            #Policy rate (Taylor rule)
      
      #Other equations
      rn[j,i] = pi0[j,i]/a2 + a0[j,i]/a1                              #Equilibrium rate of interest (depending on a0). Note: pi0[j,i]/a2 allows considering inflation shocks
      
    }
  }
}


#Chart A: SHOCK TO DEMAND

#Layout for plots
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

#Plot figure 1
plot(y[2,8:25],type="l",col=2,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 1  Output following demand shock ",ylab = 'y',xlab = 'Time',ylim=range(0.994,1.006))
abline(h=1,lty=3)

#Plot figure 2a
plot(pi[5,8:25],type="l",col=3,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 2a  Inflation following demand shock \n (persistent inflation)",ylab = 'pi',xlab = 'Time',ylim=range(0.014,0.026))
abline(h=0.02,lty=3)

#Plot figure 2b
plot(pi[2,8:25],type="l",col=3,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 2b  Inflation following demand shock \n (anchored expectations) ",ylab = 'pi',xlab = 'Time',ylim=range(0.014,0.026))
abline(h=0.02,lty=3)

#Plot figure 3
plot(r[2,8:25],type="l",col=4,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 3  Policy rate following demand shock",ylab = 'r',xlab = 'Time',ylim=range(0.035,0.065))
abline(h=0.05,lty=3)

#Chart B: (TEMPORARY) SHOCK TO INFLATION

#Layout for plots
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

#Plot figure 1
plot(y[3,8:25],type="l",col=2,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 1  Output following price shock ",ylab = 'y',xlab = 'Time',ylim=range(0.98,1.02))
abline(h=1,lty=3)

#Plot figure 2a
plot(pi[6,8:25],type="l",col=3,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 2a  Inflation following price shock \n (persistent inflation)",ylab = 'pi',xlab = 'Time',ylim=range(0.01,0.03))
abline(h=0.02,lty=3)

#Plot figure 2b
plot(pi[3,8:25],type="l",col=3,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 2b  Inflation following price shock \n (anchored expectations) ",ylab = 'pi',xlab = 'Time',ylim=range(0.01,0.03))
abline(h=0.02,lty=3)

#Plot figure 3
plot(r[3,8:25],type="l",col=4,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 3  Policy rate following price shock",ylab = 'r',xlab = 'Time',ylim=range(0.02,0.08))
abline(h=0.05,lty=3)

#Chart C: SHOCK TO POLICY RATE

#Layout for plots
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

#Plot figure 1
plot(y[4,8:25],type="l",col=2,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 1  Output following change in target inflation ",ylab = 'y',xlab = 'Time',ylim=range(0.995,1.005))
abline(h=1,lty=3)

#Plot figure 2a
plot(pi[7,8:25],type="l",col=3,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 2a  Inflation following change in target inflation \n (persistent inflation)",ylab = 'pi',xlab = 'Time',ylim=range(0.01,0.03))
abline(h=0.02,lty=3)
abline(h=0.01,lty=3)

#Plot figure 2b
plot(pi[4,8:25],type="l",col=3,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 2b  Inflation following change in target inflation \n (anchored expectations) ",ylab = 'pi',xlab = 'Time',ylim=range(0.005,0.035))
abline(h=0.02,lty=3)
abline(h=0.01,lty=3)

#Plot figure 3
plot(r[4,8:25],type="l",col=4,lwd=3,lty=1,font.main=1,cex.main=1,main="Fig. 3  Policy rate following change in target inflation",ylab = 'r',xlab = 'Time',ylim=range(0.045,0.055))
abline(h=0.05,lty=3)
