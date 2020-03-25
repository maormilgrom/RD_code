#install.packages('rdrobust')
library(dplyr)
library(rdrobust)
setwd("C:/Users/yaela/Dropbox (Brown)/Brown/RA/Neil/R")  


### NORMAL DISTRIBUTION ON BOTH SIDES
rm(list=ls(all=TRUE))
set.seed(1)

df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df$treated <- ifelse(df$x>0, 1, 0)
loop=1000
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))

for (jump in c(1,5,10,20)) {
df$y<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  
# y is a function of x, to get the density we want, more near cutoff
for(i in 1:loop) {
sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
# Using a normal dist around cutoff, will give more obs close to cutoff
sample.x=subset(sample.x, x>-100 & x<100)
sample=inner_join(df, sample.x, by="x")
sample=as.data.frame(sample)
results[i,1]=rdrobust(sample$y,sample$x)$coef[3]
results[i,2:3]=rdrobust(sample$y,sample$x)$bws[1,1:2]
}
png(paste("figures/coef_dist_x_norm_j",jump,".png", sep = ""))
plot(density(results$coef), main=paste("Jump = ",jump,sep = ""),xlab = "Treatment Effects", ylab = "Density", lwd=2) 
abline(v = jump, col="red", lwd=1, lty=2)
axis(1, round(range(results$coef)[1]:round(range(results$coef)[2])))
dev.off()
plotdata=results$bw_l
png(paste("figures/bw_dist_x_norm_j",jump,".png", sep = ""))
plot(density(plotdata), main=paste("Jump = ",jump, sep = ""),xlab = "Bandwidth", ylab = "Density", lwd=2) 
axis(1, round(range(plotdata)[1]:round(range(plotdata)[2])))
dev.off()

reg1=lm(bw_l~coef,data=results)
png(paste("figures/bw_coef_x_norm_j",jump,".png",sep = ""))
plot(results$coef,results$bw_l,xlscatterab = "Treatment Effect", ylab = "Bandwidth")
abline(reg1)
dev.off()

png(paste("figures/rdplot_eg_x_norm_j",jump,".png",sep = ""))
rdplot(sample$y,sample$x)
dev.off()
}


### Uniform draw of x's
rm(list=ls(all=TRUE))
set.seed(1)

df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df$treated <- ifelse(df$x>0, 1, 0)
loop=1000
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))

for (jump in c(1,5,10,20)) {
df$y<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  
  for(i in 1:loop) {
  sample.x$x <- round(runif(nrow(df)/10, -30, 30),digits = 2) 
    sample.x=subset(sample.x, x>-100 & x<100)
    sample=inner_join(df, sample.x, by="x")
    sample=as.data.frame(sample)
    results[i,1]=rdrobust(sample$y,sample$x)$coef[3]
    results[i,2:3]=rdrobust(sample$y,sample$x)$bws[1,1:2]
  }
  png(paste("figures/coef_dist_x_unif_j",jump,".png", sep = ""))
  plot(density(results$coef), main=paste("X Uniform, Jump = ",jump,sep = ""),xlab = "Treatment Effects", ylab = "Density", lwd=2) 
  abline(v = jump, col="red", lwd=1, lty=2)
  axis(1, round(range(results$coef)[1]:round(range(results$coef)[2])))
  dev.off()
  plotdata=results$bw_l
  png(paste("figures/bw_dist_j_x_unif",jump,".png", sep = ""))
  plot(density(plotdata), main=paste("X Uniform, Jump = ",jump, sep = ""),xlab = "Bandwidth", ylab = "Density", lwd=2) 
  axis(1, round(range(plotdata)[1]:round(range(plotdata)[2])))
  dev.off()
  
  reg1=lm(bw_l~coef,data=results)
  png(paste("figures/bw_coef_scatter_x_unif_j",jump,".png",sep = ""))
  plot(results$coef,results$bw_l,xlab = "Treatment Effect", ylab = "Bandwidth")
  abline(reg1)
  dev.off()
  
  png(paste("figures/rdplot_eg_x_unif_j",jump,".png",sep = ""))
  rdplot(sample$y,sample$x)
  dev.off()
}

### DIFFERENT POLYNOMIALS ON EACH SIDE - X Normal
rm(list=ls(all=TRUE))
set.seed(1)

df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df$treated <- ifelse(df$x>0, 1, 0)
loop=10
colnames(results) <- c("coef","bw_l","bw_h")
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
df=cbind(y=0,df)

results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
for (jump in c(1,5,10,20)) {
  df$y[df$treated == 1]<- jump*df$treated[df$treated == 1] + 0.5*df$x[df$treated == 1]  - 0.025*df$x[df$treated == 1]^2+rnorm(length(df$x[df$treated==1]),0,10)  
  df$y[df$treated == 0]<- jump*df$treated[df$treated == 0] + 0.5*df$x[df$treated == 0]  - 0.025*df$x[df$treated == 0]^2 +  0.0005*df$x[df$treated==0]^3+rnorm(length(df$x[df$treated==0]),0,10)  
  

  for(i in 1:loop) {
   sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
    # Using a normal dist around cutoff, will give more obs close to cutoff
    sample.x=subset(sample.x, x>-100 & x<100)
    sample=inner_join(df, sample.x, by="x")
    sample=as.data.frame(sample)
    results[i,1]=rdrobust(sample$y,sample$x)$coef[3]
    results[i,2:3]=rdrobust(sample$y,sample$x)$bws[1,1:2]
    
      }
  png(paste("figures/coef_dist_x_norm_l2_r3_j",jump,".png", sep = ""))
  plot(density(results$coef), main=paste("Jump = ",jump,sep = ""),xlab = "Treatment Effects", ylab = "Density", lwd=2) 
  abline(v = jump, col="red", lwd=1, lty=2)
  axis(1, round(range(results$coef)[1]:round(range(results$coef)[2])))
  dev.off()
  plotdata=results$bw_l
  png(paste("figures/bw_dist_x_norm_l2_r3_j",jump,".png", sep = ""))
  plot(density(plotdata), main=paste("Jump = ",jump, sep = ""),xlab = "Bandwidth", ylab = "Density", lwd=2) 
  axis(1, round(range(plotdata)[1]:round(range(plotdata)[2])))
  dev.off()
  
  reg1=lm(bw_l~coef,data=results)
  png(paste("figures/bw_coef_x_norm_l2_r3_scatter_j",jump,".png",sep = ""))
  plot(results$coef,results$bw_l,xlab = "Treatment Effect", ylab = "Bandwidth")
  abline(reg1)
  dev.off()
  
  png(paste("figures/rdplot_eg_x_norm_l2_r3_j",jump,".png",sep = ""))
  rdplot(sample$y,sample$x)
  plot(sample$x,sample$y)
  dev.off()
  
  results_name=paste("results_x_norm_l2_r3_j",jump,sep = "")
  assign(results_name,results)

}

### DIFFERENT POLYNOMIALS ON EACH SIDE - X uniform
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
for (jump in c(1,5,10,20)) {
  df$y[df$treated == 1]<- jump*df$treated[df$treated == 1] + 0.5*df$x[df$treated == 1]  - 0.025*df$x[df$treated == 1]^2+rnorm(length(df$x[df$treated==1]),0,10)  
  df$y[df$treated == 0]<- jump*df$treated[df$treated == 0] + 0.5*df$x[df$treated == 0]  - 0.025*df$x[df$treated == 0]^2 +  0.0005*df$x[df$treated==0]^3+rnorm(length(df$x[df$treated==0]),0,10)  
  
  
  for(i in 1:loop) {
    sample.x$x <- round(runif(nrow(df)/10, -30, 30),digits = 2) 
    # Using a normal dist around cutoff, will give more obs close to cutoff
    sample.x=subset(sample.x, x>-100 & x<100)
    sample=inner_join(df, sample.x, by="x")
    sample=as.data.frame(sample)
    results[i,1]=rdrobust(sample$y,sample$x)$coef[3]
    results[i,2:3]=rdrobust(sample$y,sample$x)$bws[1,1:2]
  }
  png(paste("figures/coef_dist_x_unif_l2_r3_j",jump,".png", sep = ""))
  plot(density(results$coef), main=paste("Jump = ",jump,sep = ""),xlab = "Treatment Effects", ylab = "Density", lwd=2) 
  abline(v = jump, col="red", lwd=1, lty=2)
  axis(1, round(range(results$coef)[1]:round(range(results$coef)[2])))
  dev.off()
  plotdata=results$bw_l
  png(paste("figures/bw_dist_x_unif_l2_r3_j",jump,".png", sep = ""))
  plot(density(plotdata), main=paste("Jump = ",jump, sep = ""),xlab = "Bandwidth", ylab = "Density", lwd=2) 
  axis(1, round(range(plotdata)[1]:round(range(plotdata)[2])))
  dev.off()
  
  reg1=lm(bw_l~coef,data=results)
  png(paste("figures/bw_coef_x_unif_l2_r3_scatter_j",jump,".png",sep = ""))
  plot(results$coef,results$bw_l,xlab = "Treatment Effect", ylab = "Bandwidth")
  abline(reg1)
  dev.off()
  
  png(paste("figures/rdplot_eg_x_unif_l2_r3_j",jump,".png",sep = ""))
  rdplot(sample$y,sample$x)
  plot(sample$x,sample$y)
  dev.off()
  
  results_name=paste("results_x_unif_l2_r3_j",jump,sep = "")
  assign(results_name,results)
  
}


###########
results_all=cbind(results_all,paste("results","x_norm_l2_r3_j",jump,sep = "")=results)

# NAMING COLUMNS BASED ON VARIABLES - didn't really work
coef=paste("coef_x_n_l2_r3_j",jump,sep = "")
results[,coef[i]]=rdrobust(sample$y,sample$x)$coef[3]
results[,coef[i]]=6
#results$coef2[i]=rdrobust(sample$y,sample$x)$coef[3]
bwl=paste("bwl_x_n_l2_r3_j",jump,sep = "")
#results$bwl[i]=rdrobust(sample$y,sample$x)$bws[1,1]
#bwr=paste("bwr_x_n_l2_r3_j",jump,sep = "")
#results$bwr[i]=rdrobust(sample$y,sample$x)$bws[1,2]

#df$y<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  



results=rdrobust(y, x)
bw_est=
  List[[length(List)+1]] = results$bws[1,]
t=rdrobust(sample$y,sample$x)

plot(sample$x,sample$y)
plot(df$x,df$y)
rdplot(sample$y,sample$x)


sample.x <- round(rnorm(200, 0, 10),digits = 2) # Using a normal dist around cutoff, will give more obs close to cutoff
sample.y<- jump*z + 0.5*sample.x  - 0.025*sample.x^2+rnorm(length(sample.x))  # y is a function of x, to get the density we want, more near cutoff

sample.y <- which(x==sample.x) 
plot(sample.x,y[x==sample.x])

#Randomly sample from T~N(50, 100) and specify the regression function.
#set.seed(1)
# y <- rnorm(200, 50, 10) + 10*z + 0.5*x - 0.025*x^2
y=50+jump*z + 0.5*x  - 0.025*x^2
noisy.y<- jump*z + 0.5*x  - 0.025*x^2+rnorm(200, 50, 10)  # y is a function of x, to get the density we want, more near cutoff
rdplot(y,x)
plot(x,noisy.y)
curve(50 + 0.5*x  - 0.025*x^2, add=T)

plot(x,y)

plot(noisy.y~x,col="red", ylim=c(min(y)-10, max(y)+10))
curve(50 + 0.5*x  - 0.025*x^2, add=T)

plot(noisy.y~x,col="red", ylim=c(min(y)-10, max(y)+10))
curve(50 + 10 + 0.5*x - 0.025*x^2, min(x)-10, 0, add=T, lwd=2)
curve(50 + 0.5*x - 0.025*x^2, 0, max(x)+10, add=T, lwd=2)
curve(50 + 0.5*x  - 0.025*x^2)
min(x)-10, 0, add=T, lwd=2)

rdrobust(y, x)
curve(x,y, col="green")
results=rdrobust(y, x)
bw_est=
List[[length(List)+1]] = results$bws[1,]


#1 σ effect size #Plot the observations and regression lines.
plot(x[tx],y[tx])
plot(tx[y],y)
plot(y~x)
abline(v=0, col="red")

plot(y~x, col=NULL, xlab="Assignment", ylab="Outcome", ylim=c(min(y)-10, max(y)+10))
abline(v=0, lty=2, lwd=2, col="darkgrey")
points(x[tx], y[tx], col="darkgrey", pch=4)
points(x[-tx], y[-tx], col="darkgrey")
curve(50 + 10 + 0.5*x - 0.025*x^2, min(x)-10, 0, add=T, lwd=2)
curve(50 + 0.5*x - 0.025*x^2, 0, max(x)+10, add=T, lwd=2)
title(main="Large effect at cutoff; continuous quadratic relationshipn")
mtext(expression(italic(Y[i]) == 50 + 10*italic(Z[i]) + 0.5*italic(X[i]) - 0.025*italic(X[i])^2 + italic(epsilon[i])), line=.5)
results=rdrobust(y=y, x=x)
bw_est=results$bws[1,]

### UNIFORM DISTRIBUTION ON BOTH SIDES
set.seed(1)
df <- as.data.frame(matrix(0, ncol = 4, nrow = 1000))
names <- c("id", "group","y","x")
colnames(df) <- names
jump=5
df$y[1:500] <- -0.5+sample(0:100, size = nrow(df)/2, replace = TRUE)/100
df$y[501:1000] <- -0.5+jump+sample(0:100, size = nrow(df)/2, replace = TRUE)/100
df$group[1:500]=0
df$group[501:1000]=1
df <- df %>% mutate(id = row_number())
df$x=df$id
df$x <- df$x+runif(nrow(df), -1, 1)-500

attach(df)
rdplot(y,x)
rdplot(y=y, x=x, binselect="es", ci=95)
results=rdrobust(y=y, x=x)
bw_est=results$bws[1,]
print(bw_est)

