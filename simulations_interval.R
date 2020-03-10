library(dplyr)
library(rdrobust)
library(ggplot2)
library(reshape2)
library(gridExtra)


rm(list=ls(all=TRUE))
setwd("C:/Users/yaela/Dropbox (Brown)/Brown/RA/RD/R") 

###### PARAMETERS
set.seed(1)
jump=10
intervals=c(5,10,20,40)
loop=1000
quadratic=T
normal.x=F
var.list=c("coef.c","coef.bc","obw.c","obw.bc")

### SETTING UP FOR EVERYTHING - A SINGLE DGP ###
df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df=subset(df,df$x!=0)
df$treated <- ifelse(df$x>0, 1, 0)
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))

quad_test=ifelse(quadratic==T,"quadratic","linear")
normal_test=ifelse(normal.x==T,"rnorm","runif")
save.ext=paste(quad_test,normal_test,sep = "_")

### DGP: 
df$y.model<- 0.5*df$x  - 0.025*df$x^2*quadratic + jump*df$treated
df$y=df$y.model+rnorm(length(df$x),0,10)

## PLOT OF DGP
figure_name=paste("figures/dgp_",quad_test,".png",sep = "")
png(figure_name)
df %T>%
  plot(y~x,., ylim = range(c(y,y.model)),
       col="blue", ylab = "Y", xlab = "X") %T>%
  par(new = T) %>%
  plot(y.model~x,., ylim = range(c(y,y.model)),
       axes = FALSE, xlab = "", ylab = "")
dev.off()

df.temp=as.data.frame(matrix(0, ncol = length(intervals), nrow = loop))
colnames(df.temp) = paste("int",intervals,sep="_")

for (df.name in var.list) {
  assign(df.name,df.temp)
}


for (i in 1:loop) {
  if (normal.x==T) {
    sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
  } else {
    sample.x$x <- round(runif(nrow(df)/10, -20,20),digits = 2) 
  }

  sample.x=subset(sample.x, x>-100 & x<100)
  sample=as.data.frame(inner_join(df, sample.x, by="x"))

  for (j in intervals) {
    t=which(j==intervals)
    sample.int=subset(sample,x> -j & x< j)
     results.current=rdrobust(sample.int$y,sample.int$x)
    coef.c[i,t]=results.current$coef[1]-jump
    coef.bc[i,t]=results.current$coef[2]-jump
    obw.c[i,t]=results.current$bws[1,1]
    obw.bc[i,t]=results.current$bws[2,1]
    
      if (i==1000) {
      figure_name=paste("figures/sample_coef_c_int_",j,save.ext,".png",sep = "")
      png(figure_name)
      sample.int %T>%
          plot(y~x,., ylim = range(c(y,y.model)),
             col="blue", ylab = "Y", xlab = "X") %T>%
        par(new = T) %>%
        plot(y.model~x,., ylim = range(c(y,y.model)),
             axes = FALSE, xlab = "", ylab = "")
      dev.off()
    }
    }  
  }

####### SAVING RESULTS TO TABLE ###########
  results_table=as.data.frame(matrix(0, ncol = 4, nrow = 0))
  results_table[1,]=round(colMeans(coef.c),digits = 4)
  results_table[2,]=round(colMeans(coef.bc),digits = 4)
  results_table[3,]=round(colMeans(obw.c),digits = 4)
  results_table[4,]=round(colMeans(obw.bc),digits = 4)
  colnames(results_table) = paste("int",intervals,sep="_")
  rownames(results_table) =var.list
  
  table_name=paste("figures/results_table_",save.ext,".pdf",sep = "")
  pdf(table_name)
  grid.table(results_table)
  dev.off()

############## FIGURES #####
fig.df <- data.frame(coef_bc=c(coef.bc$int_5,coef.bc$int_10,coef.bc$int_20,coef.bc$int_40), spec = rep(c("interval=5", "interval=10", "interval=20","interval=40"),
                                      each = loop))
fig.df$spec <- factor(fig.df$spec, levels =c("interval=5", "interval=10", "interval=20","interval=40"))
figure_name=paste("figures/coef_bc_interval_bias_",save.ext,".png",sep = "")

png(figure_name)
ggplot(fig.df, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) +
  geom_vline(xintercept=0,  linetype="dashed")
dev.off()

fig.df <- data.frame(coef_c=c(coef.c$int_5,coef.c$int_10,coef.c$int_20,coef.c$int_40), 
                  spec = rep(c("interval=5", "interval=10", "interval=20","interval=40"),                                                                                                    each = loop))
fig.df$spec <- factor(fig.df$spec, levels =c("interval=5", "interval=10", "interval=20","interval=40"))
figure_name=paste("figures/coef_c_interval_bias_",save.ext,".png",sep = "")
png(figure_name)
ggplot(fig.df, aes(x = coef_c, fill = spec)) + geom_density(alpha = 0.3) +
  geom_vline(xintercept=0,  linetype="dashed")
dev.off()

