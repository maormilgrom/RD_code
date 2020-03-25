#install.packages('rdrobust')
rm(list=ls(all=TRUE))
set.seed(1)
library(dplyr)
library(rdrobust)
library(ggplot2)
library(reshape2)
setwd("C:/Users/yaela/Dropbox (Brown)/Brown/RA/RD/R")  

### SETTING UP FOR EVERYTHING - A SINGLE DGP ###
df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df=subset(df,df$x!=0)
#df=df[-c(which(df$x==0)),] -  DOESN'T WORK, B/C DF IS NOT LONGER A DATA-FRAME
df$treated <- ifelse(df$x>0, 1, 0)

loop=1000
results<- as.data.frame(matrix(0, ncol = 8, nrow = loop))
colnames(results) <- c("coef_cl","coef_bc","bw_h_l","bw_h_r", "bw_h_length", 
                       "bw_b_l","bw_b_r","bw_b_length")
results.bw<- as.data.frame(matrix(0, ncol = 8, nrow = loop))
colnames(results.bw) <- c("coef_cl","coef_bc","bw_h_l","bw_h_r", "bw_h_length", 
                       "bw_b_l","bw_b_r","bw_b_length")
results.zero<- as.data.frame(matrix(0, ncol = 8, nrow = loop))
colnames(results.zero) <- c("coef_cl","coef_bc","bw_h_l","bw_h_r", "bw_h_length", 
                          "bw_b_l","bw_b_r","bw_b_length")
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
df$y.base<-0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  



### NORMAL DISTRIBUTION ON BOTH SIDES, SAME POLYNOMIAL
for (jump in c(1,5,10,20)) {
  df$y=df$y.base+jump*df$treated
  df$y.model<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2

  png(paste("figures/dgp_l2_r2_j",jump,".png",sep = ""))
  df.plot=df[df$x > -30 & df$x < 30,]
  plot(df.plot$x,df.plot$y, ylim = range(c(df.plot$y,df.plot$y.model)),
       col="blue", ylab = "Y DGP", xlab = "X")
  par(new = T)
  plot(df.plot$x,df.plot$y.model, ylim = range(c(df.plot$y,df.plot$y.model)),
       axes = FALSE, xlab = "", ylab = "")
  dev.off()
  for(i in 1:loop) {
  sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
  sample.x=subset(sample.x, x>-100 & x<100)
  sample=inner_join(df, sample.x, by="x")
  sample=as.data.frame(sample)
  results.current=rdrobust(sample$y,sample$x, bwselect = "msetwo")
  results[i,1:2]=t(results.current$coef[1:2,1])-jump # normalizing to zero
  results[i,3:4]=results.current$bws[1,1:2]
  results[i,5]=results[i,3]+results[i,4]
  results[i,6:7]=results.current$bws[2,1:2]
  results[i,8]=results[i,6]+results[i,7]
  
  sample.bw=subset(sample,x>=-results[i,6] & x<=results[i,7])
  results.current=rdrobust(sample.bw$y,sample.bw$x, bwselect = "msetwo")
  results.bw[i,1:2]=t(results.current$coef[1:2,1])-jump # normalizing to zero
  results.bw[i,3:4]=results.current$bws[1,1:2]
  results.bw[i,5]=results.bw[i,3]+results.bw[i,4]
  results.bw[i,6:7]=results.current$bws[2,1:2]
  results.bw[i,8]=results.bw[i,6]+results.bw[i,7]
  
  sample.zero=sample
  sample.zero$y <- ifelse(sample$x>=-results[i,6] & 
                             sample$x<=results[i,7], sample$y, 0)
  results.current=rdrobust(sample.zero$y,sample.zero$x, bwselect = "msetwo")
  results.zero[i,1:2]=t(results.current$coef[1:2,1])-jump # normalizing to zero
  results.zero[i,3:4]=results.current$bws[1,1:2]
  results.zero[i,5]=results.zero[i,3]+results.zero[i,4]
  results.zero[i,6:7]=results.current$bws[2,1:2]
  results.zero[i,8]=results.zero[i,6]+results.zero[i,7]
  

  }

# png(paste("figures/rdplot_eg_x_norm_l2_r2_j",jump,".png",sep = ""))
# rdplot(sample$y,sample$x)
# dev.off()
# 
# png(paste("figures/rdplot_eg_x_norm_l2_r2_j",jump,"_bw.png",sep = ""))
# rdplot(sample.bw$y,sample.bw$x)
# dev.off()
# 
# png(paste("figures/rdplot_eg_x_norm_l2_r2_j",jump,"_zero.png",sep = ""))
# rdplot(sample.zero$y,sample.zero$x)
# dev.off()


results_name=paste("results_x_norm_l2_r2_j",jump,sep = "")
assign(results_name,results)

results_name=paste("results_bw_x_norm_l2_r2_j",jump,sep = "")
assign(results_name,results.bw)

results_name=paste("results_zero_x_norm_l2_r2_j",jump,sep = "")
assign(results_name,results.zero)

}


### Uniform draw of x's
# df$y=0
# sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
# results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
# colnames(results) <- c("coef","bw_l","bw_h")

for (jump in c(1,5,10,20)) {
  df$y=df$y.base+jump*df$treated
  df$y.model<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2
  for(i in 1:loop) {
    sample.x$x <- round(runif(nrow(df)/10, -30, 30),digits = 2) 
    #sample.x=subset(sample.x, x>-100 & x<100)
    sample=inner_join(df, sample.x, by="x")
    sample=as.data.frame(sample)
    results.current=rdrobust(sample$y,sample$x, bwselect = "msetwo")
    results[i,1:2]=t(results.current$coef[1:2,1])-jump # normalizing to zero
    results[i,3:4]=results.current$bws[1,1:2]
    results[i,5]=results[i,3]+results[i,4]
    results[i,6:7]=results.current$bws[2,1:2]
    results[i,8]=results[i,6]+results[i,7]

    sample.bw=subset(sample,x>=-results[i,6] & x<=results[i,7])
    results.current=rdrobust(sample.bw$y,sample.bw$x, bwselect = "msetwo")
    results.bw[i,1:2]=t(results.current$coef[1:2,1])-jump # normalizing to zero
    results.bw[i,3:4]=results.current$bws[1,1:2]
    results.bw[i,5]=results.bw[i,3]+results.bw[i,4]
    results.bw[i,6:7]=results.current$bws[2,1:2]
    results.bw[i,8]=results.bw[i,6]+results.bw[i,7]
  
    sample.zero=sample
    sample.zero$y <- ifelse(sample$x>=-results[i,6] & 
                              sample$x<=results[i,7], sample$y, 0)
    results.current=rdrobust(sample.zero$y,sample.zero$x, bwselect = "msetwo")
    results.zero[i,1:2]=t(results.current$coef[1:2,1])-jump # normalizing to zero
    results.zero[i,3:4]=results.current$bws[1,1:2]
    results.zero[i,5]=results.zero[i,3]+results.zero[i,4]
    results.zero[i,6:7]=results.current$bws[2,1:2]
    results.zero[i,8]=results.zero[i,6]+results.zero[i,7]
    }

    # png(paste("figures/rdplot_eg_x_unif_l2_r2_j",jump,".png",sep = ""))
    # rdplot(sample$y,sample$x)
    # dev.off()
    # 
    # png(paste("figures/rdplot_eg_x_unif_l2_r2_j",jump,"_bw.png",sep = ""))
    # rdplot(sample.bw$y,sample.bw$x)
    # dev.off()
    # 
    # png(paste("figures/rdplot_eg_x_unif_l2_r2_j",jump,"_zero.png",sep = ""))
    # rdplot(sample.zero$y,sample.zero$x)
    # dev.off()
    # 
   
   results_name=paste("results_x_unif_l2_r2_j",jump,sep = "")
   assign(results_name,results)
 
   results_name=paste("results_bw_x_unif_l2_r2_j",jump,sep = "")
   assign(results_name,results.bw)

    results_name=paste("results_zero_x_unif_l2_r2_j",jump,sep = "")
  assign(results_name,results.zero)
  
  }

