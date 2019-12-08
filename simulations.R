########### SECOND ATTEMPT ###########################
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


### COMBINING DENSITY PLOTS
# 
# # ### coef_cl, poly l2_r2, normal
# # dat <- data.frame(coef_cl = c(results_x_norm_l2_r2_j1$coef_cl, results_x_norm_l2_r2_j5$coef_cl, 
# #                            results_x_norm_l2_r2_j10$coef_cl, results_x_norm_l2_r2_j20$coef_cl),
# #                    spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/coef_cl_l2_r2_norm.png")
# # ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ### coef_cl, poly l2_r2, uniform
# # dat <- data.frame(coef_cl = c(results_x_unif_l2_r2_j1$coef_cl, results_x_unif_l2_r2_j5$coef_cl, 
# #                               results_x_unif_l2_r2_j10$coef_cl, results_x_unif_l2_r2_j20$coef_cl),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/coef_cl_l2_r2_unif.png")
# # ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ### coef_bc, poly l2_r2, normal
# # dat <- data.frame(coef_bc = c(results_x_norm_l2_r2_j1$coef_bc, results_x_norm_l2_r2_j5$coef_bc, 
# #                               results_x_norm_l2_r2_j10$coef_bc, results_x_norm_l2_r2_j20$coef_bc),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/coef_bc_l2_r2_norm.png")
# # ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ### coef_bc, poly l2_r2, uniform
# # dat <- data.frame(coef_bc = c(results_x_unif_l2_r2_j1$coef_bc, results_x_unif_l2_r2_j5$coef_bc, 
# #                               results_x_unif_l2_r2_j10$coef_bc, results_x_unif_l2_r2_j20$coef_bc),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/coef_bc_l2_r2_unif.png")
# # ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ######## BW ######### 
# # ### bw_h_l, poly l2_r2, normal
# # dat <- data.frame(bw_h_l = c(results_x_norm_l2_r2_j1$bw_h_l, results_x_norm_l2_r2_j5$bw_h_l, 
# #                               results_x_norm_l2_r2_j10$bw_h_l, results_x_norm_l2_r2_j20$bw_h_l),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_h_l_l2_r2_norm.png")
# # ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_h_l, poly l2_r2, uniform
# # dat <- data.frame(bw_h_l = c(results_x_unif_l2_r2_j1$bw_h_l, results_x_unif_l2_r2_j5$bw_h_l, 
# #                               results_x_unif_l2_r2_j10$bw_h_l, results_x_unif_l2_r2_j20$bw_h_l),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_h_l_l2_r2_unif.png")
# # ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_h_r, poly l2_r2, normal
# # dat <- data.frame(bw_h_r = c(results_x_norm_l2_r2_j1$bw_h_r, results_x_norm_l2_r2_j5$bw_h_r, 
# #                              results_x_norm_l2_r2_j10$bw_h_r, results_x_norm_l2_r2_j20$bw_h_r),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_h_r_l2_r2_norm.png")
# # ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_h_r, poly l2_r2, uniform
# # dat <- data.frame(bw_h_r = c(results_x_unif_l2_r2_j1$bw_h_r, results_x_unif_l2_r2_j5$bw_h_r, 
# #                              results_x_unif_l2_r2_j10$bw_h_r, results_x_unif_l2_r2_j20$bw_h_r),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_h_r_l2_r2_unif.png")
# # ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_h norm
# # dat <- data.frame(bw_h_length = c(results_x_norm_l2_r2_j1$bw_h_length, results_x_norm_l2_r2_j5$bw_h_length, 
# #                                 results_x_norm_l2_r2_j10$bw_h_length, results_x_norm_l2_r2_j20$bw_h_length),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_h_length_l2_r2_norm.png")
# # ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_h uniform
# # dat <- data.frame(bw_h_length = c(results_x_unif_l2_r2_j1$bw_h_length, results_x_unif_l2_r2_j5$bw_h_length, 
# #                              results_x_unif_l2_r2_j10$bw_h_length, results_x_unif_l2_r2_j20$bw_h_length),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_h_length_l2_r2_unif.png")
# # ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # 
# # ### bw_b_l, poly l2_r2, normal
# # dat <- data.frame(bw_b_l = c(results_x_norm_l2_r2_j1$bw_b_l, results_x_norm_l2_r2_j5$bw_b_l, 
# #                              results_x_norm_l2_r2_j10$bw_b_l, results_x_norm_l2_r2_j20$bw_b_l),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_b_l_l2_r2_norm.png")
# # ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_b_l, poly l2_r2, uniform
# # dat <- data.frame(bw_b_l = c(results_x_unif_l2_r2_j1$bw_b_l, results_x_unif_l2_r2_j5$bw_b_l, 
# #                              results_x_unif_l2_r2_j10$bw_b_l, results_x_unif_l2_r2_j20$bw_b_l),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_b_l_l2_r2_unif.png")
# # ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_b_r, poly l2_r2, normal
# # dat <- data.frame(bw_b_r = c(results_x_norm_l2_r2_j1$bw_b_r, results_x_norm_l2_r2_j5$bw_b_r, 
# #                              results_x_norm_l2_r2_j10$bw_b_r, results_x_norm_l2_r2_j20$bw_b_r),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_b_r_l2_r2_norm.png")
# # ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_b_r, poly l2_r2, uniform
# # dat <- data.frame(bw_b_r = c(results_x_unif_l2_r2_j1$bw_b_r, results_x_unif_l2_r2_j5$bw_b_r, 
# #                              results_x_unif_l2_r2_j10$bw_b_r, results_x_unif_l2_r2_j20$bw_b_r),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_b_r_l2_r2_unif.png")
# # ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_b norm
# # dat <- data.frame(bw_b_length = c(results_x_norm_l2_r2_j1$bw_b_length, results_x_norm_l2_r2_j5$bw_b_length, 
# #                                 results_x_norm_l2_r2_j10$bw_b_length, results_x_norm_l2_r2_j20$bw_b_length),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_b_length_l2_r2_norm.png")
# # ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_b uniform
# # dat <- data.frame(bw_b_length = c(results_x_unif_l2_r2_j1$bw_b_length, results_x_unif_l2_r2_j5$bw_b_length, 
# #                                 results_x_unif_l2_r2_j10$bw_b_length, results_x_unif_l2_r2_j20$bw_b_length),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_b_length_l2_r2_unif.png")
# # ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ######### PLOTS DROPPING OBS OUTSIDE BW
# # ### coef_cl, poly l2_r2, normal
# # dat <- data.frame(coef_cl = c(results_bw_x_norm_l2_r2_j1$coef_cl, results_bw_x_norm_l2_r2_j5$coef_cl, 
# #                               results_bw_x_norm_l2_r2_j10$coef_cl, results_bw_x_norm_l2_r2_j20$coef_cl),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/coef_cl_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ### coef_cl, poly l2_r2, uniform
# # dat <- data.frame(coef_cl = c(results_bw_x_unif_l2_r2_j1$coef_cl, results_bw_x_unif_l2_r2_j5$coef_cl, 
# #                               results_bw_x_unif_l2_r2_j10$coef_cl, results_bw_x_unif_l2_r2_j20$coef_cl),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/coef_cl_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ### coef_bc, poly l2_r2, normal
# # dat <- data.frame(coef_bc = c(results_bw_x_norm_l2_r2_j1$coef_bc, results_bw_x_norm_l2_r2_j5$coef_bc, 
# #                               results_bw_x_norm_l2_r2_j10$coef_bc, results_bw_x_norm_l2_r2_j20$coef_bc),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/coef_bc_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ### coef_bc, poly l2_r2, uniform
# # dat <- data.frame(coef_bc = c(results_bw_x_unif_l2_r2_j1$coef_bc, results_bw_x_unif_l2_r2_j5$coef_bc, 
# #                               results_bw_x_unif_l2_r2_j10$coef_bc, results_bw_x_unif_l2_r2_j20$coef_bc),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/coef_bc_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# # dev.off()
# # 
# # ######## BW ######### 
# # ### bw_h_l, poly l2_r2, normal
# # dat <- data.frame(bw_h_l = c(results_bw_x_norm_l2_r2_j1$bw_h_l, results_bw_x_norm_l2_r2_j5$bw_h_l, 
# #                              results_bw_x_norm_l2_r2_j10$bw_h_l, results_bw_x_norm_l2_r2_j20$bw_h_l),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_h_l_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_h_l, poly l2_r2, uniform
# # dat <- data.frame(bw_h_l = c(results_bw_x_unif_l2_r2_j1$bw_h_l, results_bw_x_unif_l2_r2_j5$bw_h_l, 
# #                              results_bw_x_unif_l2_r2_j10$bw_h_l, results_bw_x_unif_l2_r2_j20$bw_h_l),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_h_l_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_h_r, poly l2_r2, normal
# # dat <- data.frame(bw_h_r = c(results_bw_x_norm_l2_r2_j1$bw_h_r, results_bw_x_norm_l2_r2_j5$bw_h_r, 
# #                              results_bw_x_norm_l2_r2_j10$bw_h_r, results_bw_x_norm_l2_r2_j20$bw_h_r),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_h_r_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_h_r, poly l2_r2, uniform
# # dat <- data.frame(bw_h_r = c(results_bw_x_unif_l2_r2_j1$bw_h_r, results_bw_x_unif_l2_r2_j5$bw_h_r, 
# #                              results_bw_x_unif_l2_r2_j10$bw_h_r, results_bw_x_unif_l2_r2_j20$bw_h_r),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_h_r_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_h norm
# # dat <- data.frame(bw_h_length = c(results_bw_x_norm_l2_r2_j1$bw_h_length, results_bw_x_norm_l2_r2_j5$bw_h_length, 
# #                                   results_bw_x_norm_l2_r2_j10$bw_h_length, results_bw_x_norm_l2_r2_j20$bw_h_length),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_h_length_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_h uniform
# # dat <- data.frame(bw_h_length = c(results_bw_x_unif_l2_r2_j1$bw_h_length, results_bw_x_unif_l2_r2_j5$bw_h_length, 
# #                                   results_bw_x_unif_l2_r2_j10$bw_h_length, results_bw_x_unif_l2_r2_j20$bw_h_length),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_h_length_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # 
# # ### bw_b_l, poly l2_r2, normal
# # dat <- data.frame(bw_b_l = c(results_bw_x_norm_l2_r2_j1$bw_b_l, results_bw_x_norm_l2_r2_j5$bw_b_l, 
# #                              results_bw_x_norm_l2_r2_j10$bw_b_l, results_bw_x_norm_l2_r2_j20$bw_b_l),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_b_l_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_b_l, poly l2_r2, uniform
# # dat <- data.frame(bw_b_l = c(results_bw_x_unif_l2_r2_j1$bw_b_l, results_bw_x_unif_l2_r2_j5$bw_b_l, 
# #                              results_bw_x_unif_l2_r2_j10$bw_b_l, results_bw_x_unif_l2_r2_j20$bw_b_l),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_b_l_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_b_r, poly l2_r2, normal
# # dat <- data.frame(bw_b_r = c(results_bw_x_norm_l2_r2_j1$bw_b_r, results_bw_x_norm_l2_r2_j5$bw_b_r, 
# #                              results_bw_x_norm_l2_r2_j10$bw_b_r, results_bw_x_norm_l2_r2_j20$bw_b_r),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_b_r_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # ### bw_b_r, poly l2_r2, uniform
# # dat <- data.frame(bw_b_r = c(results_bw_x_unif_l2_r2_j1$bw_b_r, results_bw_x_unif_l2_r2_j5$bw_b_r, 
# #                              results_bw_x_unif_l2_r2_j10$bw_b_r, results_bw_x_unif_l2_r2_j20$bw_b_r),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_b_r_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_b norm
# # dat <- data.frame(bw_b_length = c(results_bw_x_norm_l2_r2_j1$bw_b_length, results_bw_x_norm_l2_r2_j5$bw_b_length, 
# #                                   results_bw_x_norm_l2_r2_j10$bw_b_length, results_bw_x_norm_l2_r2_j20$bw_b_length),
# #                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# # png("figures/bw_b_length_l2_r2_norm_bw_only.png")
# # ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# # 
# # #### Interval bw_b uniform
# # dat <- data.frame(bw_b_length = c(results_bw_x_unif_l2_r2_j1$bw_b_length, results_bw_x_unif_l2_r2_j5$bw_b_length, 
# #                                   results_bw_x_unif_l2_r2_j10$bw_b_length, results_bw_x_unif_l2_r2_j20$bw_b_length),
# #                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# # dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# # png("figures/bw_b_length_l2_r2_unif_bw_only.png")
# # ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
# # dev.off()
# 
# #### OBS OUTSIDE BW TURNED TO ZERO
# ### coef_cl, poly l2_r2, normal
# dat <- data.frame(coef_cl = c(results_zero_x_norm_l2_r2_j1$coef_cl, results_zero_x_norm_l2_r2_j5$coef_cl, 
#                               results_zero_x_norm_l2_r2_j10$coef_cl, results_zero_x_norm_l2_r2_j20$coef_cl),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/coef_cl_l2_r2_norm.png")
# ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# dev.off()
# 
# ### coef_cl, poly l2_r2, uniform
# dat <- data.frame(coef_cl = c(results_zero_x_unif_l2_r2_j1$coef_cl, results_zero_x_unif_l2_r2_j5$coef_cl, 
#                               results_zero_x_unif_l2_r2_j10$coef_cl, results_zero_x_unif_l2_r2_j20$coef_cl),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/coef_cl_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# dev.off()
# 
# ### coef_bc, poly l2_r2, normal
# dat <- data.frame(coef_bc = c(results_zero_x_norm_l2_r2_j1$coef_bc, results_zero_x_norm_l2_r2_j5$coef_bc, 
#                               results_zero_x_norm_l2_r2_j10$coef_bc, results_zero_x_norm_l2_r2_j20$coef_bc),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/coef_bc_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# dev.off()
# 
# ### coef_bc, poly l2_r2, uniform
# dat <- data.frame(coef_bc = c(results_zero_x_unif_l2_r2_j1$coef_bc, results_zero_x_unif_l2_r2_j5$coef_bc, 
#                               results_zero_x_unif_l2_r2_j10$coef_bc, results_zero_x_unif_l2_r2_j20$coef_bc),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/coef_bc_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
# dev.off()
# 
# ######## BW ######### 
# ### bw_h_l, poly l2_r2, normal
# dat <- data.frame(bw_h_l = c(results_zero_x_norm_l2_r2_j1$bw_h_l, results_zero_x_norm_l2_r2_j5$bw_h_l, 
#                              results_zero_x_norm_l2_r2_j10$bw_h_l, results_zero_x_norm_l2_r2_j20$bw_h_l),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/bw_h_l_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# ### bw_h_l, poly l2_r2, uniform
# dat <- data.frame(bw_h_l = c(results_zero_x_unif_l2_r2_j1$bw_h_l, results_zero_x_unif_l2_r2_j5$bw_h_l, 
#                              results_zero_x_unif_l2_r2_j10$bw_h_l, results_zero_x_unif_l2_r2_j20$bw_h_l),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/bw_h_l_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# ### bw_h_r, poly l2_r2, normal
# dat <- data.frame(bw_h_r = c(results_zero_x_norm_l2_r2_j1$bw_h_r, results_zero_x_norm_l2_r2_j5$bw_h_r, 
#                              results_zero_x_norm_l2_r2_j10$bw_h_r, results_zero_x_norm_l2_r2_j20$bw_h_r),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/bw_h_r_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# ### bw_h_r, poly l2_r2, uniform
# dat <- data.frame(bw_h_r = c(results_zero_x_unif_l2_r2_j1$bw_h_r, results_zero_x_unif_l2_r2_j5$bw_h_r, 
#                              results_zero_x_unif_l2_r2_j10$bw_h_r, results_zero_x_unif_l2_r2_j20$bw_h_r),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/bw_h_r_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# #### Interval bw_h norm
# dat <- data.frame(bw_h_length = c(results_zero_x_norm_l2_r2_j1$bw_h_length, results_zero_x_norm_l2_r2_j5$bw_h_length, 
#                                   results_zero_x_norm_l2_r2_j10$bw_h_length, results_zero_x_norm_l2_r2_j20$bw_h_length),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/bw_h_length_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# #### Interval bw_h uniform
# dat <- data.frame(bw_h_length = c(results_zero_x_unif_l2_r2_j1$bw_h_length, results_zero_x_unif_l2_r2_j5$bw_h_length, 
#                                   results_zero_x_unif_l2_r2_j10$bw_h_length, results_zero_x_unif_l2_r2_j20$bw_h_length),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/bw_h_length_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# 
# ### bw_b_l, poly l2_r2, normal
# dat <- data.frame(bw_b_l = c(results_zero_x_norm_l2_r2_j1$bw_b_l, results_zero_x_norm_l2_r2_j5$bw_b_l, 
#                              results_zero_x_norm_l2_r2_j10$bw_b_l, results_zero_x_norm_l2_r2_j20$bw_b_l),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/bw_b_l_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# ### bw_b_l, poly l2_r2, uniform
# dat <- data.frame(bw_b_l = c(results_zero_x_unif_l2_r2_j1$bw_b_l, results_zero_x_unif_l2_r2_j5$bw_b_l, 
#                              results_zero_x_unif_l2_r2_j10$bw_b_l, results_zero_x_unif_l2_r2_j20$bw_b_l),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/bw_b_l_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# ### bw_b_r, poly l2_r2, normal
# dat <- data.frame(bw_b_r = c(results_zero_x_norm_l2_r2_j1$bw_b_r, results_zero_x_norm_l2_r2_j5$bw_b_r, 
#                              results_zero_x_norm_l2_r2_j10$bw_b_r, results_zero_x_norm_l2_r2_j20$bw_b_r),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/bw_b_r_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# ### bw_b_r, poly l2_r2, uniform
# dat <- data.frame(bw_b_r = c(results_zero_x_unif_l2_r2_j1$bw_b_r, results_zero_x_unif_l2_r2_j5$bw_b_r, 
#                              results_zero_x_unif_l2_r2_j10$bw_b_r, results_zero_x_unif_l2_r2_j20$bw_b_r),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/bw_b_r_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# #### Interval bw_b norm
# dat <- data.frame(bw_b_length = c(results_zero_x_norm_l2_r2_j1$bw_b_length, results_zero_x_norm_l2_r2_j5$bw_b_length, 
#                                   results_zero_x_norm_l2_r2_j10$bw_b_length, results_zero_x_norm_l2_r2_j20$bw_b_length),
#                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
# png("figures/bw_b_length_l2_r2_norm_zero.png")
# ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()
# 
# #### Interval bw_b uniform
# dat <- data.frame(bw_b_length = c(results_zero_x_unif_l2_r2_j1$bw_b_length, results_zero_x_unif_l2_r2_j5$bw_b_length, 
#                                   results_zero_x_unif_l2_r2_j10$bw_b_length, results_zero_x_unif_l2_r2_j20$bw_b_length),
#                   spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
# dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
# png("figures/bw_b_length_l2_r2_unif_zero.png")
# ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
# dev.off()


#### DIFFERENCES BETWEEN FULL, BW_ONLY AND ZEROS:
for (jump in c(1,5,10,20)) {
  jump=1
full_name=paste("results_x_norm_l2_r2_j",jump,sep = "")
bw_name=paste("results_bw_x_norm_l2_r2_j",jump,sep = "")
  
results_name=paste("results_x_norm_l2_r2_j",jump,sep = "")
assign(results_name,results)


results_diff_bw_x_norm_l2_r2_j1=results_x_norm_l2_r2_j1-
  results_bw_x_norm_l2_r2_j1
results_diff_zero_x_norm_l2_r2_j1=results_x_norm_l2_r2_j1-
  results_zero_x_norm_l2_r2_j1

results_diff_bw_x_norm_l2_r2_j5=results_x_norm_l2_r2_j5-
  results_bw_x_norm_l2_r2_j5
results_diff_zero_x_norm_l2_r2_j5=results_x_norm_l2_r2_j5-
  results_zero_x_norm_l2_r2_j5

results_diff_bw_x_norm_l2_r2_j10=results_x_norm_l2_r2_j10-
  results_bw_x_norm_l2_r2_j10
results_diff_zero_x_norm_l2_r2_j10=results_x_norm_l2_r2_j10-
  results_zero_x_norm_l2_r2_j10

results_diff_bw_x_norm_l2_r2_j20=results_x_norm_l2_r2_j20-
  results_bw_x_norm_l2_r2_j20
results_diff_zero_x_norm_l2_r2_j20=results_x_norm_l2_r2_j20-
  results_zero_x_norm_l2_r2_j20

results_diff_bw_x_unif_l2_r2_j1=results_x_unif_l2_r2_j1-
  results_bw_x_unif_l2_r2_j1
results_diff_zero_x_unif_l2_r2_j1=results_x_unif_l2_r2_j1-
  results_zero_x_unif_l2_r2_j1

results_diff_bw_x_unif_l2_r2_j5=results_x_unif_l2_r2_j5-
  results_bw_x_unif_l2_r2_j5
results_diff_zero_x_unif_l2_r2_j5=results_x_unif_l2_r2_j5-
  results_zero_x_unif_l2_r2_j5

results_diff_bw_x_unif_l2_r2_j10=results_x_unif_l2_r2_j10-
  results_bw_x_unif_l2_r2_j10
results_diff_zero_x_unif_l2_r2_j10=results_x_unif_l2_r2_j10-
  results_zero_x_unif_l2_r2_j10

results_diff_bw_x_unif_l2_r2_j20=results_x_unif_l2_r2_j20-
  results_bw_x_unif_l2_r2_j20
results_diff_zero_x_unif_l2_r2_j20=results_x_unif_l2_r2_j20-
  results_zero_x_unif_l2_r2_j20

#### DIFF FIGURES - WITHIN BW
### coef_cl, poly l2_r2, normal
dat <- data.frame(coef_cl = c(results_diff_bw_x_norm_l2_r2_j1$coef_cl, results_diff_bw_x_norm_l2_r2_j5$coef_cl,
                           results_diff_bw_x_norm_l2_r2_j10$coef_cl, results_diff_bw_x_norm_l2_r2_j20$coef_cl),
                   spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_cl_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_cl, poly l2_r2, uniform
dat <- data.frame(coef_cl = c(results_diff_bw_x_unif_l2_r2_j1$coef_cl, results_diff_bw_x_unif_l2_r2_j5$coef_cl,
                              results_diff_bw_x_unif_l2_r2_j10$coef_cl, results_diff_bw_x_unif_l2_r2_j20$coef_cl),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_cl_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, normal
dat <- data.frame(coef_bc = c(results_diff_bw_x_norm_l2_r2_j1$coef_bc, results_diff_bw_x_norm_l2_r2_j5$coef_bc,
                              results_diff_bw_x_norm_l2_r2_j10$coef_bc, results_diff_bw_x_norm_l2_r2_j20$coef_bc),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_bc_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, uniform
dat <- data.frame(coef_bc = c(results_diff_bw_x_unif_l2_r2_j1$coef_bc, results_diff_bw_x_unif_l2_r2_j5$coef_bc,
                              results_diff_bw_x_unif_l2_r2_j10$coef_bc, results_diff_bw_x_unif_l2_r2_j20$coef_bc),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_bc_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

######## BW #########
### bw_h_l, poly l2_r2, normal
dat <- data.frame(bw_h_l = c(results_diff_bw_x_norm_l2_r2_j1$bw_h_l, results_diff_bw_x_norm_l2_r2_j5$bw_h_l,
                              results_diff_bw_x_norm_l2_r2_j10$bw_h_l, results_diff_bw_x_norm_l2_r2_j20$bw_h_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_l_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_h_l, poly l2_r2, uniform
dat <- data.frame(bw_h_l = c(results_diff_bw_x_unif_l2_r2_j1$bw_h_l, results_diff_bw_x_unif_l2_r2_j5$bw_h_l,
                              results_diff_bw_x_unif_l2_r2_j10$bw_h_l, results_diff_bw_x_unif_l2_r2_j20$bw_h_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_l_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_h_r, poly l2_r2, normal
dat <- data.frame(bw_h_r = c(results_diff_bw_x_norm_l2_r2_j1$bw_h_r, results_diff_bw_x_norm_l2_r2_j5$bw_h_r,
                             results_diff_bw_x_norm_l2_r2_j10$bw_h_r, results_diff_bw_x_norm_l2_r2_j20$bw_h_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_r_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_h_r, poly l2_r2, uniform
dat <- data.frame(bw_h_r = c(results_diff_bw_x_unif_l2_r2_j1$bw_h_r, results_diff_bw_x_unif_l2_r2_j5$bw_h_r,
                             results_diff_bw_x_unif_l2_r2_j10$bw_h_r, results_diff_bw_x_unif_l2_r2_j20$bw_h_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_r_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_h norm
dat <- data.frame(bw_h_length = c(results_diff_bw_x_norm_l2_r2_j1$bw_h_length, results_diff_bw_x_norm_l2_r2_j5$bw_h_length,
                                results_diff_bw_x_norm_l2_r2_j10$bw_h_length, results_diff_bw_x_norm_l2_r2_j20$bw_h_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_length_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_h uniform
dat <- data.frame(bw_h_length = c(results_diff_bw_x_unif_l2_r2_j1$bw_h_length, results_diff_bw_x_unif_l2_r2_j5$bw_h_length,
                             results_diff_bw_x_unif_l2_r2_j10$bw_h_length, results_diff_bw_x_unif_l2_r2_j20$bw_h_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_length_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()


### bw_b_l, poly l2_r2, normal
dat <- data.frame(bw_b_l = c(results_diff_bw_x_norm_l2_r2_j1$bw_b_l, results_diff_bw_x_norm_l2_r2_j5$bw_b_l,
                             results_diff_bw_x_norm_l2_r2_j10$bw_b_l, results_diff_bw_x_norm_l2_r2_j20$bw_b_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_l_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_b_l, poly l2_r2, uniform
dat <- data.frame(bw_b_l = c(results_diff_bw_x_unif_l2_r2_j1$bw_b_l, results_diff_bw_x_unif_l2_r2_j5$bw_b_l,
                             results_diff_bw_x_unif_l2_r2_j10$bw_b_l, results_diff_bw_x_unif_l2_r2_j20$bw_b_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_l_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_b_r, poly l2_r2, normal
dat <- data.frame(bw_b_r = c(results_diff_bw_x_norm_l2_r2_j1$bw_b_r, results_diff_bw_x_norm_l2_r2_j5$bw_b_r,
                             results_diff_bw_x_norm_l2_r2_j10$bw_b_r, results_diff_bw_x_norm_l2_r2_j20$bw_b_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_r_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_b_r, poly l2_r2, uniform
dat <- data.frame(bw_b_r = c(results_diff_bw_x_unif_l2_r2_j1$bw_b_r, results_diff_bw_x_unif_l2_r2_j5$bw_b_r,
                             results_diff_bw_x_unif_l2_r2_j10$bw_b_r, results_diff_bw_x_unif_l2_r2_j20$bw_b_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_r_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_b norm
dat <- data.frame(bw_b_length = c(results_diff_bw_x_norm_l2_r2_j1$bw_b_length, results_diff_bw_x_norm_l2_r2_j5$bw_b_length,
                                results_diff_bw_x_norm_l2_r2_j10$bw_b_length, results_diff_bw_x_norm_l2_r2_j20$bw_b_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_length_l2_r2_norm_diff_bw.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_b uniform
dat <- data.frame(bw_b_length = c(results_diff_bw_x_unif_l2_r2_j1$bw_b_length, results_diff_bw_x_unif_l2_r2_j5$bw_b_length,
                                results_diff_bw_x_unif_l2_r2_j10$bw_b_length, results_diff_bw_x_unif_l2_r2_j20$bw_b_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_length_l2_r2_unif_diff_bw.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### DIFF FIGURES - ZEROS OUTSIDE BW
### coef_cl, poly l2_r2, normal
dat <- data.frame(coef_cl = c(results_diff_zero_x_norm_l2_r2_j1$coef_cl, results_diff_zero_x_norm_l2_r2_j5$coef_cl,
                              results_diff_zero_x_norm_l2_r2_j10$coef_cl, results_diff_zero_x_norm_l2_r2_j20$coef_cl),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_cl_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_cl, poly l2_r2, uniform
dat <- data.frame(coef_cl = c(results_diff_zero_x_unif_l2_r2_j1$coef_cl, results_diff_zero_x_unif_l2_r2_j5$coef_cl,
                              results_diff_zero_x_unif_l2_r2_j10$coef_cl, results_diff_zero_x_unif_l2_r2_j20$coef_cl),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_cl_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, normal
dat <- data.frame(coef_bc = c(results_diff_zero_x_norm_l2_r2_j1$coef_bc, results_diff_zero_x_norm_l2_r2_j5$coef_bc,
                              results_diff_zero_x_norm_l2_r2_j10$coef_bc, results_diff_zero_x_norm_l2_r2_j20$coef_bc),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_bc_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, uniform
dat <- data.frame(coef_bc = c(results_diff_zero_x_unif_l2_r2_j1$coef_bc, results_diff_zero_x_unif_l2_r2_j5$coef_bc,
                              results_diff_zero_x_unif_l2_r2_j10$coef_bc, results_diff_zero_x_unif_l2_r2_j20$coef_bc),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_bc_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

######## BW #########
### bw_h_l, poly l2_r2, normal
dat <- data.frame(bw_h_l = c(results_diff_zero_x_norm_l2_r2_j1$bw_h_l, results_diff_zero_x_norm_l2_r2_j5$bw_h_l,
                             results_diff_zero_x_norm_l2_r2_j10$bw_h_l, results_diff_zero_x_norm_l2_r2_j20$bw_h_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_l_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_h_l, poly l2_r2, uniform
dat <- data.frame(bw_h_l = c(results_diff_zero_x_unif_l2_r2_j1$bw_h_l, results_diff_zero_x_unif_l2_r2_j5$bw_h_l,
                             results_diff_zero_x_unif_l2_r2_j10$bw_h_l, results_diff_zero_x_unif_l2_r2_j20$bw_h_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_l_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_h_r, poly l2_r2, normal
dat <- data.frame(bw_h_r = c(results_diff_zero_x_norm_l2_r2_j1$bw_h_r, results_diff_zero_x_norm_l2_r2_j5$bw_h_r,
                             results_diff_zero_x_norm_l2_r2_j10$bw_h_r, results_diff_zero_x_norm_l2_r2_j20$bw_h_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_r_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_h_r, poly l2_r2, uniform
dat <- data.frame(bw_h_r = c(results_diff_zero_x_unif_l2_r2_j1$bw_h_r, results_diff_zero_x_unif_l2_r2_j5$bw_h_r,
                             results_diff_zero_x_unif_l2_r2_j10$bw_h_r, results_diff_zero_x_unif_l2_r2_j20$bw_h_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_r_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_h norm
dat <- data.frame(bw_h_length = c(results_diff_zero_x_norm_l2_r2_j1$bw_h_length, results_diff_zero_x_norm_l2_r2_j5$bw_h_length,
                                  results_diff_zero_x_norm_l2_r2_j10$bw_h_length, results_diff_zero_x_norm_l2_r2_j20$bw_h_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_length_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_h uniform
dat <- data.frame(bw_h_length = c(results_diff_zero_x_unif_l2_r2_j1$bw_h_length, results_diff_zero_x_unif_l2_r2_j5$bw_h_length,
                                  results_diff_zero_x_unif_l2_r2_j10$bw_h_length, results_diff_zero_x_unif_l2_r2_j20$bw_h_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_length_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()


### bw_b_l, poly l2_r2, normal
dat <- data.frame(bw_b_l = c(results_diff_zero_x_norm_l2_r2_j1$bw_b_l, results_diff_zero_x_norm_l2_r2_j5$bw_b_l,
                             results_diff_zero_x_norm_l2_r2_j10$bw_b_l, results_diff_zero_x_norm_l2_r2_j20$bw_b_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_l_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_b_l, poly l2_r2, uniform
dat <- data.frame(bw_b_l = c(results_diff_zero_x_unif_l2_r2_j1$bw_b_l, results_diff_zero_x_unif_l2_r2_j5$bw_b_l,
                             results_diff_zero_x_unif_l2_r2_j10$bw_b_l, results_diff_zero_x_unif_l2_r2_j20$bw_b_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_l_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_b_r, poly l2_r2, normal
dat <- data.frame(bw_b_r = c(results_diff_zero_x_norm_l2_r2_j1$bw_b_r, results_diff_zero_x_norm_l2_r2_j5$bw_b_r,
                             results_diff_zero_x_norm_l2_r2_j10$bw_b_r, results_diff_zero_x_norm_l2_r2_j20$bw_b_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_r_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### bw_b_r, poly l2_r2, uniform
dat <- data.frame(bw_b_r = c(results_diff_zero_x_unif_l2_r2_j1$bw_b_r, results_diff_zero_x_unif_l2_r2_j5$bw_b_r,
                             results_diff_zero_x_unif_l2_r2_j10$bw_b_r, results_diff_zero_x_unif_l2_r2_j20$bw_b_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_r_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_b norm
dat <- data.frame(bw_b_length = c(results_diff_zero_x_norm_l2_r2_j1$bw_b_length, results_diff_zero_x_norm_l2_r2_j5$bw_b_length,
                                  results_diff_zero_x_norm_l2_r2_j10$bw_b_length, results_diff_zero_x_norm_l2_r2_j20$bw_b_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_length_l2_r2_norm_diff_zero.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()

#### Interval bw_b uniform
dat <- data.frame(bw_b_length = c(results_diff_zero_x_unif_l2_r2_j1$bw_b_length, results_diff_zero_x_unif_l2_r2_j5$bw_b_length,
                                  results_diff_zero_x_unif_l2_r2_j10$bw_b_length, results_diff_zero_x_unif_l2_r2_j20$bw_b_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_length_l2_r2_unif_diff_zero.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)+ geom_vline(xintercept=0,  linetype="dashed")
dev.off()









results_diff_norm_l2_r2_j1<- as.data.frame(matrix(0, ncol = 8, nrow = loop))
colnames(results) <- c("coef_cl_diff_bw","coef_cl_diff_zero",
                       "coef_bc_diff_bw","coef_bc_diff_zero",
                       "bw_h_length_diff_bw", "bw_h_length_diff_zero", 
                       "bw_b_length_diff_bw", "bw_b_length_diff_zero") 

results_diff_x_norm_l2_r2_j1[,1]=results_x_norm_l2_r2_j1[,1]-results_zero_x_norm_l2_r2_j1

### coef_cl, poly l2_r2, normal
dat <- data.frame(coef_cl = c(results_zero_x_norm_l2_r2_j1$coef_cl, results_zero_x_norm_l2_r2_j5$coef_cl, 
                              results_zero_x_norm_l2_r2_j10$coef_cl, results_zero_x_norm_l2_r2_j20$coef_cl),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_cl_l2_r2_norm.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_cl, poly l2_r2, uniform
dat <- data.frame(coef_cl = c(results_zero_x_unif_l2_r2_j1$coef_cl, results_zero_x_unif_l2_r2_j5$coef_cl, 
                              results_zero_x_unif_l2_r2_j10$coef_cl, results_zero_x_unif_l2_r2_j20$coef_cl),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_cl_l2_r2_unif_zero.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, normal
dat <- data.frame(coef_bc = c(results_zero_x_norm_l2_r2_j1$coef_bc, results_zero_x_norm_l2_r2_j5$coef_bc, 
                              results_zero_x_norm_l2_r2_j10$coef_bc, results_zero_x_norm_l2_r2_j20$coef_bc),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_bc_l2_r2_norm_zero.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, uniform
dat <- data.frame(coef_bc = c(results_zero_x_unif_l2_r2_j1$coef_bc, results_zero_x_unif_l2_r2_j5$coef_bc, 
                              results_zero_x_unif_l2_r2_j10$coef_bc, results_zero_x_unif_l2_r2_j20$coef_bc),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_bc_l2_r2_unif_zero.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

######## BW ######### 
### bw_h_l, poly l2_r2, normal
dat <- data.frame(bw_h_l = c(results_zero_x_norm_l2_r2_j1$bw_h_l, results_zero_x_norm_l2_r2_j5$bw_h_l, 
                             results_zero_x_norm_l2_r2_j10$bw_h_l, results_zero_x_norm_l2_r2_j20$bw_h_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_l_l2_r2_norm_zero.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_l, poly l2_r2, uniform
dat <- data.frame(bw_h_l = c(results_zero_x_unif_l2_r2_j1$bw_h_l, results_zero_x_unif_l2_r2_j5$bw_h_l, 
                             results_zero_x_unif_l2_r2_j10$bw_h_l, results_zero_x_unif_l2_r2_j20$bw_h_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_l_l2_r2_unif_zero.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_r, poly l2_r2, normal
dat <- data.frame(bw_h_r = c(results_zero_x_norm_l2_r2_j1$bw_h_r, results_zero_x_norm_l2_r2_j5$bw_h_r, 
                             results_zero_x_norm_l2_r2_j10$bw_h_r, results_zero_x_norm_l2_r2_j20$bw_h_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_r_l2_r2_norm_zero.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_r, poly l2_r2, uniform
dat <- data.frame(bw_h_r = c(results_zero_x_unif_l2_r2_j1$bw_h_r, results_zero_x_unif_l2_r2_j5$bw_h_r, 
                             results_zero_x_unif_l2_r2_j10$bw_h_r, results_zero_x_unif_l2_r2_j20$bw_h_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_r_l2_r2_unif_zero.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_h norm
dat <- data.frame(bw_h_length = c(results_zero_x_norm_l2_r2_j1$bw_h_length, results_zero_x_norm_l2_r2_j5$bw_h_length, 
                                  results_zero_x_norm_l2_r2_j10$bw_h_length, results_zero_x_norm_l2_r2_j20$bw_h_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_length_l2_r2_norm_zero.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_h uniform
dat <- data.frame(bw_h_length = c(results_zero_x_unif_l2_r2_j1$bw_h_length, results_zero_x_unif_l2_r2_j5$bw_h_length, 
                                  results_zero_x_unif_l2_r2_j10$bw_h_length, results_zero_x_unif_l2_r2_j20$bw_h_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_length_l2_r2_unif_zero.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### bw_b_l, poly l2_r2, normal
dat <- data.frame(bw_b_l = c(results_zero_x_norm_l2_r2_j1$bw_b_l, results_zero_x_norm_l2_r2_j5$bw_b_l, 
                             results_zero_x_norm_l2_r2_j10$bw_b_l, results_zero_x_norm_l2_r2_j20$bw_b_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_l_l2_r2_norm_zero.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_l, poly l2_r2, uniform
dat <- data.frame(bw_b_l = c(results_zero_x_unif_l2_r2_j1$bw_b_l, results_zero_x_unif_l2_r2_j5$bw_b_l, 
                             results_zero_x_unif_l2_r2_j10$bw_b_l, results_zero_x_unif_l2_r2_j20$bw_b_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_l_l2_r2_unif_zero.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_r, poly l2_r2, normal
dat <- data.frame(bw_b_r = c(results_zero_x_norm_l2_r2_j1$bw_b_r, results_zero_x_norm_l2_r2_j5$bw_b_r, 
                             results_zero_x_norm_l2_r2_j10$bw_b_r, results_zero_x_norm_l2_r2_j20$bw_b_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_r_l2_r2_norm_zero.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_r, poly l2_r2, uniform
dat <- data.frame(bw_b_r = c(results_zero_x_unif_l2_r2_j1$bw_b_r, results_zero_x_unif_l2_r2_j5$bw_b_r, 
                             results_zero_x_unif_l2_r2_j10$bw_b_r, results_zero_x_unif_l2_r2_j20$bw_b_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_r_l2_r2_unif_zero.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_b norm
dat <- data.frame(bw_b_length = c(results_zero_x_norm_l2_r2_j1$bw_b_length, results_zero_x_norm_l2_r2_j5$bw_b_length, 
                                  results_zero_x_norm_l2_r2_j10$bw_b_length, results_zero_x_norm_l2_r2_j20$bw_b_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_length_l2_r2_norm_zero.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_b uniform
dat <- data.frame(bw_b_length = c(results_zero_x_unif_l2_r2_j1$bw_b_length, results_zero_x_unif_l2_r2_j5$bw_b_length, 
                                  results_zero_x_unif_l2_r2_j10$bw_b_length, results_zero_x_unif_l2_r2_j20$bw_b_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_length_l2_r2_unif_zero.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()







##################################################################
### DIFFERENT POLYNOMIALS ON EACH SIDE - X Normal

for (jump in c(1,5,10,20)) {
  df$y[df$treated == 1]<- jump*df$treated[df$treated == 1] + 0.5*df$x[df$treated == 1]  - 0.025*df$x[df$treated == 1]^2+rnorm(length(df$x[df$treated==1]),0,10)  
  df$y[df$treated == 0]<-  0.5*df$x[df$treated == 0]  - 0.025*df$x[df$treated == 0]^2 +  0.0005*df$x[df$treated==0]^3+rnorm(length(df$x[df$treated==0]),0,10)  
  
  
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
  dev.off()
  
  results_name=paste("results_x_norm_l2_r3_j",jump,sep = "")
  assign(results_name,results)
}

### DIFFERENT POLYNOMIALS ON EACH SIDE - X uniform
df$y=0
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")

for (jump in c(1,5,10,20)) {
  df$y[df$treated == 1]<- jump*df$treated[df$treated == 1] + 0.5*df$x[df$treated == 1]  - 0.025*df$x[df$treated == 1]^2+rnorm(length(df$x[df$treated==1]),0,10)  
  df$y[df$treated == 0]<- 0.5*df$x[df$treated == 0]  - 0.025*df$x[df$treated == 0]^2 +  0.0005*df$x[df$treated==0]^3+rnorm(length(df$x[df$treated==0]),0,10)  
  
  
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
  dev.off()
  
  results_name=paste("results_x_unif_l2_r3_j",jump,sep = "")
  assign(results_name,results)
  
}

### COMBINING DENSITY PLOTS

### 1 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j1$coef, results_x_norm_l2_r3_j1$coef, results_x_unif_l2_r2_j1$coef, results_x_unif_l2_r3_j1$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2","x_unif_l2_r3"), each = loop))
png("figures/coef_j1_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=1,  linetype="dashed")
dev.off()

### 1 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j1$bw_l, results_x_norm_l2_r3_j1$bw_l,results_x_unif_l2_r2_j1$bw_l , results_x_unif_l2_r3_j1$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j1_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### 5 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j5$coef, results_x_norm_l2_r3_j5$coef, results_x_unif_l2_r2_j5$coef, results_x_unif_l2_r3_j5$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2","x_unif_l2_r3"), each = loop))
png("figures/coef_j5_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=5,  linetype="dashed")
dev.off()

### 5 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j5$bw_l, results_x_norm_l2_r3_j5$bw_l,results_x_unif_l2_r2_j5$bw_l , results_x_unif_l2_r3_j5$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j5_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### 10 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j10$coef, results_x_norm_l2_r3_j10$coef, results_x_unif_l2_r3_j10$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r3"), each = loop))
png("figures/coef_j10_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=10,  linetype="dashed")
dev.off()

### 10 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j10$bw_l, results_x_norm_l2_r3_j10$bw_l,results_x_unif_l2_r2_j10$bw_l , results_x_unif_l2_r3_j10$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j10_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### 20 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j20$coef, results_x_norm_l2_r3_j20$coef, results_x_unif_l2_r3_j20$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r3"), each = loop))
png("figures/coef_j20_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=20,  linetype="dashed")
dev.off()

### 20 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j20$bw_l, results_x_norm_l2_r3_j20$bw_l,results_x_unif_l2_r2_j20$bw_l , results_x_unif_l2_r3_j20$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j20_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


########### FIRST sHOT ###############################

#install.packages('rdrobust')
rm(list=ls(all=TRUE))
set.seed(1)
library(dplyr)
library(rdrobust)
setwd("C:/Users/yaela/Dropbox (Brown)/Brown/RA/Neil/R")  

### SETTING UP FOR EVERYTHING ###
df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df$treated <- ifelse(df$x>0, 1, 0)

loop=1000

### NORMAL DISTRIBUTION ON BOTH SIDES, SAME POLYNOMIAL
df$y=0
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")

for (jump in c(1,5,10,20)) {
  jump=5
df$y<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  
#for(i in 1:loop) {
i=1
sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
sample.x=subset(sample.x, x>-100 & x<100)
sample=inner_join(df, sample.x, by="x")
sample=as.data.frame(sample)
results[i,1]=rdrobust(sample$y,sample$x)$coef[3]
results[i,2:3]=rdrobust(sample$y,sample$x)$bws[1,1:2]
}

png(paste("figures/coef_dist_x_norm_l2_r2_j",jump,".png", sep = ""))
plot(density(results$coef), main=paste("Jump = ",jump,sep = ""),xlab = "Treatment Effects", ylab = "Density", lwd=2) 
abline(v = jump, col="red", lwd=1, lty=2)
axis(1, round(range(results$coef)[1]:round(range(results$coef)[2])))
dev.off()
plotdata=results$bw_l
png(paste("figures/bw_dist_x_norm_l2_r2_j",jump,".png", sep = ""))
plot(density(plotdata), main=paste("Jump = ",jump, sep = ""),xlab = "Bandwidth", ylab = "Density", lwd=2) 
axis(1, round(range(plotdata)[1]:round(range(plotdata)[2])))
dev.off()

reg1=lm(bw_l~coef,data=results)
png(paste("figures/bw_coef_x_norm_l2_r2_j",jump,".png",sep = ""))
plot(results$coef,results$bw_l,xlscatterab = "Treatment Effect", ylab = "Bandwidth")
abline(reg1)
dev.off()

png(paste("figures/rdplot_eg_x_norm_l2_r2_j",jump,".png",sep = ""))
rdplot(sample$y,sample$x)
dev.off()

results_name=paste("results_x_norm_l2_r2_j",jump,sep = "")
assign(results_name,results)
}


### Uniform draw of x's
df$y=0
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")

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
  
  results_name=paste("results_x_unif_l2_r2_j",jump,sep = "")
  assign(results_name,results)
}

### DIFFERENT POLYNOMIALS ON EACH SIDE - X Normal
df$y=0
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")

for (jump in c(1,5,10,20)) {
  df$y[df$treated == 1]<- jump*df$treated[df$treated == 1] + 0.5*df$x[df$treated == 1]  - 0.025*df$x[df$treated == 1]^2+rnorm(length(df$x[df$treated==1]),0,10)  
  df$y[df$treated == 0]<-  0.5*df$x[df$treated == 0]  - 0.025*df$x[df$treated == 0]^2 +  0.0005*df$x[df$treated==0]^3+rnorm(length(df$x[df$treated==0]),0,10)  
  

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
  dev.off()
  
  results_name=paste("results_x_norm_l2_r3_j",jump,sep = "")
  assign(results_name,results)
}

### DIFFERENT POLYNOMIALS ON EACH SIDE - X uniform
df$y=0
sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))
results<- as.data.frame(matrix(0, ncol = 3, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_h")

for (jump in c(1,5,10,20)) {
  df$y[df$treated == 1]<- jump*df$treated[df$treated == 1] + 0.5*df$x[df$treated == 1]  - 0.025*df$x[df$treated == 1]^2+rnorm(length(df$x[df$treated==1]),0,10)  
  df$y[df$treated == 0]<- 0.5*df$x[df$treated == 0]  - 0.025*df$x[df$treated == 0]^2 +  0.0005*df$x[df$treated==0]^3+rnorm(length(df$x[df$treated==0]),0,10)  
  
  
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
  dev.off()
  
  results_name=paste("results_x_unif_l2_r3_j",jump,sep = "")
  assign(results_name,results)
  
}

### COMBINING DENSITY PLOTS
library(ggplot2)
library(reshape2)

### 1 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j1$coef, results_x_norm_l2_r3_j1$coef, results_x_unif_l2_r2_j1$coef, results_x_unif_l2_r3_j1$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2","x_unif_l2_r3"), each = loop))
png("figures/coef_j1_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=1,  linetype="dashed")
dev.off()

### 1 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j1$bw_l, results_x_norm_l2_r3_j1$bw_l,results_x_unif_l2_r2_j1$bw_l , results_x_unif_l2_r3_j1$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j1_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### 5 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j5$coef, results_x_norm_l2_r3_j5$coef, results_x_unif_l2_r2_j5$coef, results_x_unif_l2_r3_j5$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2","x_unif_l2_r3"), each = loop))
png("figures/coef_j5_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=5,  linetype="dashed")
dev.off()

### 5 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j5$bw_l, results_x_norm_l2_r3_j5$bw_l,results_x_unif_l2_r2_j5$bw_l , results_x_unif_l2_r3_j5$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j5_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### 10 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j10$coef, results_x_norm_l2_r3_j10$coef, results_x_unif_l2_r3_j10$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r3"), each = loop))
png("figures/coef_j10_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=10,  linetype="dashed")
dev.off()

### 10 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j10$bw_l, results_x_norm_l2_r3_j10$bw_l,results_x_unif_l2_r2_j10$bw_l , results_x_unif_l2_r3_j10$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j10_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### 20 Jumps - COEF
dat <- data.frame(dens = c(results_x_norm_l2_r2_j20$coef, results_x_norm_l2_r3_j20$coef, results_x_unif_l2_r3_j20$coef)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r3"), each = loop))
png("figures/coef_j20_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=20,  linetype="dashed")
dev.off()

### 20 Jumps - BWS
dat <- data.frame(dens = c(results_x_norm_l2_r2_j20$bw_l, results_x_norm_l2_r3_j20$bw_l,results_x_unif_l2_r2_j20$bw_l , results_x_unif_l2_r3_j20$bw_l)
                  , spec = rep(c("x_norm_l2_r2", "x_norm_l2_r3", "x_unif_l2_r2" , "x_unif_l2_r3"), each = loop))
png("figures/bw_j20_densities.png")
ggplot(dat, aes(x = dens, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

####################################

dens=apply(dens_data_j10,2,density,na.rm=TRUE)

plot(NA, xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")))
mapply(lines, dens, col=1:length(dens)) + theme(legend.position="bottom"
legend("bottom", legend=names(dens), fill=1:length(dens))

melted=melt(dens_data)
ggplot(melted) + geom_freqpoly(aes(x = value,
                                   y = ..density.., colour = variable),bins=10)
ggplot(dens_data, aes(x = 10)) + geom_density(alpha = 0.3, bins=10)

myData <- data.frame(std.nromal=rnorm(1000, m=0, sd=1),
                     wide.normal=rnorm(1000, m=0, sd=2),
                     exponent=rexp(1000, rate=1),
                     uniform=runif(1000, min=-3, max=3)
)

dens <- apply(myData, 2, density)

plot(dens, xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")))
mapply(lines, dens, col=1:length(dens))

legend("topright", legend=names(dens), fill=1:length(dens))