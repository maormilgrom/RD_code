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
df$treated <- ifelse(df$x>0, 1, 0)

loop=1000
results<- as.data.frame(matrix(0, ncol = 12, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_r","bw_length",
                       "coef_wa","bw_l_wa","bw_r_wa","bw_length_wa",
                       "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")
results.con<- as.data.frame(matrix(0, ncol = 12, nrow = loop))
colnames(results.con) <- c("coef","bw_l","bw_r","bw_length",
                           "coef_wa","bw_l_wa","bw_r_wa","bw_length_wa",
                           "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")

sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))

### NORMAL DISTRIBUTION ON BOTH SIDES, SAME POLYNOMIAL
df$y.base<-0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  
jump=10
df$y=df$y.base+jump*df$treated
df$y.noisy=df$y+rnorm(length(df$x),0,30)
df$y.model<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2

for(i in 1:loop) {
# set.seed(1)
# for(i in 1:10) {
# #i=1
  sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
  sample.x=subset(sample.x, x>-100 & x<100)
  sample=as.data.frame(inner_join(df, sample.x, by="x"))

  results.current=rdrobust(sample$y,sample$x, bwselect = "msetwo")
  results[i,1]=results.current$coef[2]-jump # normalizing to zero
  results[i,2:3]=results.current$bws[2,1:2]
  results[i,4]=results[i,2]+results[i,3]

  results.con[i,1]=results.current$coef[1]-jump # normalizing to zero
  results.con[i,2:3]=results.current$bws[1,1:2]
  results.con[i,4]=results.con[i,2]+results.con[i,3]
  
  range=2
  sample$y.wacky <- ifelse((sample$x > results[i,3]-2 & sample$x < results[i,3]) | 
                             (sample$x < -results[i,2]+2 & sample$x > -results[i,2]),
                           sample$y.noisy,sample$y)
  results.current=rdrobust(sample$y.wacky,sample$x, bwselect = "msetwo")
  results[i,5]=results.current$coef[2]-jump # normalizing to zero
  results[i,6:7]=results.current$bws[2,1:2]
  results[i,8]=results[i,6]+results[i,7]
  results[i,9:12]=results[i,1:4]-results[i,5:8]
  
  sample$y.wacky.con <- ifelse((sample$x > results.con[i,3]-2 & sample$x < results.con[i,3]) | 
                             (sample$x < -results.con[i,2]+2 & sample$x > -results.con[i,2]),
                           sample$y.noisy,sample$y)

  results.current=rdrobust(sample$y.wacky.con,sample$x, bwselect = "msetwo")
  results.con[i,5]=results.current$coef[1]-jump # normalizing to zero
  results.con[i,6:7]=results.current$bws[1,1:2]
  results.con[i,8]=results.con[i,6]+results.con[i,7]

  results.con[i,9:12]=results.con[i,1:4]-results.con[i,5:8]
  
  figure_name=paste("figures/wacky_2_eg_",i,".png",sep = "")
  png(figure_name)
  sample %>%
    filter(x > -20 & x < 20) %T>%
    plot(y.wacky~x,., ylim = range(c(y.wacky,y)),
         col="red", ylab = "Y", xlab = "X") %T>%
    par(new = T) %>%
    plot(y~x,., ylim = range(c(y,y.wacky)),
         col="blue", ylab = "Y", xlab = "X")
  abline(v = c(-results[i,2],-results[i,6], results[i,3], results[i,7]),
         col=c("blue","red","blue", "red"), lty=c(1,2,1,2), lwd=c(1,3,1,3))
  dev.off()

  figure_name=paste("figures/wacky_2_eg_",i,"_con.png",sep = "")
  png(figure_name)
  sample %>%
    filter(x > -20 & x < 20) %T>%
    plot(y.wacky.con~x,., ylim = range(c(y.wacky.con,y)),
         col="red", ylab = "Y", xlab = "X") %T>%
    par(new = T) %>%
    plot(y~x,., ylim = range(c(y,y.wacky.con)),
         col="blue", ylab = "Y", xlab = "X")
  abline(v = c(-results.con[i,2],-results.con[i,6], results.con[i,3], results.con[i,7]),
         col=c("blue","red","blue", "red"), lty=c(1,2,1,2), lwd=c(1,3,1,3))
  dev.off()
  
  }



save.image("stressout_data.RData")
load("stressout_data.RData")


#colnames(results) <- c("coef","bw_l","bw_r","bw_length",
                       "coef_wa","bw_l_wa","bw_r_wa","bw_length_wa",
                       "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")

png("figures/coeff_diff_wacky_2.png")
ggplot(results, aes(coef_diff)) + geom_density()
dev.off()

png("figures/bw_diff_wacky_2.png")
ggplot(results, aes(bw_length_diff)) + geom_density()
dev.off()

png("figures/bw_diff_cdf_wacky_2.png")
ggplot(results, aes(bw_length_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_l_diff_wacky_2.png")
ggplot(results, aes(bw_l_diff)) + geom_density()
dev.off()

png("figures/bw_l_diff_cdf_wacky_2.png")
ggplot(results, aes(bw_l_diff)) + stat_ecdf(geom = "step")
dev.off()


png("figures/bw_r_diff_wacky_2.png")
ggplot(results, aes(bw_r_diff)) + geom_density()
dev.off()

png("figures/bw_r_diff_cdf_wacky_2.png")
ggplot(results, aes(bw_r_diff)) + stat_ecdf(geom = "step")
dev.off()


png("figures/coeff_diff_con_wacky_2.png")
ggplot(results.con, aes(coef_diff)) + geom_density()
dev.off()

png("figures/bw_diff_con_wacky_2.png")
ggplot(results.con, aes(bw_length_diff)) + geom_density()
dev.off()

png("figures/bw_diff_con_cdf_wacky_2.png")
ggplot(results.con, aes(bw_length_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_l_diff_con_wacky_2.png")
ggplot(results.con, aes(bw_l_diff)) + geom_density()
dev.off()

png("figures/bw_l_diff_con_cdf_wacky_2.png")
ggplot(results.con, aes(bw_l_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_r_diff_con_wacky_2.png")
ggplot(results, aes(bw_r_diff)) + geom_density()
dev.off()

png("figures/bw_r_diff_con_cdf_wacky_2.png")
ggplot(results, aes(bw_r_diff)) + stat_ecdf(geom = "step")
dev.off()


### VERIFYING RESULTS
check=subset(sample,y!=y.wacky.con)
check %>%
  select(x,y,y.wacky,y.wacky.con)
summary(check$y)
summary(check$y.wacky)


### VERIFYING RESULTS
check=subset(sample,y!=y.wacky)
summary(check$y)
summary(check$y.wacky)

results %>%
  select(bw_l,bw_l_wa,bw_r,bw_r_wa)

sample %>%
  #filter(y!=y.wacky) %>%
  select(x) %>%
  summary()

####################### NOW REPLACING OBS OUTSIDE OF OBW ####################################
### USING CODE FROM ABOVE - DELETE ABOVE WHEN DONE
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
df$treated <- ifelse(df$x>0, 1, 0)

loop=1000
results<- as.data.frame(matrix(0, ncol = 12, nrow = loop))
colnames(results) <- c("coef","bw_l","bw_r","bw_length",
                       "coef_wa","bw_l_wa","bw_r_wa","bw_length_wa",
                       "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")
results.con<- as.data.frame(matrix(0, ncol = 12, nrow = loop))
colnames(results.con) <- c("coef","bw_l","bw_r","bw_length",
                           "coef_wa","bw_l_wa","bw_r_wa","bw_length_wa",
                           "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")

results.zero<- as.data.frame(matrix(0, ncol = 8, nrow = loop))
colnames(results.zero) <- c("coef","bw_l","bw_r","bw_length",
                            "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")

results.bwo<- as.data.frame(matrix(0, ncol = 8, nrow = loop))
colnames(results.bwo) <- c("coef","bw_l","bw_r","bw_length",
                           "coef_diff","bw_l_diff","bw_r_diff","bw_length_diff")

sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))

### NORMAL DISTRIBUTION ON BOTH SIDES, SAME POLYNOMIAL
df$y.base<-0.5*df$x  - 0.025*df$x^2+rnorm(length(df$x),0,10)  
jump=10
df$y=df$y.base+jump*df$treated
df$y.noisy=df$y+rnorm(length(df$x),0,30)
df$y.model<- jump*df$treated + 0.5*df$x  - 0.025*df$x^2


#for(i in 1:loop) {
 
  set.seed(1)
   for(i in 1:10) {
  # #i=1
  sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
  sample.x=subset(sample.x, x>-100 & x<100)
  sample=as.data.frame(inner_join(df, sample.x, by="x"))
  
  results.current=rdrobust(sample$y,sample$x, bwselect = "msetwo")
  results[i,1]=results.current$coef[2]-jump # normalizing to zero
  results[i,2:3]=results.current$bws[2,1:2]
  results[i,4]=results[i,2]+results[i,3]
  
  # results.con[i,1]=results.current$coef[1]-jump # normalizing to zero
  # results.con[i,2:3]=results.current$bws[1,1:2]
  # results.con[i,4]=results.con[i,2]+results.con[i,3]
  
  range=2
  sample$y.wacky <- ifelse((sample$x > results[i,3]-2 & sample$x < results[i,3]) | 
                             (sample$x < -results[i,2]+2 & sample$x > -results[i,2]),
                           sample$y.noisy,sample$y)
  results.current=rdrobust(sample$y.wacky,sample$x, bwselect = "msetwo")
  results[i,5]=results.current$coef[2]-jump # normalizing to zero
  results[i,6:7]=results.current$bws[2,1:2]
  results[i,8]=results[i,6]+results[i,7]
  results[i,9:12]=results[i,1:4]-results[i,5:8]
  

  
  sample.bwo=subset(sample,x> -results[i,6] & x< results[i,7])
  results.current=rdrobust(sample.bwo$y.wacky,sample.bwo$x, bwselect = "msetwo")
  results.bwo[i,1]=results.current$coef[2]-jump # normalizing to zero
  results.bwo[i,2:3]=results.current$bws[2,1:2]
  results.bwo[i,4]=results.bwo[i,2]+results.bwo[i,3]
  results.bwo[i,5:8]=results[i,5:8]-results.bwo[i,1:4]
  
  sample.zero=sample
  sample.zero$y.zero <- ifelse(sample$x> -results[i,6] & 
                            sample$x< results[i,7], sample$y.wacky, 0)
  results.current=rdrobust(sample.zero$y.zero,sample.zero$x, bwselect = "msetwo")
  results.zero[i,1]=results.current$coef[2]-jump # normalizing to zero
  results.zero[i,2:3]=results.current$bws[2,1:2]
  results.zero[i,4]=results.zero[i,2]+results.zero[i,3]
  results.zero[i,5:8]=results[i,5:8]-results.zero[i,1:4]
  
  
  # sample$y.wacky.con <- ifelse((sample$x > results.con[i,3]-2 & sample$x < results.con[i,3]) | 
  #                                (sample$x < -results.con[i,2]+2 & sample$x > -results.con[i,2]),
  #                              sample$y.noisy,sample$y)
  # 
  # results.current=rdrobust(sample$y.wacky.con,sample$x, bwselect = "msetwo")
  # results.con[i,5]=results.current$coef[1]-jump # normalizing to zero
  # results.con[i,6:7]=results.current$bws[1,1:2]
  # results.con[i,8]=results.con[i,6]+results.con[i,7]
  # 
  # results.con[i,9:12]=results.con[i,1:4]-results.con[i,5:8]
  coef_base=paste("Base: Coef = ",round(results[i,1],digits = 2)+jump,sep = "")
  coef_wacky=paste("Wacky: Coef = ",round(results[i,5],digits = 2)+jump,sep = "")
  coef_zero=paste("Zero: Coef = ",round(results.zero[i,1],digits = 2)+jump,sep = "")
  coef_bwo=paste("Drop: Coef = ",round(results.bwo[i,1],digits = 2)+jump,sep = "")
  
  figure_name=paste("figures/wacky_2_eg_",i,"_zero.png",sep = "")
  png(figure_name)
  sample.zero %>%
    filter(x > -20 & x < 20) %T>%
    plot(y.wacky~x,., ylim = range(c(y.wacky,y.zero)),
         col="red", ylab = "Y", xlab = "X") %T>%
      par(new = T) %>%
    plot(y.zero~x,., ylim = range(c(y.wacky,y.zero)),
         col="blue", ylab = "Y", xlab = "X") 
    abline(v = c(-results[i,2],-results[i,6],-results.zero[i,2],
               results[i,3], results[i,7], results.zero[i,3]),
         col=c("blue","red","green", "blue", "red","green"), 
         lty=c(1,2,3,1,2,3), lwd=c(1,2,3,1,2,3))
  legend("top", legend=c(coef_base, coef_wacky, coef_zero),
         col=c("blue", "red", "green"), lty=1:3, cex=0.8)
  dev.off()

  figure_name=paste("figures/wacky_2_eg_",i,"_bwo.png",sep = "")
  png(figure_name)
  temp.bwo=sample.bwo %>%
    filter(x > -20 & x < 20)
  temp=sample %>%
    filter(x > -20 & x < 20)
  plot(temp$y.wacky~temp$x, ylim = range(c(temp.bwo$y.wacky,temp$y.wacky)),
       xlim = range(c(temp.bwo$x,temp$x)),
       col="red", ylab = "Y", xlab = "X")
  par(new = T)
    plot(temp.bwo$y.wacky~temp.bwo$x, ylim = range(c(temp.bwo$y.wacky,temp$y.wacky)),
       xlim = range(c(temp.bwo$x,temp$x)),
              col="blue", ylab = "Y", xlab = "X")
    abline(v = c(-results[i,2],-results[i,6], -results.bwo[i,2],
               results[i,3], results[i,7], results.bwo[i,3]),
         col=c("blue","red","green", "blue", "red","green"),
         lty=c(1,2,3,1,2,3), lwd=c(1,2,3,1,2,3))
    legend("top", legend=c(coef_base, coef_wacky, coef_bwo),
           col=c("blue", "red", "green"), lty=1:3, cex=0.8)
    dev.off()
  
}

   save.image("stressout_cont_data.RData")


png("figures/coeff_diff_wacky_bwo_2.png")
ggplot(results.bwo, aes(coef_diff)) + geom_density()
dev.off()

png("figures/coeff_diff_cdf_wacky_bwo_2.png")
ggplot(results.bwo, aes(coef_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_diff_wacky_bwo_2.png")
ggplot(results.bwo, aes(bw_length_diff)) + geom_density()
dev.off()

png("figures/bw_diff_cdf_wacky_bwo_2.png")
ggplot(results.bwo, aes(bw_length_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_l_diff_wacky_bwo_2.png")
ggplot(results.bwo, aes(bw_l_diff)) + geom_density()
dev.off()

png("figures/bw_l_diff_cdf_wacky_bwo_2.png")
ggplot(results.bwo, aes(bw_l_diff)) + stat_ecdf(geom = "step")
dev.off()


png("figures/bw_r_diff_wacky_bwo_2.png")
ggplot(results.bwo, aes(bw_r_diff)) + geom_density()
dev.off()

png("figures/bw_r_diff_cdf_wacky_bwo_2.png")
ggplot(results.bwo, aes(bw_r_diff)) + stat_ecdf(geom = "step")
dev.off()


png("figures/coeff_diff_wacky_zero_2.png")
ggplot(results.zero, aes(coef_diff)) + geom_density()
dev.off()

png("figures/coeff_diff_cdf_wacky_zero_2.png")
ggplot(results.zero, aes(coef_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_diff_wacky_zero_2.png")
ggplot(results.zero, aes(bw_length_diff)) + geom_density()
dev.off()

png("figures/bw_diff_cdf_wacky_zero_2.png")
ggplot(results.zero, aes(bw_length_diff)) + stat_ecdf(geom = "step")
dev.off()

png("figures/bw_l_diff_wacky_zero_2.png")
ggplot(results.zero, aes(bw_l_diff)) + geom_density()
dev.off()

png("figures/bw_l_diff_cdf_wacky_zero_2.png")
ggplot(results.zero, aes(bw_l_diff)) + stat_ecdf(geom = "step")
dev.off()


png("figures/bw_r_diff_wacky_zero_2.png")
ggplot(results.zero, aes(bw_r_diff)) + geom_density()
dev.off()

png("figures/bw_r_diff_cdf_wacky_zero_2.png")
ggplot(results.zero, aes(bw_r_diff)) + stat_ecdf(geom = "step")
dev.off()
###########################################################

  #rdrobust(sample$y.wacky,sample$x, bwselect = "msetwo")$bws
  obw_pre=rdrobust(sample$y,sample$x, bwselect = "msetwo")$bws[2,1:2]
  sample$y.wacky <- ifelse((sample$x > obw_pre[2]-1 & sample$x < obw_pre[2]) | 
                             (sample$x < -obw_pre[1]+1 & sample$x > -obw_pre[1]),
                           sample$wa,sample$y.noisy)
  
  
  sample %>%
    filter(x > -30 & x < 30) %T>%
    plot(y~x,., ylim = range(c(y,y.model)),
         col="blue", ylab = "Y DGP", xlab = "X") %T>%
    par(new = T) %>%
    plot(y.model~x,., ylim = range(c(y,y.model)),
         axes = FALSE, xlab = "", ylab = "")

  png("figures/wacky_2_eg_1000.png")
  sample %>%
    filter(x > -20 & x < 20) %T>%
    plot(y.wacky~x,., ylim = range(c(y.wacky,y)),
         col="red", ylab = "Y", xlab = "X") %T>%
    par(new = T) %>%
    plot(y~x,., ylim = range(c(y,y.wacky)),
         col="blue", ylab = "Y", xlab = "X") 
#    par(new = T) %>%
 #   plot(y.model~x,.,ylim = range(c(y,y.model)),
  #       axes = FALSE, xlab = "", ylab = "")
        abline(v = c(-results[1000,2],-results[1000,6], results[1000,3], results[1000,7]),
             col=c("blue","red","blue", "red"), lty=c(1,2,1,2), lwd=c(1,3,1,3))
      dev.off()
