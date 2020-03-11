rm(list=ls(all=TRUE))
set.seed(1)
library(dplyr)
library(rdrobust)
library(ggplot2)
library(reshape2)
library(gridExtra)
setwd("C:/Users/yaela/Dropbox (Brown)/Brown/RA/RD/R")  

###### PARAMETERS
set.seed(1)
jump=10
loop=1000
quadratic=T
symmetric=T
normal.x=T
gdp.sd=10
noisy.sd=30
so.int=2 # interval inside obw for adding noise
bc=F #bias-corrected estimates or conventional
treat="zero" # select zero or bwo

### SETTING UP FOR EVERYTHING - A SINGLE DGP ###
df <- as.data.frame(matrix(0, ncol = 0, nrow = length(seq(-100,100,0.01))))
df$x=round(seq(-100,100,0.01), digits=2)
df=subset(df,df$x!=0)
df$treated <- ifelse(df$x>0, 1, 0)

# Definitions
quad_test=ifelse(quadratic==T,"quadratic","linear")
normal_test=ifelse(normal.x==T,"rnorm","runif")
bc_test=ifelse(bc==T,"bc","c")
save.ext=paste(bc_test,quad_test,normal_test,sep = "_")
sym_test=ifelse(symmetric==T,"mserd","msetwo")

results<- as.data.frame(matrix(0, ncol = 4, nrow = loop))
colnames(results) <- c("coef","obw","obw_l","obw_r")


results.treat<- as.data.frame(matrix(0, ncol = 6, nrow = loop))
colnames(results.treat) <- c("coef","obw","obw_l","obw_r",
                             "coef_diff","obw_diff")

sample.x <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(df)/10))


### DGP: 
df$y.model<- 0.5*df$x  - 0.025*df$x^2*quadratic + jump*df$treated
df$y=df$y.model+rnorm(length(df$x),0,gdp.sd)

###### DGP GENERAL FIGURES ##############

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

######### Iterations ##########

for(i in 1:loop) {
  if (normal.x==T) {
    sample.x$x <- round(rnorm(nrow(df)/10, 0, 10),digits = 2) 
  } else {
    sample.x$x <- round(runif(nrow(df)/10, -20,20),digits = 2) 
  }
  
  sample.x=subset(sample.x, x>-100 & x<100)
  sample=as.data.frame(inner_join(df, sample.x, by="x"))
  results.current=rdrobust(sample$y,sample$x,bwselect = sym_test)
  results[i,1]=results.current$coef[bc+1]-jump # normalizing to zero
  results[i,3:4]=results.current$bws[bc+1,1:2]
  results[i,2]=results[i,3]+results[i,4]
  

  if (treat=="zero") {
    sample.treat=sample
    sample.treat$y <- ifelse(sample$x> -results[i,3] & 
                                     sample$x< results[i,4], sample$y, 0)
    
  } else if (treat=="bwo") {
    sample.treat=subset(sample,x> -results[i,3] & x< results[i,4])
  }
  
  results.current=rdrobust(sample.treat$y,sample.treat$x)
  results.treat[i,1]=results.current$coef[bc+1]-jump # normalizing to zero
  results.treat[i,3:4]=results.current$bws[bc+1,1:2]
  results.treat[i,2]=results.treat[i,3]+results.treat[i,4]
  
  results.treat[i,5:6]=results[i,1:2]-results.treat[i,1:2] # computing diff from base

    #### FIGURES INSIDE LOOP - SPECIFIC DRAWS ####
  if (i <= 5) {
    
    coef_base=paste("Base: Coef = ",round(results[i,1],digits = 2)+jump,sep = "")
    coef_treat=paste(treat,": Coef = ",round(results.treat[i,1],digits = 2)+jump,sep = "")
    
    figure_name=paste("figures/sample",i,treat,save.ext,"model.png",sep = "_")
    png(figure_name)
    temp.treat=sample.treat %>%
      filter(x > -20 & x < 20)
    temp=sample %>%
      filter(x > -20 & x < 20)
    plot(temp$y~temp$x, ylim = range(c(temp.treat$y,temp$y)),
         xlim = range(c(temp.treat$x,temp$x)),
         col="red", ylab = "Y", xlab = "X")
    par(new = T)
    plot(temp.treat$y~temp.treat$x, ylim = range(c(temp.treat$y,temp$y)),
         xlim = range(c(temp.treat$x,temp$x)),
         col="blue", ylab = "Y", xlab = "X")
    par(new = T)
    plot(temp$y.model~temp$x, ylim = range(c(temp.treat$y,temp$y)),
         xlim = range(c(temp.treat$x,temp$x)),
         col="black", ylab = "Y", xlab = "X")
    abline(v = c(-results[i,3], -results.treat[i,3],
                 results[i,4], results.treat[i,4]),
           col=c("blue","red", "blue", "red"),
           lty=c(1,2,1,2), lwd=c(1,2,1,2))
    legend("top", legend=c(coef_base, coef_treat),
           col=c("blue", "red"), lty=1:2, cex=0.8)
    dev.off()
    
    
    figure_name=paste("figures/rd_plot",i,"_",save.ext,".png",sep = "")
    png(figure_name)
    rdplot(sample$y,sample$x)
    dev.off()
    
    
    figure_name=paste("figures/sample",i,"_",save.ext,".png",sep = "")
    png(figure_name)
    sample %>%
      filter(x > -30 & x < 30) %T>%
      plot(y~x,., ylim = range(c(y,y.model)),
           col="blue", ylab = "Y", xlab = "X") %T>%
      par(new = T) %>%
      plot(y.model~x,., ylim = range(c(y,y.model)),
           axes = FALSE, xlab = "", ylab = "")
    dev.off()
    
    
  }
}

data_name=paste("base_data","_",treat,"_",save.ext,".RData",sep = "")
#save.image(data_name)

######## SUMMARY FIGURES ###########


#load(data_name)

## PDF'S OF LEVELS
figure_name=paste("figures/coef","_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results, aes(coef)) + geom_density() + 
  geom_vline(aes(xintercept=0), colour="#BB0000", linetype="dashed")
dev.off()

figure_name=paste("figures/coef_so","_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results, aes(coef_so)) + geom_density() + 
  geom_vline(aes(xintercept=0), colour="#BB0000", linetype="dashed")
dev.off()

figure_name=paste("figures/coef","_",treat,"_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results.treat, aes(coef)) + geom_density() +
  geom_vline(aes(xintercept=0), colour="#BB0000", linetype="dashed")
dev.off()

## so_base_diff_CDF
figure_name=paste("figures/coef_diff_cdf_base_so","_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results, aes(coef_diff)) + stat_ecdf(geom = "step")
dev.off()

figure_name=paste("figures/bw_diff_cdf_base_so","_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results, aes(obw_diff)) + stat_ecdf(geom = "step")
dev.off()

### treat_base_diff
figure_name=paste("figures/coef_diff_cdf_base","_",treat,"_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results.treat, aes(coef_diff)) + stat_ecdf(geom = "step")
dev.off()

figure_name=paste("figures/obw_diff_cdf_base","_",treat,"_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results.treat, aes(obw_diff)) + stat_ecdf(geom = "step")
dev.off()

### treat_so_diff
figure_name=paste("figures/coef_diff_cdf_so","_",treat,"_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results.treat, aes(coef_diff_so)) + stat_ecdf(geom = "step")
dev.off()

figure_name=paste("figures/obw_diff_cdf_so","_",treat,"_",save.ext,".png",sep = "")
png(figure_name)
ggplot(results.treat, aes(obw_diff_so)) + stat_ecdf(geom = "step")
dev.off()


### TESTING CORRELATION BETWEEN OBW_DIFF AND INITIAL OBW ###
load("stressout_data_single_bw.RData")
cor(results$obw,results.treat$obw_diff)
cor(results$coef,results.treat$coef_diff)
coef_means<- as.data.frame(matrix(0, ncol = 5, nrow = 4))
rownames(coef_means) <- c("coef_base","coef_so","coef_zero","coef_bwo")
colnames(coef_means) <- c("quadratic_asymmetric","quadratic_symmetric",
                          "quadratic_symmetric_low_sd","linear_symmetric",
                          "linear_symmetric_low_sd")


####### SAVING RESULTS TO TABLE ###########
results_table=as.data.frame(matrix(0, ncol = 5, nrow = 0))
results_table[1,1]=round(mean(results$coef),digits = 4)
results_table[1,2]=round(mean(results$coef_so),digits = 4)
results_table[1,3]=round(mean(results.treat$coef),digits = 4)
results_table[1,4]=round(mean(results.treat$coef_diff),digits = 4)
results_table[1,5]=round(mean(results.treat$coef_diff_so),digits = 4)
results_table[2,1]=round(mean(results$obw),digits = 4)
results_table[2,2]=round(mean(results$obw_so),digits = 4)
results_table[2,3]=round(mean(results.treat$obw),digits = 4)
results_table[2,4]=round(mean(results.treat$obw_diff),digits = 4)
results_table[2,5]=round(mean(results.treat$obw_diff_so),digits = 4)
colnames(results_table) = c("base","so",treat,"diff_base","diff_so")
rownames(results_table) = c("coef","obw")

table_name=paste("figures/results_table_",treat,"_",save.ext,".pdf",sep = "")
pdf(table_name)
grid.table(results_table)
dev.off()




