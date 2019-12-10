### COMBINING DENSITY PLOTS
*rm(list=ls(all=TRUE))
set.seed(1)
library(dplyr)
library(rdrobust)
library(ggplot2)
library(reshape2)
setwd("C:/Users/yaela/Dropbox (Brown)/Brown/RA/RD/R")  

load("simulations_data.RData")

### coef_cl, poly l2_r2, normal
dat <- data.frame(coef_cl = c(results_x_norm_l2_r2_j1$coef_cl, results_x_norm_l2_r2_j5$coef_cl,
                              results_x_norm_l2_r2_j10$coef_cl, results_x_norm_l2_r2_j20$coef_cl),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_cl_l2_r2_norm.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_cl, poly l2_r2, uniform
dat <- data.frame(coef_cl = c(results_x_unif_l2_r2_j1$coef_cl, results_x_unif_l2_r2_j5$coef_cl,
                              results_x_unif_l2_r2_j10$coef_cl, results_x_unif_l2_r2_j20$coef_cl),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_cl_l2_r2_unif.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, normal
dat <- data.frame(coef_bc = c(results_x_norm_l2_r2_j1$coef_bc, results_x_norm_l2_r2_j5$coef_bc,
                              results_x_norm_l2_r2_j10$coef_bc, results_x_norm_l2_r2_j20$coef_bc),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_bc_l2_r2_norm.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, uniform
dat <- data.frame(coef_bc = c(results_x_unif_l2_r2_j1$coef_bc, results_x_unif_l2_r2_j5$coef_bc,
                              results_x_unif_l2_r2_j10$coef_bc, results_x_unif_l2_r2_j20$coef_bc),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_bc_l2_r2_unif.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

######## BW #########
### bw_h_l, poly l2_r2, normal
dat <- data.frame(bw_h_l = c(results_x_norm_l2_r2_j1$bw_h_l, results_x_norm_l2_r2_j5$bw_h_l,
                             results_x_norm_l2_r2_j10$bw_h_l, results_x_norm_l2_r2_j20$bw_h_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_l_l2_r2_norm.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_l, poly l2_r2, uniform
dat <- data.frame(bw_h_l = c(results_x_unif_l2_r2_j1$bw_h_l, results_x_unif_l2_r2_j5$bw_h_l,
                             results_x_unif_l2_r2_j10$bw_h_l, results_x_unif_l2_r2_j20$bw_h_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_l_l2_r2_unif.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_r, poly l2_r2, normal
dat <- data.frame(bw_h_r = c(results_x_norm_l2_r2_j1$bw_h_r, results_x_norm_l2_r2_j5$bw_h_r,
                             results_x_norm_l2_r2_j10$bw_h_r, results_x_norm_l2_r2_j20$bw_h_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_r_l2_r2_norm.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_r, poly l2_r2, uniform
dat <- data.frame(bw_h_r = c(results_x_unif_l2_r2_j1$bw_h_r, results_x_unif_l2_r2_j5$bw_h_r,
                             results_x_unif_l2_r2_j10$bw_h_r, results_x_unif_l2_r2_j20$bw_h_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_r_l2_r2_unif.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_h norm
dat <- data.frame(bw_h_length = c(results_x_norm_l2_r2_j1$bw_h_length, results_x_norm_l2_r2_j5$bw_h_length,
                                  results_x_norm_l2_r2_j10$bw_h_length, results_x_norm_l2_r2_j20$bw_h_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_length_l2_r2_norm.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_h uniform
dat <- data.frame(bw_h_length = c(results_x_unif_l2_r2_j1$bw_h_length, results_x_unif_l2_r2_j5$bw_h_length,
                                  results_x_unif_l2_r2_j10$bw_h_length, results_x_unif_l2_r2_j20$bw_h_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_length_l2_r2_unif.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### bw_b_l, poly l2_r2, normal
dat <- data.frame(bw_b_l = c(results_x_norm_l2_r2_j1$bw_b_l, results_x_norm_l2_r2_j5$bw_b_l,
                             results_x_norm_l2_r2_j10$bw_b_l, results_x_norm_l2_r2_j20$bw_b_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_l_l2_r2_norm.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_l, poly l2_r2, uniform
dat <- data.frame(bw_b_l = c(results_x_unif_l2_r2_j1$bw_b_l, results_x_unif_l2_r2_j5$bw_b_l,
                             results_x_unif_l2_r2_j10$bw_b_l, results_x_unif_l2_r2_j20$bw_b_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_l_l2_r2_unif.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_r, poly l2_r2, normal
dat <- data.frame(bw_b_r = c(results_x_norm_l2_r2_j1$bw_b_r, results_x_norm_l2_r2_j5$bw_b_r,
                             results_x_norm_l2_r2_j10$bw_b_r, results_x_norm_l2_r2_j20$bw_b_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_r_l2_r2_norm.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_r, poly l2_r2, uniform
dat <- data.frame(bw_b_r = c(results_x_unif_l2_r2_j1$bw_b_r, results_x_unif_l2_r2_j5$bw_b_r,
                             results_x_unif_l2_r2_j10$bw_b_r, results_x_unif_l2_r2_j20$bw_b_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_r_l2_r2_unif.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_b norm
dat <- data.frame(bw_b_length = c(results_x_norm_l2_r2_j1$bw_b_length, results_x_norm_l2_r2_j5$bw_b_length,
                                  results_x_norm_l2_r2_j10$bw_b_length, results_x_norm_l2_r2_j20$bw_b_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_length_l2_r2_norm.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_b uniform
dat <- data.frame(bw_b_length = c(results_x_unif_l2_r2_j1$bw_b_length, results_x_unif_l2_r2_j5$bw_b_length,
                                  results_x_unif_l2_r2_j10$bw_b_length, results_x_unif_l2_r2_j20$bw_b_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_length_l2_r2_unif.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

######### PLOTS DROPPING OBS OUTSIDE BW
### coef_cl, poly l2_r2, normal
dat <- data.frame(coef_cl = c(results_bw_x_norm_l2_r2_j1$coef_cl, results_bw_x_norm_l2_r2_j5$coef_cl,
                              results_bw_x_norm_l2_r2_j10$coef_cl, results_bw_x_norm_l2_r2_j20$coef_cl),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_cl_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_cl, poly l2_r2, uniform
dat <- data.frame(coef_cl = c(results_bw_x_unif_l2_r2_j1$coef_cl, results_bw_x_unif_l2_r2_j5$coef_cl,
                              results_bw_x_unif_l2_r2_j10$coef_cl, results_bw_x_unif_l2_r2_j20$coef_cl),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_cl_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = coef_cl, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, normal
dat <- data.frame(coef_bc = c(results_bw_x_norm_l2_r2_j1$coef_bc, results_bw_x_norm_l2_r2_j5$coef_bc,
                              results_bw_x_norm_l2_r2_j10$coef_bc, results_bw_x_norm_l2_r2_j20$coef_bc),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/coef_bc_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

### coef_bc, poly l2_r2, uniform
dat <- data.frame(coef_bc = c(results_bw_x_unif_l2_r2_j1$coef_bc, results_bw_x_unif_l2_r2_j5$coef_bc,
                              results_bw_x_unif_l2_r2_j10$coef_bc, results_bw_x_unif_l2_r2_j20$coef_bc),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/coef_bc_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = coef_bc, fill = spec)) + geom_density(alpha = 0.3) + geom_vline(xintercept=0,  linetype="dashed")
dev.off()

######## BW #########
### bw_h_l, poly l2_r2, normal
dat <- data.frame(bw_h_l = c(results_bw_x_norm_l2_r2_j1$bw_h_l, results_bw_x_norm_l2_r2_j5$bw_h_l,
                             results_bw_x_norm_l2_r2_j10$bw_h_l, results_bw_x_norm_l2_r2_j20$bw_h_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_l_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_l, poly l2_r2, uniform
dat <- data.frame(bw_h_l = c(results_bw_x_unif_l2_r2_j1$bw_h_l, results_bw_x_unif_l2_r2_j5$bw_h_l,
                             results_bw_x_unif_l2_r2_j10$bw_h_l, results_bw_x_unif_l2_r2_j20$bw_h_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_l_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = bw_h_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_r, poly l2_r2, normal
dat <- data.frame(bw_h_r = c(results_bw_x_norm_l2_r2_j1$bw_h_r, results_bw_x_norm_l2_r2_j5$bw_h_r,
                             results_bw_x_norm_l2_r2_j10$bw_h_r, results_bw_x_norm_l2_r2_j20$bw_h_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_r_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_h_r, poly l2_r2, uniform
dat <- data.frame(bw_h_r = c(results_bw_x_unif_l2_r2_j1$bw_h_r, results_bw_x_unif_l2_r2_j5$bw_h_r,
                             results_bw_x_unif_l2_r2_j10$bw_h_r, results_bw_x_unif_l2_r2_j20$bw_h_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_r_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = bw_h_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_h norm
dat <- data.frame(bw_h_length = c(results_bw_x_norm_l2_r2_j1$bw_h_length, results_bw_x_norm_l2_r2_j5$bw_h_length,
                                  results_bw_x_norm_l2_r2_j10$bw_h_length, results_bw_x_norm_l2_r2_j20$bw_h_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_h_length_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_h uniform
dat <- data.frame(bw_h_length = c(results_bw_x_unif_l2_r2_j1$bw_h_length, results_bw_x_unif_l2_r2_j5$bw_h_length,
                                  results_bw_x_unif_l2_r2_j10$bw_h_length, results_bw_x_unif_l2_r2_j20$bw_h_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_h_length_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = bw_h_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()


### bw_b_l, poly l2_r2, normal
dat <- data.frame(bw_b_l = c(results_bw_x_norm_l2_r2_j1$bw_b_l, results_bw_x_norm_l2_r2_j5$bw_b_l,
                             results_bw_x_norm_l2_r2_j10$bw_b_l, results_bw_x_norm_l2_r2_j20$bw_b_l),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_l_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_l, poly l2_r2, uniform
dat <- data.frame(bw_b_l = c(results_bw_x_unif_l2_r2_j1$bw_b_l, results_bw_x_unif_l2_r2_j5$bw_b_l,
                             results_bw_x_unif_l2_r2_j10$bw_b_l, results_bw_x_unif_l2_r2_j20$bw_b_l),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_l_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = bw_b_l, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_r, poly l2_r2, normal
dat <- data.frame(bw_b_r = c(results_bw_x_norm_l2_r2_j1$bw_b_r, results_bw_x_norm_l2_r2_j5$bw_b_r,
                             results_bw_x_norm_l2_r2_j10$bw_b_r, results_bw_x_norm_l2_r2_j20$bw_b_r),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_r_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

### bw_b_r, poly l2_r2, uniform
dat <- data.frame(bw_b_r = c(results_bw_x_unif_l2_r2_j1$bw_b_r, results_bw_x_unif_l2_r2_j5$bw_b_r,
                             results_bw_x_unif_l2_r2_j10$bw_b_r, results_bw_x_unif_l2_r2_j20$bw_b_r),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_r_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = bw_b_r, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_b norm
dat <- data.frame(bw_b_length = c(results_bw_x_norm_l2_r2_j1$bw_b_length, results_bw_x_norm_l2_r2_j5$bw_b_length,
                                  results_bw_x_norm_l2_r2_j10$bw_b_length, results_bw_x_norm_l2_r2_j20$bw_b_length),
                  spec = rep(c("norm_j1", "norm_j5", "norm_j10","norm_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("norm_j1", "norm_j5", "norm_j10","norm_j20"))
png("figures/bw_b_length_l2_r2_norm_bw_only.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### Interval bw_b uniform
dat <- data.frame(bw_b_length = c(results_bw_x_unif_l2_r2_j1$bw_b_length, results_bw_x_unif_l2_r2_j5$bw_b_length,
                                  results_bw_x_unif_l2_r2_j10$bw_b_length, results_bw_x_unif_l2_r2_j20$bw_b_length),
                  spec = rep(c("unif_j1", "unif_j5", "unif_j10","unif_j20"), each = loop))
dat$spec <- factor(dat$spec, levels =c("unif_j1", "unif_j5", "unif_j10","unif_j20"))
png("figures/bw_b_length_l2_r2_unif_bw_only.png")
ggplot(dat, aes(x = bw_b_length, fill = spec)) + geom_density(alpha = 0.3)
dev.off()

#### OBS OUTSIDE BW TURNED TO ZERO
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
  
  
  
  
  
  
  
