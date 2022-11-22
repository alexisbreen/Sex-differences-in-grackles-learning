#####################################################################################################################################################

#Code for plotting Figure 2 for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script


#Required packages
library(survival)
library(tidyverse)

#NOTE: Forward_Simulations.R script has to be run before executing below!
#NOTE: RL_Execution.R script has to be run before executing below!
#NOTE: Panels j and l will vary by simulation - play around and see for yourself!

#####################################################################################################################################################
#Pre-graph processing
#####################################################################################################################################################

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#Next, need to ready data for survival plots by...
#Subsetting by phase & non-skip birds & calculate max trials per individual & assign censored column for survival plots
I_Surv <- d %>% filter(Phase == 1 & Criterion == 1) %>% group_by(id) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))
R_Surv <- d %>% filter(Phase == 2 & Criterion == 1 & skip == 0) %>% group_by(id) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))

#And fitting survival trajectories
I_Fit <- survfit(Surv(Trial, cens) ~ sex, data = I_Surv)
R_Fit <- survfit(Surv(Trial, cens) ~ sex, data = R_Surv)

#For obtaining survival probability medians to plot
med_IF <- summary(I_Fit)$table[,'median'][1] #35
med_IM <- summary(I_Fit)$table[,'median'][2] #32
med_RF <- summary(R_Fit)$table[,'median'][1] #81
med_RM <- summary(R_Fit)$table[,'median'][2] #64

#Also, need to define function to execute a stacked density plot in base r, based on: https://stackoverflow.com/questions/25328533/overlapping-stacked-density-plots
stacked.density <- function(data, fac = 3, xlim, col = 'black', 
                            left = -200, bottom = 1, right = 200, top = 2,
                            alpha = 0.4, show.xaxis = T, height = 2,
                            xlab = '', ylab = ''){
  
  xvals = unlist(lapply(data, function(d) d$x))
  if(missing(xlim)) xlim=c(min(xvals), max(xvals))
  
  col = sapply(col, alpha)
  if(length(col) == 1) col = rep(col, length(data))
  
  plot(1, type = "n", xlim = xlim, ylim = c(1,length(data) + height),
       yaxt='n', xaxt=ifelse(show.xaxis, 'l', 'n'), xlab = xlab, ylab = ylab)
  
  rect(left,bottom,right,top, col = alpha("grey", alpha = .3), border = NA)
  
  z = length(data):1
  for(i in 1:length(data)){
    d = data[[ z[i] ]]
    lines(d$x, fac*d$y + i, col = col[i], lwd = 1)
    polygon(d$x, fac*d$y+ i, col = alpha(col[i], alpha = 0.8), border = NA)
    abline(h = i, lwd = 1)
  }
}

#Now, need to perform population- and individual level simulations to plot

#Population-level forward simulations
dat_pop_sim <- Post_Study_Sim_Fxn(N_F = 5000, N_M = 5000, N_sim = 1, mu_on = 0)

#Next, need to ready simulated population-level data for survival plots by...
#Subsetting by phase & calculate max trials per individual & assign censored column for survival plots
I_Surv_pop_sim <- dat_pop_sim %>% filter(Phase == 1) %>% group_by(id) %>% filter(trialafter == max(trialafter)) %>% mutate(cens = rep(1))
R_Surv_pop_sim <- dat_pop_sim %>% filter(Phase == 2) %>% group_by(id) %>% filter(trialafter == max(trialafter)) %>% mutate(cens = rep(1))

#And fitting survival trajectories
I_Fit_pop_sim <- survfit(Surv(trialafter, cens) ~ sex, data = I_Surv_pop_sim)
R_Fit_pop_sim <- survfit(Surv(trialafter, cens) ~ sex, data = R_Surv_pop_sim)

#For obtaining survival probability medians to plot - note these can vary slightly due to simulation stochasticity
med_IF_sim <- summary(I_Fit_pop_sim)$table[,'median'][1] #32
med_IM_sim <- summary(I_Fit_pop_sim)$table[,'median'][2] #31
med_RF_sim <- summary(R_Fit_pop_sim)$table[,'median'][1] #79
med_RM_sim <- summary(R_Fit_pop_sim)$table[,'median'][2] #62

#Individual-level forward simulations
dat_ind_sim <- Post_Study_Sim_Fxn(N_F = 3, N_M = 3, N_sim = 1, mu_on = 1)

#Wrangle simulated individual-level data into sex and phase-specific data frames
male_ind_sim_init <- dat_ind_sim %>% filter(sex == 2 & Phase == 1)
male_ind_sim_rev <- dat_ind_sim %>% filter(sex == 2 & Phase == 2)
female_ind_sim_init <- dat_ind_sim %>% filter(sex == 1 & Phase == 1)
female_ind_sim_rev <- dat_ind_sim %>% filter(sex == 1 & Phase == 2)

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(male_ind_sim_init)){
  if(male_ind_sim_init$id[i] == 4){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 3, 4)
  } else if(male_ind_sim_init$id[i] == 5){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 7, 8)
  } else if(male_ind_sim_init$id[i] == 6){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 11, 12)
  }
}

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(male_ind_sim_rev)){
  if(male_ind_sim_rev$id[i] == 4){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 3, 4)
  } else if(male_ind_sim_rev$id[i] == 5){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 7, 8)
  } else if(male_ind_sim_rev$id[i] == 6){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 11, 12)
  }
}

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(female_ind_sim_init)){
  if(female_ind_sim_init$id[i] == 2){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 5, 6)
  } else if(female_ind_sim_init$id[i] == 3){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 9, 10)
  }
}

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(female_ind_sim_rev)){
  if(female_ind_sim_rev$id[i] == 2){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 5, 6)
  } else if(female_ind_sim_rev$id[i] == 3){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 9, 10)
  }
}

#And get switch counts for plotting
switch_init <- matrix(NA, 1, 6)
switch_rev <- matrix(NA, 1, 6)
for(id in min(female_ind_sim_init$id):max(female_ind_sim_init$id)){
  switch_init[, id] <- sum(female_ind_sim_init$switch[which(female_ind_sim_init$id == id)])
}
for(id in min(male_ind_sim_init$id):max(male_ind_sim_init$id)){
  switch_init[, id] <- sum(male_ind_sim_init$switch[which(male_ind_sim_init$id == id)])
}  
for(id in min(female_ind_sim_rev$id):max(female_ind_sim_rev$id)){
  switch_rev[, id] <- sum(female_ind_sim_rev$switch[which(female_ind_sim_rev$id == id)])
}
for(id in min(male_ind_sim_rev$id):max(male_ind_sim_rev$id)){
  switch_rev[, id] <- sum(male_ind_sim_rev$switch[which(male_ind_sim_rev$id == id)])
}  

#####################################################################################################################################################
#Figure 2
#####################################################################################################################################################

#pdf(file = "Figure_2.pdf", height = 11, width = 10) #turn on if want pdf of plot

#Set-up plot space
par(mfrow = c(4,6), mar = c(3,5,3,2), oma = c(3,2,5,0))
layout(matrix(c(1,1,2,3,3,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,10,11,11,12),nrow=4,ncol=6,byrow=TRUE))

#Panel A - behavioural data
par(mar = c(3,5,3,2.5))

#Initial 
plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
abline(v = c(32,35), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(I_Fit, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Initial learning", side = 3, adj = 1.75, cex = 1.2, line = 3, font = 2)
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 3)
mtext("Across", side = 3, at = 50, cex = 1.2, line = 1)
text(x = (med_IF + 5), y = 0, med_IF)
text(x = (med_IM - 5), y = 0, med_IM)
mtext("Behaviour",  cex = 1.2, side = 2, line = 5, font = 2)
mtext("a", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.25)

#Posterior
par(mar = c(3,2.5,3,2))

data_init <- list(
  
  male = density(s_speed_xpop$init_M),
  female = density(s_speed_xpop$init_F),
  across = density(s_speed_xpop$init_X)
  
)

stacked.density(data_init, fac = 6, height = 1.5, col=c("black", "#fde725", "#5ec962"), alpha=0.2, show.xaxis = F,
                left = -100, bottom = 1, right = 150, top = 2,)
abline(v = 0, lty = 5)
axis(side = 1, at = c(0), labels = c("0"), cex.axis = 1.3)
axis(side = 2, at = c(1,2,3), labels = c("M-F", "F", "M"), las = 2, cex.axis = 1.3)
mtext("Posterior", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Density",  cex = 1.2, side = 2, line = 3)
mtext("b", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.6)

#Reversal
par(mar = c(3,5,3,2.5))

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(200), labels = c("200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
abline(v = c(64,81), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(R_Fit, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Reversal learning", side = 3, adj = 2.25, cex = 1.2, line = 3, font = 2)
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 3)
mtext("Across", side = 3, at = 100, cex = 1.2, line = 1)
text(x = (med_RF + 10), y = 0, med_RF)
text(x = (med_RM - 10), y = 0, med_RM)
mtext("c", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.25)

#Posterior
par(mar = c(3,2.5,3,2))

data_rev <- list(
  
  male = density(s_speed_xpop$rev_M),
  female = density(s_speed_xpop$rev_F),
  across = density(s_speed_xpop$rev_X)
  
  
)

stacked.density(data_rev, fac = 12, height = 1.5, col=c("black", "#fde725", "#5ec962"), alpha=0.2, show.xaxis = F,
                left = -100, bottom = 1, right = 150, top = 2,)
abline(v = 0, lty = 5)
axis(side = 1, at = c(0), labels = c("0"), cex.axis = 1.3)
axis(side = 2, at = c(1,2,3), labels = c("M-F", "F", "M"), las = 2, cex.axis = 1.3)
mtext("Posterior", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Density",  cex = 1.2, side = 2, line = 3)
mtext("d", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.6)

#Panel B - updating rate
par(mar = c(3,5,3,2))

#Treatments go along x-axis at these points (for this and all following plots)
x <- c(1:10) 

#Phi

#Initial
s_phi_init_plot <- s_phi_init[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_phi_init <- apply(s_phi_init_plot, 2, mean)
HPDI_phi_init <- apply(s_phi_init_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.3,.3), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.3,-0.2,-0.1,0,0.1,0.2), labels = c("-0.3","","","0","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.3), labels = c("0.3"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 3)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-1.1,3.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-1.1,6.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(8.5,-1.1,9.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-1.1,10.5,1.1,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_init_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_phi_init[1,], x1 = x, y1 = HPDI_phi_init[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_phi_init, col = "red", pch = 19)
mtext("e", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.15)

#Reversal
s_phi_rev_plot <- s_phi_rev[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_phi_rev <- apply(s_phi_rev_plot, 2, mean)
HPDI_phi_rev <- apply(s_phi_rev_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.3,.3), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab= NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.3,-0.2,-0.1,0,0.1,0.2), labels = c("-0.3","","","0","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.3), labels = c("0.3"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 3)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-1.1,3.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-1.1,6.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(8.5,-1.1,9.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-1.1,10.5,1.1,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_rev_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_phi_rev[1,], x1 = x, y1 = HPDI_phi_rev[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_phi_rev, col = "red", pch = 19)
mtext("f", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.14)

#Panel C - risk-sensitivity

#Lambda

#Initial
s_L_init_plot <- s_L_init[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_L_init <- apply(s_L_init_plot, 2, mean)
HPDI_L_init <- apply(s_L_init_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-11,11), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 3)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-12,3.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-12,6.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(8.5,-12,9.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-12,10.5,12,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1, col = "black")
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_init_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_L_init[1,], x1 = x, y1 = HPDI_L_init[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_L_init, col = "red", pch = 19)
mtext("Mechanisms",  cex = 1.2, side = 2, line = 5, font = 2, adj = 2.8)
mtext("g", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.15)

#Reversal
s_L_rev_plot <- s_L_rev[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_L_rev <- apply(s_L_rev_plot, 2, mean)
HPDI_L_rev <- apply(s_L_rev_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-11,11), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 3)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-12,3.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-12,6.5,12,col = alpha("grey",alpha=.3),border = NA)
rect(8.5,-12,9.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-12,10.5,12,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_rev_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_L_rev[1,], x1 = x, y1 = HPDI_L_rev[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_L_rev, col = "red", pch = 19)
mtext("h", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.15)

#Panel D - forward simulations

#Initial - population-level
par(mar = c(3,5,3,2.5))

plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
mtext("Ten thousand 'birds'", side = 3, at = 50, cex = 1.2, line = 1)
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 3)
abline(v = c(31,32), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(I_Fit_pop_sim, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
text(x = (med_IF_sim + 5), y = 0, med_IF_sim)
text(x = (med_IM_sim - 5), y = 0, med_IM_sim)
mtext("Simulations",  cex = 1.2, side = 2, line = 5, font = 2)
mtext("i", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.24)

#Initial - individual-level
par(mar = c(3,2.5,3,2))

plot(NULL, xlim = c(1,50), ylim = c(1,12.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3,5,7,9,11), labels = c("+", "+","+","+","+", "+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4,6,8,10,12), labels = c("-", "-","-","-","-","-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50), labels = c("Trial 1","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(50), labels = c("50"), cex.axis = 1.3)
mtext("Choice",  cex = 1.2, side = 2, line = 3)
for(id in min(female_ind_sim_init$id):max(female_ind_sim_init$id)) lines(x = female_ind_sim_init$trialafter[which(female_ind_sim_init$id == id)], y = female_ind_sim_init$Choice[which(female_ind_sim_init$id == id)], col = alpha("#fde725", 0.8), lwd = 1)
for(id in min(male_ind_sim_init$id):max(male_ind_sim_init$id)) lines(x = male_ind_sim_init$trialafter[which(male_ind_sim_init$id == id)], y = male_ind_sim_init$Choice[which(male_ind_sim_init$id == id)], col = alpha("#5ec962", 0.8), lwd = 1)
abline(h = 2.5, lty = 2)
abline(h = 6.5, lty = 2)
abline(h = 10.5, lty = 2)
abline(h = 4.5, lty = 1)
abline(h = 8.5, lty = 1)
mtext("Typical 'birds'", side = 3, at = 25, cex = 1.2, line = 1)
text(x = 39.5, y = 12.5, labels = "Switches", cex = 1)
text(x = 47.5, y = 1.5, labels = switch_init[,1], cex = 1)
text(x = 47.5, y = 3.5, labels = switch_init[,4], cex = 1)
text(x = 47.5, y = 5.5, labels = switch_init[,2], cex = 1)
text(x = 47.5, y = 7.5, labels = switch_init[,5], cex = 1)
text(x = 47.5, y = 9.5, labels = switch_init[,3], cex = 1)
text(x = 47.5, y = 11.5, labels = switch_init[,6], cex = 1)
mtext("j", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.5)

#Reversal - population-level
par(mar = c(3,5,3,2.5))

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(200), labels = c("200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
mtext("Ten thousand 'birds'", side = 3, at = 100, cex = 1.2, line = 1)
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 3)
abline(v = c(62,79), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(R_Fit_pop_sim, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
text(x = (med_RF_sim + 10), y = 0, med_RF_sim)
text(x = (med_RM_sim - 10), y = 0, med_RM_sim)
mtext("k", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.25)

#Reversal - individual-level
par(mar = c(3,2.5,3,2))

plot(NULL, xlim = c(1,100), ylim = c(1,12.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3,5,7,9,11), labels = c("-", "-","-","-","-","-"), cex.axis = 1.3)
axis(side = 2, at = c(2,4,6,8,10,12), labels = c("+", "+","+","+","+", "+"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.3)
mtext("Choice",  cex = 1.2, side = 2, line = 3)
for(id in min(female_ind_sim_rev$id):max(female_ind_sim_rev$id)) lines(x = female_ind_sim_rev$trialafter[which(female_ind_sim_rev$id == id)], y = female_ind_sim_rev$Choice[which(female_ind_sim_rev$id == id)], col = alpha("#fde725", 0.8), lwd = 1)
for(id in min(male_ind_sim_rev$id):max(male_ind_sim_rev$id)) lines(x = male_ind_sim_rev$trialafter[which(male_ind_sim_rev$id == id)], y = male_ind_sim_rev$Choice[which(male_ind_sim_rev$id == id)], col = alpha("#5ec962", 0.8), lwd = 1)
abline(h = 2.5, lty = 2)
abline(h = 6.5, lty = 2)
abline(h = 10.5, lty = 2)
abline(h = 4.5, lty = 1)
abline(h = 8.5, lty = 1)
mtext("Typical 'birds'", side = 3, at = 50, cex = 1.2, line = 1)
text(x = 78, y = 12.5, labels = "Switches", cex = 1)
text(x = 95, y = 1.5, labels = switch_rev[,1], cex = 1)
text(x = 95, y = 3.5, labels = switch_rev[,4], cex = 1)
text(x = 95, y = 5.5, labels = switch_rev[,2], cex = 1)
text(x = 95, y = 7.5, labels = switch_rev[,5], cex = 1)
text(x = 95, y = 9.5, labels = switch_rev[,3], cex = 1)
text(x = 95, y = 11.5, labels = switch_rev[,6], cex = 1)
mtext("l", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.5)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males (M)", "Females (F)", "Males-Females (M-F)"), fill = alpha(c("#5ec962","#fde725", "black"), 0.8), text.width = .23, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf of plot

#End script
#####################################################################################################################################################