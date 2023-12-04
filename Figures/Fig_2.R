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
#Note: Poisson_Execution.R script has to be run before executing below!
#NOTE: Panels M and P will vary by simulation - play around and see for yourself!

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

#For obtaining survival probability means to plot
mu_IF <- round(summary(I_Fit)$table[,'rmean'][1]) #36
mu_IM <- round(summary(I_Fit)$table[,'rmean'][2]) #35
mu_RF <- round(summary(R_Fit)$table[,'rmean'][1]) #81
mu_RM <- round(summary(R_Fit)$table[,'rmean'][2]) #67

#Filter out extra learning trials
d2 <- d %>% filter(Criterion != 2)

#Empty vectors
switch_initial  <- rep(NA, length(unique(d2$id)))
switch_reversal <- rep(NA, length(unique(d2$id)))
sex <-  rep(NA, length(unique(d2$id)))
bird <- rep(NA, length(unique(d2$id)))
skip_initial <- rep(NA, length(unique(d2$id)))
skip_reversal <- rep(NA, length(unique(d2$id)))

#Fill empty vectors
for (id in 1:max(d2$id)) {
  switch_initial[id] <- sum(d2$switch[which(d2$id == id & d2$Phase == 1)])
  switch_reversal[id] <- sum(d2$switch[which(d2$id == id & d2$Phase == 2)])
  sex[id] <- unique(d2$sex[d2$id ==id])
  bird[id] <- unique(d2$id[d2$id == id])
  skip_initial[id] <- max(d2$skip[which(d2$id == id & d2$Phase == 1)])
  skip_reversal[id] <- max(d2$skip[which(d2$id == id & d2$Phase == 2)])
}

#Put vectors into data frame
d_switch <- as.data.frame(
  list(
    bird = c(bird, bird),
    switches = c(switch_initial,switch_reversal), 
    sex = c(sex, sex),
    phase = as.integer(c(rep(1,49),rep(2,49))),
    skip = c(skip_initial,skip_reversal))
)

#Phase and sex specific data frames
Switch_IF <- d_switch %>% group_by(bird) %>% filter(phase == 1 & sex == 1) %>% mutate(sex = ifelse(sex == 2, 1, 2))
Switch_IM <- d_switch %>% group_by(bird) %>% filter(phase == 1 & sex == 2) %>% mutate(sex = ifelse(sex == 2, 1, 2))
Switch_RF <- d_switch %>% group_by(bird) %>% filter(phase == 2 & sex == 1) %>% mutate(sex = ifelse(sex == 2, 1, 2))
Switch_RM <- d_switch %>% group_by(bird) %>% filter(phase == 2 & sex == 2 & skip == 0) %>% mutate(sex = ifelse(sex == 2, 1, 2))

#Calculate means
Switch_IF_mu <- mean(Switch_IF$switches)
Switch_IM_mu <- mean(Switch_IM$switches)
Switch_RF_mu <- mean(Switch_RF$switches)
Switch_RM_mu <- mean(Switch_RM$switches)

#Combine means 
Switch_mu <- round(rbind(Switch_IM_mu, Switch_IF_mu, Switch_RM_mu, Switch_RF_mu))

#Also, need to define function to execute a stacked density plot in base r, based on: https://stackoverflow.com/questions/25328533/overlapping-stacked-density-plots
stacked.density <- function(data, fac = 3, xlim, col = 'black', 
                            left = -200, bottom = 1, right = 200, top = 2,
                            alpha = 0.4, show.xaxis = T, height = 2,
                            xlab = '', ylab = ''){
  
  xvals = unlist(lapply(data, function(d) d$x))
  if(missing(xlim)) xlim=c((min(xvals)-5), max(xvals))
  
  col = sapply(col, alpha)
  if(length(col) == 1) col = rep(col, length(data))
  
  plot(1, type = "n", xlim = xlim, ylim = c(1,length(data) + height),
       yaxt='n', xaxt=ifelse(show.xaxis, 'l', 'n'), xlab = xlab, ylab = ylab)
  
  z = length(data):1
  for(i in 1:length(data)){
    d = data[[ z[i] ]]
    lines(d$x, fac*d$y + i, col = col[i], lwd = 1)
    polygon(d$x, fac*d$y+ i, col = alpha(col[i], alpha = 0.8), border = NA)
    abline(h = i, lwd = 1)
    abline(v = 0, lty = 5, col = "black")
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

#For obtaining survival probability means to plot - note these can vary slightly due to simulation stochasticity
mu_IF_sim <- round(summary(I_Fit_pop_sim)$table[,'rmean'][1]) #35
mu_IM_sim <- round(summary(I_Fit_pop_sim)$table[,'rmean'][2]) #38
mu_RF_sim <- round(summary(R_Fit_pop_sim)$table[,'rmean'][1]) #91
mu_RM_sim <- round(summary(R_Fit_pop_sim)$table[,'rmean'][2]) #71

#Empty vectors
switch_initial_sim  <- rep(NA, length(unique(dat_pop_sim$id)))
switch_reversal_sim <- rep(NA, length(unique(dat_pop_sim$id)))
sex_sim <-  rep(NA, length(unique(dat_pop_sim$id)))
bird_sim <- rep(NA, length(unique(dat_pop_sim$id)))
skip_initial_sim <- rep(NA, length(unique(dat_pop_sim$id)))
skip_reversal_sim <- rep(NA, length(unique(dat_pop_sim$id)))

#Fill empty vectors
for (id in 1:max(dat_pop_sim$id)) {
  switch_initial_sim[id] <- sum(dat_pop_sim$switch[which(dat_pop_sim$id == id & dat_pop_sim$Phase == 1)])
  switch_reversal_sim[id] <- sum(dat_pop_sim$switch[which(dat_pop_sim$id == id & dat_pop_sim$Phase == 2)])
  sex_sim[id] <- unique(dat_pop_sim$sex[dat_pop_sim$id ==id])
  bird_sim[id] <- unique(dat_pop_sim$id[dat_pop_sim$id == id])
  skip_initial_sim[id] <- max(dat_pop_sim$skip[which(dat_pop_sim$id == id & dat_pop_sim$Phase == 1)])
  skip_reversal_sim[id] <- max(dat_pop_sim$skip[which(dat_pop_sim$id == id & dat_pop_sim$Phase == 2)])
}

#Put vectors into data frame
d_switch_sim <- as.data.frame(
  list(
    bird = c(bird_sim, bird_sim),
    switches = c(switch_initial_sim,switch_reversal_sim), 
    sex = c(sex_sim, sex_sim),
    phase = as.integer(c(rep(1,max(dat_pop_sim$id)),rep(2,max(dat_pop_sim$id)))),
    skip = c(skip_initial_sim,skip_reversal_sim))
)

#Phase and sex specific data frames
Switch_IF_sim <- d_switch_sim %>% group_by(bird) %>% filter(phase == 1 & sex == 1) %>% mutate(sex = ifelse(sex == 2, 1, 2))
Switch_IM_sim <- d_switch_sim %>% group_by(bird) %>% filter(phase == 1 & sex == 2) %>% mutate(sex = ifelse(sex == 2, 1, 2))
Switch_RF_sim <- d_switch_sim %>% group_by(bird) %>% filter(phase == 2 & sex == 1) %>% mutate(sex = ifelse(sex == 2, 1, 2))
Switch_RM_sim <- d_switch_sim %>% group_by(bird) %>% filter(phase == 2 & sex == 2) %>% mutate(sex = ifelse(sex == 2, 1, 2))

#Calculate means
Switch_IF_mu_sim <- mean(Switch_IF_sim$switches)
Switch_IM_mu_sim <- mean(Switch_IM_sim$switches)
Switch_RF_mu_sim <- mean(Switch_RF_sim$switches)
Switch_RM_mu_sim <- mean(Switch_RM_sim$switches)

#Combine means 
Switch_mu_sim <- round(rbind(Switch_IM_mu_sim, Switch_IF_mu_sim, Switch_RM_mu_sim, Switch_RF_mu_sim))

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
par(mfrow = c(4,8), mar = c(3,5,3,2), oma = c(3,2,5,0))
layout(matrix(c(1,1,2,3,4,4,5,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,12,13,14,14,15,16),nrow=4,ncol=8,byrow=TRUE))

#Panel A - behavioural data
par(mar = c(3,5,3,1.5))

#Initial 
plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
abline(v = c(mu_IM,mu_IF), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8), lwd = 1.5)
lines(I_Fit, fun = "event", conf.int = FALSE, lty = 1, lwd = 1.5, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 2.5)
mtext("Speed", side = 3, at = 50, cex = 1.2, line = 1)
text(x = (mu_IF + 6), y = 0, mu_IF)
text(x = (mu_IM - 6), y = 0, mu_IM)
mtext("Behaviour",  cex = 1.2, side = 2, line = 4.5, font = 2)
mtext("A", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.35)

#Initial learning switches
par(mar = c(3,3,3,2))

plot(NULL, xlim = c(0.5,2.5), ylim = c(0,50), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4), labels = c("M", "F","M", "F"), cex.axis = 1.3)
axis(side = 2, at = c(0,10,20,30,40,50), labels = c("0","10","20","30","40","50"), cex.axis = 1.3)
mtext("Count",  cex = 1.2, side = 2, line = 2.5)
mtext("Switches", side = 3, at = 1.05, cex = 1.2, line = 1)
mtext("Initial learning", side = 3, cex = 1.2, line = 3.5, font = 2, at = 0)
points(jitter(Switch_IM$sex, 8), Switch_IM$switches, col = alpha(c("#5ec962"),0.6), cex = 1)
points(jitter(Switch_IF$sex, 5), Switch_IF$switches, col = alpha(c("#fde725"),0.6), cex = 1)
mtext("B", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -1.2)
text(x = 1, y = Switch_mu[1], labels = "-14-", cex = 1)
text(x = 2, y = Switch_mu[2], labels = "-14-", cex = 1)

#Posterior
par(mar = c(3,1,3,2))

data_init <- list(
  
  across_speed = density(s_speed_xpop$init_X),
  across_switch = density(s_switch_xpop$init_X)
  
)

stacked.density(data_init, fac = 4, height = .95, col=c("black"), alpha = 0.2, show.xaxis = F)

axis(side = 1, at = c(0), labels = c("0"), cex.axis = 1.3)
text(x = -15, y = 2.9, labels = "Speed", cex = 1)
text(x = -12, y = 1.9, labels = "Switches", cex = 1)
mtext("M-F", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Posterior density",  cex = 1.2, side = 2, line = 1)
mtext("C", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.4)

#Reversal
par(mar = c(3,5,3,1.5))

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(200), labels = c("200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
abline(v = c(mu_RM,mu_RF), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8),lwd = 1.5)
lines(R_Fit, fun = "event", conf.int = FALSE, lty = 1, lwd = 1.5, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 2.5)
mtext("Speed", side = 3, at = 100, cex = 1.2, line = 1)
text(x = (mu_RF + 12), y = 0, mu_RF)
text(x = (mu_RM - 12), y = 0, mu_RM)
mtext("D", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.35)


#Reversal learning switches
par(mar = c(3,3,3,2))

plot(NULL, xlim = c(0.5,2.5), ylim = c(0,100), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4), labels = c("M", "F","M", "F"), cex.axis = 1.3)
axis(side = 2, at = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100"), cex.axis = 1.3)
mtext("Count",  cex = 1.2, side = 2, line = 2.5)
mtext("Switches", side = 3, at = 1.05, cex = 1.2, line = 1)
mtext("Reversal learning", side = 3, cex = 1.2, line = 3.5, font = 2, at = 0)
points(jitter(Switch_RM$sex, 8), Switch_RM$switches, col = alpha(c("#5ec962"),0.6), cex = 1)
points(jitter(Switch_RF$sex, 5), Switch_RF$switches, col = alpha(c("#fde725"),0.6), cex = 1)
text(x = 1, y = Switch_mu[3], labels = "-25-", cex = 1)
text(x = 2, y = Switch_mu[4], labels = "-36-", cex = 1)
mtext("E", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -1.2)

#Posterior
par(mar = c(3,1,3,2))

data_rev <- list(
  
  across_speed = density(s_speed_xpop$rev_X),
  across_switch = density(s_switch_xpop$rev_X)
  
  
)

stacked.density(data_rev, fac = 4, height = .95, col=c("black"), alpha=0.2, show.xaxis = F,
                left = -100, bottom = 1, right = 150, top = 2,)
axis(side = 1, at = c(0), labels = c("0"), cex.axis = 1.3)
text(x = -37.5, y = 2.9, labels = "Speed", cex = 1)
text(x = -33, y = 1.9, labels = "Switches", cex = 1)
mtext("M-F", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Posterior density",  cex = 1.2, side = 2, line = 1)
mtext("F", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.4)

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
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F"), cex.axis = 1.3)
axis(side = 1, at = c(10), labels = c("M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.3,-0.2,-0.1,0,0.1,0.2), labels = c("-0.3","","","0","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.3), labels = c("0.3"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 2.25)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_init_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.4)) 
arrows(x0 = x, y0 = HPDI_phi_init[1,], x1 = x, y1 = HPDI_phi_init[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_phi_init, col = "red", pch = 19)
mtext("G", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.13)

#Reversal
s_phi_rev_plot <- s_phi_rev[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_phi_rev <- apply(s_phi_rev_plot, 2, mean)
HPDI_phi_rev <- apply(s_phi_rev_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.3,.3), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab= NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F"), cex.axis = 1.3)
axis(side = 1, at = c(10), labels = c("M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.3,-0.2,-0.1,0,0.1,0.2), labels = c("-0.3","","","0","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.3), labels = c("0.3"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 2.25)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_rev_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.4)) 
arrows(x0 = x, y0 = HPDI_phi_rev[1,], x1 = x, y1 = HPDI_phi_rev[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_phi_rev, col = "red", pch = 19)
mtext("H", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.13)

#Panel C - risk-sensitivity

#Lambda

#Initial
s_L_init_plot <- s_L_init[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_L_init <- apply(s_L_init_plot, 2, mean)
HPDI_L_init <- apply(s_L_init_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-11,11), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F"), cex.axis = 1.3)
axis(side = 1, at = c(10), labels = c("M-F"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 2.25)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1, col = "black")
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_init_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.4)) 
arrows(x0 = x, y0 = HPDI_L_init[1,], x1 = x, y1 = HPDI_L_init[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_L_init, col = "red", pch = 19)
mtext("Mechanisms",  cex = 1.2, side = 2, line = 4.5, font = 2, adj = 2.8)
mtext("I", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.12)

#Reversal
s_L_rev_plot <- s_L_rev[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_L_rev <- apply(s_L_rev_plot, 2, mean)
HPDI_L_rev <- apply(s_L_rev_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-11,11), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F"), cex.axis = 1.3)
axis(side = 1, at = c(10), labels = c("M-F"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 2.25)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_rev_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.4)) 
arrows(x0 = x, y0 = HPDI_L_rev[1,], x1 = x, y1 = HPDI_L_rev[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_L_rev, col = "red", pch = 19)
mtext("J", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.13)

#Panel D - forward simulations

#Initial - population-level
par(mar = c(3,5,3,1.5))

plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
mtext("Speed", side = 3, at = 50, cex = 1.2, line = 1)
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 2.5)
abline(v = c(mu_IM_sim,mu_IF_sim), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8), lwd = 1.5)
lines(I_Fit_pop_sim, fun = "event", conf.int = FALSE, lty = 1, lwd = 1.5, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
text(x = (mu_IF_sim + 6), y = 0, mu_IF_sim)
text(x = (mu_IM_sim - 6), y = 0, mu_IM_sim)
mtext("Simulations",  cex = 1.2, side = 2, line = 4.5, font = 2)
mtext("K", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.35)


#Reversal learning switches
par(mar = c(3,3,3,2))

plot(NULL, xlim = c(0.5,2.5), ylim = c(0,50), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4), labels = c("M", "F","M", "F"), cex.axis = 1.3)
axis(side = 2, at = c(0,10,20,30,40,50), labels = c("0","10","20","30","40","50"), cex.axis = 1.3)
mtext("Count",  cex = 1.2, side = 2, line = 2.5)
mtext("Switches", side = 3, at = 1.05, cex = 1.2, line = 1)
for(i in 1:100)points(jitter(Switch_IM_sim$sex[i], 8), Switch_IM_sim$switches[i], col = alpha(c("#5ec962"),0.6), cex = 1)
for(i in 1:100)points(jitter(Switch_IF_sim$sex[i], 5), Switch_IF_sim$switches[i], col = alpha(c("#fde725"),0.6), cex = 1)
mtext("L", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -1.1)
text(x = 1, y = Switch_mu_sim[1], labels = "-13-", cex = 1)
text(x = 2, y = Switch_mu_sim[2], labels = "-14-", cex = 1)

#Initial - individual-level
par(mar = c(3,1,3,2))

plot(NULL, xlim = c(1,50), ylim = c(1,12.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3,5,7,9,11), labels = c("+", "+","+","+","+", "+"), cex.axis = 1.3, tick = FALSE, line = -1)
axis(side = 2, at = c(2,4,6,8,10,12), labels = c("-", "-","-","-","-","-"), cex.axis = 1.3, tick = FALSE, line = -1)
axis(side = 1, at = c(1,10,20,30,40,50), labels = c("Trial 1","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(50), labels = c("50"), cex.axis = 1.3)
mtext("Choice",  cex = 1.2, side = 2, line = 1)
for(id in min(female_ind_sim_init$id):max(female_ind_sim_init$id)) lines(x = female_ind_sim_init$trialafter[which(female_ind_sim_init$id == id)], y = female_ind_sim_init$Choice[which(female_ind_sim_init$id == id)], col = alpha("#fde725", 0.8), lwd = 1)
for(id in min(male_ind_sim_init$id):max(male_ind_sim_init$id)) lines(x = male_ind_sim_init$trialafter[which(male_ind_sim_init$id == id)], y = male_ind_sim_init$Choice[which(male_ind_sim_init$id == id)], col = alpha("#5ec962", 0.8), lwd = 1)
abline(h = 2.5, lty = 2)
abline(h = 6.5, lty = 2)
abline(h = 10.5, lty = 2)
abline(h = 4.5, lty = 1)
abline(h = 8.5, lty = 1)
mtext("Av. 'birds'", side = 3, at = 25, cex = 1.2, line = 1)
text(x = 35.5, y = 12.5, labels = "Switches", cex = 1)
text(x = 45.5, y = 1.5, labels = switch_init[,1], cex = 1)
text(x = 45.5, y = 3.5, labels = switch_init[,4], cex = 1)
text(x = 45.5, y = 5.5, labels = switch_init[,2], cex = 1)
text(x = 45.5, y = 7.5, labels = switch_init[,5], cex = 1)
text(x = 45.5, y = 9.5, labels = switch_init[,3], cex = 1)
text(x = 45.5, y = 11.5, labels = switch_init[,6], cex = 1)
mtext("M", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.45)

#Reversal - population-level
par(mar = c(3,5,3,1.5))

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180), labels = c("Trial 1","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(200), labels = c("200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
mtext("Speed", side = 3, at = 100, cex = 1.2, line = 1)
mtext("Cum. prop. finish",  cex = 1.2, side = 2, line = 2.5)
abline(v = c(mu_RM_sim,mu_RF_sim), lty = 5, col = alpha(c("#5ec962","#fde725"), 0.8), lwd = 1.5)
lines(R_Fit_pop_sim, fun = "event", conf.int = FALSE, lty = 1, lwd = 1.5, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
text(x = (mu_RF_sim + 12), y = 0, mu_RF_sim)
text(x = (mu_RM_sim - 12), y = 0, mu_RM_sim)
mtext("N", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.35)


#Reversal learning switches
par(mar = c(3,3,3,2))

plot(NULL, xlim = c(0.5,2.5), ylim = c(0,100), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4), labels = c("M", "F","M", "F"), cex.axis = 1.3)
axis(side = 2, at = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100"), cex.axis = 1.3)
mtext("Count",  cex = 1.2, side = 2, line = 2.5)
mtext("Switches", side = 3, at = 1.05, cex = 1.2, line = 1)
for(i in 1:100)points(jitter(Switch_RM_sim$sex[i], 8), Switch_RM_sim$switches[i], col = alpha(c("#5ec962"), 0.6), cex = 1)
for(i in 1:100)points(jitter(Switch_RF_sim$sex[i], 5), Switch_RF_sim$switches[i], col = alpha(c("#fde725"), 0.6), cex = 1)
mtext("O", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -1.2)
text(x = 1, y = Switch_mu_sim[3], labels = "-23-", cex = 1)
text(x = 2, y = Switch_mu_sim[4], labels = "-35-", cex = 1)


#Reversal - individual-level
par(mar = c(3,1,3,2))

plot(NULL, xlim = c(1,100), ylim = c(1,12.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3,5,7,9,11), labels = c("-", "-","-","-","-","-"), cex.axis = 1.3, tick = FALSE, line = -1)
axis(side = 2, at = c(2,4,6,8,10,12), labels = c("+", "+","+","+","+", "+"), cex.axis = 1.3, tick = FALSE, line = -1)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","","","","","","","","",""), cex.axis = 1.3)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.3)
mtext("Choice",  cex = 1.2, side = 2, line = 1)
for(id in min(female_ind_sim_rev$id):max(female_ind_sim_rev$id)) lines(x = female_ind_sim_rev$trialafter[which(female_ind_sim_rev$id == id)], y = female_ind_sim_rev$Choice[which(female_ind_sim_rev$id == id)], col = alpha("#fde725", 0.8), lwd = 1)
for(id in min(male_ind_sim_rev$id):max(male_ind_sim_rev$id)) lines(x = male_ind_sim_rev$trialafter[which(male_ind_sim_rev$id == id)], y = male_ind_sim_rev$Choice[which(male_ind_sim_rev$id == id)], col = alpha("#5ec962", 0.8), lwd = 1)
abline(h = 2.5, lty = 2)
abline(h = 6.5, lty = 2)
abline(h = 10.5, lty = 2)
abline(h = 4.5, lty = 1)
abline(h = 8.5, lty = 1)
mtext("Av. 'birds'", side = 3, at = 50, cex = 1.2, line = 1)
text(x = 71, y = 12.5, labels = "Switches", cex = 1)
text(x = 91, y = 1.5, labels = switch_rev[,1], cex = 1)
text(x = 91, y = 3.5, labels = switch_rev[,4], cex = 1)
text(x = 91, y = 5.5, labels = switch_rev[,2], cex = 1)
text(x = 91, y = 7.5, labels = switch_rev[,5], cex = 1)
text(x = 91, y = 9.5, labels = switch_rev[,3], cex = 1)
text(x = 91, y = 11.5, labels = switch_rev[,6], cex = 1)
mtext("P", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.4)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males (M)", "Females (F)", "Males-Females (M-F)"), fill = alpha(c("#5ec962","#fde725", "black"), 0.8), text.width = .23, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf of plot

#End script
#####################################################################################################################################################