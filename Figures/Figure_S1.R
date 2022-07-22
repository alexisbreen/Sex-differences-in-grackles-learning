#####################################################################################################################################################

#Code for plotting Figure S1 for the manuscript

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script
library(tidyverse)
library(survival)

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#####################################################################################################################################################
#Pre-graph processing
#####################################################################################################################################################

#First, need to ready data for survival plots by...
#Subsetting data by population & phase & skip-birds & calculate max trials per individual & assign censored column for survival plots
I_Surv_AZ <- d %>% filter(sex_phase == 1 | sex_phase == 3) %>% filter(Population == 1) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))
R_Surv_AZ <- d %>% filter(sex_phase == 2 | sex_phase == 4) %>% filter(Population == 1 & skip == 0) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))

I_Surv_SB <- d %>% filter(sex_phase == 1 | sex_phase == 3) %>% filter(Population == 2) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))
R_Surv_SB <- d %>% filter(sex_phase == 2 | sex_phase == 4) %>% filter(Population == 2 & skip == 0) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1)) 

I_Surv_WL <- d %>% filter(sex_phase == 1 | sex_phase == 3) %>% filter(Population == 3) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))
R_Surv_WL <- d %>% filter(sex_phase == 2 | sex_phase == 4) %>% filter(Population == 3) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))

#And fitting survival trajectories
#To do this need library(survival)
I_Fit_AZ <- survfit(Surv(Trial, cens) ~ sex, data = I_Surv_AZ)
R_Fit_AZ <- survfit(Surv(Trial, cens) ~ sex, data = R_Surv_AZ)

I_Fit_SB <- survfit(Surv(Trial, cens) ~ sex, data = I_Surv_SB)
R_Fit_SB <- survfit(Surv(Trial, cens) ~ sex, data = R_Surv_SB)

I_Fit_WL <- survfit(Surv(Trial, cens) ~ sex, data = I_Surv_WL)
R_Fit_WL <- survfit(Surv(Trial, cens) ~ sex, data = R_Surv_WL)

#For obtaining survival probability means and standard errors reported in plot
summary(I_Fit_AZ)$table
summary(R_Fit_AZ)$table

summary(I_Fit_SB)$table
summary(R_Fit_SB)$table

summary(I_Fit_WL)$table
summary(R_Fit_WL)$table

#####################################################################################################################################################
#Figure S1
#####################################################################################################################################################

#pdf(file = "Figure_S1.pdf", height = 8, width = 8) #Turn on if want pdf

#Plot set-up
par(mfrow = c(3,2), mar = c(3,5,3,1), oma = c(0,1,3.5,0))

#
##
###FIRST ROW
##
#

plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
axis(side = 2, at = c(0.5), cex.axis = 1.3)
abline(v = c(44,44), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(I_Fit_AZ, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Initial learning", side = 3, adj = .5, cex = 1.2, line = 2.5, font = 2)
mtext("Core", side = 3, adj = .5, cex = 1.2, line = 0.5)

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
axis(side = 2, at = c(0.5), cex.axis = 1.3)
abline(v = c(71,86), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(R_Fit_AZ, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Reversal learning", side = 3, adj = .5, cex = 1.2, line = 2.5, font = 2)
mtext("Core", side = 3, adj = .5, cex = 1.2, line = 0.5)

#
##
###SECOND ROW
##
#

plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
axis(side = 2, at = c(0.5), cex.axis = 1.3)
abline(v = c(30,33), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(I_Fit_SB, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Middle", side = 3, adj = .5, cex = 1.2, line = 0.5)

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
axis(side = 2, at = c(0.5), cex.axis = 1.3)
abline(v = c(83,98), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(R_Fit_SB, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Middle", side = 3, adj = .5, cex = 1.2, line = 0.5)

#
##
###THIRD ROW
##
#

plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
axis(side = 2, at = c(0.5), cex.axis = 1.3)
abline(v = c(38,43), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(I_Fit_WL, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Edge", side = 3, adj = .5, cex = 1.2, line = 0.5)

plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
axis(side = 2, at = c(0.5), cex.axis = 1.3)
abline(v = c(70,78), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(R_Fit_WL, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Edge", side = 3, adj = .5, cex = 1.2, line = 0.5)

#Legend

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males", "Females"), fill = c("#5ec962","#fde725"), text.width = .13, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf

#End script
#####################################################################################################################################################
