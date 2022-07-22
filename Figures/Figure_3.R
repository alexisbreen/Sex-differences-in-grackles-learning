#####################################################################################################################################################

#Code for plotting Figure 3 for the manuscript

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script
library(tidyverse)

#Note: it is not possible to replicate Figure 3 - which is a hand-picked representation of the overall dynamics - as simulations vary by 'go'

#pdf(file = "Figure_3.pdf", height = 10, width = 10) #Turn on to plot pdf

#
##
###FIRST ROW
##
#

#Plot set-up
par(mfrow=c(6,2), mar=c(4,5,0,1), oma = c(0,0,8,0))

#Simulate one male and one female - note the Post_Study_Simulation_Six_Birds.R file must have been run to use the below function
dat1 <- Post_Study_Sim_Pair_Fxn() 

#Wrangle simulated data
male_sim_init1 <- dat1 %>% filter(sex == 2 & Phase == 1)
male_sim_init1$Choice2 <- ifelse(male_sim_init1$Choice == 1, 3, 4)
male_phi_init1 <- format(round(unique(male_sim_init1$phi_init), digits = 2), nsmall = 2)
male_lambda_init1 <- format(round(unique(male_sim_init1$lambda_init), digits = 2), nsmall = 2)

male_sim_rev1 <- dat1 %>% filter(sex == 2 & Phase == 2)
male_sim_rev1$Choice2 <- ifelse(male_sim_rev1$Choice == 1, 3, 4)
male_phi_rev1 <- format(round(unique(male_sim_rev1$phi_rev), digits = 2), nsmall = 2)
male_lambda_rev1 <- format(round(unique(male_sim_rev1$lambda_rev), digits = 2), nsmall = 2)

female_sim_init1 <- dat1 %>% filter(sex == 1 & Phase == 1)
female_phi_init1 <- format(round(unique(female_sim_init1$phi_init), digits = 2), nsmall = 2)
female_lambda_init1 <- format(round(unique(female_sim_init1$lambda_init), digits = 2), nsmall = 2)

female_sim_rev1 <- dat1 %>% filter(sex == 1 & Phase == 2)
female_phi_rev1 <- format(round(unique(female_sim_rev1$phi_rev), digits = 2), nsmall = 2)
female_lambda_rev1 <- format(round(unique(female_sim_rev1$lambda_rev), digits = 2), nsmall = 2)

#Plot initial learning
plot(NULL, xlim = c(1,100), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
lines(x = male_sim_init1$trialafter, y = male_sim_init1$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_init1$trialafter, y = female_sim_init1$Choice , col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 90, y = 3.8, labels = bquote(phi == .(male_phi_init1)), cex = 1.3)
text(x = 90, y = 3.2, labels = bquote(lambda == .(male_lambda_init1)), cex = 1.3)
text(x = 90, y = 1.8, labels = bquote(phi == .(female_phi_init1)), cex = 1.3)
text(x = 90, y = 1.2, labels = bquote(lambda == .(female_lambda_init1)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)
mtext('Initial learning', side = 3, line = 1, adj = 0.5, font = 2, cex = 1.3)

#Plot reversal learning
plot(NULL, xlim = c(1,200), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(2,4), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(1,3), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
lines(x = male_sim_rev1$trialafter, y = male_sim_rev1$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_rev1$trialafter, y = female_sim_rev1$Choice, col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 180, y = 3.8, labels = bquote(phi == .(male_phi_rev1)), cex = 1.3)
text(x = 180, y = 3.2, labels = bquote(lambda == .(male_lambda_rev1)), cex = 1.3)
text(x = 180, y = 1.8, labels = bquote(phi == .(female_phi_rev1)), cex = 1.3)
text(x = 180, y = 1.2, labels = bquote(lambda == .(female_lambda_rev1)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)
mtext('Reversal learning', side = 3, line = 1, adj = 0.5, font = 2, cex = 1.3)

#
##
###SECOND ROW
##
#

#Plot set-up
par(mar=c(4,5,0,1))

#Simulate one male and one female - note the Post_Study_Simulation_Six_Birds.R file must have been run to use the below function
dat2 <- Post_Study_Sim_Pair_Fxn()

#Wrangle simulated data
male_sim_init2 <- dat2 %>% filter(sex == 2 & Phase == 1)
male_sim_init2$Choice2 <- ifelse(male_sim_init2$Choice == 1, 3, 4)
male_phi_init2 <- format(round(unique(male_sim_init2$phi_init), digits = 2), nsmall = 2)
male_lambda_init2 <- format(round(unique(male_sim_init2$lambda_init), digits = 2), nsmall = 2)

male_sim_rev2 <- dat2 %>% filter(sex == 2 & Phase == 2)
male_sim_rev2$Choice2 <- ifelse(male_sim_rev2$Choice == 1, 3, 4)
male_phi_rev2 <- format(round(unique(male_sim_rev2$phi_rev), digits = 2), nsmall = 2)
male_lambda_rev2 <- format(round(unique(male_sim_rev2$lambda_rev), digits = 2), nsmall = 2)

female_sim_init2 <- dat2 %>% filter(sex == 1 & Phase == 1)
female_phi_init2 <- format(round(unique(female_sim_init2$phi_init), digits = 2), nsmall = 2)
female_lambda_init2 <- format(round(unique(female_sim_init2$lambda_init), digits = 2), nsmall = 2)

female_sim_rev2 <- dat2 %>% filter(sex == 1 & Phase == 2)
female_phi_rev2 <- format(round(unique(female_sim_rev2$phi_rev), digits = 2), nsmall = 2)
female_lambda_rev2 <- format(round(unique(female_sim_rev2$lambda_rev), digits = 2), nsmall = 2)

#Plot initial learning
plot(NULL, xlim = c(1,100), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
lines(x = male_sim_init2$trialafter, y = male_sim_init2$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_init2$trialafter, y = female_sim_init2$Choice , col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 90, y = 3.8, labels = bquote(phi == .(male_phi_init2)), cex = 1.3)
text(x = 90, y = 3.2, labels = bquote(lambda == .(male_lambda_init2)), cex = 1.3)
text(x = 90, y = 1.8, labels = bquote(phi == .(female_phi_init2)), cex = 1.3)
text(x = 90, y = 1.2, labels = bquote(lambda == .(female_lambda_init2)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#Plot reversal learning
plot(NULL, xlim = c(1,200), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(2,4), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(1,3), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(0,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
lines(x = male_sim_rev2$trialafter, y = male_sim_rev2$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_rev2$trialafter, y = female_sim_rev2$Choice, col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 180, y = 3.8, labels = bquote(phi == .(male_phi_rev2)), cex = 1.3)
text(x = 180, y = 3.2, labels = bquote(lambda == .(male_lambda_rev2)), cex = 1.3)
text(x = 180, y = 1.8, labels = bquote(phi == .(female_phi_rev2)), cex = 1.3)
text(x = 180, y = 1.2, labels = bquote(lambda == .(female_lambda_rev2)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#
##
###THIRD ROW
##
#

#Simulate one male and one female - note the Post_Study_Simulation_Six_Birds.R file must have been run to use the below function
dat3 <- Post_Study_Sim_Pair_Fxn() 

#Wrangle simulated data
male_sim_init3 <- dat3 %>% filter(sex == 2 & Phase == 1)
male_sim_init3$Choice2 <- ifelse(male_sim_init3$Choice == 1, 3, 4)
male_phi_init3 <- format(round(unique(male_sim_init3$phi_init), digits = 2), nsmall = 2)
male_lambda_init3 <- format(round(unique(male_sim_init3$lambda_init), digits = 2), nsmall = 2)

male_sim_rev3 <- dat3 %>% filter(sex == 2 & Phase == 2)
male_sim_rev3$Choice2 <- ifelse(male_sim_rev3$Choice == 1, 3, 4)
male_phi_rev3 <- format(round(unique(male_sim_rev3$phi_rev), digits = 2), nsmall = 2)
male_lambda_rev3 <- format(round(unique(male_sim_rev3$lambda_rev), digits = 2), nsmall = 2)

female_sim_init3 <- dat3 %>% filter(sex == 1 & Phase == 1)
female_phi_init3 <- format(round(unique(female_sim_init3$phi_init), digits = 2), nsmall = 2)
female_lambda_init3 <- format(round(unique(female_sim_init3$lambda_init), digits = 2), nsmall = 2)

female_sim_rev3 <- dat3 %>% filter(sex == 1 & Phase == 2)
female_phi_rev3 <- format(round(unique(female_sim_rev3$phi_rev), digits = 2), nsmall = 2)
female_lambda_rev3 <- format(round(unique(female_sim_rev3$lambda_rev), digits = 2), nsmall = 2)

#Plot initial learning
plot(NULL, xlim = c(1,100), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
lines(x = male_sim_init3$trialafter, y = male_sim_init3$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_init3$trialafter, y = female_sim_init3$Choice , col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 90, y = 3.8, labels = bquote(phi == .(male_phi_init3)), cex = 1.3)
text(x = 90, y = 3.2, labels = bquote(lambda == .(male_lambda_init3)), cex = 1.3)
text(x = 90, y = 1.8, labels = bquote(phi == .(female_phi_init3)), cex = 1.3)
text(x = 90, y = 1.2, labels = bquote(lambda == .(female_lambda_init3)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#Plot reversal learning
plot(NULL, xlim = c(1,200), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(2,4), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(1,3), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
lines(x = male_sim_rev3$trialafter, y = male_sim_rev3$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_rev3$trialafter, y = female_sim_rev3$Choice, col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 180, y = 3.8, labels = bquote(phi == .(male_phi_rev3)), cex = 1.3)
text(x = 180, y = 3.2, labels = bquote(lambda == .(male_lambda_rev3)), cex = 1.3)
text(x = 180, y = 1.8, labels = bquote(phi == .(female_phi_rev3)), cex = 1.3)
text(x = 180, y = 1.2, labels = bquote(lambda == .(female_lambda_rev3)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#
##
###FOURTH ROW
##
#

#Simulate one male and one female - note the Post_Study_Simulation_Six_Birds.R file must have been run to use the below function
dat4 <- Post_Study_Sim_Pair_Fxn()

#Wrangle simulated data
male_sim_init4 <- dat4 %>% filter(sex == 2 & Phase == 1)
male_sim_init4$Choice2 <- ifelse(male_sim_init4$Choice == 1, 3, 4)
male_phi_init4 <- format(round(unique(male_sim_init4$phi_init), digits = 2), nsmall = 2)
male_lambda_init4 <- format(round(unique(male_sim_init4$lambda_init), digits = 2), nsmall = 2)

male_sim_rev4 <- dat4 %>% filter(sex == 2 & Phase == 2)
male_sim_rev4$Choice2 <- ifelse(male_sim_rev4$Choice == 1, 3, 4)
male_phi_rev4 <- format(round(unique(male_sim_rev4$phi_rev), digits = 2), nsmall = 2)
male_lambda_rev4 <- format(round(unique(male_sim_rev4$lambda_rev), digits = 2), nsmall = 2)

female_sim_init4 <- dat4 %>% filter(sex == 1 & Phase == 1)
female_phi_init4 <- format(round(unique(female_sim_init4$phi_init), digits = 2), nsmall = 2)
female_lambda_init4 <- format(round(unique(female_sim_init4$lambda_init), digits = 2), nsmall = 2)

female_sim_rev4 <- dat4 %>% filter(sex == 1 & Phase == 2)
female_phi_rev4 <- format(round(unique(female_sim_rev4$phi_rev), digits = 2), nsmall = 2)
female_lambda_rev4 <- format(round(unique(female_sim_rev4$lambda_rev), digits = 2), nsmall = 2)

#Plot initial learning
plot(NULL, xlim = c(1,100), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
lines(x = male_sim_init4$trialafter, y = male_sim_init4$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_init4$trialafter, y = female_sim_init4$Choice , col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 90, y = 3.8, labels = bquote(phi == .(male_phi_init4)), cex = 1.3)
text(x = 90, y = 3.2, labels = bquote(lambda == .(male_lambda_init4)), cex = 1.3)
text(x = 90, y = 1.8, labels = bquote(phi == .(female_phi_init4)), cex = 1.3)
text(x = 90, y = 1.2, labels = bquote(lambda == .(female_lambda_init4)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#Plot reversal learning
plot(NULL, xlim = c(1,200), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(2,4), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(1,3), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
lines(x = male_sim_rev4$trialafter, y = male_sim_rev4$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_rev4$trialafter, y = female_sim_rev4$Choice, col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 180, y = 3.8, labels = bquote(phi == .(male_phi_rev4)), cex = 1.3)
text(x = 180, y = 3.2, labels = bquote(lambda == .(male_lambda_rev4)), cex = 1.3)
text(x = 180, y = 1.8, labels = bquote(phi == .(female_phi_rev4)), cex = 1.3)
text(x = 180, y = 1.2, labels = bquote(lambda == .(female_lambda_rev4)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#
##
###FIFTH ROW
##
#

#Simulate one male and one female - note the Post_Study_Simulation_Six_Birds.R file must have been run to use the below function
dat5 <- Post_Study_Sim_Pair_Fxn() 

#Wrangle simulated data
male_sim_init5 <- dat5 %>% filter(sex == 2 & Phase == 1)
male_sim_init5$Choice2 <- ifelse(male_sim_init5$Choice == 1, 3, 4)
male_phi_init5 <- format(round(unique(male_sim_init5$phi_init), digits = 2), nsmall = 2)
male_lambda_init5 <- format(round(unique(male_sim_init5$lambda_init), digits = 2), nsmall = 2)

male_sim_rev5 <- dat5 %>% filter(sex == 2 & Phase == 2)
male_sim_rev5$Choice2 <- ifelse(male_sim_rev5$Choice == 1, 3, 4)
male_phi_rev5 <- format(round(unique(male_sim_rev5$phi_rev), digits = 2), nsmall = 2)
male_lambda_rev5 <- format(round(unique(male_sim_rev5$lambda_rev), digits = 2), nsmall = 2)

female_sim_init5 <- dat5 %>% filter(sex == 1 & Phase == 1)
female_phi_init5 <- format(round(unique(female_sim_init5$phi_init), digits = 2), nsmall = 2)
female_lambda_init5 <- format(round(unique(female_sim_init5$lambda_init), digits = 2), nsmall = 2)

female_sim_rev5 <- dat5 %>% filter(sex == 1 & Phase == 2)
female_phi_rev5 <- format(round(unique(female_sim_rev5$phi_rev), digits = 2), nsmall = 2)
female_lambda_rev5 <- format(round(unique(female_sim_rev5$lambda_rev), digits = 2), nsmall = 2)

#Plot initial learning
plot(NULL, xlim = c(1,100), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
lines(x = male_sim_init5$trialafter, y = male_sim_init5$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_init5$trialafter, y = female_sim_init5$Choice , col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 90, y = 3.8, labels = bquote(phi == .(male_phi_init5)), cex = 1.3)
text(x = 90, y = 3.2, labels = bquote(lambda == .(male_lambda_init5)), cex = 1.3)
text(x = 90, y = 1.8, labels = bquote(phi == .(female_phi_init5)), cex = 1.3)
text(x = 90, y = 1.2, labels = bquote(lambda == .(female_lambda_init5)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#Plot reversal learning
plot(NULL, xlim = c(1,200), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(2,4), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(1,3), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
lines(x = male_sim_rev5$trialafter, y = male_sim_rev5$Choice2, col = "#5ec962", lwd = 1)
lines(x = female_sim_rev5$trialafter, y = female_sim_rev5$Choice, col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 180, y = 3.8, labels = bquote(phi == .(male_phi_rev5)), cex = 1.3)
text(x = 180, y = 3.2, labels = bquote(lambda == .(male_lambda_rev5)), cex = 1.3)
text(x = 180, y = 1.8, labels = bquote(phi == .(female_phi_rev5)), cex = 1.3)
text(x = 180, y = 1.2, labels = bquote(lambda == .(female_lambda_rev5)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#
##
###SIXTH ROW
##
#

#Simulate one male and one female - note the Post_Study_Simulation_Six_Birds.R file must have been run to use the below function
dat6 <- Post_Study_Sim_Pair_Fxn() 

#Wrangle simulated data
male_sim_init6 <- dat6 %>% filter(sex == 2 & Phase == 1)
male_sim_init6$Choice2 <- ifelse(male_sim_init6$Choice == 1, 3, 4)
male_phi_init6 <- format(round(unique(male_sim_init6$phi_init), digits = 2), nsmall = 2)
male_lambda_init6<- format(round(unique(male_sim_init6$lambda_init), digits = 2), nsmall = 2)

male_sim_rev6 <- dat6 %>% filter(sex == 2 & Phase == 2)
male_sim_rev6$Choice2 <- ifelse(male_sim_rev6$Choice == 1, 3, 4)
male_phi_rev6 <- format(round(unique(male_sim_rev6$phi_rev), digits = 2), nsmall = 2)
male_lambda_rev6 <- format(round(unique(male_sim_rev6$lambda_rev), digits = 2), nsmall = 2)

female_sim_init6 <- dat6 %>% filter(sex == 1 & Phase == 1)
female_phi_init6 <- format(round(unique(female_sim_init6$phi_init), digits = 2), nsmall = 2)
female_lambda_init6 <- format(round(unique(female_sim_init6$lambda_init), digits = 2), nsmall = 2)

female_sim_rev6 <- dat6 %>% filter(sex == 1 & Phase == 2)
female_phi_rev6 <- format(round(unique(female_sim_rev6$phi_rev), digits = 2), nsmall = 2)
female_lambda_rev6 <- format(round(unique(female_sim_rev6$lambda_rev), digits = 2), nsmall = 2)

#Plot initial learning
plot(NULL, xlim = c(1,100), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(1,3), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(2,4), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
lines(x = male_sim_init6$trialafter, y = as.numeric(male_sim_init6$Choice2, 1), col = "#5ec962", lwd = 1)
lines(x = female_sim_init6$trialafter, y = as.numeric(female_sim_init6$Choice, 1), col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 90, y = 3.8, labels = bquote(phi == .(male_phi_init6)), cex = 1.3)
text(x = 90, y = 3.2, labels = bquote(lambda == .(male_lambda_init6)), cex = 1.3)
text(x = 90, y = 1.8, labels = bquote(phi == .(female_phi_init6)), cex = 1.3)
text(x = 90, y = 1.2, labels = bquote(lambda == .(female_lambda_init6)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#Plot reversal learning
plot(NULL, xlim = c(1,200), ylim = c(0.5,4.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 2, at = c(2,4), labels = c("F+", "F+"), cex.axis = 1.3)
axis(side = 2, at = c(1,3), labels = c("F-", "F-"), cex.axis = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
lines(x = male_sim_rev6$trialafter, y = as.numeric(male_sim_rev6$Choice2, 1), col = "#5ec962", lwd = 1)
lines(x = female_sim_rev6$trialafter, y = as.numeric(female_sim_rev6$Choice, 1), col = "#fde725", lwd = 1)
abline(h = 2.5, lty = 2)
text(x = 180, y = 3.8, labels = bquote(phi == .(male_phi_rev6)), cex = 1.3)
text(x = 180, y = 3.2, labels = bquote(lambda == .(male_lambda_rev6)), cex = 1.3)
text(x = 180, y = 1.8, labels = bquote(phi == .(female_phi_rev6)), cex = 1.3)
text(x = 180, y = 1.2, labels = bquote(lambda == .(female_lambda_rev6)), cex = 1.3)
mtext("Choice",  cex = 1.3, side = 2, line = 3)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Simulated males", "Simulated females"), fill = c("#5ec962","#fde725"), text.width = .3, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.8)

#dev.off() #Turn on to plot pdf

#End script
#####################################################################################################################################################

