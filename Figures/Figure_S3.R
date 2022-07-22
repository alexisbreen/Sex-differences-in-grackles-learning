#####################################################################################################################################################

#Code for plotting Figure S3 for the manuscript

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#####################################################################################################################################################
#Pre-graph processing
#####################################################################################################################################################

#Subset by sex, phase & skip-birds, and get max trial
max_real_FI <- d %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(Trial))
max_real_FR <- d %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(Trial))
max_real_MI <- d %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(Trial))
max_real_MR <- d %>% filter(sex == 2 & Phase == 2 & skip == 0) %>% summarise(max = max(Trial))

#Calculate proportion choosing correct choice-option acrosss and phase
Prop_correct_real_FI <- sapply(1:max_real_FI$max, function(x) mean(d$Correct[d$Trial == x & d$sex != 2 & d$Phase != 2]))
Prop_correct_real_FI[is.na(Prop_correct_real_FI)] <- 0

Prop_correct_real_FR <- sapply(1:max_real_FR$max, function(x) mean(d$Correct[d$Trial == x & d$sex != 2 & d$Phase != 1]))
Prop_correct_real_FR[is.na(Prop_correct_real_FR)] <- 0

Prop_correct_real_MI <- sapply(1:max_real_MI$max, function(x) mean(d$Correct[d$Trial == x & d$sex != 1 & d$Phase != 2]))
Prop_correct_real_MI[is.na(Prop_correct_real_MI)] <- 0

Prop_correct_real_MR <- sapply(1:max_real_MR$max, function(x) mean(d$Correct[d$Trial == x & d$sex != 1 & d$Phase != 1]))
Prop_correct_real_MR[is.na(Prop_correct_real_MR)] <- 0

#Perform 10 simulations using - note Post_Study_Simulation_Full_Birds.R file must have been run to use below functions!
dat1 <- Post_Study_Sim_Fxn()
dat2 <- Post_Study_Sim_Fxn()
dat3 <- Post_Study_Sim_Fxn()
dat4 <- Post_Study_Sim_Fxn()
dat5 <- Post_Study_Sim_Fxn()
dat6 <- Post_Study_Sim_Fxn()
dat7 <- Post_Study_Sim_Fxn()
dat8 <- Post_Study_Sim_Fxn()
dat9 <- Post_Study_Sim_Fxn()
dat10 <- Post_Study_Sim_Fxn()

#Subset sims by sex and phase, and get max trial
max_sim_FI1 <- dat1 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR1 <- dat1 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI1 <- dat1 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR1 <- dat1 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI2 <- dat2 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR2 <- dat2 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI2 <- dat2 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR2 <- dat2 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI3 <- dat3 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR3 <- dat3 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI3 <- dat3 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR3 <- dat3 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI4 <- dat4 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR4 <- dat4 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI4 <- dat4 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR4 <- dat4 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI5 <- dat5 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR5 <- dat5 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI5 <- dat5 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR5 <- dat5 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI6 <- dat6 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR6 <- dat6 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI6 <- dat6 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR6 <- dat6 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI7 <- dat7 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR7 <- dat7 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI7 <- dat7 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR7 <- dat7 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI8 <- dat8 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR8 <- dat8 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI8 <- dat8 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR8 <- dat8 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI9 <- dat9 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR9 <- dat9 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI9 <- dat9 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR9 <- dat9 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

max_sim_FI10 <- dat10 %>% filter(sex == 1 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_FR10 <- dat10 %>% filter(sex == 1 & Phase == 2) %>% summarise(max = max(trialafter))
max_sim_MI10 <- dat10 %>% filter(sex == 2 & Phase == 1) %>% summarise(max = max(trialafter))
max_sim_MR10 <- dat10 %>% filter(sex == 2 & Phase == 2) %>% summarise(max = max(trialafter))

#Calculate proportion choosing correct choice-option across sex and phase for simulations
Prop_correct_sim_FI1 <- sapply(1:max_sim_FI1$max, function(x) mean(dat1$Payoff[dat1$trialafter == x & dat1$sex == 1 & dat1$Phase == 1]))
Prop_correct_sim_MI1 <- sapply(1:max_sim_MI1$max, function(x) mean(dat1$Payoff[dat1$trialafter == x & dat1$sex == 2 & dat1$Phase == 1]))
Prop_correct_sim_FR1 <- sapply(1:max_sim_FR1$max, function(x) mean(dat1$Payoff[dat1$trialafter == x & dat1$sex == 1 & dat1$Phase == 2]))
Prop_correct_sim_MR1 <- sapply(1:max_sim_MR1$max, function(x) mean(dat1$Payoff[dat1$trialafter == x & dat1$sex == 2 & dat1$Phase == 2]))

Prop_correct_sim_FI2 <- sapply(1:max_sim_FI2$max, function(x) mean(dat2$Payoff[dat2$trialafter == x & dat2$sex == 1 & dat2$Phase == 1]))
Prop_correct_sim_MI2 <- sapply(1:max_sim_MI2$max, function(x) mean(dat2$Payoff[dat2$trialafter == x & dat2$sex == 2 & dat2$Phase == 1]))
Prop_correct_sim_FR2 <- sapply(1:max_sim_FR2$max, function(x) mean(dat2$Payoff[dat2$trialafter == x & dat2$sex == 1 & dat2$Phase == 2]))
Prop_correct_sim_MR2 <- sapply(1:max_sim_MR2$max, function(x) mean(dat2$Payoff[dat2$trialafter == x & dat2$sex == 2 & dat2$Phase == 2]))

Prop_correct_sim_FI3 <- sapply(1:max_sim_FI3$max, function(x) mean(dat3$Payoff[dat3$trialafter == x & dat3$sex == 1 & dat3$Phase == 1]))
Prop_correct_sim_MI3 <- sapply(1:max_sim_MI3$max, function(x) mean(dat3$Payoff[dat3$trialafter == x & dat3$sex == 2 & dat3$Phase == 1]))
Prop_correct_sim_FR3 <- sapply(1:max_sim_FR3$max, function(x) mean(dat3$Payoff[dat3$trialafter == x & dat3$sex == 1 & dat3$Phase == 2]))
Prop_correct_sim_MR3 <- sapply(1:max_sim_MR3$max, function(x) mean(dat3$Payoff[dat3$trialafter == x & dat3$sex == 2 & dat3$Phase == 2]))

Prop_correct_sim_FI4 <- sapply(1:max_sim_FI4$max, function(x) mean(dat4$Payoff[dat4$trialafter == x & dat4$sex == 1 & dat4$Phase == 1]))
Prop_correct_sim_MI4 <- sapply(1:max_sim_MI4$max, function(x) mean(dat4$Payoff[dat4$trialafter == x & dat4$sex == 2 & dat4$Phase == 1]))
Prop_correct_sim_FR4 <- sapply(1:max_sim_FR4$max, function(x) mean(dat4$Payoff[dat4$trialafter == x & dat4$sex == 1 & dat4$Phase == 2]))
Prop_correct_sim_MR4 <- sapply(1:max_sim_MR4$max, function(x) mean(dat4$Payoff[dat4$trialafter == x & dat4$sex == 2 & dat4$Phase == 2]))

Prop_correct_sim_FI5 <- sapply(1:max_sim_FI5$max, function(x) mean(dat5$Payoff[dat5$trialafter == x & dat5$sex == 1 & dat5$Phase == 1]))
Prop_correct_sim_MI5 <- sapply(1:max_sim_MI5$max, function(x) mean(dat5$Payoff[dat5$trialafter == x & dat5$sex == 2 & dat5$Phase == 1]))
Prop_correct_sim_FR5 <- sapply(1:max_sim_FR5$max, function(x) mean(dat5$Payoff[dat5$trialafter == x & dat5$sex == 1 & dat5$Phase == 2]))
Prop_correct_sim_MR5 <- sapply(1:max_sim_MR5$max, function(x) mean(dat5$Payoff[dat5$trialafter == x & dat5$sex == 2 & dat5$Phase == 2]))

Prop_correct_sim_FI6 <- sapply(1:max_sim_FI6$max, function(x) mean(dat6$Payoff[dat6$trialafter == x & dat6$sex == 1 & dat6$Phase == 1]))
Prop_correct_sim_MI6 <- sapply(1:max_sim_MI6$max, function(x) mean(dat6$Payoff[dat6$trialafter == x & dat6$sex == 2 & dat6$Phase == 1]))
Prop_correct_sim_FR6 <- sapply(1:max_sim_FR6$max, function(x) mean(dat6$Payoff[dat6$trialafter == x & dat6$sex == 1 & dat6$Phase == 2]))
Prop_correct_sim_MR6 <- sapply(1:max_sim_MR6$max, function(x) mean(dat6$Payoff[dat6$trialafter == x & dat6$sex == 2 & dat6$Phase == 2]))

Prop_correct_sim_FI7 <- sapply(1:max_sim_FI7$max, function(x) mean(dat7$Payoff[dat7$trialafter == x & dat7$sex == 1 & dat7$Phase == 1]))
Prop_correct_sim_MI7 <- sapply(1:max_sim_MI7$max, function(x) mean(dat7$Payoff[dat7$trialafter == x & dat7$sex == 2 & dat7$Phase == 1]))
Prop_correct_sim_FR7 <- sapply(1:max_sim_FR7$max, function(x) mean(dat7$Payoff[dat7$trialafter == x & dat7$sex == 1 & dat7$Phase == 2]))
Prop_correct_sim_MR7 <- sapply(1:max_sim_MR7$max, function(x) mean(dat7$Payoff[dat7$trialafter == x & dat7$sex == 2 & dat7$Phase == 2]))

Prop_correct_sim_FI8 <- sapply(1:max_sim_FI8$max, function(x) mean(dat8$Payoff[dat8$trialafter == x & dat8$sex == 1 & dat8$Phase == 1]))
Prop_correct_sim_MI8 <- sapply(1:max_sim_MI8$max, function(x) mean(dat8$Payoff[dat8$trialafter == x & dat8$sex == 2 & dat8$Phase == 1]))
Prop_correct_sim_FR8 <- sapply(1:max_sim_FR8$max, function(x) mean(dat8$Payoff[dat8$trialafter == x & dat8$sex == 1 & dat8$Phase == 2]))
Prop_correct_sim_MR8 <- sapply(1:max_sim_MR8$max, function(x) mean(dat8$Payoff[dat8$trialafter == x & dat8$sex == 2 & dat8$Phase == 2]))

Prop_correct_sim_FI9 <- sapply(1:max_sim_FI9$max, function(x) mean(dat9$Payoff[dat9$trialafter == x & dat9$sex == 1 & dat9$Phase == 1]))
Prop_correct_sim_MI9 <- sapply(1:max_sim_MI9$max, function(x) mean(dat9$Payoff[dat9$trialafter == x & dat9$sex == 2 & dat9$Phase == 1]))
Prop_correct_sim_FR9 <- sapply(1:max_sim_FR9$max, function(x) mean(dat9$Payoff[dat9$trialafter == x & dat9$sex == 1 & dat9$Phase == 2]))
Prop_correct_sim_MR9 <- sapply(1:max_sim_MR9$max, function(x) mean(dat9$Payoff[dat9$trialafter == x & dat9$sex == 2 & dat9$Phase == 2]))

Prop_correct_sim_FI10 <- sapply(1:max_sim_FI10$max, function(x) mean(dat10$Payoff[dat10$trialafter == x & dat10$sex == 1 & dat10$Phase == 1]))
Prop_correct_sim_MI10 <- sapply(1:max_sim_MI10$max, function(x) mean(dat10$Payoff[dat10$trialafter == x & dat10$sex == 2 & dat10$Phase == 1]))
Prop_correct_sim_FR10 <- sapply(1:max_sim_FR10$max, function(x) mean(dat10$Payoff[dat10$trialafter == x & dat10$sex == 1 & dat10$Phase == 2]))
Prop_correct_sim_MR10 <- sapply(1:max_sim_MR10$max, function(x) mean(dat10$Payoff[dat10$trialafter == x & dat10$sex == 2 & dat10$Phase == 2]))

#####################################################################################################################################################
#Figure S3

#Note: it is not possible to replicate Figure S3 - which is a hand-picked representation of the overall dynamics - as simulations vary by 'go' 

#####################################################################################################################################################


#pdf(file = "Figure_S3.pdf", height = 5, width = 12) #Turn on if want pdf

#Set-up plot space
par(mfrow=c(2,2), mar = c(2,3,1,2), oma = c(1,1,5,1))

#
##
###FIRST ROW - MALES
##
#

plot(NULL, xlim = c(1, 300), ylim = c(-.05, 1.05), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
mtext("Prop. correct",  cex = 1.3, side = 2, line = 2.5)
axis(side = 1, at = c(1,50,100,150,200,250,300), labels = c("Trial 1","50", "100","150","200","250","300"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9), labels = c("0","","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
lines(Prop_correct_sim_MI1, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI2, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI3, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI4, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI5, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI6, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI7, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI8, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI9, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MI10, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_real_MI, lwd = 1.5, col = alpha(c("#5ec962")))

mtext("Initial learning", side = 3, adj = 0.5, cex = 1.2, line = 1, font = 2)

plot(NULL, xlim = c(1, 300), ylim = c(-.05, 1.05), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
mtext("Prop. correct",  cex = 1.3, side = 2, line = 2.5)
axis(side = 2, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9), labels = c("0","","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
lines(Prop_correct_sim_MR1, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR2, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR3, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR4, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR5, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR6, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR7, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR8, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR9, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_sim_MR10, lwd = .5, col = alpha(c("#5ec962"),.2))
lines(Prop_correct_real_MR, lwd = 1.5, col = alpha(c("#5ec962")))
axis(side = 1, at = c(1,50,100,150,200,250,300), labels = c("Trial 1","50", "100","150","200","250","300"), cex.axis = 1.3)
mtext("Reversal learning", side = 3, adj = 0.5, cex = 1.2, line = 1, font = 2)

#
##
###SECOND ROW - FEMALES
##
#

plot(NULL, xlim = c(1, 300), ylim = c(-.05, 1.05), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
mtext("Prop. correct",  cex = 1.3, side = 2, line = 2.5)
axis(side = 2, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9), labels = c("0","","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
lines(Prop_correct_sim_FI1, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI2, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI3, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI4, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI5, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI6, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI7, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI8, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI9, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FI10, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_real_FI, lwd = 1.5, col = alpha(c("#fde725")))
axis(side = 1, at = c(1,50,100,150,200,250,300), labels = c("Trial 1","50", "100","150","200","250","300"), cex.axis = 1.3)


plot(NULL, xlim = c(1, 300), ylim = c(-.05, 1.05), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
mtext("Prop. correct",  cex = 1.3, side = 2, line = 2.5)
axis(side = 2, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9), labels = c("0","","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(1), cex.axis = 1.3)
lines(Prop_correct_sim_FR1, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR2, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR3, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR4, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR5, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR6, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR7, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR8, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR9, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_sim_FR10, lwd = .5, col = alpha(c("#fde725"),.2))
lines(Prop_correct_real_FR, lwd = 1.5, col = alpha(c("#fde725")))
axis(side = 1, at = c(1,50,100,150,200,250,300), labels = c("Trial 1","50", "100","150","200","250","300"), cex.axis = 1.3)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males", "Females"), fill = c("#5ec962","#fde725"), text.width = .2, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf

#End script
#####################################################################################################################################################
