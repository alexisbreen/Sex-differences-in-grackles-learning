#####################################################################################################################################################

#Code for plotting Figure 2 for the manuscript

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script
library(survival)
library(tidyverse)
library(ggridges)

#####################################################################################################################################################
#Pre-graph processing
#####################################################################################################################################################

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#Next, need to ready data for survival plots by...
#Subsetting by phase & non-skip birds & calculate max trials per individual & assign censored column for survival plots
I_Surv <- d %>% filter(Phase == 1) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))
R_Surv <- d %>% filter(Phase == 2 & skip == 0) %>% group_by(id, sex) %>% filter(Trial == max(Trial)) %>% mutate(cens = rep(1))

#And fitting survival trajectories
I_Fit <- survfit(Surv(Trial, cens) ~ sex, data = I_Surv)
R_Fit <- survfit(Surv(Trial, cens) ~ sex, data = R_Surv)

#For obtaining survival probability means and standard errors reported in main text
summary(I_Fit)$table
summary(R_Fit)$table

#Third, need to ready data for switch plots by...
#Determining when a switch occurred in our data set
d$switch <- NA
for(i in 1:nrow(d)){
  if(d$Trial[i] > 1){
    if(d$Choice[i] == 1){
      if(d$Choice[i - 1] == 1){
        d$switch[i] <- 0
      } else {
        d$switch[i] <- 1
      }
    } else {
      if(d$Choice[i - 1] == 2){
        d$switch[i] <- 0
      } else {
        d$switch[i] <- 1
      }
    }
  } else {
    d$switch[i] <- 0
  }
}

#And subsetting by sex & phase & non-skip birds
MI <- d %>% filter(sex == 2 & Phase == 1)
MR <- d %>% filter(sex == 2 & Phase == 2 & skip == 0) 
FI <- d %>% filter(sex == 1 & Phase == 1)
FR <- d %>% filter(sex == 1 & Phase == 2)

#As well as determining max trial
Max_MI <- max(MI$Trial)
Max_MR <- max(MR$Trial)
Max_FI <- max(FI$Trial)
Max_FR <- max(FR$Trial)

#Plus readying empty matrices for performing cumulative proportion switch counts across trials
MIX <- matrix(0,1,Max_MI)
MRX <- matrix(0,1,Max_MR)
FIX <- matrix(0,1,Max_FI)
FRX <- matrix(0,1,Max_FR)

#To perform those calculations, we do: e.g., for(i in 1:Max_MI){MIX[1 , i] <- (sum(MI$switch[which(MI$Trial < i + 1)]) / length(MI$switch[which(MI$Trial < i + 1)])) } 
#In other words: for every trial in the max number of trials, fill matrix for that trial with <-
#the sum of switches (i.e., 1s) up to that trial (use < i + 1 to designate this)
#divided by the length of all switch and non-switches up to that trial 
#for example:

                                               #Trial 1  Trial 2  Trial 3  Trial 4
                               #Bird 1             0         1        0        Bird 1 finished at trial 3
                               #Bird 2             1         1        0       0
                               #Bird 3             0         0        1       1

                #Cumulative switch sum            1         3        4       5
                #Cumulative switch length         3         6        9       11
                #Calculation                  1/3 = 0.3 3/6 = 0.5 4/9 = 0.4 5/11 = 0.45

#It is possible to quickly see this in action by looking at reversal skip birds (birds 18, 22 & 29) 
check <- d %>% filter(sex == 2 & Phase == 2 & skip == 1)
#And then look at each of their 'max' trials
max(check$Trial[which(check$id == 18)]) #Returns 35 'max' trial
max(check$Trial[which(check$id == 22)]) #Returns 23 'max' trial
max(check$Trial[which(check$id == 29)]) #Returns 20 'max' trial
#So at 18 trials there will be length 20 * 3 = 60 b/c all three birds reached 20 trials
length(check$switch[which(check$Trial < 20 + 1)]) #Returns 60
#But at 21 trials there will be length (20 * 3) + (1 * 2) = 62 b/c only two birds gained an additional trial 21
length(check$switch[which(check$Trial < 21 + 1)]) #Returns 62
#And at 'max' possible trial 35 there will be length (20 * 3) + (3 * 2) + (1 * 12) = 78 b/c 3 birds reached 18 trials, 2 birds gained any additional 3 trials, & 1 bird went on for another 12 trials
length(check$switch[which(check$Trial < 35 + 1)]) #Returns 78
#Thus summing across all switches that occured (14) - can check by running: sum(check$switch[which(check$Trial < 35 + 1)])
#And dividing into max length of 78, should return 0.18 cumulative prop. switches across 35 possible trials
sum(check$switch[which(check$Trial < 35 + 1)])/length(check$switch[which(check$Trial < 35 + 1)]) #Returns 0.18

#Now back to the actual calculations
for(i in 1:Max_MI){MIX[1 , i] <- (sum(MI$switch[which(MI$Trial < i + 1)]) / length(MI$switch[which(MI$Trial < i + 1)])) } 
for(i in 1:Max_FI){FIX[1 , i] <- (sum(FI$switch[which(FI$Trial < i + 1)]) / length(FI$switch[which(FI$Trial < i + 1)])) }
for(i in 1:Max_MR){MRX[1 , i] <- (sum(MR$switch[which(MR$Trial < i + 1)]) / length(MR$switch[which(MR$Trial < i + 1)])) }
for(i in 1:Max_FR){FRX[1 , i] <- (sum(FR$switch[which(FR$Trial < i + 1)]) / length(FR$switch[which(FR$Trial < i + 1)])) }

#Averages & standard errors of cumulative switch counts reported in main text
se <- function(x) sd(x)/sqrt(length(x))

mu_MIX <- apply(MIX, 1, mean)
se_MIX <- apply(MIX, 1, se)

mu_FIX <- apply(FIX, 1, mean)
se_FIX <- apply(FIX, 1, se)

mu_MRX <- apply(MRX, 1, mean)
se_MRX <- apply(MRX, 1, se)

mu_FRX <- apply(FRX, 1, mean)
se_FRX <- apply(FRX, 1, se)

#####################################################################################################################################################
#Figure 2
#####################################################################################################################################################

#pdf(file = "Figure_2.pdf", height = 11, width = 10) #turn on if want pdf of plot

#Set-up plot space
par(mfrow = c(4,2), mar = c(3,5,3,2), oma = c(0,0,5,0))

#Panel A, left plot
plot(NULL, xlim = c(1, 100), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
abline(v = c(40,41), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(I_Fit, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Initial learning", side = 3, adj = .5, cex = 1.2, line = 3, font = 2)
mtext("Cum. prop. pass",  cex = 1.2, side = 2, line = 3)
mtext("Across", side = 3, at = 50, cex = 1.2, line = 1)
mtext(expression(paste(bold(A))), side = 3, cex = 1.2, line = .8, adj = -.14)

#Panel A, right plot
plot(NULL, xlim = c(1, 200), ylim = c(0,1), xlab = NA, ylab = NA,  xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3)
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.1,.2,.3,.4,.6,.7,.8,.9), labels = c("0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.5, 1), cex.axis = 1.3)
abline(v = c(72,85), lty = 2, col = alpha(c("#5ec962","#fde725"), 0.8))
lines(R_Fit, fun = "event", conf.int = FALSE, lty = 1, lwd = 2, col = alpha(c("#fde725","#5ec962"), 0.8), xlim = c(0, 1))
mtext("Reversal learning", side = 3, adj = .5, cex = 1.2, line = 3, font = 2)
mtext("Prop. in-test",  cex = 1.2, side = 2, line = 3)
mtext("Across", side = 3, at = 100, cex = 1.2, line = 1)

#Panel B, left plot
plot(NULL, xlim = c(1,100),ylim = c(0,.50), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,.10,.20,.30,.40,.50), labels = c("0","","","","","0.5"), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Across", side = 3, at = 50, cex = 1.2, line = 1)
lines(1:Max_FI, FIX[1, 1:Max_FI], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MI, MIX[1, 1:Max_MI], lwd = 2, col = alpha(c("#5ec962"), .8))
mtext(expression(paste(bold(B))), side = 3, cex = 1.2, line = 1, adj = -.14)

#Panel B, right plot
plot(NULL, xlim = c(1,200),ylim = c(0,.50), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,.10,.20,.30,.40,.50), labels = c("0","","","","","0.5"), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Across", side = 3, at = 100, cex = 1.2, line = 1)
lines(1:Max_FR, FRX[1, 1:Max_FR], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MR, MRX[1, 1:Max_MR], lwd = 2, col = alpha(c("#5ec962"), .8))

#Panel C - note model outputs must have already been processed using STAN_Execution.R b/c e.g., s_phi_init, used below, is estimated via computations

#Treatments go along x-axis at these points (for this and all following plots)
x <- c(1:10) 

#Phi

#Calculate mean & HPDI
mu_phi_init <- apply(s_phi_init, 2, mean)
HPDI_phi_init <- apply(s_phi_init, 2, HPDI)

#Left plot
plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.31,.31), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3), labels = c("-0.3","","","0","","","0.3"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 2.5)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-1.1,3.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-1.1,6.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(8.5,-1.1,9.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-1.1,10.5,1.1,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 2, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_init[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)[as.numeric(x)]) #sample 100 points
arrows(x0 = x, y0 = HPDI_phi_init[1,], x1 = x, y1 = HPDI_phi_init[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_phi_init, col = "red", pch = 19)
mtext(expression(paste(bold(C))), side = 3, cex = 1.2, line = 1, adj = -.14)

#Calculate mean & HPDI
mu_phi_rev <- apply(s_phi_rev, 2, mean)
HPDI_phi_rev <- apply(s_phi_rev, 2, HPDI)

#Right plot
plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.31,.31), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab= NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3), labels = c("-0.3","","","0","","","0.3"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 2.5)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-1.1,3.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-1.1,6.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(8.5,-1.1,9.5,1.1,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-1.1,10.5,1.1,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 2, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_rev[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)[as.numeric(x)]) #sample 100 points
arrows(x0 = x, y0 = HPDI_phi_rev[1,], x1 = x, y1 = HPDI_phi_rev[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_phi_rev, col = "red", pch = 19)

#Panel D - note model outputs must have already been processed using STAN_Execution.R b/c e.g., s_L_init, used below, is estimated via computations

#Lambda

#Calculate mean & HPDI
mu_L_init <- apply(s_L_init, 2, mean)
HPDI_L_init <- apply(s_L_init, 2, HPDI)

#Left plot
plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-10,10), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 2.5)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-12,3.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-12,6.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(8.5,-12,9.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-12,10.5,12,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1, col = "black")
abline(h = c(0), lty = 2, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_init[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)[as.numeric(x)]) #sample 100 pointsarrows(x0 = x, y0 = PI_L_init[1,], x1 = x, y1 = PI_L_init[2,], length = 0, col = "red")
arrows(x0 = x, y0 = HPDI_L_init[1,], x1 = x, y1 = HPDI_L_init[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_L_init, col = "red", pch = 19)
mtext(expression(paste(bold(D))), side = 3, cex = 1.2, line = 1, adj = -.14)

#Calculate mean & HPDI
mu_L_rev <- apply(s_L_rev, 2, mean)
HPDI_L_rev <- apply(s_L_rev, 2, HPDI)

#Right plot
plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-10,10), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 2.5)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
rect(2.5,-12,3.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(5.5,-12,6.5,12,col = alpha("grey",alpha=.3),border = NA)
rect(8.5,-12,9.5,12,col = alpha("grey",alpha=.3), border = NA)
rect(9.5,-12,10.5,12,col = alpha("grey",alpha=.3), border = NA)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 2, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_rev[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)[as.numeric(x)]) #sample 100 pointsarrows(x0 = x, y0 = PI_L_init[1,], x1 = x, y1 = PI_L_init[2,], length = 0, col = "red")
arrows(x0 = x, y0 = HPDI_L_rev[1,], x1 = x, y1 = HPDI_L_rev[2,], length = 0, col = "red", lwd = 1.5)
points(x = x, y = mu_L_rev, col = "red", pch = 19)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males (M)", "Females (F)", "Contrast (M-F)"), fill = c("#5ec962","#fde725","black"), text.width = .23, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf of plot

#####################################################################################################################################################
#Figure 2 insets - note these were inserted via Inkscape
#####################################################################################################################################################

#And for plot insets of posterior density female-male contrasts, execute below
#Note Poisson models must have been already run using STAN_Execution.R & posteriors extracted

#Speed Initial
s_speed_init_X <- as.data.frame(
  list(
    
    AZ_init_X = exp(sp1$a[,1,1,1]) - exp(sp1$a[,1,2,1]),
    SB_init_X = exp(sp1$a[,2,1,1]) - exp(sp1$a[,2,2,1]),
    WL_init_X = exp(sp1$a[,3,1,1]) - exp(sp1$a[,3,2,1]),
    init_X  = exp(sp2$a[,1,1]) - exp(sp2$a[,2,1])
  )
)

#Speed Reversal
s_speed_rev_X <- as.data.frame(
  list(
    
    AZ_rev_X = exp(sp1$a[,1,1,2]) - exp(sp1$a[,1,2,2]),
    SB_rev_X = exp(sp1$a[,2,1,2]) - exp(sp1$a[,2,2,2]),
    WL_rev_X = exp(sp1$a[,3,1,2]) - exp(sp1$a[,3,2,2]),
    rev_X  = exp(sp2$a[,1,2]) - exp(sp2$a[,2,2])
  )
)

#Switch Initial
s_switch_init_X <- as.data.frame(
  list(
    
    AZ_init_X = exp(sw1$a[,1,1,1]) - exp(sw1$a[,1,2,1]),
    SB_init_X = exp(sw1$a[,2,1,1]) - exp(sw1$a[,2,2,1]),
    WL_init_X = exp(sw1$a[,3,1,1]) - exp(sw1$a[,3,2,1]),
    init_X  = exp(sw2$a[,1,1]) - exp(sw2$a[,2,1])
  )
)

#Switch Reversal
s_switch_rev_X <- as.data.frame(
  list(
    
    AZ_rev_X = exp(sw1$a[,1,1,2]) - exp(sw1$a[,1,2,2]),
    SB_rev_X = exp(sw1$a[,2,1,2]) - exp(sw1$a[,2,2,2]),
    WL_rev_X = exp(sw1$a[,3,1,2]) - exp(sw1$a[,3,2,2]),
    rev_X  = exp(sw2$a[,1,2]) - exp(sw2$a[,2,2])
  )
)


#Get data into long from wide format
#Speed initial contrasts
speed_init_X <- s_speed_init_X %>% 
  gather("column", "dens", 1:4) %>% 
  mutate(column = factor(column, levels = c("AZ_init_X", "SB_init_X", "WL_init_X", "init_X"), labels = c("Core", "Middle", "Edge", "Across")))
#Speed reversal contrasts
speed_rev_X <- s_speed_rev_X %>% 
  gather("column", "dens", 1:4) %>% 
  mutate(column = factor(column, levels = c("AZ_rev_X", "SB_rev_X", "WL_rev_X", "rev_X"), labels = c("Core", "Middle", "Edge", "Across")))
#Switch initial contrasts
switch_init_X <- s_switch_init_X %>% 
  gather("column", "dens", 1:4) %>% 
  mutate(column = factor(column, levels = c("AZ_init_X", "SB_init_X", "WL_init_X", "init_X"), labels = c("Core", "Middle", "Edge", "Across")))
#Switch reversal contrasts
switch_rev_X <- s_switch_rev_X %>% 
  gather("column", "dens", 1:4) %>% 
  mutate(column = factor(column, levels = c("AZ_rev_X", "SB_rev_X", "WL_rev_X", "rev_X"), labels = c("Core", "Middle", "Edge", "Across")))

#Inset 1 - Speed initial learning contrasts

#pdf(file = "Figure2_inset1.pdf", height = 3, width = 3) #Turn on if want pdf of plot

plot_speed_init_X <- ggplot(speed_init_X, aes(x = dens, y = as.factor(column), fill = column)) + 
  stat_density_ridges(alpha = .3) + 
  scale_fill_manual(values = c("grey","grey","grey","grey") ) +
  scale_x_continuous(name = "Posterior probability distribution", expand = c(0, 0), breaks = 0, labels = "0") + #not in Figure 2 but here for clarity
  scale_y_discrete(expand = c(0, 0.05), limits = rev) +
  geom_vline(xintercept = 0, linetype = "solid", colour = "red") + 
  theme_bw() +   
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.text = element_text(colour = "black", size = 10),
    axis.title = element_blank(),
    axis.ticks = element_line(colour = "black")
  )

plot_speed_init_X

#dev.off() #Turn on if want pdf of plot

#Inset 2 - Speed reversal learning contrasts

#pdf(file = "Figure2_inset2.pdf", height = 3, width = 3) #Turn on if want pdf of plot

plot_speed_rev_X <- ggplot(speed_rev_X, aes(x = dens, y = as.factor(column), fill = column)) + 
  stat_density_ridges(alpha = .3) + 
  scale_fill_manual(values = c("grey","grey","grey","grey") ) +
  scale_x_continuous(name = "Posterior probability distribution", expand = c(0, 0), breaks = 0, labels = "0") +  #not in Figure 2 but here for clarity
  scale_y_discrete(expand = c(0, 0.05), limits = rev) +
  geom_vline(xintercept = 0, linetype = "solid", colour = "red") + 
  theme_bw() +   
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.text = element_text(colour = "black", size = 10),
    axis.title = element_blank(),
    axis.ticks = element_line(colour = "black")
  )

plot_speed_rev_X

#dev.off() #Turn on if want pdf of plot

#Inset 3 - Switch initial learning contrasts

#pdf(file = "Figure2_inset3.pdf", height = 3, width = 3) #Turn on if want pdf of plot

plot_switch_init_X <- ggplot(switch_init_X, aes(x = dens, y = as.factor(column), fill = column)) + 
  stat_density_ridges(alpha = .3) + 
  scale_fill_manual(values = c("grey","grey","grey","grey") ) +
  scale_x_continuous(name = "Posterior probability distribution", expand = c(0, 0), breaks = 0, labels = "0") +  #not in Figure 2 but here for clarity
  scale_y_discrete(expand = c(0, 0.05), limits = rev) +
  geom_vline(xintercept = 0, linetype = "solid", colour = "red") + 
  theme_bw() +   
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.text = element_text(colour = "black", size = 10),
    axis.title = element_blank(),
    axis.ticks = element_line(colour = "black")
  )

plot_switch_init_X

#dev.off() #Turn on if want pdf of plot

#Inset 4 - Speed reversal learning contrasts

#pdf(file = "Figure2_inset4.pdf", height = 3, width = 3) #Turn on if want pdf of plot

plot_switch_rev_X <- ggplot(switch_rev_X, aes(x = dens, y = as.factor(column), fill = column)) + 
  stat_density_ridges(alpha = .3) + 
  scale_fill_manual(values = c("grey","grey","grey","grey") ) +
  scale_x_continuous(name = "Posterior probability distribution", expand = c(0, 0), breaks = 0, labels = "0") +  #not in Figure 2 but here for clarity
  scale_y_discrete(expand = c(0, 0.05), limits = rev) +
  geom_vline(xintercept = 0, linetype = "solid", colour = "red") + 
  theme_bw() +   
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.text = element_text(colour = "black", size = 10),
    axis.title = element_blank(),
    axis.ticks = element_line(colour = "black")
  )

plot_switch_rev_X

#dev.off() #Turn on if want pdf of plot

#End script
#####################################################################################################################################################

