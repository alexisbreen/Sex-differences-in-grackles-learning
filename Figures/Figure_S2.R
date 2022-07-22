#####################################################################################################################################################

#Code for plotting Figure S2 for the manuscript

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#####################################################################################################################################################
#Pre-graph processing
#####################################################################################################################################################

#Determine when a switch occurred in our data set
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

#And subset by population, sex, phase & skip-birds

#Initial
MI_AZ <- d %>% filter(Population == 1 & sex == 2 & Phase == 1)
FI_AZ <- d %>% filter(Population == 1 & sex == 1 & Phase == 1)
MI_SB <- d %>% filter(Population == 2 & sex == 2 & Phase == 1)
FI_SB <- d %>% filter(Population == 2 & sex == 1 & Phase == 1)
MI_WL <- d %>% filter(Population == 3 & sex == 2 & Phase == 1)
FI_WL <- d %>% filter(Population == 3 & sex == 1 & Phase == 1)

#Reversal
MR_AZ <- d %>% filter(Population == 1 & sex == 2 & Phase == 2 & skip == 0)
FR_AZ <- d %>% filter(Population == 1 & sex == 1 & Phase == 2)
MR_SB <- d %>% filter(Population == 2 & sex == 2 & Phase == 2 & skip == 0)
FR_SB <- d %>% filter(Population == 2 & sex == 1 & Phase == 2)
MR_WL <- d %>% filter(Population == 3 & sex == 2 & Phase == 2)
FR_WL <- d %>% filter(Population == 3 & sex == 1 & Phase == 2)

#Determine max trials by population, sex and phase

#Initial
Max_MI_AZ <- max(MI_AZ$Trial)
Max_FI_AZ <- max(FI_AZ$Trial)
Max_MI_SB <- max(MI_SB$Trial)
Max_FI_SB <- max(FI_SB$Trial)
Max_MI_WL <- max(MI_WL$Trial)
Max_FI_WL <- max(FI_WL$Trial)

#Reversal
Max_MR_AZ <- max(MR_AZ$Trial)
Max_FR_AZ <- max(FR_AZ$Trial)
Max_MR_SB <- max(MR_SB$Trial)
Max_FR_SB <- max(FR_SB$Trial)
Max_MR_WL <- max(MR_WL$Trial)
Max_FR_WL <- max(FR_WL$Trial)

#Ready matrices for for performing cumulative proportion switch counts across trials

#Initial
MI_AZ_X <- matrix(0,1,Max_MI_AZ)
FI_AZ_X <- matrix(0,1,Max_FI_AZ)
MI_SB_X <- matrix(0,1,Max_MI_SB)
FI_SB_X <- matrix(0,1,Max_FI_SB)
MI_WL_X <- matrix(0,1,Max_MI_WL)
FI_WL_X <- matrix(0,1,Max_FI_WL)

#Reversal
MR_AZ_X <- matrix(0,1,Max_MR_AZ)
FR_AZ_X <- matrix(0,1,Max_FR_AZ)
MR_SB_X <- matrix(0,1,Max_MR_SB)
FR_SB_X <- matrix(0,1,Max_FR_SB)
MR_WL_X <- matrix(0,1,Max_MR_WL)
FR_WL_X <- matrix(0,1,Max_FR_WL)

#Perform those calculations - for an in-depth explanation, see L76 - 105 in Figure_2.R

#Initial
for(i in 1:Max_MI_AZ){MI_AZ_X[1 , i] <- (sum(MI_AZ$switch[which(MI_AZ$Trial < i + 1)]) / length(MI_AZ$switch[which(MI_AZ$Trial < i + 1)])) } 
for(i in 1:Max_FI_AZ){FI_AZ_X[1 , i] <- (sum(FI_AZ$switch[which(FI_AZ$Trial < i + 1)]) / length(FI_AZ$switch[which(FI_AZ$Trial < i + 1)])) } 
for(i in 1:Max_MI_SB){MI_SB_X[1 , i] <- (sum(MI_SB$switch[which(MI_SB$Trial < i + 1)]) / length(MI_SB$switch[which(MI_SB$Trial < i + 1)])) } 
for(i in 1:Max_FI_SB){FI_SB_X[1 , i] <- (sum(FI_SB$switch[which(FI_SB$Trial < i + 1)]) / length(FI_SB$switch[which(FI_SB$Trial < i + 1)])) } 
for(i in 1:Max_MI_WL){MI_WL_X[1 , i] <- (sum(MI_WL$switch[which(MI_WL$Trial < i + 1)]) / length(MI_WL$switch[which(MI_WL$Trial < i + 1)])) } 
for(i in 1:Max_FI_WL){FI_WL_X[1 , i] <- (sum(FI_WL$switch[which(FI_WL$Trial < i + 1)]) / length(FI_WL$switch[which(FI_WL$Trial < i + 1)])) } 

#Reversal
for(i in 1:Max_MR_AZ){MR_AZ_X[1 , i] <- (sum(MR_AZ$switch[which(MR_AZ$Trial < i + 1)]) / length(MR_AZ$switch[which(MR_AZ$Trial < i + 1)])) } 
for(i in 1:Max_FR_AZ){FR_AZ_X[1 , i] <- (sum(FR_AZ$switch[which(FR_AZ$Trial < i + 1)]) / length(FR_AZ$switch[which(FR_AZ$Trial < i + 1)])) } 
for(i in 1:Max_MR_SB){MR_SB_X[1 , i] <- (sum(MR_SB$switch[which(MR_SB$Trial < i + 1)]) / length(MR_SB$switch[which(MR_SB$Trial < i + 1)])) } 
for(i in 1:Max_FR_SB){FR_SB_X[1 , i] <- (sum(FR_SB$switch[which(FR_SB$Trial < i + 1)]) / length(FR_SB$switch[which(FR_SB$Trial < i + 1)])) } 
for(i in 1:Max_MR_WL){MR_WL_X[1 , i] <- (sum(MR_WL$switch[which(MR_WL$Trial < i + 1)]) / length(MR_WL$switch[which(MR_WL$Trial < i + 1)])) } 
for(i in 1:Max_FR_WL){FR_WL_X[1 , i] <- (sum(FR_WL$switch[which(FR_WL$Trial < i + 1)]) / length(FR_WL$switch[which(FR_WL$Trial < i + 1)])) } 

#Averages & standard errors of cumulative switch counts reported in legend
se <- function(x) sd(x)/sqrt(length(x))

#Initial
mu_MI_AZ_X <- apply(MI_AZ_X, 1, mean)
se_MI_AZ_X <- apply(MI_AZ_X, 1, se)

mu_FI_AZ_X <- apply(FI_AZ_X, 1, mean)
se_FI_AZ_X <- apply(FI_AZ_X, 1, se)

mu_MI_SB_X <- apply(MI_SB_X, 1, mean)
se_MI_SB_X <- apply(MI_SB_X, 1, se)

mu_FI_SB_X <- apply(FI_SB_X, 1, mean)
se_FI_SB_X <- apply(FI_SB_X, 1, se)

mu_MI_WL_X <- apply(MI_WL_X, 1, mean)
se_MI_WL_X <- apply(MI_WL_X, 1, se)

mu_FI_WL_X <- apply(FI_WL_X, 1, mean)
se_FI_WL_X <- apply(FI_WL_X, 1, se)

#Reversal
mu_MR_AZ_X <- apply(MR_AZ_X, 1, mean)
se_MR_AZ_X <- apply(MR_AZ_X, 1, se)

mu_FR_AZ_X <- apply(FR_AZ_X, 1, mean)
se_FR_AZ_X <- apply(FR_AZ_X, 1, se)

mu_MR_SB_X <- apply(MR_SB_X, 1, mean)
se_MR_SB_X <- apply(MR_SB_X, 1, se)

mu_FR_SB_X <- apply(FR_SB_X, 1, mean)
se_FR_SB_X <- apply(FR_SB_X, 1, se)

mu_MR_WL_X <- apply(MR_WL_X, 1, mean)
se_MR_WL_X <- apply(MR_WL_X, 1, se)

mu_FR_WL_X <- apply(FR_WL_X, 1, mean)
se_FR_WL_X <- apply(FR_WL_X, 1, se)

#####################################################################################################################################################
#Figure S2
#####################################################################################################################################################


#pdf(file = "Figure_S2.pdf", height = 8, width = 8) #Turn on if want pdf

par(mfrow = c(3,2), mar = c(3,5,3,1), oma = c(0,1,3.5,0))

#Core

#Initial
plot(NULL, xlim = c(1,100),ylim = c(0,1), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,0.10,0.20,0.30,0.40,0.60,0.70,0.80,0.90), labels = c("0.0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at =c(1.0), cex.axis = 1.3)
axis(side = 2, at =c(0.5), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Initial learning", side = 3, adj = .5, cex = 1.2, line = 2.5, font = 2)
mtext("Core", side = 3, adj = .5, cex = 1.2, line = 0.5)
lines(1:Max_FI_AZ, FI_AZ_X[1, 1:Max_FI_AZ], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MI_AZ, MI_AZ_X[1, 1:Max_MI_AZ], lwd = 2, col = alpha(c("#5ec962"), .8))

#Reversal
plot(NULL, xlim = c(1,200),ylim = c(0,1), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,0.10,0.20,0.30,0.40,0.60,0.70,0.80,0.90), labels = c("0.0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at =c(1.0), cex.axis = 1.3)
axis(side = 2, at =c(0.5), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Reversal learning", side = 3, adj = .5, cex = 1.2, line = 2.5, font = 2)
mtext("Core", side = 3, adj = .5, cex = 1.2, line = 0.5)
lines(1:Max_FR_AZ, FR_AZ_X[1, 1:Max_FR_AZ], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MR_AZ, MR_AZ_X[1, 1:Max_MR_AZ], lwd = 2, col = alpha(c("#5ec962"), .8))

#Middle

#Initial
plot(NULL, xlim = c(1,100),ylim = c(0,1), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,0.10,0.20,0.30,0.40,0.60,0.70,0.80,0.90), labels = c("0.0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at =c(1.0), cex.axis = 1.3)
axis(side = 2, at =c(0.5), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Middle", side = 3, adj = .5, cex = 1.2, line = 0.5)
lines(1:Max_FI_SB, FI_SB_X[1, 1:Max_FI_SB], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MI_SB, MI_SB_X[1, 1:Max_MI_SB], lwd = 2, col = alpha(c("#5ec962"), .8))

#Reversal
plot(NULL, xlim = c(1,200),ylim = c(0,1), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,0.10,0.20,0.30,0.40,0.60,0.70,0.80,0.90), labels = c("0.0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at =c(1.0), cex.axis = 1.3)
axis(side = 2, at =c(0.5), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Middle", side = 3, adj = .5, cex = 1.2, line = 0.5)
lines(1:Max_FR_SB, FR_SB_X[1, 1:Max_FR_SB], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MR_SB, MR_SB_X[1, 1:Max_MR_SB], lwd = 2, col = alpha(c("#5ec962"), .8))

#Edge

#Initial
plot(NULL, xlim = c(1,100),ylim = c(0,1), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","20","","40","","60","","80","","100"), cex.axis = 1.3)
axis(side = 2, at = c(0,0.10,0.20,0.30,0.40,0.60,0.70,0.80,0.90), labels = c("0.0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at =c(1.0), cex.axis = 1.3)
axis(side = 2, at =c(0.5), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Edge", side = 3, adj = .5, cex = 1.2, line = 0.5)
lines(1:Max_FI_WL, FI_WL_X[1, 1:Max_FI_WL], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MI_WL, MI_WL_X[1, 1:Max_MI_WL], lwd = 2, col = alpha(c("#5ec962"), .8))

#Reversal
plot(NULL, xlim = c(1,200),ylim = c(0,1), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, xlab = NA, ylab = NA,  xaxt = "none")
axis(side = 1, at = c(1,20,40,60,80,100,120,140,160,180,200), labels = c("Trial 1","","40","","80","","120","","160","","200"), cex.axis = 1.3)
axis(side = 2, at = c(0,0.10,0.20,0.30,0.40,0.60,0.70,0.80,0.90), labels = c("0.0","","","","","","","",""), cex.axis = 1.3)
axis(side = 2, at =c(1.0), cex.axis = 1.3)
axis(side = 2, at =c(0.5), cex.axis = 1.3)
mtext("Cum. prop. switches",  cex = 1.2, side = 2, line = 3)
mtext("Edge", side = 3, adj = .5, cex = 1.2, line = 0.5)
lines(1:Max_FR_WL, FR_WL_X[1, 1:Max_FR_WL], lwd = 2, col = alpha(c("#fde725"),.8))
lines(1:Max_MR_WL, MR_WL_X[1, 1:Max_MR_WL], lwd = 2, col = alpha(c("#5ec962"), .8))

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males", "Females"), fill = c("#5ec962","#fde725"), text.width = .13, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf

#End script
#####################################################################################################################################################





