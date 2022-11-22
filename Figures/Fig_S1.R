#####################################################################################################################################################

#Code for plotting Supplementary Figure 1 for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#Required package
library(tidyverse)

#####################################################################################################################################################
#Supplementary Figure 1
#####################################################################################################################################################

#pdf(file = "Fig_S1.pdf", height = 8, width = 10) #turn on if want pdf of plot

#Set-up plot space
par(mfrow = c(1,2), mar = c(3,5,3,2), oma = c(0,0,4,0))

#Individual-level forward simulations
dat_ind_sim <- Post_Study_Sim_Fxn(N_F = 10, N_M = 10, N_sim = 1, mu_on = 1)

#Wrangle simulated individual-level data into sex and phase-specific data frames
male_ind_sim_init <- dat_ind_sim %>% filter(sex == 2 & Phase == 1)
male_ind_sim_rev <- dat_ind_sim %>% filter(sex == 2 & Phase == 2)
female_ind_sim_init <- dat_ind_sim %>% filter(sex == 1 & Phase == 1)
female_ind_sim_rev <- dat_ind_sim %>% filter(sex == 1 & Phase == 2)

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(male_ind_sim_init)){
  if(male_ind_sim_init$id[i] == 11){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 3, 4)
  } else if(male_ind_sim_init$id[i] == 12){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 7, 8)
  } else if(male_ind_sim_init$id[i] == 13){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 11, 12)
  } else if(male_ind_sim_init$id[i] == 14){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 15, 16)
  } else if(male_ind_sim_init$id[i] == 15){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 19, 20)
  } else if(male_ind_sim_init$id[i] == 16){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 23, 24)
  } else if(male_ind_sim_init$id[i] == 17){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 27, 28)
  } else if(male_ind_sim_init$id[i] == 18){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 31, 32)
  } else if(male_ind_sim_init$id[i] == 19){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 35, 36)
  } else if(male_ind_sim_init$id[i] == 20){
    male_ind_sim_init$Choice[i] <- ifelse(male_ind_sim_init$Choice[i] == 1, 39, 40)
  }
}

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(male_ind_sim_rev)){
  if(male_ind_sim_rev$id[i] == 11){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 3, 4)
  } else if(male_ind_sim_rev$id[i] == 12){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 7, 8)
  } else if(male_ind_sim_rev$id[i] == 13){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 11, 12)
  } else if(male_ind_sim_rev$id[i] == 14){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 15, 16)
  } else if(male_ind_sim_rev$id[i] == 15){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 19, 20)
  } else if(male_ind_sim_rev$id[i] == 16){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 23, 24)
  } else if(male_ind_sim_rev$id[i] == 17){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 27, 28)
  } else if(male_ind_sim_rev$id[i] == 18){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 31, 32)
  } else if(male_ind_sim_rev$id[i] == 19){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 35, 36)
  } else if(male_ind_sim_rev$id[i] == 20){
    male_ind_sim_rev$Choice[i] <- ifelse(male_ind_sim_rev$Choice[i] == 1, 39, 40)
  }
}

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(female_ind_sim_init)){
  if(female_ind_sim_init$id[i] == 2){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 5, 6)
  } else if(female_ind_sim_init$id[i] == 3){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 9, 10)
  } else if(female_ind_sim_init$id[i] == 4){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 13, 14)
  } else if(female_ind_sim_init$id[i] == 5){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 17, 18)
  } else if(female_ind_sim_init$id[i] == 6){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 21, 22)
  } else if(female_ind_sim_init$id[i] == 7){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 25, 26)
  } else if(female_ind_sim_init$id[i] == 8){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 29, 30)
  } else if(female_ind_sim_init$id[i] == 9){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 33, 34)
  } else if(female_ind_sim_init$id[i] == 10){
    female_ind_sim_init$Choice[i] <- ifelse(female_ind_sim_init$Choice[i] == 1, 37, 38)
  } 
}

#Because plotting all birds along y-axis, need to shift choice-scores up
for(i in 1:nrow(female_ind_sim_rev)){
  if(female_ind_sim_rev$id[i] == 2){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 5, 6)
  } else if(female_ind_sim_rev$id[i] == 3){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 9, 10)
  } else if(female_ind_sim_rev$id[i] == 4){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 13, 14)
  } else if(female_ind_sim_rev$id[i] == 5){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 17, 18)
  } else if(female_ind_sim_rev$id[i] == 6){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 21, 22)
  } else if(female_ind_sim_rev$id[i] == 7){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 25, 26)
  } else if(female_ind_sim_rev$id[i] == 8){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 29, 30)
  } else if(female_ind_sim_rev$id[i] == 9){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 33, 34)
  } else if(female_ind_sim_rev$id[i] == 10){
    female_ind_sim_rev$Choice[i] <- ifelse(female_ind_sim_rev$Choice[i] == 1, 37, 38)
  } 
}

#And get switch counts for plotting
switch_init <- matrix(NA, 1, 20)
switch_rev <- matrix(NA, 1, 20)
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

#Initial
plot(NULL, xlim = c(1,100), ylim = c(1.5,40.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.2, cex.sub = 1.3)
axis(side = 2, at = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39), labels = c("+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+"), cex.axis = 1.2)
axis(side = 2, at = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40), labels = c("-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"), cex.axis = 1.2)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","","","","","","","","",""), cex.axis = 1.2)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.2)
mtext("Choice",  cex = 1.2, side = 2, line = 3)
for(id in min(female_ind_sim_init$id):max(female_ind_sim_init$id)) lines(x = female_ind_sim_init$trialafter[which(female_ind_sim_init$id == id)], y = female_ind_sim_init$Choice[which(female_ind_sim_init$id == id)], col = alpha("#fde725", 0.8), lwd = 1.5)
for(id in min(male_ind_sim_init$id):max(male_ind_sim_init$id)) lines(x = male_ind_sim_init$trialafter[which(male_ind_sim_init$id == id)], y = male_ind_sim_init$Choice[which(male_ind_sim_init$id == id)], col = alpha("#5ec962", 0.8), lwd = 1.5)
mtext("Typical 'birds'", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Initial learning", side = 3, adj = .5, cex = 1.2, line = 3, font = 2)
#Dashed
abline(h = 6.5, lty = 2)
abline(h = 10.5, lty = 2)
abline(h = 14.5, lty = 2)
abline(h = 18.5, lty = 2)
abline(h = 22.5, lty = 2)
abline(h = 26.5, lty = 2)
abline(h = 30.5, lty = 2)
abline(h = 34.5, lty = 2)
abline(h = 38.5, lty = 2)
#Solid
abline(h = 2.5, lty = 2)
abline(h = 4.5, lty = 1)
abline(h = 8.5, lty = 1)
abline(h = 12.5, lty = 1)
abline(h = 16.5, lty = 1)
abline(h = 20.5, lty = 1)
abline(h = 24.5, lty = 1)
abline(h = 28.5, lty = 1)
abline(h = 32.5, lty = 1)
abline(h = 36.5, lty = 1)
#Switches
text(x = 93, y = 41, labels = "Switches", cex = 1)
text(x = 95, y = 1.5, labels = switch_init[,1], cex = 1)
text(x = 95, y = 3.5, labels = switch_init[,11], cex = 1)
text(x = 95, y = 5.5, labels = switch_init[,2], cex = 1)
text(x = 95, y = 7.5, labels = switch_init[,12], cex = 1)
text(x = 95, y = 9.5, labels = switch_init[,3], cex = 1)
text(x = 95, y = 11.5, labels = switch_init[,13], cex = 1)
text(x = 95, y = 13.5, labels = switch_init[,4], cex = 1)
text(x = 95, y = 15.5, labels = switch_init[,14], cex = 1)
text(x = 95, y = 17.5, labels = switch_init[,5], cex = 1)
text(x = 95, y = 19.5, labels = switch_init[,15], cex = 1)
text(x = 95, y = 21.5, labels = switch_init[,6], cex = 1)
text(x = 95, y = 23.5, labels = switch_init[,16], cex = 1)
text(x = 95, y = 25.5, labels = switch_init[,7], cex = 1)
text(x = 95, y = 27.5, labels = switch_init[,17], cex = 1)
text(x = 95, y = 29.5, labels = switch_init[,8], cex = 1)
text(x = 95, y = 31.5, labels = switch_init[,18], cex = 1)
text(x = 95, y = 33.5, labels = switch_init[,9], cex = 1)
text(x = 95, y = 35.5, labels = switch_init[,19], cex = 1)
text(x = 95, y = 37.5, labels = switch_init[,10], cex = 1)
text(x = 95, y = 39.5, labels = switch_init[,20], cex = 1) 

#Reversal
plot(NULL, xlim = c(1,100), ylim = c(1.5,40.5),  xlab = NA, ylab = NA, xaxt = "none", yaxt="none", cex.lab = 1.3, cex.axis = 1.2, cex.sub = 1.3)
axis(side = 2, at = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39), labels = c("-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"), cex.axis = 1.2)
axis(side = 2, at = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40), labels = c("+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+"), cex.axis = 1.2)
axis(side = 1, at = c(1,10,20,30,40,50,60,70,80,90,100), labels = c("Trial 1","","","","","","","","","",""), cex.axis = 1.2)
axis(side = 1, at = c(100), labels = c("100"), cex.axis = 1.2)
mtext("Choice",  cex = 1.2, side = 2, line = 3)
for(id in min(female_ind_sim_rev$id):max(female_ind_sim_rev$id)) lines(x = female_ind_sim_rev$trialafter[which(female_ind_sim_rev$id == id)], y = female_ind_sim_rev$Choice[which(female_ind_sim_rev$id == id)], col = alpha("#fde725", 0.8), lwd = 1.5)
for(id in min(male_ind_sim_rev$id):max(male_ind_sim_rev$id)) lines(x = male_ind_sim_rev$trialafter[which(male_ind_sim_rev$id == id)], y = male_ind_sim_rev$Choice[which(male_ind_sim_rev$id == id)], col = alpha("#5ec962", 0.8), lwd = 1.5)
mtext("Typical 'birds'", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Reversal learning", side = 3, adj = .5, cex = 1.2, line = 3, font = 2)
#Dashed
abline(h = 6.5, lty = 2)
abline(h = 10.5, lty = 2)
abline(h = 14.5, lty = 2)
abline(h = 18.5, lty = 2)
abline(h = 22.5, lty = 2)
abline(h = 26.5, lty = 2)
abline(h = 30.5, lty = 2)
abline(h = 34.5, lty = 2)
abline(h = 38.5, lty = 2)
#Solid
abline(h = 2.5, lty = 2)
abline(h = 4.5, lty = 1)
abline(h = 8.5, lty = 1)
abline(h = 12.5, lty = 1)
abline(h = 16.5, lty = 1)
abline(h = 20.5, lty = 1)
abline(h = 24.5, lty = 1)
abline(h = 28.5, lty = 1)
abline(h = 32.5, lty = 1)
abline(h = 36.5, lty = 1)
#Switches
text(x = 93, y = 41, labels = "Switches", cex = 1)
text(x = 95, y = 1.5, labels = switch_rev[,1], cex = 1)
text(x = 95, y = 3.5, labels = switch_rev[,11], cex = 1)
text(x = 95, y = 5.5, labels = switch_rev[,2], cex = 1)
text(x = 95, y = 7.5, labels = switch_rev[,12], cex = 1)
text(x = 95, y = 9.5, labels = switch_rev[,3], cex = 1)
text(x = 95, y = 11.5, labels = switch_rev[,13], cex = 1)
text(x = 95, y = 13.5, labels = switch_rev[,4], cex = 1)
text(x = 95, y = 15.5, labels = switch_rev[,14], cex = 1)
text(x = 95, y = 17.5, labels = switch_rev[,5], cex = 1)
text(x = 95, y = 19.5, labels = switch_rev[,15], cex = 1)
text(x = 95, y = 21.5, labels = switch_rev[,6], cex = 1)
text(x = 95, y = 23.5, labels = switch_rev[,16], cex = 1)
text(x = 95, y = 25.5, labels = switch_rev[,7], cex = 1)
text(x = 95, y = 27.5, labels = switch_rev[,17], cex = 1)
text(x = 95, y = 29.5, labels = switch_rev[,8], cex = 1)
text(x = 95, y = 31.5, labels = switch_rev[,18], cex = 1)
text(x = 95, y = 33.5, labels = switch_rev[,9], cex = 1)
text(x = 95, y = 35.5, labels = switch_rev[,19], cex = 1)
text(x = 95, y = 37.5, labels = switch_rev[,10], cex = 1)
text(x = 95, y = 39.5, labels = switch_rev[,20], cex = 1)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males (M)", "Females (F)"), fill = alpha(c("#5ec962","#fde725"), 0.8), text.width = .23, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.2)

#dev.off() #Turn on if want pdf of plot

#End script
#####################################################################################################################################################