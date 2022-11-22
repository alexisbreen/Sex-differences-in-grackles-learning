#####################################################################################################################################################

#Code for plotting Supplementary Figure 2 for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################

#Load data if not already in global environment (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#Required package
library(tidyverse)

#####################################################################################################################################################
#Pre-graph processing
#####################################################################################################################################################

#Filter out extra learning trials
d <- d %>% filter(Criterion != 2)

#Empty vectors
switch_initial  <- rep(NA, length(unique(d$id)))
switch_reversal <- rep(NA, length(unique(d$id)))
sex <-  rep(NA, length(unique(d$id)))
bird <- rep(NA, length(unique(d$id)))
skip_initial <- rep(NA, length(unique(d$id)))
skip_reversal <- rep(NA, length(unique(d$id)))

#Fill empty vectors
for (id in 1:max(d$id)) {
  switch_initial[id] <- sum(d$switch[which(d$id == id & d$Phase == 1)])
  switch_reversal[id] <- sum(d$switch[which(d$id == id & d$Phase == 2)])
  sex[id] <- unique(d$sex[d$id ==id])
  bird[id] <- unique(d$id[d$id == id])
  skip_initial[id] <- max(d$skip[which(d$id == id & d$Phase == 1)])
  skip_reversal[id] <- max(d$skip[which(d$id == id & d$Phase == 2)])
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
SI_F <- d_switch %>% group_by(bird) %>% filter(phase == 1 & sex == 1) %>% mutate(sex = ifelse(sex == 2, 1, 2))
SI_M <- d_switch %>% group_by(bird) %>% filter(phase == 1 & sex == 2) %>% mutate(sex = ifelse(sex == 2, 1, 2))
SR_F <- d_switch %>% group_by(bird) %>% filter(phase == 2 & sex == 1) %>% mutate(sex = ifelse(sex == 2, 1, 2))
SR_M <- d_switch %>% group_by(bird) %>% filter(phase == 2 & sex == 2 & skip == 0) %>% mutate(sex = ifelse(sex == 2, 1, 2))

#Calculate means
SI_M_mu <- mean(SI_M$switches)
SI_F_mu <- mean(SI_F$switches)
SR_M_mu <- mean(SR_M$switches)
SR_F_mu <- mean(SR_F$switches)

#Calculate medians
SI_M_md <- median(SI_M$switches)
SI_F_md <- median(SI_F$switches)
SR_M_md <- median(SR_M$switches)
SR_F_md <- median(SR_F$switches)

#Combine means and medians
S_mu <- rbind(SI_M_mu, SI_F_mu, SR_M_mu, SR_F_mu)
S_md <- rbind(SI_M_md, SI_F_md, SR_M_md, SR_F_md)

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

#####################################################################################################################################################
#Supplementary Figure 2
#####################################################################################################################################################

#pdf(file = "Fig_S2.pdf", height = 4, width = 10) #turn on if want pdf of plot

#Set-up plot space
par(mfrow = c(1,6), mar = c(3,5,3,2), oma = c(3,2,5,0))
layout(matrix(c(1,1,2,3,3,4),nrow=1,ncol=6,byrow=TRUE))

#Initial learning switches
par(mar = c(3,5,3,2.5))

plot(NULL, xlim = c(0.5,2.5), ylim = c(0,50), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4), labels = c("M", "F","M", "F"), cex.axis = 1.3)
axis(side = 2, at = c(0,10,20,30,40,50), labels = c("0","10","20","30","40","50"), cex.axis = 1.3)
mtext("Total switches",  cex = 1.2, side = 2, line = 3)
mtext("Initial learning", side = 3, at = 1.5, cex = 1.2, line = 1)
points(jitter(SI_M$sex, 3.5), SI_M$switches, col = alpha(c("#5ec962"),0.8), cex = 2)
points(jitter(SI_F$sex, 3.5), SI_F$switches, col = alpha(c("#fde725"),0.8), cex = 2)
points(S_mu[1:2,], pch = 2, col = "red", cex = 2)
points(S_md[1:2,], pch = 4, col = "red", cex = 2)
abline(h = 0, lty = 2, col = "black")
mtext("a", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.25)

#Posterior
par(mar = c(3,2.5,3,2))

data_init <- list(
  
  male = density(s_switch_xpop$init_M),
  female = density(s_switch_xpop$init_F),
  across = density(s_switch_xpop$init_X)
  
)

stacked.density(data_init, fac = 4, height = 1.5, col=c("black", "#fde725", "#5ec962"), alpha=0.2, show.xaxis = F,
                left = -100, bottom = 1, right = 150, top = 2,)
abline(v = 0, lty = 5)
axis(side = 1, at = c(0), labels = c("0"), cex.axis = 1.3)
axis(side = 2, at = c(1,2,3), labels = c("M-F", "F", "M"), las = 2, cex.axis = 1.3)
mtext("Posterior", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Density",  cex = 1.2, side = 2, line = 3)
mtext("b", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.6)

#Reversal learning switches
par(mar = c(3,5,3,2.5))

plot(NULL, xlim = c(0.5,2.5), ylim = c(0,100), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4), labels = c("M", "F","M", "F"), cex.axis = 1.3)
axis(side = 2, at = c(0,20,40,60,80,100), labels = c("0","20","40","60","80","100"), cex.axis = 1.3)
mtext("Total switches",  cex = 1.2, side = 2, line = 3)
mtext("Reversal learning", side = 3, at = 1.5, cex = 1.2, line = 1)
points(jitter(SR_M$sex, 3.5), SR_M$switches, col = alpha(c("#5ec962"),0.8), cex = 2)
points(jitter(SR_F$sex, 3.5), SR_F$switches, col = alpha(c("#fde725"),0.8), cex = 2)
points(S_mu[3:4,], pch = 2, col = "red", cex = 2)
points(S_md[3:4,], pch = 4, col = "red", cex = 2)
abline(h = 0, lty = 2, col = "black")
mtext("c", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.25)

#Posterior
par(mar = c(3,2.5,3,2))

data_rev <- list(
  
  male = density(s_switch_xpop$init_M),
  female = density(s_switch_xpop$rev_F),
  across = density(s_switch_xpop$rev_X)
  
  
)

stacked.density(data_rev, fac = 4.5, height = 1.5, col=c("black", "#fde725", "#5ec962"), alpha=0.2, show.xaxis = F,
                left = -100, bottom = 1, right = 150, top = 2,)
abline(v = 0, lty = 5)
axis(side = 1, at = c(0), labels = c("0"), cex.axis = 1.3)
axis(side = 2, at = c(1,2,3), labels = c("M-F", "F", "M"), las = 2, cex.axis = 1.3)
mtext("Posterior", side = 3, adj = .5, cex = 1.2, line = 1)
mtext("Density",  cex = 1.2, side = 2, line = 3)
mtext("d", side = 3, cex = 1.2, line = 1, las = 1, font = 2, adj = -.6)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males (M)", "Females (F)", "Males-Females (M-F)"), fill = alpha(c("#5ec962","#fde725", "black"), 0.8), text.width = .23, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #Turn on if want pdf of plot

#End script
#####################################################################################################################################################