#####################################################################################################################################################

#Code for plotting Supplementary Figure 1 for the manuscript

#Risk-sensitive learning is a winning strategy for leading an urban invasion

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#NOTE: RL_Execution.R script has to be run before executing below!

#pdf(file = "Fig_S1.pdf", height = 7.3, width = 10) #turn on if want pdf of plot

#Set-up plot space
par(mfrow = c(2,2), mar = c(3,5,3,2), oma = c(3,0,5,0))

#Panel A - updating rate without and with extra learning trials 

#Treatments go along x-axis at these points (for this and all following plots)
x <- c(1:10) 

#Phi - no extra initial learning trials
s_phi_init_plot <- s_phi_init[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_phi_init <- apply(s_phi_init_plot, 2, mean)
HPDI_phi_init <- apply(s_phi_init_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.4,.4), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3), labels = c("-0.4","","","","0","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.4), labels = c("0.4"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 3)
mtext("Initial learning excluding extra trials", side = 3, adj = .5, cex = 1.2, line = 3, font = 2)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_init_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_phi_init[1,], x1 = x, y1 = HPDI_phi_init[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_phi_init, col = "red", pch = 19)

#Phi - extra learning trials

#Calculate mean & HPDI
mu_phi_init2 <- apply(s_phi_init2, 2, mean)
HPDI_phi_init2 <- apply(s_phi_init2, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-.4,.4), xaxt = "none", yaxt = "none", cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3), labels = c("-0.4","","","","0","","",""), cex.axis = 1.3)
axis(side = 2, at = c(0.4), labels = c("0.4"), cex.axis = 1.3)
mtext(expression(paste("Information updating ", italic(phi))),  cex = 1.2, side = 2, line = 3)
mtext("Initial learning including extra trials", side = 3, adj = .5, cex = 1.2, line = 3, font = 2)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1)
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_phi_init2[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_phi_init2[1,], x1 = x, y1 = HPDI_phi_init2[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_phi_init2, col = "red", pch = 19)

#Lambda - no extra learning trials
s_L_init_plot <- s_L_init[, -10:-11] #drop across-pop male & female estimates b/c only plot contrast

#Calculate mean & HPDI
mu_L_init <- apply(s_L_init_plot, 2, mean)
HPDI_L_init <- apply(s_L_init_plot, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-11,11), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-10,-5,0,5,10), labels = c("-10","-5","0","5","10"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 3)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1, col = "black")
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_init_plot[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_L_init[1,], x1 = x, y1 = HPDI_L_init[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_L_init, col = "red", pch = 19)

#Lambda - extra learning traisl 

#Calculate mean & HPDI
mu_L_init2 <- apply(s_L_init2, 2, mean)
HPDI_L_init2 <- apply(s_L_init2, 2, HPDI)

plot(x = NULL, y = NULL, xlim = c(0.8,10.1), ylim = c(-11,11), cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.sub = 1.3, frame = TRUE, ylab = NA, xaxt = "n", xlab = NA)
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("M", "F", "M-F","M", "F", "M-F","M", "F", "M-F", "M-F"), cex.axis = 1.3)
axis(side = 2, at = c(-10,-5,0,5,10), labels = c("-10","-5","0","5","10"), cex.axis = 1.3)
mtext(expression(paste("Risk sensitivity ", italic(lambda))),  cex = 1.2, side = 2, line = 3)
mtext("Core", side = 3, at = 2, cex = 1.2, line = 1)
mtext("Middle", side = 3, at = 5, cex = 1.2, line = 1)
mtext("Edge", side = 3, at = 8, cex = 1.2, line = 1)
mtext("Across", side = 3, at = 10, cex = 1.2, line = 1)
abline(v = c(3.5,6.5,9.5), lty = 1, col = "black")
abline(h = c(0), lty = 5, col = "black")
for(i in 1:100) points(jitter(x, 1), s_L_init2[i, ], col = alpha(c("#5ec962","#fde725","black","#5ec962","#fde725","black","#5ec962","#fde725","black","black"),0.2)) 
arrows(x0 = x, y0 = HPDI_L_init2[1,], x1 = x, y1 = HPDI_L_init2[2,], length = 0, col = "red", lwd = 1)
points(x = x, y = mu_L_init2, col = "red", pch = 19)

#Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("top", c("Males (M)", "Females (F)", "Males-Females (M-F)"), fill = alpha(c("#5ec962","#fde725", "black"), 0.8), text.width = .23, bty = "n", xjust = 0.05, x.intersp = 0.5, horiz=TRUE, xpd = NA, cex = 1.6)

#dev.off() #turn on if want pdf of plot

#End script
#####################################################################################################################################################