#####################################################################################################################################################

#Code for plotting heatmap in Figure 3 for the manuscript

#Risk-sensitive learning is a winning strategy for leading an urban invasion

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#Required packages
library(fields)

#NOTE: Evolutionary_model.R script has to be run before executing below! 
#This may take very long if you don't have access to a computing cluster, but you can also change to less generations/smaller populations

#Remove first 2000 time steps and calculate mean for each parameter combination

result <- result_Grackle_evo
Nsim <- unique(seq$Nsim)
burn_in <- 2000

MeanPhi <- matrix(NA, nrow = nrow(seq), ncol = Nsim )
MeanLambda <- matrix(NA, nrow = nrow(seq), ncol = Nsim )

for (i in 1:nrow(seq)){
  for (j in 1:Nsim) {
    result[[i]][[j]]$avg_phi <- result[[i]][[j]]$avg_phi[-(1:burn_in)]
    result[[i]][[j]]$avg_lambda <- result[[i]][[j]]$avg_lambda[-(1:burn_in)]

    MeanPhi[i,j] <- mean(result[[i]][[j]]$avg_phi)
    MeanLambda[i,j] <- mean(result[[i]][[j]]$avg_lambda)
  }
}

OverallPhi <- apply(MeanPhi, 1, mean)
OverallLambda <- apply(MeanLambda, 1, mean)

####
###
##
# Plotting script
##
###
####

#graphics.off()
#pdf("Figure_4.pdf", height = 6.3, width = 14.17)

par(mfrow = c(1,2),
    mar = c(2,6,1,1),
    oma = c(5,1,1,4))

colors <- colorRampPalette(c("black", "darkred", "red","orange", "yellow", "lightyellow"))

z <- matrix(NA, nrow = length(unique(seq$u)), ncol = length(unique(seq$s)) )
for (i in unique(seq$u)) {
  for (j in unique(seq$s)) {
    z[which(unique(seq$u)==i),  which(unique(seq$s)==j)] <- OverallPhi[which(seq$u==i & seq$s==j) ]
  }
}

image(1:7, 1:7 ,z, col=colors(1000), zlim= c(0,1),xaxt="n", yaxt = "n", xlab="", ylab="")
axis(side=1, at=1:7, labels=c("0.0001","0.0005","0.001","0.005","0.01","0.05","0.1"))
axis(side=2, at=1:7, labels=unique(seq$s))
mtext(side = 3, line = 0, expression(paste("Information updating ",italic(phi))), cex = 2)

mtext(side = 1, line = 4, expression(paste("Environmental stability ",italic(u))), cex = 2, outer = TRUE)
mtext(side = 2, line = -0.8, expression(paste("Environmental stochasticity ",italic(s))), cex = 2, outer = TRUE)

z <- matrix(NA, nrow = length(unique(seq$u)), ncol = length(unique(seq$s)) )
for (i in unique(seq$u)) {
  for (j in unique(seq$s)) {
    z[which(unique(seq$u)==i),  which(unique(seq$s)==j)] <- OverallLambda[which(seq$u==i & seq$s==j) ]
    
  }
}

image(1:7, 1:7 ,z, col=colors(1000), zlim= c(7,12),xaxt="n", yaxt = "n", xlab="", ylab="")
axis(side=1, at=1:7, labels=c("0.0001","0.0005","0.001","0.005","0.01","0.05","0.1"), cex = 0.8)
axis(side=2, at=1:7, labels=unique(seq$s))
mtext(side = 3, line = 0, expression(paste("Risk sensitivity ",italic(lambda))), cex = 2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", c("1","", "0.9","", "0.8","", "0.7","","0.6","","0.5","","0.4","","0.3","","0.2","","0.1", "","0"), col= colors(1000)[rev(c(1,50,100,150,200,250, 300,350,400,450,500,550,600,650,700,750,800,850,900,950, 1000))], xpd = TRUE, inset = c(0, 0),bty="n", pch=15,cex = 1.2, pt.cex = 3.2)
legend("topright", c("12","", "11.5","", "11","", "10.5","","10","","9.5","","9","","8.5","","8","","7.5", "","7"), col= colors(1000)[rev(c(1,50,100,150,200,250, 300,350,400,450,500,550,600,650,700,750,800,850,900,950, 1000))], xpd = TRUE, inset = c(0, 0),bty="n", pch=15,cex = 1.2, pt.cex = 3.2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(1:10, 1:10, type = "n", bty = "n", xaxt = "n", yaxt = "n")
polygon(c(1.2,1.2,0.9), c(2.65,9.74,9.74), col = "grey20", border = "grey20")

polygon(c(3.35,7.2,3.35), c(1.8,1.3,1.3), col = "grey20", border = "grey20")

#dev.off()

#End script
#####################################################################################################################################################
