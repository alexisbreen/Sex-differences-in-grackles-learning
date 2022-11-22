#####################################################################################################################################################

#Code for collating, cleaning, and curating the utilised grackle data for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

library(tidyverse)

#Load original data sheets
dat_AZ <- read.csv(file.choose(), header = T) 
dat_SB <- read.csv(file.choose(), header = T) 
dat_WL <- read.csv(file.choose(), header = T) 

#Select relevant columns
d_AZ <- dat_AZ[ ,c(4,6,8,11)] 
d_SB <- dat_SB[ ,c(3:5,8)] 
d_WL <- dat_WL[ ,c(5,6,8,11)] 

#Make column names consistent
c <- c("ID", "Phase", "Trial", "Correct")
colnames(d_AZ) <- c
colnames(d_SB) <- c
colnames(d_WL) <- c

#Drop non-relevant rows i.e., refresher trials, non-choice cases &/or serial reversal phases
d_AZ <- d_AZ[!(d_AZ$Phase > 1),] 
d_AZ <- d_AZ[!(d_AZ$Correct == -1),] 
d_SB <- d_SB[!(d_SB$Phase == "Refresher"),] 
d_WL <- d_WL[!(d_WL$Phase > 1),] 
d_WL <- d_WL[!(d_WL$Correct == -1),] 

#Rename experimental phase
d_AZ$Phase <- ifelse(d_AZ$Phase == 0, 1, 2) 
d_SB$Phase <- ifelse(d_SB$Phase == "Initial", 1, 2) 
d_WL$Phase <- ifelse(d_WL$Phase == 0, 1, 2) 

#Add population designator
d_AZ$Population <- rep(1, times = nrow(d_AZ)) 
d_SB$Population <- rep(2, times = nrow(d_SB)) 
d_WL$Population <- rep(3, times = nrow(d_WL))

#Find and drop birds that were dropped from initial phase due to lack of participation i.e., not in phase 2
table(d_AZ$ID, d_AZ$Phase)
d_AZ <- d_AZ[!(d_AZ$ID == "Chimichanga" | d_AZ$ID == "Concha" | d_AZ$ID == "Elote" | d_AZ$ID == "Fresca" | d_AZ$ID == "Paella" | d_AZ$ID == "Pupusa"),] 
table(d_SB$ID, d_SB$Phase) #none
table(d_WL$ID, d_WL$Phase)
d_WL <- d_WL[!(d_WL$ID == "Carlota" | d_WL$ID == "Chocolate" | d_WL$ID == "Wachil" | d_WL$ID == "Xango"),]

#Reset row number after dropping; if not, error check below returns wrong rows
rownames(d_AZ) <- NULL
rownames(d_SB) <- NULL
rownames(d_WL) <- NULL

#Write function to check for data entry errors: Phase cannot exceed 2; Trial must be continuous count; Correct cannot exceed 1
Check_Entry_Errors <- function(df){
  for(i in 1:nrow(df)){ 
    if(df$Trial[i] > 1){
      if(df$Phase[i] != 1 & df$Phase[i] != 2){print(i)} 
      if((df$Trial[i] - df$Trial[i - 1]) != 1){print(i)}
      if(df$Correct[i] != 0 & df$Correct[i] != 1){print(i)}
    } else {
      if(df$Phase[i] != 1 & df$Phase[i] != 2){print(i)}
      if(df$Trial[i] != 1){print(i)}
      if(df$Correct[i] != 0 & df$Correct[i] != 1){print(i)}
    }
  }
}

#Data entry error check for d_AZ
Check_Entry_Errors(d_AZ)

#Above data entry error check for d_AZ returned following message: "Error in df$Trial[i] - df$Trial[i - 1] : non-numeric argument to binary operator"   
#This indicates that the variable-types need recoding
#Look at variable type structure
str(d_AZ)

#Trial and Correct need to be numeric-type rather than character-type
d_AZ$Trial <- as.numeric(d_AZ$Trial)
d_AZ$Correct <- as.numeric(d_AZ$Correct) 

#Above variable-type conversion returned following message: "Warning message : NAs introduced by coercion"
#This indicates the presence of NA(s) in data sheet 
#Check for NA(s)
which(is.na(d_AZ$Trial))
which(is.na(d_AZ$Correct))

#For both Trial & Correct, NA found in row 1151 b/c dash given for a motivation test i.e., non-trial
#Remove this row, reset row number & recheck entire data sheet
d_AZ <- d_AZ[-(which(is.na(d_AZ$Trial))),] 
rownames(d_AZ) <- NULL 
which(is.na(d_AZ)) #Returns integer(0) i.e., R saying no NAs found in data sheet

#Resume data entry error check for d_AZ
Check_Entry_Errors(d_AZ)

#Rows 46, 53, 454, 455, 656, 810 & 1773 flagged in error check, either b/c of a number jump or repeat in Trial column: jump, repeat, repeat, jump, jump, jump, jump, respectively.  
#Drop Trial column with errors, create new clean Trial column & recheck for errors
d_AZ <- d_AZ[, -3] 
d_AZ <- d_AZ %>% group_by(ID, Phase) %>% mutate(Trial = row_number())
Check_Entry_Errors(d_AZ) #None found - d_AZ now clean!

#Variable-type, NA cases, and entry error check for d_SB
str(d_SB) #Variable-type OK
which(is.na(d_SB)) #Returns integer(0) i.e., R saying no NAs found in data sheet
Check_Entry_Errors(d_SB) #None found - d_SB clean!

#Variable-type, NA cases, and data entry error check for d_SB
str(d_WL) #Variable-type OK
which(is.na(d_WL)) #Returns integer(0) i.e., R saying no NAs found in data sheet
Check_Entry_Errors(d_WL) 

#Rows 1956 & 1957 flagged in data error check b/c of number jump followed by repeat in Trial column
#Drop Trial column with errors, create new clean Trial column & recheck for errors
d_WL <- d_WL[, -3] 
d_WL <- d_WL %>% group_by(ID, Phase) %>% mutate(Trial = row_number())
Check_Entry_Errors(d_WL) #None found - d_WL now clean!

#Combine cleaned data sheets for analysis
dat <- rbind(d_AZ, d_SB, d_WL)

#Add unique bird number identifier & reorder by bird number
dat$id <- sapply(1:nrow(dat), function(i) which(unique(dat$ID) == dat$ID[i]))
dat <- dat[with(dat, order(dat$id)), ]

#Determine colour of chosen tube variable for each bird within each phase
dat$Choice <- NA
for (i in 1: nrow(dat)) {
  if (dat$Phase[i] == 1){
    if (dat$Correct[i] == 1){
      dat$Choice[i] <- 1
    } else {
      dat$Choice[i] <- 2
    } 
  } else {
    if (dat$Correct[i] == 1){
      dat$Choice[i] <- 2
    } else {
      dat$Choice[i] <- 1
    } 
  }
}

#Determine sex variable 
#Data on subject sex was downloaded or obtained on 28 May 2022 from:

#Tempe, AZ birds: file g_flexmanip_datasummary.csv from https://knb.ecoinformatics.org/view/doi:10.5063/F1862DWC
#Santa Barbara, CA birds: Table 4 in https://peerj.com/articles/2215/
#Woodland, CA birds: tab TrappingCA from https://docs.google.com/spreadsheets/d/1joWy9tfmAf7gIpqppFMw-iiw_OxSfNVGCqGKnDNqShM/edit#gid=2125540615

#Based on sex data in above files, assign sex (1 = female; 2 = male)

#NOTE (VERY IMPORTANT!!!): Pina and Bunuelo birds below use a letter from the Latin alphabet - a Spanish n with a tilde: https://en.wikipedia.org/wiki/%C3%91#:~:text=%C3%91%2C%20or%20%C3%B1%20(Spanish%3A,%2D%20or%20lower%2Dcase%20N.
#And this letter does not format correctly when uploading this sheet into R
#So in order reproduce our data sheet, please replace whatever symbol - typically a question mark (?) - that R has used, with the correct Spanish n with a tilde
dat$sex <- NA
for(i in 1:nrow(dat)){
  if(dat$ID[i] == "Adobo" | dat$ID[i] == "Chalupa" | dat$ID[i] == "Memela" | dat$ID[i] == "Tapa" | 
     dat$ID[i] == "Yuca" | dat$ID[i] == "Cerveza" | dat$ID[i] == "Horchata" | dat$ID[i] == "Margarita" | 
     dat$ID[i] == "Michelada" | dat$ID[i] == "Alegria" | dat$ID[i] == "Cocinera" | dat$ID[i] == "Cutuy" | 
     dat$ID[i] == "Flan" | dat$ID[i] == "Galandra" | dat$ID[i] == "Kel" | dat$ID[i] == "Piña" | #Make sure this bird has the proper Spanish n with a tilde: https://en.wikipedia.org/wiki/%C3%91#:~:text=%C3%91%2C%20or%20%C3%B1%20(Spanish%3A,%2D%20or%20lower%2Dcase%20N.
     dat$ID[i] == "Tzanatl preciosa"){
    dat$sex[i] <- 1
  } else {
    if(dat$ID[i] == "Avocada" | dat$ID[i] == "Burrito" | dat$ID[i] == "Chilaquile" | dat$ID[i] == "Diablo" | 
       dat$ID[i] == "Fideo" | dat$ID[i] == "Guacamole" | dat$ID[i] == "Habanero" | dat$ID[i] == "Huachinango" |
       dat$ID[i] == "Marisco" | dat$ID[i] == "Mofongo" | dat$ID[i] == "Mole" | dat$ID[i] == "Pizza" | 
       dat$ID[i] == "Pollito" | dat$ID[i] == "Queso" | dat$ID[i] == "Taco" | dat$ID[i] == "Taquito" | 
       dat$ID[i] == "Tomatillo" | dat$ID[i] == "Batido" | dat$ID[i] == "Jugo" | dat$ID[i] == "Refresco" | 
       dat$ID[i] == "Tequila" | dat$ID[i] == "Ak'xi'" | dat$ID[i] == "Buñuelo" | #Make sure this bird has the proper Spanish n with a tilde: https://en.wikipedia.org/wiki/%C3%91#:~:text=%C3%91%2C%20or%20%C3%B1%20(Spanish%3A,%2D%20or%20lower%2Dcase%20N.
       dat$ID[i] == "Camote" | dat$ID[i] == "Cuervo" | dat$ID[i] == "Dulce de Leche" | dat$ID[i] == "Helado" | 
       dat$ID[i] == "Kau" | dat$ID[i] == "Polvorones" | dat$ID[i] == "Tembleque" | dat$ID[i] == "Xunub" | 
       dat$ID[i] == "Zapote Negro"){
      dat$sex[i] <- 2
    }
  }
}

#Determine sex & phase variable for each bird
dat$sex_phase <- NA
for(i in 1:nrow(dat)){
  if(dat$sex[i] == 1 & dat$Phase[i] == 1){
    dat$sex_phase[i] <- 1
  } else if (dat$sex[i] == 1 & dat$Phase[i] == 2){
    dat$sex_phase[i] <- 2
  } else if (dat$sex[i] == 2 & dat$Phase[i] == 1){
    dat$sex_phase[i] <- 3
  } else if (dat$sex[i] == 2 & dat$Phase[i] == 2){
    dat$sex_phase[i] <- 4
  }
}

#Need to address the three birds pulled early-on by experimenters from phase 2 due to time constraints
#We can still estimate phi and lambda for these birds no matter the trial length - see Ch. 15 in Rethinking: https://github.com/Booleans/statistical-rethinking/blob/master/Statistical%20Rethinking%202nd%20Edition.pdf
#However, we need to designate a skip column for these birds for our Poisson modelling, and for graphing etc. 

dat$skip <- NA
for(i in 1:nrow(dat)){
  if(dat$Phase[i] == 1){
    dat$skip[i] <- 0
  } else {
    if(dat$ID[i] == "Guacamole" | dat$ID[i] == "Huachinango" | dat$ID[i] == "Batido"){
      dat$skip[i] <- 1
    } else {
      dat$skip[i] <- 0
    }
  }
}

#Original data has two different learning criterion applied by experimenters: 

#(1) fixed-window method (core & middle pops), where birds had to make 17/20 correct choices, with a min. of 8 and 9 correct in either of the last two 10-trial blocks, 
#which was assessed at the end of each block (e.g., 20, 30, 40); and which therefore doesn't always reflect the true trial to pass (by a maximum of 2 trials extra)
#(2) sliding-window method (edge pop), where birds had to make 17/20 correct choices, with a min. of 8 and 9 correct in either of the last two sets of 10 trials,
#which was assessed at each trial rather than at the end of each 10-trial block. 
#This second method of assessing learning is nonsensical: because a bird can meet criterion *before* making their choice.
#For example, consider the following sequence of 20 incorrect (0) and correct (1) choices, broken into 10-trial sets: 0001111111 1111111110
#Now at the start of the next trial, the bird will already pass, because the first set loses a zero and gains a 1: 0011111111 111111110?

#We want to examine learning across all birds using an informative and consistent method
#Thus, we apply a straightforward 17/20 method to all birds, beginning at trial 17
dat$Criterion <- NA
for(i in 1:nrow(dat)){
  dat$Criterion[i] <- 0
  if(dat$Trial[i] == 17 | dat$Trial[i] == 18 | dat$Trial[i] == 19){
    if(sum(dat$Correct[(i - (dat$Trial[i - 1])): i]) >= 17){
      dat$Criterion[i] <- 1
    }
  } else if(dat$Trial[i] >= 20){
    if(sum(dat$Correct[(i - 19):i]) >= 17){
      dat$Criterion[i] <- 1
    }
  } 
}

#Extra trials beyond criterion 
#Currently, the sheet will have multiple birds with multiple trials where criterion was met - need to only have first case
#Can have a pass, followed by a no-pass, and then later on more passes - detect this
for(i in 1:nrow(dat)){  
  if(dat$Trial[i] >= 18){ #If trial 18 or bigger
    if(dat$Criterion[i - 1] == 1){ #If bird passed in previous trial, indicated by summing b/c 0 + 1 = 1
      dat$Criterion[i] <- 2 #Assign 2
    }
  }
  #Now because first-pass trial will always be followed by a 2, it is possible to detect all cases post-initial pass, and assign 2
  if(dat$Trial[i] >= 18){ #If trial 18 or bigger
    if(sum(dat$Criterion[(i-1):i]) >= 2){ #If bird already passed, indicated by summing b/c 1 + 2 = 3 (where 2 can be assigned from condition above or from previous loop through current statement) 
      dat$Criterion[i] <- 2 #Assign 2
    }
  }
}

#Check all birds only have one criterion 1 - condition is met
#There should be three 0s for criterion 1 in phase 2 for the three birds pulled early on (see above) - condition is met
table(dat$ID, dat$Criterion, dat$sex,dat$Phase)

#Let's safety check our criterion assignment, by comparing it to cumulative counts 
#First, we apply the sliding scale counts
dat$sum_count <- NA
for(i in 1:nrow(dat)){
  if(dat$Trial[i] == 17 | dat$Trial[i] == 18 | dat$Trial[i] == 19){
    dat$sum_count[i] <- sum(dat$Correct[(i - (dat$Trial[i - 1])): i])
  } else if(dat$Trial[i] >= 20){
    dat$sum_count[i] <- sum(sum(dat$Correct[(i - 19):i]))
  } else if(dat$Trial[i] < 17){
    dat$sum_count[i] <- 0
  }
}

for(i in 1:nrow(dat)){
  if(dat$Criterion[i] == 1 & dat$Correct[i] != 1 & dat$sum_count[i] != 17){
    print(i)
  } 
} #No printed error rows returned - Criterion assignment safety checked!

#We want to begin estimation of phi and lambda in reversal learning based on attractions that encompass ALL previous choices, irrespective of whether they're extra or not
#So we need to add a drop column to exclude extra learning trials in reversal learning but not initial learning
#Because we run a separate EWA model for initial and reversal learning, neither model include the extra learning trials (see STAN_Execution.R script)
#Rather, reversal estimations will precisely capture attraction scores at stimulus-reward swapping
dat$drop <- NA
for(i in 1:nrow(dat)){
  if(dat$Phase[i] == 2 & dat$Criterion[i] == 2){
    dat$drop[i] <- 1
  } else {
    dat$drop[i] <- 0
  }
}

#As an aside regarding extra initial learning trials, these are already an inherent feature of core- and middle-population birds, because of the fixed-window method applied by the experimenter(s), detailed above.
#And if we apply the sliding-window method originally used by experimenters to the edge-population data, we find that these birds also gained extra learning trials due to experimenter oversight
#SW_Criterion of 0 = not passed; 1 = passing trial; 2 = extra learning trials, either in initial or reversal 
dat$SW_Criterion <- NA
for(i in 1:nrow(dat)){
  
  dat$SW_Criterion[i] <- 0 #Baseline for all rows
  
  #Trial meet SW_Criterion
  #Beginning at trial 18
  if(dat$Trial[i] == 18){
    if(sum(dat$Correct[(i - 17):i]) >= 17){
      if((sum(dat$Correct[(i-17):(i-8)]) >= 8) & (sum(dat$Correct[(i-7):i]) == 8)){
        dat$SW_Criterion[i] <- 1
      }
    }
  }
  #Moving to trial 19
  if(dat$Trial[i] == 19){
    if(sum(dat$Correct[(i - 18):i]) >= 17){
      if((sum(dat$Correct[(i-18):(i-9)]) >= 8) & (sum(dat$Correct[(i-8):i]) == 8)){
        dat$SW_Criterion[i] <- 1
      }
    }
  }
  #Any trial equal to or above 20
  if(dat$Trial[i] >= 20){
    if(sum(dat$Correct[(i - 19):i]) >= 17){
      if((sum(dat$Correct[(i - 19):(i - 10)]) >= 8) & (sum(dat$Correct[(i - 9):i]) >= 8)){
        dat$SW_Criterion[i] <- 1
      }
    }
  }
  #Extra trials beyond SW_Criterion 
  #Currently, the sheet will have multiple birds with multiple trials where SW_Criterion was met - need to only have first case
  #Can have a pass, followed by a no-pass, and then later on more passes - detect this
  if(dat$Trial[i] >= 18){ #If trial 18 or bigger
    if(dat$SW_Criterion[i - 1] == 1){ #If bird passed in previous trial, indicated by summing b/c 0 + 1 = 1
      dat$SW_Criterion[i] <- 2 #Assign 2
    }
  }
  #Now because first-pass trial will always be followed by a 2, it is possible to detect all cases post-initial pass, and assign 2
  if(dat$Trial[i] >= 18){ #If trial 18 or bigger
    if(sum(dat$SW_Criterion[(i-1):i]) >= 2){ #If bird already passed, indicated by summing b/c 1 + 2 = 3 (where 2 can be assigned from condition above or from previous loop through current statement) 
      dat$SW_Criterion[i] <- 2 #Assign 2
    }
  }
}

edge_oversight <- dat %>% filter(Population == 3) #Only edge pop birds
table(edge_oversight$ID, edge_oversight$SW_Criterion, edge_oversight$Phase) #Returns 11 in initial (range: 1 - 10), 7 in reversal (range: 1 - 14)
#Thus irrespective of the original learning method used by experimenters, birds experienced extra learning trials

#For our supplementary Poisson, we need choice-option switch counts
dat$switch <- NA
for(i in 1:nrow(dat)){
  if(dat$Trial[i] > 1){
    if(dat$Choice[i] == 1){
      if(dat$Choice[i - 1] == 1){
        dat$switch[i] <- 0
      } else {
        dat$switch[i] <- 1
      }
    } else {
      if(dat$Choice[i - 1] == 2){
        dat$switch[i] <- 0
      } else {
        dat$switch[i] <- 1
      }
    }
  } else {
    dat$switch[i] <- 0
  }
}

#Drop ID column b/c special characters can corrupt files
dat <- dat[, -1]

#Final NA check
which(is.na(dat)) #Returns integer(0) i.e., R saying no NAs found in data sheet

#To download clean data sheet to csv
#write.csv(dat,"C:\\Users\\Alexis Breen\\Desktop\\Grackle_data_clean.csv", row.names = FALSE)

#End script
#####################################################################################################################################################