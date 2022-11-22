#####################################################################################################################################################

#Code for performing the evolutionary model of grackle learning for the manuscript 

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################

#Begin script

library(rethinking)

# Structure of this script
# (1) Parameter combinations for simulation
# (2) Simulation function that takes parameter values as input and executes evolutionary algorithm
# (3) Run simulation for all parameter values in parallelized way through mclapply


#Here we construct dataframe with all parameter combinations we simulate

seq<- expand.grid(Nsim = 10,                 #Number of independent simulations
                  N=300,                     #Population size
                  t_life=1000,               #Lifespan (number of foraging decisions)
                  t_life_burnin=200,         #Burn-in time (to get rid of initial conditions; learning should respond to steady state of the environment)
                  t_gen  = 7000,             #Number of generations
                  u = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1),   #Environmental stability, i.e., probability that optimal behavior changes per time step
                  s = c(0     , 0.01 , 0.1  , 0.15, 0.2 , 0.35, 0.3),     #Environmental stochasticity, i.e., probability that optimal behavior does not payoff
                  mu_phi = 0.05,                                          #Mutation parameter for phi
                  mu_lambda= 0.05)                                        #Mutation parameter for lambda



#Define simulation function

sim_Grackle_evo <- function(Nsim, N, t_life, t_life_burnin, t_gen , u , s, mu_phi, mu_lambda) {
  
  #Create empty list for output matrices of all N=Nsim simulation runs
  Combined_list <- list()
  
  #Loop over all N=Nsim separate simulations
  for(xsim in 1:Nsim){
    
    #Initialize population with random starting values
    phi <- runif(N, 0, 1)
    lambda <- runif(N, 0, 15)
    
    #Output objects
    avg_phi    <- c()
    avg_lambda <- c()
    avg_correct <- c()
    
    #Loop over generations
    for (gen in 1:t_gen) {
      
      #Create value matrix for all individuals
      A <- matrix(0, ncol = 2, nrow = N)
      
      #Set optimal behavior for first day of life
      optimal <- sample(1:2, 1)
      
      #Vector to count collected rewards 
      rewards <- rep(0, N)
      prop_correct <- c()
      
      #Loop over days in life (foraging decisions)
      for (t in 1:t_life) {
        
        #Switch optimal behavior with probability u
        optimal <- ifelse( runif(1) > u , optimal , c(1,2)[-optimal] )
        
        #Environmental stochasticity: with probability 1-s optimal behavior produces reward,
        #with probability s sub-optimal behavior produces reward
        realized <- ifelse( runif(1) > s , optimal , c(1,2)[-optimal] )
        
        #Get choice probabilities based on attraction scores
        p <- exp(lambda*A) / rowSums(exp(lambda*A))
        
        #Make choice proportional to attraction scores
        choice <- apply(p, 1, function(x) sample(1:2, 1, prob = x))
        
        #Determine if birds receive payoff
        pay <- sapply(choice, function(x) ifelse(x == realized, 1, 0) )
        
        #Update attractions for chosen option
        for (id in 1:N) A[id,choice[id]] <- A[id,choice[id]] + phi[id] * (pay[id] - A[id,choice[id]])
        
        #Add payoff to gathered rewards after burn-in period
        if (t > t_life_burnin) rewards <- rewards + pay
        
        prop_correct[t] <- sum(pay)/N
      }#day
      
      #At the end of their life, we produce next generation through roulette selection based on obtained rewards
      
      offspring <- sample(1:N, N, replace = TRUE, prob = rewards)
      
      #Update phi and lambda values with small mutation rates
      
      phi    <-inv_logit(logit(phi[offspring]) + rnorm(N, 0, mu_phi))
      lambda <-     exp(log(lambda[offspring]) + rnorm(N, 0, mu_lambda))
      
      #Keep lambda in reasonable limit
      lambda <- ifelse(lambda > 15, 15, lambda)
      
      avg_phi[gen]    <- mean(phi)
      avg_lambda[gen] <- mean(lambda)
      avg_correct[gen] <- mean(prop_correct)
      
    }#gen
    
    
  Combined_list[[xsim]]<- list(avg_phi= avg_phi,
                               avg_lambda=avg_lambda, 
                               avg_correct = avg_correct) 
  
}#xsim

  return(Combined_list)  
  
}#end simulation function



# Pass to mclapply; this should be run on larger computer cluster, otherwise it will run VERY long with present parameter values.
# Change mc.cores to number of available cores

library(parallel)

result_Grackle_evo <- mclapply(
  1:nrow(seq) ,
  function(i) sim_Grackle_evo(seq$Nsim[i], seq$N[i], seq$t_life[i] , seq$t_life_burnin[i],
                           seq$t_gen[i], seq$u[i], seq$s[i], seq$mu_phi[i],
                           seq$mu_lambda[i]),
  mc.cores=1)


#End script
#####################################################################################################################################################

