/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Code to run STAN Poisson regression modelling total-choice-option-switches-in-test across populations for the manuscript

//Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

//Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Stan is a programming language written in C++
//Models written in stan consist of different, defined blocks
//Annotation is denoted by the use of //
//Stan user manual: https://mc-stan.org/users/documentation/

//Data block: Define and name the size of each observed variable

data{ //Begin block

  int switches[98]; //Unique per-bird total choice-option switch count
  int sex[98];      //Sex of bird i.e., female (1) or male (2)
  int phase[98];    //Experimental phase i.e., initial (1) or reversal (2)
  int id[98];       //Unique individual identification
  int skip[98];     //Variable to tell stan to skip over the three males pulled from reversal; 0 = no skip; 1 = skip

} //End block

//Parameter block: Define and name the size of each unobserved variable.

parameters{ //Begin block

  real a[2,2];   //Fixed effects - indexed by sex & phase
  real b[49];    //Random effect i.e., bird id
  real sigma_id; //Standard deviation for random effect

} //End block

//Model block: Name and define the model

model{ //Begin block

  //Priors

  for(i in 1:2){             //For sex in 1 - 2
    for(j in 1:2){           //For phase in 1 - 2
      a[i,j] ~ normal(3, 1); //For fixed effects, assign non-'explosive' prior - see section 11.2 in Rethinking: https://github.com/Booleans/statistical-rethinking/blob/master/Statistical%20Rethinking%202nd%20Edition.pdf
    }                        //End j
  }                          //End i

  b ~ normal(0,sigma_id);         //For random id effect, assign adaptive prior including hyper-parameter for variation among birds
  sigma_id ~ exponential(1);      //For standard dev on random effect, assign exponential prior b/c variances bound to be positive

  //Estimate on exponentiated i.e., outcome scale

  for(i in 1:98){                                                  //For every bird
    if(skip[i] == 0){                                              //If bird not designated a 'skip'
      switches[i] ~ poisson(exp(a[sex[i], phase[i]] + b[id[i]] )); //Estimate influence of sex & phase on total-switches-in-test
    }                                                              //End i
  }                                                                //End j
} //End block
