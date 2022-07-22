# Sex-differences-in-grackles-learning

This GitHub repository hosts open materials for the manuscript

**Range-expanding male birds buffer environmental change by strategising risk-sensitive learning**

authored by Alexis J. Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

[![DOI](https://zenodo.org/badge/426571130.svg)](https://zenodo.org/badge/latestdoi/426571130)

Data Processing folder contains:

- Data_Processing.R script to collate, clean & curate the utilised data
- The original data sheets: Santa_Barbara_CA_data.csv, Tempe_AZ_data.csv & Woodland_CA_data.csv
- Metadata regarding original data sheets

Data folder contains:
 
- Grackle_data_clean.csv produced from Data_Processing.R script & used for all analyses/graphing

Figures folder contains:

- Figure_2.R script to reproduce Figure 2
- Figure_3.R script to reproduce Figure 3
- Figure_S1.R script to reproduce Figure S1
- Figure_S2.R script to reproduce Figure S2
- Figure_S3.R script to reproduce Figure S3

STAN folder contains:

- STAN_Execution.R script to prepare data for, run, and post-process (e.g., extract posteriors) all multi-level STAN models
- STAN_Poisson_Speed_Full.stan script to run multi-level Poisson regression modelling total-trials-in-test between populations
- STAN_Poisson_Speed_Across_Pop.stan script to run multi-level Poisson regression modelling total-trials-in test across populations
- STAN_Poisson_Switch_Full.stan script to run multi-level Poisson regression modelling total-switches-in-test between populations
- STAN_Poisson_Switch_Across_Pop.stan script to run multi-level Poisson regression modelling total-switches-in-test across populations
- STAN_Comp_Full.stan scrip to run multi-level reinforcement learning computational model - indexed by population & treatment (i.e., sex/phase) - estimating influence of phi & sigma 
- STAN_Comp_Across_Pop.stan script to run multi-level reinforcement learning computational model - indexed by sex & phase - estimating influence of phi & sigma

Simulations folder contains:

- Post_Study_Simulation_Full_Birds.R script to simulate 'new' birds matched to full sample size of data set from the estimated population of varying effects of STAN_Comp_Across_Pop.stan 
- Post_Study_Simulation_Six_Birds.R script to simulate 'new' bird pairs - six in total - from the estimated population of varying effects of STAN_Comp_Across_Pop.stan 

Stage 1 Pre-registration folder contains:

- Breen_Deffner_prereg.pdf copy of Peer Community In Registered Reports peer-reviewed and approved Stage 1 pre-registration 
- Grackle_models_and_graphs.r script to run all material - simulations, models, model-validation checks & graphs - used in Breen_Deffner_prereg.pdf

**Softward requirements**

- Stan (for running the multi-level models): https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

- Rethinking (for processing fitted model outputs): https://github.com/rmcelreath/rethinking

- R (for running all code): https://www.rstudio.com/

**Potentially useful background material**

- Statistical rethinking: A Bayesian course with examples in R and Stan by Prof. Richard McElreath: https://github.com/Booleans/statistical-rethinking/blob/master/Statistical%20Rethinking%202nd%20Edition.pdf

- Tidyverse (for data wrangling): https://r4ds.had.co.nz/

- ggplot (for building some of the graphs): https://r-graphics.org/
