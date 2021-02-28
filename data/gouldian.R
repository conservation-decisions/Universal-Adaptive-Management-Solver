#This file contains the basic toy examples used to test and compare 
#the universal solver to the other solvers

reward <- matrix(c(0,20,-5,15,-5,15,-5,15),nrow = 2)
gamma <- 0.9 #discount factor
b_full<-c(1,0) #initial state: low
Tmax <- 5 #horizon time for the Dirichlet solver
Tmax_hmMDP <- 50 #horizon time for the hmMDP solver
N_MDP <-100 #number of MDPs tested

file_mean_params <- 'res/meanparams/meanparamsgouldian.csv'
file_random_mdp <- 'res/randomMDP/gouldian_randomMDP.csv'
file_pomdpx <- 'res/POMDPX/gouldian.pomdpx'#name of the file out
file_outpolicy <-'data/POLICYX/gouldian.policyx'
file_simulations_opt <-'res/simopt/simoptgouldian.csv'
file_simulations_hmMDP <-'res/simhmMDP/simhmMDPgouldian.csv'
file_simulations_params <-'res/simparams/simparamsgouldian.csv'
performance_file <- 'res/performance/performancegouldian.csv'

#files of the previous case study
file_mean_params_4Exp <- 'res/meanparams/meanparamsgouldian4Exp.csv'
file_pomdpx_4Exp <- 'data/gouldian4Exp.pomdpx'
file_outpolicy_4Exp <-'data/POLICYX/gouldian4Exp.policyx'
file_simulations_hmMDP_4Exp <-'res/simhmMDP/simhmMDP4Expgouldian.csv'

#files for the simulations of AM when the real model is
#on model predicted by the experts 
file_simulations_opt_expert_model <-'res/expert_model/simoptgouldian_expert_model.csv'
file_simulations_hmMDP_expert_model <-'res/expert_model/simhmMDPgouldian_expert_model.csv'
file_simulations_hmMDP_4Exp_expert_model <-'res/expert_model/simhmMDP4Expgouldian_expert_model.csv'
file_simulations_params_expert_model <-'res/expert_model/simparamsgouldian_expert_model.csv'
performance_file_expert_model <- 'res/performance/performancegouldian_expert_model.csv'
