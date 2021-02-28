#This file contains the basic toy examples used to test and compare 
#the universal solver to the other solvers

reward <- matrix(c(1,5,3,7),nrow = 2)
gamma <- 0.9 #discount factor
b_full<-c(1,0) #initial state: low
Tmax <- 8 #horizon time for the Dirichlet solver
Tmax_hmMDP <- 50 #horizon time for the tests for the hmMDP solver
N_MDP <-100 #number of MDPs tested

file_mean_params <- 'res/meanparams/meanparams2states2actions.csv'
file_random_mdp <- 'res/randomMDP/2states2actions_randomMDP.csv'
file_pomdpx <- 'res/POMDPX/2states2actions.pomdpx'#name of the file out
file_outpolicy <-'data/POLICYX/2states2actions.policyx'
file_simulations_opt <-'res/simopt/simopt2states2actions.csv'
file_simulations_hmMDP <-'res/simhmMDP/simhmMDP2states2actions.csv'
file_simulations_params <-'res/simparams/simparams2states2actions.csv'

performance_file <- 'res/performance/performance2states2actions.csv'