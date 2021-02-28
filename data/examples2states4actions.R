#This file contains the basic toy examples used to test and compare 
#the universal solver to the other solvers
N <- 4
reward <- matrix(c(seq(1,3,length.out = N), seq(3.5,6,length.out = N)),
                 byrow = TRUE, nrow = 2)
gamma <- 0.9 #discount factor
b_full<-c(1,0) #initial state: low
Tmax <- 5 #horizon time for the Dirichlet solver
Tmax_hmMDP <- 50 #horizon time for the hmMDP solver
N_MDP <-100 #number of MDPs tested

file_mean_params <- 'res/meanparams/meanparams2states4actions.csv'
file_random_mdp <- 'res/randomMDP/2states4actions_randomMDP.csv'
file_pomdpx <- 'res/POMDPX/2states4actions.pomdpx'#name of the file out
file_outpolicy <-'data/POLICYX/2states4actions.policyx'
file_simulations_opt <-'res/simopt/simopt2states4actions.csv'
file_simulations_hmMDP <-'res/simhmMDP/simhmMDP2states4actions.csv'
file_simulations_params <-'res/simparams/simparams2states4actions.csv'

performance_file <- 'res/performance/performance2states4actions.csv'