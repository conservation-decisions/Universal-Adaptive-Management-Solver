#This file contains the basic toy examples used to test and compare 
#the universal solver to the other solvers
N <- 6
reward <- matrix(c(seq(0,3,length.out = N), seq(4,9,length.out = N)),
                 byrow = TRUE, nrow = 2)
gamma <- 0.9 #discount factor
b_full<-c(1,0) #initial state: low
Tmax <- 4 #horizon time for the Dirichlet solver
Tmax_hmMDP <- 50 #horizon time the hmMDP solver
N_MDP <-100 #number of MDPs tested

file_mean_params <- 'res/meanparams/meanparams2states6actions.csv'
file_random_mdp <- 'res/randomMDP/2states6actions_randomMDP.csv'
file_pomdpx <- 'res/POMDPX/2states6actions.pomdpx'#name of the file out
file_outpolicy <-'data/POLICYX/2states6actions.policyx'
file_simulations_opt <-'res/simopt/simopt2states6actions.csv'
file_simulations_hmMDP <-'res/simhmMDP/simhmMDP2states6actions.csv'
file_simulations_params <-'res/simparams/simparams2states6actions.csv'

performance_file <- 'res/performance/performance2states6actions.csv'