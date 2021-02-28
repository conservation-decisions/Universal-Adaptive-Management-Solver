#This file contains the basic toy examples used to test and compare 
#the universal solver to the other solvers

reward <- matrix(c(0.00,20.00,-1.00,19.00,-2.00,18.00,-2.36,
                   17.64,-3.36,16.64,-4.36,15.64)
                 ,nrow = 2)
gamma <- 0.9 #discount factor
b_full<-c(1,0) #initial state: low
Tmax <- 4 #horizon time for the Dirichlet solver
Tmax_hmMDP <- 50 #horizon time for the hmMDP solver
N_MDP <-100 #number of MDPs tested

file_mean_params <- 'res/meanparams/meanparamspotoroo.csv'
file_random_mdp <- 'res/randomMDP/potoroo_randomMDP.csv'
file_pomdpx <- 'res/POMDPX/potoroo.pomdpx'#name of the file out
file_outpolicy <-'data/POLICYX/potoroo.policyx'
file_simulations_opt <-'res/simopt/simoptpotoroo.csv'
file_simulations_hmMDP <-'res/simhmMDP/simhmMDPpotoroo.csv'
file_simulations_params <-'res/simparams/simparamspotoroo.csv'

performance_file <- 'res/performance/performancepotoroo.csv'