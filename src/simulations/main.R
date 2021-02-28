#This is the main file to run simulations
library(MDPtoolbox)
#load usefull functions
source("src/simulations/read_policyx.R")
source("src/simulations/generate_random_mdp.R")
source("src/simulations/sim_mdp_momdp_policy.R")
source("src/simulations/sim_mdp_parameter_uncertainty.R")
source("src/simulations/build_matrices_hmMDP.R")
source("src/Dirichlet solver/dirichlet_solver.R")

#load example parameters
file_example <- "data/examples2states2actions.R"
# file_example <- "data/examples2states4actions.R"
# file_example <- "data/examples2states6actions.R"
# file_example <- "data/examples2states10actions.R"
# file_example <- 'data/gouldian.R'
# file_example <- 'data/potoroo.R'
if (file_example != 'data/gouldian.R'){
    source(file_example)
    
    #parameters of the problem
    S<-nrow(reward)
    A<-ncol(reward)
    
    file_mdp <- generate_random_mdp(reward, N_MDP, file_random_mdp) #generate a file of N random MDPs
    random_mdp <- read.csv(file_mdp)
    
    ## SIMULATIONS ####
    
    # HMMDP
    #load alpha vectors
    alpha_momdp <- read_policyx2(file_outpolicy) #alpha vectors of the hmMDP
    
    #build hmMDP matrices as a POMDP
    tr_momdp <- transition_hmMDP(file_example)
    
    obs_momdp <- obs_hmMDP(file_example)
    
    #Dirichlet solver
    l <- dirichlet_solver(file_example)
    V_MDP <- l$V_MDP
    states <- l$states
    
    for (mdp_id in seq(nrow(random_mdp))){
      print(mdp_id)
      tr_mdp <- list()
      for (a in seq(A)){
        tr_mdp[[a]]<- matrix(random_mdp[mdp_id, seq((a-1)*S**2+1,a*S**2)],
                             nrow = S, byrow = TRUE)
      }
      tr_mdp <- array(as.numeric(unlist(tr_mdp)), dim=c(S,S,A))
      rew_mdp <- reward
      
      #simulation with hmMDP 
      tab_hmMDP <- sim_mdp_momdp_policy(b_full, Tmax_hmMDP, tr_mdp, rew_mdp, 
                                        tr_momdp, obs_momdp, alpha_momdp, gamma)
      
      trajectory_hmMDP <- tab_hmMDP[, seq(nrow(alpha_momdp$vectors)+1,
                                          Tmax_hmMDP+nrow(alpha_momdp$vectors))]
      
      #simulation with parameter uncertainty
      tab_params <- sim_mdp_parameter_uncertainty(b_full, tr_mdp, rew_mdp,
                                                  states,
                                                  V_MDP, gamma)
      
      #optimal solution
      mdp_sol<-mdp_finite_horizon(tr_mdp, rew_mdp, gamma, Tmax_hmMDP)
      V_opt <- mdp_sol$V[which(b_full == 1),]
      V_opt <- rev(V_opt)
      
      if (mdp_id == 1){
        tab_hmMDP_res <- trajectory_hmMDP
        tab_params_res <- tab_params
        V_opt_res <- V_opt
      } else {
        tab_hmMDP_res <- rbind(tab_hmMDP_res, trajectory_hmMDP)
        tab_params_res <- rbind(tab_params_res, tab_params)
        V_opt_res <- rbind(V_opt_res,V_opt)
      }
    }
    
    write.csv(tab_hmMDP_res, file_simulations_hmMDP, row.names = FALSE)
    write.csv(tab_params_res, file_simulations_params, row.names = FALSE)
    write.csv(V_opt_res, file_simulations_opt, row.names = FALSE)
    
} else { #gouldian finch case study
    source(file_example)
    
    #parameters of the problem
    S<-nrow(reward)
    A<-ncol(reward)
    
    set.seed(2020)
    file_mdp <- generate_random_mdp(reward, N_MDP, file_random_mdp) #generate a file of N random MDPs
    random_mdp <- read.csv(file_mdp)
    ## SIMULATIONS ####
    
    # robust HMMDP
    # load alpha vectors
    alpha_momdp <- read_policyx2(file_outpolicy) #alpha vectors of the hmMDP
    
    # build hmMDP matrices as a POMDP
    data_mean_parameters <- read.csv(file_mean_params, sep = ",")
    # write the pomdpx file
    Num_S <- nrow(reward)#number of fully observable states
    Num_a <- ncol(reward)#number of actions
    mod <- data_mean_parameters$opt
    Num_mod <- length(mod)
    
    #build hmMDP matrices as a POMDP
    tr_momdp <- transition_hmMDP(file_example)
    
    obs_momdp <- obs_hmMDP(file_example)
    
    ## 4 experts hmMDP ####
    # load alpha vectors
    alpha_momdp_4Exp <- read_policyx2(file_outpolicy_4Exp) #alpha vectors of the hmMDP
    
    # build hmMDP matrices as a POMDP
    data_mean_parameters_4Exp <- read.csv(file_mean_params_4Exp, sep = ",")
    
    # write the pomdpx file
    Num_S <- nrow(reward)#number of fully observable states
    Num_a <- ncol(reward)#number of actions
    mod_4Exp <- data_mean_parameters_4Exp$opt
    Num_mod_4Exp <- length(mod_4Exp)
    
    for (act_id in seq(Num_a)){
      assign(paste0("tr", act_id), 
             matrix(0, ncol = Num_mod_4Exp*Num_S , nrow =  Num_mod_4Exp*Num_S))
    }
    for (i in seq(Num_mod_4Exp)){
      mod_id <- mod_4Exp[i]
      params <- unlist(c(data_mean_parameters_4Exp[which(data_mean_parameters_4Exp$opt
                                                        == mod_id),-1]))
      for (act_id in c(1:Num_a)){
        tr <- get(paste0("tr",act_id))
        mat <- matrix(c(params[(act_id-1)*2+1], params[(act_id-1)*2+2],
                        1-params[(act_id-1)*2+1], 1-params[(act_id-1)*2+2]),
                      nrow = 2)
        tr[seq((i-1)*Num_S+1,(i)*Num_S),
           seq((i-1)*Num_S+1,(i)*Num_S) ] <- mat
        assign(paste0("tr",act_id), tr)
      }
    }
    all_values_tr <-c()
    for (act_id in seq(Num_a)){
      tr <- get(paste0("tr", act_id))
      all_values_tr <- c(all_values_tr, tr)
    }
    tr_momdp_4Exp <- array(all_values_tr,
                          dim = c(Num_mod_4Exp*Num_S, Num_mod_4Exp*Num_S, Num_a))
    
    rew_momdp_4Exp <- matrix(0, ncol = A, nrow <-Num_mod*Num_S )
    for (act_id in seq(A)){
      rew_momdp_4Exp[,act_id] <- rep(reward[,act_id], Num_mod_4Exp)
    }
    
    obs_momdp_4Exp <- matrix(rep(c(diag(Num_S)),Num_mod_4Exp), ncol = Num_S, byrow = T)
    obs_momdp_4Exp <- array(rep(c(obs_momdp_4Exp), A), 
                           dim = c(Num_mod_4Exp*Num_S,Num_S,A))
    
    #Dirichlet solver
    l <- dirichlet_solver(file_example)
    V_MDP <- l$V_MDP
    states <- l$states
    
    
    for (mdp_id in seq(nrow(random_mdp))){
      print(mdp_id)
      tr_mdp <- list()
      for (a in seq(A)){
        tr_mdp[[a]]<- matrix(random_mdp[mdp_id, seq((a-1)*S**2+1,a*S**2)],
                             nrow = S, byrow = TRUE)
      }
      tr_mdp <- array(as.numeric(unlist(tr_mdp)), dim=c(S,S,A))
      rew_mdp <- reward
      
      #simulation with hmMDP 
      tab_hmMDP <- sim_mdp_momdp_policy(b_full, Tmax_hmMDP, tr_mdp, rew_mdp, 
                                        tr_momdp, obs_momdp, alpha_momdp, gamma)
      
      trajectory_hmMDP <- tab_hmMDP[, seq(nrow(alpha_momdp$vectors)+1,
                                          Tmax_hmMDP+nrow(alpha_momdp$vectors))]
      
      #simulation with 4 experts hmMDP 
      tab_hmMDP_4Exp <- sim_mdp_momdp_policy(b_full, Tmax_hmMDP, tr_mdp, rew_mdp, 
                                        tr_momdp_4Exp, obs_momdp_4Exp, 
                                        alpha_momdp_4Exp, gamma)
      
      trajectory_hmMDP_4Exp <- tab_hmMDP_4Exp[,seq(nrow(alpha_momdp_4Exp$vectors)+1,
                                          Tmax_hmMDP+nrow(alpha_momdp_4Exp$vectors))]
      
      #simulation with parameter uncertainty
      tab_params <- sim_mdp_parameter_uncertainty(b_full, tr_mdp, rew_mdp,
                                                  states,
                                                  V_MDP, gamma)
      
      #optimal solution
      mdp_sol<-mdp_finite_horizon(tr_mdp, rew_mdp, gamma, Tmax_hmMDP)
      V_opt <- mdp_sol$V[which(b_full == 1),]
      V_opt <- rev(V_opt)
      
      if (mdp_id == 1){
        tab_hmMDP_res <- trajectory_hmMDP
        tab_hmMDP_4Exp_res <- trajectory_hmMDP_4Exp
        tab_params_res <- tab_params
        V_opt_res <- V_opt
      } else {
        tab_hmMDP_res <- rbind(tab_hmMDP_res, trajectory_hmMDP)
        tab_hmMDP_4Exp_res <- rbind(tab_hmMDP_4Exp_res, trajectory_hmMDP_4Exp)
        tab_params_res <- rbind(tab_params_res, tab_params)
        V_opt_res <- rbind(V_opt_res,V_opt)
      }
    }
    
    write.csv(tab_hmMDP_res, file_simulations_hmMDP, row.names = FALSE)
    write.csv(tab_params_res, file_simulations_params, row.names = FALSE)
    write.csv(tab_hmMDP_4Exp_res, file_simulations_hmMDP_4Exp, row.names = FALSE)
    write.csv(V_opt_res, file_simulations_opt, row.names = FALSE)
    
    ## run simulation with the experts having advantage
    params <- unlist(c(data_mean_parameters_4Exp[which(data_mean_parameters_4Exp$opt
                                                  == 3),-1]))
    tr_mdp <- list()
    for (a in seq(A)){
      tr_mdp[[a]]<-  matrix(c(params[(a-1)*2+1], params[(a-1)*2+2],
                              1-params[(a-1)*2+1], 1-params[(a-1)*2+2]),
                            nrow = 2)
    }
    tr_mdp <- array(as.numeric(unlist(tr_mdp)), dim=c(S,S,A))
    rew_mdp <- reward
    
    #simulation with hmMDP 
    tab_hmMDP <- sim_mdp_momdp_policy(b_full, Tmax_hmMDP, tr_mdp, rew_mdp, 
                                      tr_momdp, obs_momdp, alpha_momdp, gamma)
    
    trajectory_hmMDP <- tab_hmMDP[, seq(nrow(alpha_momdp$vectors)+1,
                                        Tmax_hmMDP+nrow(alpha_momdp$vectors))]
    
    #simulation with 4 experts hmMDP 
    tab_hmMDP_4Exp <- sim_mdp_momdp_policy(b_full, Tmax_hmMDP, tr_mdp, rew_mdp, 
                                           tr_momdp_4Exp, obs_momdp_4Exp, 
                                           alpha_momdp_4Exp, gamma)
    
    trajectory_hmMDP_4Exp <- tab_hmMDP_4Exp[,seq(nrow(alpha_momdp_4Exp$vectors)+1,
                                                 Tmax_hmMDP+nrow(alpha_momdp_4Exp$vectors))]
    
    #simulation with parameter uncertainty
    tab_params <- sim_mdp_parameter_uncertainty(b_full, tr_mdp, rew_mdp,
                                                states,
                                                V_MDP, gamma)
    
    #optimal solution
    mdp_sol<-mdp_finite_horizon(tr_mdp, rew_mdp, gamma, Tmax_hmMDP)
    V_opt <- mdp_sol$V[which(b_full == 1),]
    V_opt <- rev(V_opt)
    
    tab_hmMDP_res <- trajectory_hmMDP
    tab_hmMDP_4Exp_res <- trajectory_hmMDP_4Exp
    tab_params_res <- tab_params
    tab_opt_res <- V_opt
    
    write.csv(tab_hmMDP_res, file_simulations_hmMDP_expert_model,
              row.names = FALSE)
    write.csv(tab_params_res, file_simulations_params_expert_model,
              row.names = FALSE)
    write.csv(tab_hmMDP_4Exp_res, file_simulations_hmMDP_4Exp_expert_model,
              row.names = FALSE)
    write.csv(tab_opt_res, file_simulations_opt_expert_model,
              row.names = FALSE)
}
