transition_hmMDP <- function(file_example){
  #inputs file name
  #returns the transition matrix of the hmMDP
  
  source(file_example)
  data_mean_parameters <- read.csv(file_mean_params, sep = ",")
  
  Num_S <- nrow(reward)#number of fully observable states
  Num_a <- ncol(reward)#number of actions
  mod <- data_mean_parameters$opt
  Num_mod <- length(mod)
  
  for (act_id in seq(Num_a)){
    assign(paste0("tr", act_id), 
           matrix(0, ncol = Num_mod*Num_S , nrow =  Num_mod*Num_S))
  }
  for (i in seq(Num_mod)){
    mod_id <- mod[i]
    params <- unlist(c(data_mean_parameters[which(data_mean_parameters$opt
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
  tr_momdp <- array(all_values_tr,
                    dim = c(Num_mod*Num_S, Num_mod*Num_S, Num_a))
  return(tr_momdp)
}

reward_hmMDP <- function(file_example){
  #inputs file name
  #returns the reward matrix of the hmMDP
  source(file_example)
  
  data_mean_parameters <- read.csv(file_mean_params, sep = ",")
  
  Num_S <- nrow(reward)#number of fully observable states
  Num_a <- ncol(reward)#number of actions
  mod <- data_mean_parameters$opt
  Num_mod <- length(mod)
  
  rew_momdp <- matrix(0, ncol = Num_a, nrow <-Num_mod*Num_S )
  for (act_id in seq(Num_a)){
    rew_momdp[,act_id] <- rep(reward[,act_id], Num_mod)
  }
  return(rew_momdp)
}

obs_hmMDP <- function(file_example){
  #inputs file name
  #returns the observation matrix of the hmMDP
  source(file_example)
  
  data_mean_parameters <- read.csv(file_mean_params, sep = ",")
  
  Num_S <- nrow(reward)#number of fully observable states
  Num_a <- ncol(reward)#number of actions
  mod <- data_mean_parameters$opt
  Num_mod <- length(mod)
  obs_momdp <- matrix(rep(c(diag(Num_S)),Num_mod), ncol = Num_S, byrow = T)
  obs_momdp <- array(rep(c(obs_momdp), Num_a), 
                   dim = c(Num_mod*Num_S,Num_S,Num_a))
  return(obs_momdp)
}
