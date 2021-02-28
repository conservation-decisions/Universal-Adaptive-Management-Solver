#this file aims to build the hmMDP ;pomdpx file 
#using the functions in the folders

#load useful functions
source("src/building hmMDP/generate_pomdpx.R")
source("src/building hmMDP/mean_parameters.R")

#load example parameters
# source("data/examples2states2actions.R")
# source('data/examples2states4actions.R')
source('data/examples2states6actions.R')
# source("data/examples2states10actions.R")
# source('data/gouldian4Exp.R')
# source('data/potoroo.R')


#compute the mean parameters of each opt space
data_mean_parameters <- mean_parameters(reward, gamma,1e4)
write.csv(data_mean_parameters, file_mean_params, row.names = FALSE)

data_mean_parameters <- read.csv(file_mean_params)
#write the pomdpx file
Num_S <- nrow(reward)#number of fully observable states
Num_a <- ncol(reward)#number of actions
mod <- data_mean_parameters$opt
Num_mod <- length(mod)
b_par <- rep(1/Num_mod, Num_mod)

transition <- list()

for (i in seq(Num_mod)){
  mod_id <- mod[i]
  params <- unlist(c(data_mean_parameters[which(data_mean_parameters$opt
                                         == mod_id),-1]))
  model <- c()
  for (act_id in c(1:Num_a)){
    mat <- matrix(c(params[(act_id-1)*2+1], params[(act_id-1)*2+2],
                    1-params[(act_id-1)*2+1], 1-params[(act_id-1)*2+2]),
                  nrow = 2)
    model <- c(model, mat)
  }
  model <- array(model, dim = c(Num_S, Num_S, Num_a))
  transition[[i]] <- model
}

write_hmMDPx(transition, reward, b_full, b_par, gamma, file_pomdpx)
