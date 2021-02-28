update_state <- function(hyperstate, a, o, S, A){
  #inputs 
  #hyperstate : values of the alpha parameters of the Dirichlet distributions of
  #the parameters
  #physical_state is the actual state of the system: integer in 1...S
  #a is the taken action : integer in 1...A
  #o is the new observation: integer in 1...S
  #S is the number of states
  #A is the number of actions
  
  #returns hyperstate_new the new alphas and new physical state
  
  hyperstate_new <- hyperstate
  physical_state <- hyperstate_new[S**2*A+1]
  hyperstate_new[S**2*(a-1)+(physical_state-1)*S+o] <- 
    hyperstate_new[S**2*(a-1)+(physical_state-1)*S+o] + 1
  hyperstate_new[S**2*A+1] <- o
  return(hyperstate_new)
}

sim_mdp_parameter_uncertainty <- function(state_prior, tr_mdp, rew_mdp,
                                          states,
                                          V_MDP, disc = 0.95, n_it = 100) {
  S <- nrow(rew_mdp)
  A <- ncol(rew_mdp)
  Tmax <- ncol(V_MDP$policy)
  for (j in c(1:n_it)){
    V <- 0 #Reward of the simulation
    set.seed(as.integer((as.double(Sys.time()) * 1000 + Sys.getpid())%%2^31))
    rand <- stats::runif(1)
    if (rand <= state_prior[1]) {
      physical_state = 1 #low
    }  else {
      physical_state = 2 #high
    }
    alphas <- init_alphas(S,A)
    s <- c(alphas, physical_state)
    index <-get_index(s,states)
    #optimal policy at time 1
    actions <- V_MDP$policy[index, 1]
    
    V <- c(V, V + rew_mdp[physical_state, actions])
    
    for (i in seq(1, Tmax-1)) {
      o1 <- physical_state[i]
      a1 <- actions[i]
      
      #next observation given belief, action and obs
      set.seed(as.integer((as.double(Sys.time()) *j*i*1000 + Sys.getpid())%%2^31))
      rand <- stats::runif(1)
      if (rand <= tr_mdp[o1,1,a1]){
        o2 <- 1 #low
        physical_state <- c(physical_state, 1)
      }  else {
        o2 <- 2 #high
        physical_state <- c(physical_state, 2)
      }
      
      #update state
      s <-update_state(s,a1,o2, S, A)
      
      #update index
      index <- get_index(s, states)
      
      #next action
      a2 <- V_MDP$policy[index, i+1]
      actions <- c(actions, a2)
      
      #update sum of discounted rewards
      V <- c(V, V[i+1] + disc**i*rew_mdp[o2, a2])
    }
    if (j == 1){
      data <- matrix(V, nrow =1)
    } else {
      data <- rbind(data, V)
    }
  }
  return(data)
}

