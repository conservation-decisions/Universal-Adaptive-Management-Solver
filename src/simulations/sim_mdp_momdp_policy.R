sum_2_by_2 <- function(vector){
  n <- length(vector)/2
  v <- rep(0,n)
  for (i in c(1:n)){
    v[i]<-vector[2*(i-1)+1] + vector[2*i]
  }
  v
}


sim_mdp_momdp_policy <- function(state_prior, Tmax, tr_mdp, rew_mdp, 
                                 tr_momdp, obs_momdp, alpha_momdp, disc = 0.95,
                                 n_it = 100) {
  S <- nrow(alpha_momdp$vectors)
  for (j in c(1:n_it)){
    V <- 0 #Reward of the simulation
    set.seed(as.integer((as.double(Sys.time()) * 1000 + Sys.getpid())%%2^31))
    rand <- stats::runif(1)
    if (rand <= state_prior[1]) {
      real_state <- 1 #low
      belief <- matrix(rep(c(1/S,0), S), ncol = 2*S)
    }  else {
      real_state <- 2 #high
      belief <- matrix(rep(c(0,1/S), S), ncol = 2*S)
    }
    belief_mod <- sum_2_by_2(belief)
    output <- Interp_policy2(belief_mod, obs = real_state,
                             alpha = alpha_momdp$vectors, 
                             alpha_action = alpha_momdp$action, 
                             alpha_obs = alpha_momdp$obs)
    actions <- output[[2]][1]
    mod_probs <- matrix(belief_mod, ncol = S)
    V <- c(V,V + rew_mdp[real_state, actions])
    for (i in c(1:(Tmax))) {
      o1 <- real_state[i]
      a1 <- actions[i]
      
      #next observation given belief, action and obs
      set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))
      rand <- stats::runif(1)
      if (rand <= tr_mdp[o1,1,a1]) {
        o2 <- 1 #low
        real_state <- c(real_state, 1)
      }  else {
        o2 <- 2 #high
        real_state <- c(real_state, 2)
      }
      
      #update belief
      s_p <-update_belief(belief[i,], tr_momdp, obs_momdp, o2, a1)
      belief <- rbind(belief, s_p)
      belief_modi <- sum_2_by_2(s_p)
      mod_probs <- rbind(mod_probs, belief_modi)
      
      #next action
      output <-Interp_policy2(belief_modi, obs = o2,
                              alpha = alpha_momdp$vectors, 
                              alpha_action = alpha_momdp$action, 
                              alpha_obs = alpha_momdp$obs)
      actions <- c(actions, output[[2]][1])
      
      #update reward
      V <- c(V, V[i+1] + disc**i*rew_mdp[o2, output[[2]][1]])
    }
    if (j == 1){
      data <- matrix(c(mod_probs[nrow(mod_probs),], V), ncol = S+length(V))
    } else {
      data <- rbind(data, c(mod_probs[nrow(mod_probs),], V))
    }
  }
  return(data)
}


