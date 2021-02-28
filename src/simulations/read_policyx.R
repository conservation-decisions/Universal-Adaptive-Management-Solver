## some additional funcitons ####
read_policyx2 = function(file){
  xml <- xml2::read_xml(file)
  xml_vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), 
                                                " ")[[1]])
  get_action <- function(v) as.numeric(xml2::xml_attr(v, "action"))
  get_obs <- function(v) as.numeric(xml2::xml_attr(v, 'obsValue'))
  n_states <- length(get_vector(xml_vectors[[1]]))
  alpha <- vapply(xml_vectors, get_vector, numeric(n_states))
  alpha_action <- vapply(xml_vectors, get_action, double(1)) + 
    1
  alpha_obs <- vapply(xml_vectors, get_obs, double(1)) + 
    1
  list(vectors = alpha, action = alpha_action, obs = alpha_obs)
} #read momdp

Interp_policy2 = function (initial, obs, alpha, alpha_action, alpha_obs){
  id = which(alpha_obs == obs)
  alpha2 = alpha[,id]
  alpha_action2 = alpha_action[id]
  a <- initial %*% alpha2
  if (sum(a == 0) == length(a)) {
    output = list(0, 1)
  }
  else {
    output = list(max(a), alpha_action2[which.max(a)])
  }
  output
}

update_belief <- function(state_prior, transition, observation, z0, a0){
  L <- length(state_prior)
  belief <-
    vapply(seq_len(L), function(i){
      state_prior %*% transition[, i, a0] * observation[i, z0, a0]
    }, numeric(1))
  belief / sum(belief)
}

mdp_compute_value = function(P, PR, discount){
  # computes the optimal value function using the value iteration algo
  iter <- 0
  V <- c(0,0)
  is_done <- F
  epsilon = 0.0001
  thresh <- epsilon * (1 - discount)/discount
  max_iter = 50000
  while (!is_done) {
    iter <- iter + 1
    Vprev <- V
    bellman <- mdp_bellman_operator(P, PR, discount, 
                                    V)
    V <- bellman[[1]]
    policy <- bellman[[2]]
    variation <- max(V - Vprev)
    if (variation < thresh) {
      is_done <- T
      #print("MDP Toolbox: iterations stopped, epsilon-optimal policy found")
    }
    else if (iter == max_iter) {
      is_done <- T
      #print("MDP Toolbox: iterations stopped by maximum number of iteration condition")
    }
  }
  return(list('V' = V, policy = policy))
}
