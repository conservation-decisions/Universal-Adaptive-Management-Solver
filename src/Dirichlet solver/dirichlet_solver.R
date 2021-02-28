#this file compiles a set of functions that help building the transition
#and reward matrices
library(prodlim)
library(Matrix)
library(MDPtoolbox)

init_alphas <-function(S,A){
  #initilialises the alphas at one
  #S is the number of states
  #A is the number of actions
  return(rep(1, S**2*A))
}

next_states_list <- function(hyperstate, action, S, A){
  #inputs a hyperstate : S**2*A first elements describe the values of the alphas
  #the S**2*A +1 th element describes the physical state.
  #action is the taken action: integer in 1...A
  #S is the number of states
  #A is the number of actions
  
  #returns a list of 2 elements
  #next_states is a matrix describing the immediate next hyperstates
  #probabilities is a vector of corresponding probabilities 
  #of transitionning from the current hyperstate to the 
  #hyperstates in next_states
  
  next_states <- matrix(nrow = 0, ncol = ( S**2*A +1) )
  alphas <- hyperstate[seq(S**2*A)]
  physical_state <- hyperstate[S**2*A +1]
  
  bloc<- alphas[seq(((action-1)*S**2 + (physical_state-1)*S +1),
                    ((action-1)*S**2 + (physical_state-1)*S +S))]
  probabilities <- c()
  for (s_next in seq(S)){
    #probability of going from physical state to s_next with action
    proba <- bloc[s_next]/sum(bloc)
    probabilities <- c(probabilities, proba)
    #alphas corresponding to the next state
    next_alphas <- alphas
    next_alphas[((action-1)*S**2 + (physical_state-1)*S +s_next)] <-
      next_alphas[((action-1)*S**2 + (physical_state-1)*S +s_next)] +1
    
    new_state <- matrix(c(next_alphas, s_next), 
                        nrow = 1, ncol = ( S**2*A +1) )
    next_states <- rbind(next_states, new_state)
  }
  return(list(next_states = next_states, probabilities = probabilities))
}

get_index <- function(s, states){
  #inputs s a matrix of hyperstates
  #states is a list of reachable hyperstates 
  
  #returns a vector of the indexes of the hyperstates in s
  
  index_matrix <- as.data.frame(do.call(rbind, states))
  index_vector <- c()
  
  if (is.matrix(s)){
    for (id in seq(nrow(s))){
      wantVec <- c(s[id,])
      ind <- row.match(wantVec,index_matrix)
      index_vector <- c(index_vector, ind)
    }
  } else {
    index_vector <- row.match(s,index_matrix)
  }
  
  return(index_vector)
}

indexes_probabilities <- function(alphas, physical_state, S, A, Tmax){
  #inputs the values of the alpha parameters of the Dirichlet distributions of
  #the parameters
  #physical_state is the actual state of the system: integer in 1...S
  #S is the number of states
  #A is the number of actions
  #Tmax is the time horizon
  
  #returns a list of a data.table. Each element of the list is a data.table of 
  #from (index) to (next_index) transition probabilities (probability) 
  #for each action
  #and the list of states reachable at each time step
  
  s <- matrix(c(alphas, physical_state), nrow = 1)
  #states is a list containing all the reachable points from the initial point
  # at each time step
  states <- list()
  states[[1]] <- s
  
  #data_action is a list containing a data.frame for each action
  #from (index) to (next_index) transition probabilities (probability)
  data_action <- list()
  for (action in seq(A)){
    data_action[[action]] <- data.frame(index = numeric(), next_index = numeric(),
                                        probability = numeric())
  }
  
  #main loop
  for (time in seq(2,Tmax)){
    all_next_states <-matrix(nrow = 0, ncol = ( S**2*A +1) )
    mat_old <- states[[time-1]]
    N_old <- nrow(mat_old)
    
    for (id in seq(N_old)){
      for (action in seq(A)){
        data<-next_states_list(mat_old[id,], action, S, A)#matrix of next states
        
        next_states <- data$next_states
        probabilities <- data$probabilities
        
        all_next_states <- rbind(all_next_states,next_states)
        all_next_states <- unique(all_next_states)
        
        states[[time]] <- all_next_states
        
        index <- get_index(mat_old[id,], states)
        next_index <- get_index(next_states, states)
        
        new_data <- data.frame(index =index, next_index = next_index,
                               probability = probabilities)
        data_action[[action]] <- rbind(data_action[[action]], new_data)
      }
    }
  }
  return(list(data_action = data_action, states = states))
}

transition_matrix <- function(data_action, states, A){
  #data action a list of a data.frames. Each element of the list is a data.frame 
  #from (index) to (next_index) transition probabilities (probability) 
  #for each action
  #states is the list of states reachable at each time step
  #A is the number of actions
  
  #returns a list of A sparse transition matrices of size S*S
  transition_probabilities <- list()
  for (action in seq(A)){
    i <- data_action[[action]]$index
    j <- data_action[[action]]$next_index
    prob <- data_action[[action]]$probability
    
    #add identity for the last states
    last_states <- states[[length(states)]]
    ilast <- get_index(last_states, states)
    jlast <- ilast
    problast <- rep(1, length(ilast))
    
    i <-c(i,ilast)
    j <-c(j,jlast)
    prob <- c(prob, problast)
    transition_probabilities[[action]]<-
      sparseMatrix(i,j, x =prob,
                   dims = c(max(i,j), max(i,j)))
  }
  return(transition_probabilities)
}

reward_matrix <- function(states, A, reward){
  #states is the list of states reachable at each time step
  #A is the number of actions
  #reward is the rewards matrix S*A
  #returns a sparse matrix of the rewards of size S*A
  
  index_matrix <- do.call(rbind, states)
  
  physical_states <- index_matrix[,ncol(index_matrix)]
  index_states <- seq(nrow(index_matrix))
  #indexes and rewards to build the reward matrix
  states_index_matrix <-c()
  actions_index_matrix <- c()
  rew_value_matrix <- c()
  
  #states in the new indexation
  for (action in seq(A)){
    states_index_matrix <- c(states_index_matrix, index_states)
    actions_index_matrix <- c(actions_index_matrix, 
                              rep(action, length(index_states)))
    rew <- reward[physical_states, action]
    rew_value_matrix <- c(rew_value_matrix, rew)
  }
  return(sparseMatrix(i = states_index_matrix,
                      j=actions_index_matrix, 
                      x = rew_value_matrix))
}

dirichlet_solver <- function(file_example){
  #solver of the AM problem using Dirichlet distributions among transition
  #probabilities
  
  #inputs file of the example selected in ./data
  #returns a list like object of containing the solution 
  
  source(file_example)
  
  #parameters of the problem
  A <- ncol(reward) #number of actions
  S <- nrow(reward) #number of actions
  
  #initial states
  physical_state <- which(b_full ==1)
  alphas <- init_alphas(S,A)
  #accessing the transition probabilities for each action,
  # and all reachable states from the initial state
  data<-indexes_probabilities(alphas, physical_state, S, A, Tmax)
  data_action <- data$data_action
  states <- data$states
  
  #we are going to use a new indexation in order to build our matrices
  #building transition probabilities
  tr_mdp <- transition_matrix(data_action, states, A)
  
  #building reward matrix
  rew_mdp <- reward_matrix(states, A, reward)
  
  #solving the MDP
  V_MDP <- mdp_finite_horizon(tr_mdp, rew_mdp, gamma, Tmax)
  return(list(V_MDP = V_MDP, states = states))
}
