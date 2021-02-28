generate_random_mdp <- function(reward, N, file){
  #reward is a reward matrix of size S*A
  #N is the number of MDPs generated
  
  #this function aims to generate a file .csv of MDPs with random parameters
  #the file contains the parameters' value generated randomly
  
  S <- 2 #we consider only the case of 2-state n-action problems
  A <- ncol(reward)
  
  for (j in seq(N)){#random MDP      
    set.seed(as.integer((as.double(Sys.time())*j*1000 + Sys.getpid())%%2^31))

    random_values1 = c()
    for (i in seq(2*A)){
      random <- runif(1)
      random <- c(random, 1-random)
      random_values1 <- c(random_values1, random)
    }
    
    if (j == 1){
      random_mdp = random_values1
    } else{
      random_mdp = rbind(random_mdp, random_values1)
    }
  }
  write.csv(random_mdp, file, row.names = FALSE)
  return(file)
}
