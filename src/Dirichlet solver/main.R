#this file aims to solve the problem of a MDP with unknown parameters 
#We solve the problem for S states and A actions
#and a time horizon Tmax

#load useful functions
source("src/Dirichlet solver/dirichlet_solver.R")

#load example parameters
file_example <- "data/examples2states2actions.R"
# file_example <-"data/examples2states3actions.R"
# file_example <-"data/examples2states10actions.R"

l <- dirichlet_solver(file_example)
V_MDP <- l$V_MDP
states <- l$states
