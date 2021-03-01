#this file aims to solve the hmMDP build using the MC-UAMS algorithm

#load example parameters
# source("data/examples2states2actions.R")
# source('data/examples2states4actions.R')
source('data/examples2states6actions.R')
# source("data/examples2states10actions.R")
# source('data/gouldian4Exp.R')
# source('data/potoroo.R')


#solve the POMDPX after creating the POMDPX

#write an external command for sarsop to run and call it using system(cmd)
precision <- 1e-1  #set precision for sarsop
timeout <- 3600 #set timeout for solving
#change the path to the directory on your machine containing sarsop
cmd <- paste("./sarsop/src/pomdpsol.exe \"", 
             file_pomdpx, 
             "\" --precision ", precision, 
             " --timeout ", timeout ,
             " --output ", file_outpolicy,
             sep="")
    
system(cmd)