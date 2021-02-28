#This is the main file to analyse the results of simulations
library(MDPtoolbox)
library(data.table)
#load usefull functions
source("src/simulations/read_policyx.R")
source("src/simulations/generate_random_mdp.R")
source("src/simulations/sim_mdp_momdp_policy.R")
source("src/simulations/sim_mdp_parameter_uncertainty.R")
source("src/Dirichlet solver/dirichlet_solver.R")

#load example parameters
# file_example <- "data/examples2states2actions.R"
# file_example <- "data/examples2states4actions.R"
# file_example <- "data/examples2states6actions.R"
# file_example <- "data/examples2states10actions.R"
# file_example <- "data/gouldian.R"
file_example <- 'data/potoroo.R'

if (file_example != "data/gouldian.R"){
  
  source(file_example)
  
  tab_hmMDP_res <- read.csv(file_simulations_hmMDP)
  tab_params_res <- read.csv(file_simulations_params)
  tab_opt_res <- read.csv(file_simulations_opt)
  
  a <- rep(seq(1,N_MDP),100)
  a <- a[order(a)]
  tab_hmMDP_res$mdp_id <- a
  tab_hmMDP_res <- data.table(tab_hmMDP_res)
  tab_hmMDP_res_mean <- tab_hmMDP_res[ , lapply(.SD, mean), by = mdp_id]
  
  tab_params_res$mdp_id <- a
  tab_params_res<- data.table(tab_params_res)
  tab_params_res_mean <- tab_params_res[ , lapply(.SD, mean), by = mdp_id]
  
  #performance at Tmax
  col <- paste0("V",Tmax)
  sim_Tmax <-data.frame(V_opt = tab_opt_res[[col]],
                        V_hmMDP = tab_hmMDP_res_mean[[col]],
                        V_params = tab_params_res_mean[[col]])

  #performance at infinity
  col2 <- paste0("V",Tmax_hmMDP)
  sim_inf <-data.frame(V_opt = tab_opt_res[[col2]],
                        V_hmMDP = tab_hmMDP_res_mean[[col2]])
  
  
  ## instant rewards ####
  instant_rewards_hmMDP <-
    tab_hmMDP_res[,2:ncol(tab_hmMDP_res)]-tab_hmMDP_res[,1:(ncol(tab_hmMDP_res)-1)]
  instant_rewards_hmMDP <- t((1/gamma)**seq(0,ncol(instant_rewards_hmMDP)-1)*
                               t(instant_rewards_hmMDP))
  
  instant_rewards_params <- 
    tab_params_res[,2:ncol(tab_params_res)]-
    tab_params_res[,1:(ncol(tab_params_res)-1)]
  instant_rewards_params <- t((1/gamma)**seq(0,ncol(instant_rewards_params)-1)*
                                t(instant_rewards_params))
  
  instant_rewards_opt <- 
    tab_opt_res[,2:ncol(tab_opt_res)]-tab_opt_res[,1:(ncol(tab_opt_res)-1)]
  instant_rewards_opt <- t((1/gamma)**seq(0,ncol(instant_rewards_opt)-1)*
                             t(instant_rewards_opt))
  
  instant_rewards_hmMDP <- as.data.table(instant_rewards_hmMDP)
  instant_rewards_params <- as.data.table(instant_rewards_params)
  instant_rewards_opt <- as.data.table(instant_rewards_opt)
  
  #name after mdp_id
  a <- rep(seq(1,N_MDP),100)
  a <- a[order(a)]
  instant_rewards_hmMDP$mdp_id <- a
  instant_rewards_hmMDP <- data.table(instant_rewards_hmMDP)
  instant_rewards_hmMDP_mean <- instant_rewards_hmMDP[ , lapply(.SD, mean), by = mdp_id]
  
  instant_rewards_params$mdp_id <- a
  instant_rewards_params <- data.table(instant_rewards_params)
  instant_rewards_params_mean <- instant_rewards_params[ , lapply(.SD, mean), by = mdp_id]
  
  #instant performance at Tmax
  col <- paste0("V",Tmax-1)
  instant_sim_Tmax <-data.frame(V_opt = instant_rewards_opt[[col]],
                        V_hmMDP = instant_rewards_hmMDP_mean[[col]],
                        V_params = instant_rewards_params_mean[[col]])
  
  #instant performance at infinity
  col2 <- paste0("V",Tmax_hmMDP)
  instant_sim_inf <-data.frame(V_opt = instant_rewards_opt[[col2]],
                       V_hmMDP = instant_rewards_hmMDP_mean[[col2]])
  
  
  #performance table
  performance_table <- data.frame(perf_mean_Tmax = numeric(),
                                  perf_sd_Tmax = numeric(),
                                  perf_mean_inf = numeric(),
                                  perf_sd_inf = numeric(),
                                  inst_mean_Tmax = numeric(),
                                  inst_sd_Tmax = numeric(),
                                  inst_mean_inf = numeric(),
                                  inst_sd_inf = numeric()
                                  )
  row_hmMDP <- 
    data.frame(perf_mean_Tmax = 
                      mean((sim_Tmax$V_opt - sim_Tmax$V_hmMDP)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                      1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_hmMDP)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                      mean((sim_inf$V_opt - sim_inf$V_hmMDP)/sim_inf$V_opt),
               perf_sd_inf = 
                      1.96*sd((sim_inf$V_opt - sim_inf$V_hmMDP)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_hmMDP)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_hmMDP)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_hmMDP)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_hmMDP)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  
  row_params <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_params)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_params)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_params)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_params)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_params)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_params)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_params)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_params)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
 
  performance_table <- rbind(performance_table, row_hmMDP, row_params)
  row.names(performance_table) <- c("MC-UAMS","PUBD")
  
  write.csv(performance_table, performance_file)
  
  #trajectories plots
  Tplot<-Tmax_hmMDP
  trajectory_hmMDP <- as.matrix(tab_hmMDP_res)[,seq(Tplot)]
  trajectory_hmMDP_mean <- apply(trajectory_hmMDP, 2, mean)
  
  trajectory_params_mean <- apply(tab_params_res[,1:(Tmax)],
                                  2, mean)
  
  trajectory_opt_mean <- apply(tab_opt_res[,seq(Tplot)], 2, mean)
  
  plot(trajectory_hmMDP_mean, type = 'o',pch =0,
       ylim = c(min(trajectory_hmMDP),max(trajectory_opt_mean)),
       ylab = 'Expected sum of discounted rewards',
       xlab = 'Time steps')
  lines(trajectory_params_mean, type = 'o',
        pch = 1, col = 'red')
  lines(trajectory_opt_mean, type = 'o',
        pch = 2, col = 'blue')
  legend("topleft", bty = "n",
         legend = c("Optimal", "PUBD","UAMS"),
         col = c("blue","red",'black'),
         lty = c(1,1,1), pch = c(2,1,0),
         cex = 0.7)
  
  #instant trajectories
  Tplot<-25
  
  cols <- paste0("V", 2:(Tplot+1))
  trajectory_hmMDP <- as.matrix(instant_rewards_hmMDP[, mget(cols)])
  trajectory_hmMDP_mean <- apply(trajectory_hmMDP, 2, mean)
  
  cols2 <- paste0("V", 2:(Tmax))
  trajectory_params <- as.matrix(instant_rewards_params[, mget(cols2)])
  trajectory_params_mean <- apply(trajectory_params,2, mean)
  
  trajectory_opt <- as.matrix(instant_rewards_opt[, mget(cols)])
  trajectory_opt_mean <- apply(trajectory_opt, 2, mean)
  
  plot(trajectory_hmMDP_mean, type = 'o',pch =0,
       ylim = c(min(trajectory_hmMDP_mean),max(trajectory_opt_mean)+1),
       ylab = 'Expected instant rewards',
       xlab = 'Time steps')
  lines(trajectory_params_mean, type = 'o',
        pch = 1, col = 'red')
  lines(trajectory_opt_mean, type = 'o',
        pch = 2, col = 'blue')
  legend("topleft", bty = "n",horiz=T,
         legend = c("Optimal", "PUBD","UAMS"),
         col = c("blue","red",'black'),
         lty = c(1,1,1), pch = c(2,1,0),
         cex = 0.7)
} else {
  source(file_example)
  
  tab_hmMDP_res <- read.csv(file_simulations_hmMDP)
  tab_hmMDP_4Exp_res <- read.csv(file_simulations_hmMDP_4Exp)
  tab_params_res <- read.csv(file_simulations_params)
  tab_opt_res <- read.csv(file_simulations_opt)
  
  a <- rep(seq(1,N_MDP),100)
  a <- a[order(a)]
  tab_hmMDP_res$mdp_id <- a
  tab_hmMDP_res <- data.table(tab_hmMDP_res)
  tab_hmMDP_res_mean <- tab_hmMDP_res[ , lapply(.SD, mean), by = mdp_id]
  
  tab_hmMDP_4Exp_res$mdp_id <- a
  tab_hmMDP_4Exp_res <- data.table(tab_hmMDP_4Exp_res)
  tab_hmMDP_4Exp_res_mean <- tab_hmMDP_4Exp_res[ , lapply(.SD, mean), by = mdp_id]
  
  tab_params_res$mdp_id <- a
  tab_params_res<- data.table(tab_params_res)
  tab_params_res_mean <- tab_params_res[ , lapply(.SD, mean), by = mdp_id]
  
  #performance at Tmax
  col <- paste0("V",Tmax)
  sim_Tmax <-data.frame(V_opt = tab_opt_res[[col]],
                        V_hmMDP = tab_hmMDP_res_mean[[col]],
                        V_params = tab_params_res_mean[[col]],
                        V_4Exp = tab_hmMDP_4Exp_res_mean[[col]])
  
  #performance at infinity
  col2 <- paste0("V",Tmax_hmMDP)
  sim_inf <-data.frame(V_opt = tab_opt_res[[col2]],
                       V_hmMDP = tab_hmMDP_res_mean[[col2]],
                       V_4Exp = tab_hmMDP_4Exp_res_mean[[col2]])
  
  
  ## instant rewards ####
  instant_rewards_hmMDP <-
    tab_hmMDP_res[,2:ncol(tab_hmMDP_res)]-tab_hmMDP_res[,1:(ncol(tab_hmMDP_res)-1)]
  instant_rewards_hmMDP <- t((1/gamma)**seq(0,ncol(instant_rewards_hmMDP)-1)*
                               t(instant_rewards_hmMDP))
  
  instant_rewards_hmMDP_4Exp <-
    tab_hmMDP_4Exp_res[,2:ncol(tab_hmMDP_4Exp_res)]-
    tab_hmMDP_4Exp_res[,1:(ncol(tab_hmMDP_4Exp_res)-1)]
  instant_rewards_hmMDP_4Exp <- t((1/gamma)**seq(0,ncol(instant_rewards_hmMDP_4Exp)-1)*
                                    t(instant_rewards_hmMDP_4Exp))
  
  instant_rewards_params <- 
    tab_params_res[,2:ncol(tab_params_res)]-
    tab_params_res[,1:(ncol(tab_params_res)-1)]
  instant_rewards_params <- t((1/gamma)**seq(0,ncol(instant_rewards_params)-1)*
                                t(instant_rewards_params))
  
  instant_rewards_opt <- 
    tab_opt_res[,2:ncol(tab_opt_res)]-tab_opt_res[,1:(ncol(tab_opt_res)-1)]
  instant_rewards_opt <- t((1/gamma)**seq(0,ncol(instant_rewards_opt)-1)*
                             t(instant_rewards_opt))
  
  instant_rewards_hmMDP <- as.data.table(instant_rewards_hmMDP)
  instant_rewards_hmMDP_4Exp <- as.data.table(instant_rewards_hmMDP_4Exp)
  instant_rewards_params <- as.data.table(instant_rewards_params)
  instant_rewards_opt <- as.data.table(instant_rewards_opt)
  
  #name after mdp_id
  a <- rep(seq(1,N_MDP),100)
  a <- a[order(a)]
  instant_rewards_hmMDP$mdp_id <- a
  instant_rewards_hmMDP <- data.table(instant_rewards_hmMDP)
  instant_rewards_hmMDP_mean <- instant_rewards_hmMDP[ , lapply(.SD, mean), by = mdp_id]
  
  instant_rewards_hmMDP_4Exp$mdp_id <- a
  instant_rewards_hmMDP_4Exp <- data.table(instant_rewards_hmMDP_4Exp)
  instant_rewards_hmMDP_4Exp_mean <- instant_rewards_hmMDP_4Exp[ , lapply(.SD, mean), by = mdp_id]
  
  instant_rewards_params$mdp_id <- a
  instant_rewards_params <- data.table(instant_rewards_params)
  instant_rewards_params_mean <- instant_rewards_params[ , lapply(.SD, mean), by = mdp_id]
  
  #instant performance at Tmax
  col <- paste0("V",Tmax+1)
  instant_sim_Tmax <-data.frame(V_opt = instant_rewards_opt[[col]],
                                V_hmMDP = instant_rewards_hmMDP_mean[[col]],
                                V_4Exp = instant_rewards_hmMDP_4Exp_mean[[col]],
                                V_params = instant_rewards_params_mean[[col]])
  
  #instant performance at infinity
  col2 <- paste0("V",Tmax_hmMDP)
  instant_sim_inf <-data.frame(V_opt = tab_opt_res[[col2]],
                               V_hmMDP = tab_hmMDP_res_mean[[col2]],
                               V_4Exp = instant_rewards_hmMDP_4Exp_mean[[col2]])
  
  
  #performance table
  performance_table <- data.frame(perf_mean_Tmax = numeric(),
                                  perf_sd_Tmax = numeric(),
                                  perf_mean_inf = numeric(),
                                  perf_sd_inf = numeric(),
                                  inst_mean_Tmax = numeric(),
                                  inst_sd_Tmax = numeric(),
                                  inst_mean_inf = numeric(),
                                  inst_sd_inf = numeric()
  )
  row_hmMDP <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_hmMDP)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_hmMDP)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_hmMDP)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_hmMDP)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_hmMDP)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_hmMDP)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_hmMDP)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_hmMDP)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  row_hmMDP_4Exp <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_4Exp)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_4Exp)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_4Exp)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_4Exp)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_4Exp)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_4Exp)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_4Exp)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_4Exp)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  row_params <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_params)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_params)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_params)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_params)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_params)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_params)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_params)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_params)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  
  performance_table <- rbind(performance_table, row_hmMDP, row_hmMDP_4Exp, row_params)
  row.names(performance_table) <- c("MC-UAMS","4 Exp", "PUBD")
  
  write.csv(performance_table, performance_file)
  
  #trajectories plots
  Tplot<-Tmax_hmMDP
  trajectory_hmMDP <- as.matrix(tab_hmMDP_res)[,seq(Tplot)]
  trajectory_hmMDP_mean <- apply(trajectory_hmMDP, 2, mean)
  
  trajectory_hmMDP_4Exp <- as.matrix(tab_hmMDP_4Exp_res)[,seq(Tplot)]
  trajectory_hmMDP_4Exp_mean <- apply(trajectory_hmMDP_4Exp, 2, mean)
  
  trajectory_params_mean <- apply(tab_params_res[,1:(Tmax)],
                                  2, mean)
  
  trajectory_opt_mean <- apply(tab_opt_res[,seq(Tplot)], 2, mean)
  
  plot(trajectory_hmMDP_mean, type = 'o',pch =0,
       ylim = c(min(trajectory_hmMDP_mean),max(trajectory_opt_mean)),
       ylab = 'Expected sum of discounted rewards',
       xlab = 'Time steps')
  lines(trajectory_hmMDP_4Exp_mean, type = "o", pch = 4, col="green")
  lines(trajectory_params_mean, type = 'o',
        pch = 1, col = 'red')
  lines(trajectory_opt_mean, type = 'o',
        pch = 2, col = 'blue')
  legend("topleft", bty = "n",
         legend = c("Optimal", "PUBD","UAMS","4Experts"),
         col = c("blue","red",'black', 'green'),
         lty = c(1,1,1,1), pch = c(2,1,0,4), lwd = 2,
         cex = 0.7, horiz = T)
  
  #instant trajectories
  Tplot<-25
  
  cols <- paste0("V", 2:(Tplot))
  trajectory_hmMDP <- as.matrix(instant_rewards_hmMDP[, mget(cols)])
  trajectory_hmMDP_mean <- apply(trajectory_hmMDP, 2, mean)
  
  trajectory_hmMDP_4Exp <- as.matrix(instant_rewards_hmMDP_4Exp[, mget(cols)])
  trajectory_hmMDP_4Exp_mean <- apply(trajectory_hmMDP_4Exp, 2, mean)
  
  cols2 <- paste0("V", 2:(Tmax+1))
  trajectory_params <- as.matrix(instant_rewards_params[, mget(cols2)])
  trajectory_params_mean <- apply(trajectory_params,2, mean)
  
  trajectory_opt <- as.matrix(instant_rewards_opt[, mget(cols)])
  trajectory_opt_mean <- apply(trajectory_opt, 2, mean)
  
  plot(trajectory_hmMDP_mean, type = 'o',pch =0,
       ylim = c(min(trajectory_hmMDP_4Exp),max(trajectory_opt_mean)),
       ylab = 'Expected instant rewards',
       xlab = 'Time steps')
  lines(trajectory_hmMDP_4Exp_mean, type = "o", pch = 4, col="green")
  lines(trajectory_params_mean, type = 'o',
        pch = 1, col = 'red')
  lines(trajectory_opt_mean, type = 'o',
        pch = 2, col = 'blue')
  legend("topleft", bty = "n",horiz=T,
         legend = c("Optimal", "PUBD","UAMS"),
         col = c("blue","red",'black'),
         lty = c(1,1,1), pch = c(2,1,0),
         cex = 0.7)
  
  ## real model is the model of expert number 3 ####
  
  tab_hmMDP_res <- read.csv(file_simulations_hmMDP_expert_model)
  tab_hmMDP_4Exp_res <- read.csv(file_simulations_hmMDP_4Exp_expert_model)
  tab_params_res <- read.csv(file_simulations_params_expert_model)
  tab_opt_res <- read.csv(file_simulations_opt_expert_model)
  tab_opt_res<-tab_opt_res$x
  
  #performance at Tmax
  col <- paste0("V",Tmax+1)
  sim_Tmax <-data.frame(V_opt = tab_opt_res[Tmax+1],
                        V_hmMDP = tab_hmMDP_res[[col]],
                        V_params = tab_params_res[[col]],
                        V_4Exp = tab_hmMDP_4Exp_res[[col]])
  
  #performance at infinity
  col2 <- paste0("V",Tmax_hmMDP)
  sim_inf <-data.frame(V_opt = tab_opt_res[Tmax_hmMDP],
                       V_hmMDP = tab_hmMDP_res[[col2]],
                       V_4Exp = tab_hmMDP_4Exp_res[[col2]])
  
  
  #instant rewards ####
  instant_rewards_hmMDP <- cbind(tab_hmMDP_res[,1],
                                 tab_hmMDP_res[,2:ncol(tab_hmMDP_res)]-
                                   tab_hmMDP_res[,1:(ncol(tab_hmMDP_res)-1)])
  instant_rewards_hmMDP <- t((1/gamma)**seq(0,ncol(instant_rewards_hmMDP)-1)*
                               t(instant_rewards_hmMDP))
  
  instant_rewards_hmMDP_4Exp <-cbind(tab_hmMDP_4Exp_res[,1],
                                     tab_hmMDP_4Exp_res[,2:ncol(tab_hmMDP_4Exp_res)]-
                                       tab_hmMDP_4Exp_res[,1:(ncol(tab_hmMDP_4Exp_res)-1)])
  instant_rewards_hmMDP_4Exp <- t((1/gamma)**seq(0,ncol(instant_rewards_hmMDP_4Exp)-1)*
                                    t(instant_rewards_hmMDP_4Exp))
  
  instant_rewards_params <-cbind(tab_params_res[,1],
                                 tab_params_res[,2:ncol(tab_params_res)]-
                                   tab_params_res[,1:(ncol(tab_params_res)-1)])
  instant_rewards_params <- t((1/gamma)**seq(0,ncol(instant_rewards_params)-1)*
                                t(instant_rewards_params))
  
  tab_opt_res <- matrix(rep(tab_opt_res,100), nrow = 100, byrow = T)
  instant_rewards_opt <-cbind(tab_opt_res[,1],
                              tab_opt_res[,2:ncol(tab_opt_res)]-tab_opt_res[,1:(ncol(tab_opt_res)-1)])
  instant_rewards_opt <- t((1/gamma)**seq(0,ncol(instant_rewards_opt)-1)*t(instant_rewards_opt))
  
  instant_rewards_hmMDP <- as.data.table(instant_rewards_hmMDP)
  instant_rewards_hmMDP_4Exp <- as.data.table(instant_rewards_hmMDP_4Exp)
  instant_rewards_params <- as.data.table(instant_rewards_params)
  instant_rewards_opt <- as.data.table(instant_rewards_opt)
  
  #instant performance at Tmax
  col <- paste0("V",Tmax+1)
  instant_sim_Tmax <-data.frame(V_opt = instant_rewards_opt[[col]],
                                V_hmMDP = instant_rewards_hmMDP[[col]],
                                V_4Exp = instant_rewards_hmMDP_4Exp[[col]],
                                V_params = instant_rewards_params[[col]])
  
  #instant performance at infinity
  col2 <- paste0("V",Tmax_hmMDP)
  instant_sim_inf <-data.frame(V_opt = instant_rewards_opt[[col]],
                               V_hmMDP = instant_rewards_hmMDP[[col2]],
                               V_4Exp = instant_rewards_hmMDP_4Exp[[col2]])
  
  
  #performance table
  performance_table <- data.frame(perf_mean_Tmax = numeric(),
                                  perf_sd_Tmax = numeric(),
                                  perf_mean_inf = numeric(),
                                  perf_sd_inf = numeric(),
                                  inst_mean_Tmax = numeric(),
                                  inst_sd_Tmax = numeric(),
                                  inst_mean_inf = numeric(),
                                  inst_sd_inf = numeric()
  )
  row_hmMDP <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_hmMDP)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_hmMDP)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_hmMDP)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_hmMDP)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_hmMDP)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_hmMDP)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_hmMDP)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_hmMDP)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  row_hmMDP_4Exp <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_4Exp)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_4Exp)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_4Exp)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_4Exp)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_4Exp)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_4Exp)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_4Exp)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_4Exp)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  row_params <- 
    data.frame(perf_mean_Tmax = 
                 mean((sim_Tmax$V_opt - sim_Tmax$V_params)/sim_Tmax$V_opt),
               perf_sd_Tmax = 
                 1.96*sd((sim_Tmax$V_opt - sim_Tmax$V_params)/sim_Tmax$V_opt)/(nrow(sim_Tmax)**0.5),
               perf_mean_inf =
                 mean((sim_inf$V_opt - sim_inf$V_params)/sim_inf$V_opt),
               perf_sd_inf = 
                 1.96*sd((sim_inf$V_opt - sim_inf$V_params)/sim_inf$V_opt)/(nrow(sim_inf)**0.5),
               inst_mean_Tmax = 
                 mean((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_params)/instant_sim_Tmax$V_opt),
               inst_sd_Tmax =  
                 1.96*sd((instant_sim_Tmax$V_opt - instant_sim_Tmax$V_params)/instant_sim_Tmax$V_opt)/(nrow(instant_sim_Tmax)**0.5),
               inst_mean_inf =
                 mean((instant_sim_inf$V_opt - instant_sim_inf$V_params)/instant_sim_inf$V_opt),
               inst_sd_inf = 
                 1.96*sd((instant_sim_inf$V_opt - instant_sim_inf$V_params)/instant_sim_inf$V_opt)/(nrow(instant_sim_inf)**0.5)
    )
  
  performance_table <- rbind(performance_table, row_hmMDP, row_hmMDP_4Exp, row_params)
  row.names(performance_table) <- c("MC-UAMS","4 Exp", "PUBD")
  
  write.csv(performance_table, performance_file_expert_model)
  
  #trajectories plots
  Tplot<-Tmax_hmMDP
  trajectory_hmMDP <- as.matrix(tab_hmMDP_res)[,seq(Tplot)]
  trajectory_hmMDP_mean <- apply(trajectory_hmMDP, 2, mean)
  
  trajectory_hmMDP_4Exp <- as.matrix(tab_hmMDP_4Exp_res)[,seq(Tplot)]
  trajectory_hmMDP_4Exp_mean <- apply(trajectory_hmMDP_4Exp, 2, mean)
  
  trajectory_params_mean <- apply(tab_params_res[,1:(Tmax)],
                                  2, mean)
  
  trajectory_opt_mean <- apply(tab_opt_res[,seq(Tplot)], 2, mean)
  
  plot(trajectory_hmMDP_mean, type = 'o',pch =0,
       ylim = c(min(trajectory_hmMDP_mean),max(trajectory_opt_mean)),
       ylab = 'Expected sum of discounted rewards',
       xlab = 'Time steps')
  lines(trajectory_hmMDP_4Exp_mean, type = "o", pch = 4, col="green")
  lines(trajectory_params_mean, type = 'o',
        pch = 1, col = 'red')
  lines(trajectory_opt_mean, type = 'o',
        pch = 2, col = 'blue')
  legend("topleft", bty = "n",
         legend = c("Optimal", "PUBD","UAMS","4Experts"),
         col = c("blue","red",'black', 'green'),
         lty = c(1,1,1,1), pch = c(2,1,0,4), lwd = 2,
         cex = 0.7, horiz = T)
  
  #instant trajectories
  Tplot<-25
  
  cols <- paste0("V", 2:(Tplot))
  trajectory_hmMDP <- as.matrix(instant_rewards_hmMDP[, mget(cols)])
  trajectory_hmMDP_mean <- apply(trajectory_hmMDP, 2, mean)
  
  trajectory_hmMDP_4Exp <- as.matrix(instant_rewards_hmMDP_4Exp[, mget(cols)])
  trajectory_hmMDP_4Exp_mean <- apply(trajectory_hmMDP_4Exp, 2, mean)
  
  cols2 <- paste0("V", 2:(Tmax+1))
  trajectory_params <- as.matrix(instant_rewards_params[, mget(cols2)])
  trajectory_params_mean <- apply(trajectory_params,2, mean)
  
  trajectory_opt <- as.matrix(instant_rewards_opt[, mget(cols)])
  trajectory_opt_mean <- apply(trajectory_opt, 2, mean)
  
  plot(trajectory_hmMDP_mean, type = 'o',pch =0,
       ylim = c(min(trajectory_hmMDP_4Exp),max(trajectory_opt_mean)),
       ylab = 'Expected instant rewards',
       xlab = 'Time steps')
  lines(trajectory_hmMDP_4Exp_mean, type = "o", pch = 4, col="green")
  lines(trajectory_params_mean, type = 'o',
        pch = 1, col = 'red')
  lines(trajectory_opt_mean, type = 'o',
        pch = 2, col = 'blue')
  legend("topleft", bty = "n",horiz=T,
         legend = c("Optimal", "PUBD","UAMS"),
         col = c("blue","red",'black'),
         lty = c(1,1,1), pch = c(2,1,0),
         cex = 0.7)
}
