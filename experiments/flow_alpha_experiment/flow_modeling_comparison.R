setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
source("plot_functions.R")

dbs <- c( "flow_alpha_experiment/db/flow.db",
          "flow_alpha_experiment/db/flow_mbmc.db",
          "flow_alpha_experiment/db/flow_fm.db",
          "flow_alpha_experiment/db/constant.db",
          "flow_alpha_experiment/old_db/flow_cut_he.db",
          "flow_alpha_experiment/old_db/flow_cut_he_mbmc.db",
          "flow_alpha_experiment/old_db/flow_fm.db",
          "flow_alpha_experiment/old_db/constant.db")

algo <- c( "flow","flow_mbmc","flow_fm","constant","flow_old", "flow_mbmc_old", "flow_fm_old","constant_old")

select_km1_soed = 'select graph,k,epsilon,flow_region_size_alpha AS alpha, seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_km1 = 'select graph,k,epsilon,flow_region_size_alpha AS alpha,seed,1 AS soed, kMinusOne AS km1, imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_soed = 'select graph,k,epsilon,seed,soed-cut AS km1, soed, imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'

select_detailed = 'select graph,k,epsilon,seed,km1,soed,imbalance, hns_after_coarsening, coarsening_contraction_limit, coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'

aggreg = function(df) data.frame(min_km1=min(df$km1), 
                                 avg_km1=mean(df$km1), 
                                 min_cut=min(df$cut), 
                                 avg_cut=mean(df$cut), 
                                 min_soed=min(df$soed), 
                                 avg_soed=mean(df$soed), 
                                 avg_imbalance=mean(df$imbalance), 
                                 avg_ctime=mean(df$coarseningTime), 
                                 avg_rtime=mean(df$uncoarseningRefinementTime), 
                                 avg_time=mean(df$totalPartitionTime),
                                 cnt=length(df$seed))

kahypar_sea <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="flow_alpha_experiment/db/kahypar_sea.db"),
                                select_soed), c("graph","k"), aggreg)

flow_dbs <- list()
for(i in 1:length(dbs)) {
  flow_dbs[[i]] <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=dbs[i]),
                                   select_km1_soed), c("graph","k","alpha"), aggreg)
}

#extract graph classes from graph names
kahypar_sea$type <- as.factor(apply(kahypar_sea, 1, function(x) graphclass(x)))
for(i in 1:length(dbs)) {
  flow_dbs[[i]]$type <- as.factor(apply(flow_dbs[[i]], 1, function(x) graphclass(x)))
}

# give each DF a name to identify the algorithm
kahypar_sea$algorithm = "kahypar_sea"
for(i in 1:length(dbs)) {
  flow_dbs[[i]]$algorithm <- algo[i]
}

 flow <- flow_dbs[[1]]
 flow_mbmc <- flow_dbs[[2]]
 flow_fm <- flow_dbs[[3]]
 constant <- flow_dbs[[4]]
 flow_old <- flow_dbs[[5]]
 flow_mbmc_old <- flow_dbs[[6]]
 flow_fm_old <- flow_dbs[[7]]
 constant_old <- flow_dbs[[8]]
 

 ############################################################################################
 
 to_latex_math_mode <- function(x) {
   return(paste("$",x,"$", sep=""))
 }
 
 flow_alpha_table <- function(kahypar, ...) {
   dataframes <- list(...)
   
   aggreg = function(df) data.frame(gmean_km1=gm_mean(df$avg_km1),   
                                    gmean_time=gm_mean(df$avg_time))
   ref <- ddply(kahypar,c(),aggreg)
   ref_vec <- c("Ref.", "\\multicolumn{2}{c||}{\\FlowVariant{-}{-}{+}}", 
                         paste("\\multicolumn{2}{c|}{",to_latex_math_mode(round(ref$gmean_km1[1], digits=2)),"}",sep=""), 
                         "\\multicolumn{2}{c|}{}")

   alpha_vec <- list(c(1),c(2),c(4),c(8),c(16))
   for(df in dataframes) {
     alpha_df <- ddply(df, c("alpha"), aggreg)
     alpha_df$gmean_km1 <-  round((alpha_df$gmean_km1 / ref$gmean_km1 - 1.0) * 100, digits = 2)
     alpha_df$gmean_time <- round(alpha_df$gmean_time, digits = 2)
     alpha_vec[[1]] <- c(alpha_vec[[1]], -alpha_df$gmean_km1[1])
     alpha_vec[[2]] <- c(alpha_vec[[2]], -alpha_df$gmean_km1[2])
     alpha_vec[[3]] <- c(alpha_vec[[3]], -alpha_df$gmean_km1[3])
     alpha_vec[[4]] <- c(alpha_vec[[4]], -alpha_df$gmean_km1[4])
     alpha_vec[[5]] <- c(alpha_vec[[5]], -alpha_df$gmean_km1[5])
   }
   for(vec in alpha_vec) {
     cat(paste(sapply(vec, to_latex_math_mode),collapse=" & "))
     cat(" \\\\ \n")
   }
   cat(paste("\\cmidrule{1-", length(dataframes)+1 ,"}% \n", sep=""))
   cat(paste(ref_vec,collapse=" & "))
   cat(" \\\\ \n")
 }
 
 
 
 sink("../master_thesis/experiments/flow_alpha/flow_alpha_modeling_comparison_table.tex")
 flow_alpha_table(kahypar_sea, flow_old, flow, flow_mbmc_old, flow_mbmc, flow_fm_old, flow_fm)
 sink()
 
 