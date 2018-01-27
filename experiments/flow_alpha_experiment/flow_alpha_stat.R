setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
source("plot_functions.R")

paper <- "experiment_paper"
experiment <- "flow_alpha"
modeling <- "m2"
flow_algo <- "ibfs2"

dbs <- c( paste("flow_alpha_experiment/db_",flow_algo,"/flow.db",sep=""),
          paste("flow_alpha_experiment/db_",flow_algo,"/flow_mbmc.db",sep=""),
          paste("flow_alpha_experiment/db_",flow_algo,"/flow_fm.db",sep=""),
          paste("flow_alpha_experiment/db_",flow_algo,"/constant.db",sep=""),
          paste("flow_alpha_experiment/db_",flow_algo,"/flow_eff.db",sep=""),
          paste("flow_alpha_experiment/db_",flow_algo,"/flow_mbmc_eff.db",sep=""),
          paste("flow_alpha_experiment/db_",flow_algo,"/flow_fm_eff.db",sep=""))

algo <- c( "flow","flow_mbmc","flow_fm","constant","flow_eff","flow_mbmc_eff","flow_fm_eff")

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

kahypar_sea <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("flow_alpha_experiment/db_",flow_algo,"/kahypar_sea.db",sep="")),
                                select_soed), c("graph","k"), aggreg)
kahypar_ca <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("flow_alpha_experiment/db_",flow_algo,"/kahypar_ca.db",sep="")),
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
 flow_eff <- flow_dbs[[5]]
 flow_mbmc_eff <- flow_dbs[[6]]
 flow_fm_eff <- flow_dbs[[7]]
 
 missing_instances <- function(db, complete_db) {
   K <- c(2,4,8,16,32,64,128)
   for(hg in levels(factor(db$graph))) {
     for(k in K) {
       if(length(db[db$graph == hg & db$k == k,]$alpha) != 5) {
         missing_alpha <- setdiff(c(1,2,4,8,16), db[db$graph == hg & db$k == k,]$alpha)
         print(paste("graph=",hg," - k=",k,sep=""))
         print(paste("alpha=",missing_alpha,sep=""))
         db <- rbind(db, complete_db[complete_db$graph == hg &
                                     complete_db$k == k &
                                     complete_db$alpha %in% missing_alpha,])
       }
     }
   }
   return(db)
 }
 
 flow_eff <- missing_instances(flow_eff, flow)
 flow_mbmc_eff <- missing_instances(flow_mbmc_eff, flow_mbmc)
 flow_fm_eff <- missing_instances(flow_fm_eff, flow_fm)


 ############################################################################################
 
 to_latex_math_mode <- function(x) {
   return(paste("$",x,"$", sep=""))
 }
 
 format <- function(x) {
   return(formatC(x, digits=2, format='f'))
 }
 
 flow_alpha_table <- function(kahypar, ...) {
   dataframes <- list(...)
   
   aggreg = function(df) data.frame(gmean_km1=gm_mean(df$avg_km1),   
                                    gmean_time=gm_mean(df$avg_time))
   ref <- ddply(kahypar,c(),aggreg)
   ref_vec <- c("Ref.", "\\multicolumn{2}{c||}{\\FlowVariant{-}{-}{+}}", 
                         formatC(ref$gmean_km1[1], digits=2,format='f'), 
                         formatC(ref$gmean_time[1],digits=2,format='f'),
                         "\\multicolumn{2}{c||}{}",
                         "\\multicolumn{2}{c|}{}")

   alpha_vec <- list(c(1),c(2),c(4),c(8),c(16))
   for(df in dataframes) {
     alpha_df <- ddply(df, c("alpha"), aggreg)
     alpha_df$gmean_km1 <-  (alpha_df$gmean_km1 / ref$gmean_km1 - 1.0) * 100
     alpha_df$gmean_time <- alpha_df$gmean_time
     alpha_vec[[1]] <- c(alpha_vec[[1]], format(-alpha_df$gmean_km1[1]), format(alpha_df$gmean_time[1]))
     alpha_vec[[2]] <- c(alpha_vec[[2]], format(-alpha_df$gmean_km1[2]), format(alpha_df$gmean_time[2]))
     alpha_vec[[3]] <- c(alpha_vec[[3]], format(-alpha_df$gmean_km1[3]), format(alpha_df$gmean_time[3]))
     alpha_vec[[4]] <- c(alpha_vec[[4]], format(-alpha_df$gmean_km1[4]), format(alpha_df$gmean_time[4]))
     alpha_vec[[5]] <- c(alpha_vec[[5]], format(-alpha_df$gmean_km1[5]), format(alpha_df$gmean_time[5]))
   }
   for(vec in alpha_vec) {
     vec[1] <- to_latex_math_mode(vec[1])
     cat(paste(vec,collapse=" & "))
     cat(" \\\\ \n")
   }
   cat(paste("\\cmidrule{1-", 2*length(dataframes)+1 ,"}% \n", sep=""))
   cat(paste(ref_vec,collapse=" & "))
   cat(" \\\\ \n")
 }
 
 flow_alpha_effectiveness_table <- function(kahypar, ...) {
   dataframes <- list(...)
   
   aggreg = function(df) data.frame(gmean_km1=gm_mean(df$avg_km1))
   ref <- ddply(kahypar,c(),aggreg)
   ref_vec <- c("Ref.", " \\multicolumn{1}{|c|}{\\FlowVariant{-}{-}{+}}", 
                format(ref$gmean_km1[1]), "")
   
   alpha_vec <- list(c(1),c(2),c(4),c(8),c(16))
   for(df in dataframes) {
     alpha_df <- ddply(df, c("alpha"), aggreg)
     alpha_df$gmean_km1 <-  (alpha_df$gmean_km1 / ref$gmean_km1 - 1.0) * 100
     alpha_vec[[1]] <- c(alpha_vec[[1]], format(-alpha_df$gmean_km1[1]))
     alpha_vec[[2]] <- c(alpha_vec[[2]], format(-alpha_df$gmean_km1[2]))
     alpha_vec[[3]] <- c(alpha_vec[[3]], format(-alpha_df$gmean_km1[3]))
     alpha_vec[[4]] <- c(alpha_vec[[4]], format(-alpha_df$gmean_km1[4]))
     alpha_vec[[5]] <- c(alpha_vec[[5]], format(-alpha_df$gmean_km1[5]))
   }
   for(vec in alpha_vec) {
     vec[1] <- to_latex_math_mode(vec[1])
     cat(paste(vec,collapse=" & "))
     cat(" \\\\ \n")
   }
   cat(paste("\\cmidrule{1-", length(dataframes)+1 ,"}% \n", sep=""))
   cat(paste(ref_vec,collapse=" & "))
   cat(" \\\\ \n")
 }
 
 sink(output_file(paper,experiment,"flow_alpha_table",modeling,flow_algo))
 flow_alpha_table(kahypar_sea, flow, flow_mbmc, flow_fm, constant)
 sink()
 
 sink(output_file(paper,experiment,"flow_alpha_effectiveness_table",modeling,flow_algo))
 flow_alpha_effectiveness_table(kahypar_ca, flow_eff, flow_mbmc_eff, flow_fm_eff)
 sink()
 
