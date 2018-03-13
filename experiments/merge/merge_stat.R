setwd("/home/heuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments")
source("plot_functions.R")


dbs <- c( "merge/db/old_kahypar.db",
          "merge/db/new_kahypar.db")

algo <- c( "old_kahypar",
           "new_kahypar")

select_km1_soed = 'select graph,k,epsilon,flow_region_size_alpha AS alpha, seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_km1_soed_2 = 'select graph,k,epsilon,flow_max_alpha AS alpha, seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
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

kahypar_sea <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="merge/db/kahypar_sea.db"),
                                select_soed), c("graph","k"), aggreg)
kahypar_old <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="merge/db/old_kahypar.db"),
                                select_km1_soed), c("graph","k","alpha"), aggreg)
kahypar_new <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="merge/db/new_kahypar.db"),
                                select_km1_soed_2), c("graph","k","alpha"), aggreg)
kahypar_new_he_removal <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="merge/db/new_kahypar_he_removal.db"),
                                           select_km1_soed_2), c("graph","k","alpha"), aggreg)


#extract graph classes from graph names
kahypar_sea$type <- as.factor(apply(kahypar_sea, 1, function(x) graphclass(x)))
kahypar_old$type <- as.factor(apply(kahypar_old, 1, function(x) graphclass(x)))
kahypar_new$type <- as.factor(apply(kahypar_new, 1, function(x) graphclass(x)))
kahypar_new_he_removal$type <- as.factor(apply(kahypar_new_he_removal, 1, function(x) graphclass(x)))


# give each DF a name to identify the algorithm
kahypar_sea$algorithm = "kahypar_sea"
kahypar_old$algorithm = "kahypar_old"
kahypar_new$algorithm = "kahypar_new"
kahypar_new_he_removal$algorithm = "kahypar_new_he_removal"


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
   ref_vec <- c("Ref.", "\\multicolumn{2}{c}{\\FlowVariant{-}{-}{+}}", 
                         formatC(ref$gmean_km1[1], digits=2,format='f'), 
                         formatC(ref$gmean_time[1],digits=2,format='f'),
                         "\\multicolumn{6}{c}{}")

   alpha_vec <- list(c(1),c(2),c(4),c(8),c(16))
   for(df in dataframes) {
     alpha_df <- ddply(df, c("alpha"), aggreg)
     alpha_df$gmean_km1 <-  (1.0 - alpha_df$gmean_km1 / ref$gmean_km1 ) * 100
     alpha_df$gmean_time <- alpha_df$gmean_time
     alpha_vec[[1]] <- c(alpha_vec[[1]], format(alpha_df$gmean_km1[1]), format(alpha_df$gmean_time[1]))
     alpha_vec[[2]] <- c(alpha_vec[[2]], format(alpha_df$gmean_km1[2]), format(alpha_df$gmean_time[2]))
     alpha_vec[[3]] <- c(alpha_vec[[3]], format(alpha_df$gmean_km1[3]), format(alpha_df$gmean_time[3]))
     alpha_vec[[4]] <- c(alpha_vec[[4]], format(alpha_df$gmean_km1[4]), format(alpha_df$gmean_time[4]))
     alpha_vec[[5]] <- c(alpha_vec[[5]], format(alpha_df$gmean_km1[5]), format(alpha_df$gmean_time[5]))
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
 
 
 flow_alpha_table(kahypar_sea, kahypar_old, kahypar_new, kahypar_new_he_removal)
 
