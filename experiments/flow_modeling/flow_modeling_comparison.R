setwd("/home/heuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments")
source("plot_functions.R")

dbs <- c( "flow_modeling/hypergraph_ibfs/flow_m1_eps1.db",
          "flow_modeling/hypergraph_ibfs/flow_m2_eps1.db",
          "flow_modeling/hypergraph_ibfs/flow_m1_eps3.db",
          "flow_modeling/hypergraph_ibfs/flow_m2_eps3.db",
          "flow_modeling/hypergraph_ibfs/flow_m1_eps5.db",
          "flow_modeling/hypergraph_ibfs/flow_m2_eps5.db",
          "flow_modeling/graph_ibfs/flow_m1_eps1.db",
          "flow_modeling/graph_ibfs/flow_m2_eps1.db",
          "flow_modeling/graph_ibfs/flow_m1_eps3.db",
          "flow_modeling/graph_ibfs/flow_m2_eps3.db",
          "flow_modeling/graph_ibfs/flow_m1_eps5.db",
          "flow_modeling/graph_ibfs/flow_m2_eps5.db")

algo <- c( "flow_m1_eps1_hg","flow_m2_eps1_hg",
           "flow_m1_eps3_hg","flow_m2_eps3_hg",
           "flow_m1_eps5_hg","flow_m2_eps5_hg",
           "flow_m1_eps1_graph","flow_m2_eps1_graph",
           "flow_m1_eps3_graph","flow_m2_eps3_graph",
           "flow_m1_eps5_graph","flow_m2_eps5_graph")

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

kahypar_sea <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="flow_alpha_experiment/db_bk/kahypar_sea.db"),
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

 flow_m1_eps1_hg <- flow_dbs[[1]]
 flow_m2_eps1_hg <- flow_dbs[[2]]
 flow_m1_eps3_hg <- flow_dbs[[3]]
 flow_m2_eps3_hg <- flow_dbs[[4]]
 flow_m1_eps5_hg <- flow_dbs[[5]]
 flow_m2_eps5_hg <- flow_dbs[[6]]
 flow_m1_eps1_graph <- flow_dbs[[7]]
 flow_m2_eps1_graph <- flow_dbs[[8]]
 flow_m1_eps3_graph <- flow_dbs[[9]]
 flow_m2_eps3_graph <- flow_dbs[[10]]
 flow_m1_eps5_graph <- flow_dbs[[11]]
 flow_m2_eps5_graph <- flow_dbs[[12]]
 

 ############################################################################################
 
 format <- function(x) {
   return(formatC(x, digits=1, format='f'))
 }
 
 
 flow_alpha_table <- function(kahypar, ...) {
   dataframes <- list(...)
   
   aggreg = function(df) data.frame(gmean_km1=gm_mean(df$avg_km1),   
                                    gmean_time=gm_mean(df$avg_time))

   n <- length(dataframes)
   alpha_vec <- list(c(1),c(2),c(4),c(8),c(16))
   for(i in c(1:(n/2))) {
     alpha_m1 <- ddply(dataframes[[2*i-1]], c("alpha"), aggreg)
     alpha_m2 <- ddply(dataframes[[2*i]], c("alpha"), aggreg)
     improvement <-  (1.0 - alpha_m2$gmean_km1 / alpha_m1$gmean_km1) * 100
     alpha_vec[[1]] <- c(alpha_vec[[1]], format(improvement[1]))
     alpha_vec[[2]] <- c(alpha_vec[[2]], format(improvement[2]))
     alpha_vec[[3]] <- c(alpha_vec[[3]], format(improvement[3]))
     alpha_vec[[4]] <- c(alpha_vec[[4]], format(improvement[4]))
     alpha_vec[[5]] <- c(alpha_vec[[5]], format(improvement[5]))
   }
   for(vec in alpha_vec) {
     vec[1] <- to_latex_math_mode(vec[1])
     cat(paste(vec,collapse=" & "))
     cat(" \\\\ \n")
   }
 }
 
 sink("../experiment_paper/experiments/flow_alpha/flow_alpha_modeling_comparison_epsilon_table.tex")
 flow_alpha_table(kahypar_sea, 
                  flow_m1_eps1_hg, flow_m2_eps1_hg, 
                  flow_m1_eps3_hg, flow_m2_eps3_hg, 
                  flow_m1_eps5_hg, flow_m2_eps5_hg,
                  flow_m1_eps1_graph, flow_m2_eps1_graph,
                  flow_m1_eps3_graph, flow_m2_eps3_graph,
                  flow_m1_eps5_graph, flow_m2_eps5_graph)
 sink()
 
 