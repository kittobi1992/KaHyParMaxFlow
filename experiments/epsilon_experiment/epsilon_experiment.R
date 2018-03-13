#setwd("/home/heuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments")
source("plot_functions.R")

library(gridExtra)
library(grid)


dbs <- c( "epsilon_experiment/db/kahypar_ca_eps1.db",
          "epsilon_experiment/db/kahypar_ca_eps3.db",
          "epsilon_experiment/db/kahypar_ca_eps5.db",
          "epsilon_experiment/db/kahypar_ca_eps10.db",
          "epsilon_experiment/db/kahypar_mf_eps1.db",
          "epsilon_experiment/db/kahypar_mf_eps3.db",
          "epsilon_experiment/db/kahypar_mf_eps5.db",
          "epsilon_experiment/db/kahypar_mf_eps10.db")

algo <- c( "kahypar_ca_eps1",
           "kahypar_ca_eps3",
           "kahypar_ca_eps5",
           "kahypar_ca_eps10",
           "kahypar_mf_eps1",
           "kahypar_mf_eps3",
           "kahypar_mf_eps5",
           "kahypar_mf_eps10")

select_km1_soed = 'select graph,k,epsilon,seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_km1 = 'select graph,k,epsilon,seed,1 AS soed, kMinusOne AS km1, imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
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


full_instance_stats = dbGetQuery(dbConnect(SQLite(), dbname="common_dbs/hgr_stats.db"),
                                 "Select * from experiments")

flow_dbs <- list()
for(i in 1:length(dbs)) {
  flow_dbs[[i]] <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=dbs[i]),
                                   select_km1_soed), c("graph","k"), aggreg)
}

for(i in 1:length(dbs)) {
  flow_dbs[[i]]$type <- as.factor(apply(flow_dbs[[i]], 1, function(x) graphclass(x)))
}

# restrict benchmark set to all instances for which we currently have results
semi_join_filter = semi_join(flow_dbs[[1]], flow_dbs[[2]], by=c('graph','k'))
for(i in 3:length(dbs)) {
  semi_join_filter = semi_join(semi_join_filter, flow_dbs[[i]], by=c('graph','k'))
}

for(i in 1:length(dbs)) {
  flow_dbs[[i]]= semi_join(flow_dbs[[i]], semi_join_filter, by=c('graph','k'))
}

# give each DF a name to identify the algorithm
for(i in 1:length(dbs)) {
  flow_dbs[[i]]$algorithm <- algo[i]
}

for(i in 1:length(dbs)) {
  flow_dbs[[i]] = merge(flow_dbs[[i]],full_instance_stats,by='graph')
}

kahypar_ca_eps1 <- flow_dbs[[1]]
kahypar_ca_eps3 <- flow_dbs[[2]]
kahypar_ca_eps5 <- flow_dbs[[3]]
kahypar_ca_eps10 <- flow_dbs[[4]]
kahypar_mf_eps1 <- flow_dbs[[5]]
kahypar_mf_eps3 <- flow_dbs[[6]]
kahypar_mf_eps5 <- flow_dbs[[7]]
kahypar_mf_eps10 <- flow_dbs[[8]]



##############################
# geometric mean calculation
##############################

print("epsilon=0.01")
print(calculateGmeansFilter(filter = "*",
                            avg_obj = "avg_km1", min_obj = "min_km1",
                            kahypar = kahypar_ca_eps1,
                            kahypar_mf_eps1 = kahypar_mf_eps1))

print("epsilon=0.03")
print(calculateGmeansFilter(filter = "*",
                            avg_obj = "avg_km1", min_obj = "min_km1",
                            kahypar = kahypar_ca_eps3,
                            kahypar_mf_eps3 = kahypar_mf_eps3))

print("epsilon=0.05")
print(calculateGmeansFilter(filter = "*",
                            avg_obj = "avg_km1", min_obj = "min_km1",
                            kahypar = kahypar_ca_eps5,
                            kahypar_mf_eps5 = kahypar_mf_eps5))

print("epsilon=0.1")
print(calculateGmeansFilter(filter = "*",
                            avg_obj = "avg_km1", min_obj = "min_km1",
                            kahypar = kahypar_ca_eps10,
                            kahypar_mf_eps10 = kahypar_mf_eps10))
