setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
source("plot_functions.R")

dbs <- c( "flow_alpha_experiment/db/flow.db",
          "flow_alpha_experiment/db/flow_mbmc.db",
          "flow_alpha_experiment/db/flow_fm.db")

algo <- c( "flow","flow_mbmc","flow_fm")

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
                                select_soed), c("graph","k","seed"), aggreg)

flow_dbs <- list()
for(i in 1:length(dbs)) {
  flow_dbs[[i]] <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=dbs[i]),
                                   select_km1_soed), c("graph","k","alpha","seed"), aggreg)
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
 
 kahypar_sea$alpha <- 1
 
 aggreg = function(df) data.frame(min_time=min(df$avg_time),
                                  max_time=max(df$avg_time))
 all <- rbind(kahypar_sea,flow,flow_mbmc,flow_fm)
 all <- ddply(all, c("graph","k"), aggreg)
 
write.csv(all, file = "effectiveness_table.csv")

 