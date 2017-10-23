working_dir="/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments"
source(paste(working_dir, "flow_network_functions.R", sep="/"))

aggreg = function(df) data.frame(avg_hn_degree=mean(df$avgHypernodeDegree),
                                 avg_he_size=mean(df$avgHyperedgeSize),
                                 avg_num_nodes=mean(df$flow_network_num_nodes), 
                                 avg_num_edges=mean(df$flow_network_num_edges), 
                                 avg_cut=min(df$cut), 
                                 avg_max_flow=mean(df$max_flow), 
                                 min_network_build_time=mean(df$min_network_build_time), 
                                 avg_network_build_time=mean(df$avg_network_build_time), 
                                 min_max_flow_time=mean(df$min_max_flow_time), 
                                 avg_max_flow_time=mean(df$avg_max_flow_time))

db_name=paste(working_dir,"flow_network_fix_size/flow_network_test.db",sep="/")
flow_network_db = dbGetQuery(dbConnect(SQLite(), dbname=db_name), "select * from experiments")
flow_network_db$num_hypernodes <- ifelse(flow_network_db$num_hypernodes > 10000, 25000, flow_network_db$num_hypernodes)
flow_network_db <- ddply(flow_network_db, c("hypergraph","flow_network","flow_algorithm","num_hypernodes"), aggreg)

flow_network_db$type <- as.factor(apply(flow_network_db, 1, function(x) graphclass(x)))

flow_network_db$avg_cut <- as.numeric(as.character(flow_network_db$avg_cut))
flow_network_db$avg_max_flow <- as.numeric(as.character(flow_network_db$avg_max_flow))
flow_network_db$num_hypernodes <- as.numeric(as.character(flow_network_db$num_hypernodes))
flow_network_db$avg_num_nodes <- as.numeric(as.character(flow_network_db$avg_num_nodes))
flow_network_db$avg_num_edges <- as.numeric(as.character(flow_network_db$avg_num_edges))
flow_network_db$min_network_build_time <- as.numeric(as.character(flow_network_db$min_network_build_time))
flow_network_db$avg_network_build_time <- as.numeric(as.character(flow_network_db$avg_network_build_time))
flow_network_db$min_max_flow_time <- as.numeric(as.character(flow_network_db$min_max_flow_time))
flow_network_db$avg_max_flow_time <- as.numeric(as.character(flow_network_db$avg_max_flow_time))

flow_network_db$flow_network <- factor(flow_network_db$flow_network)
flow_network_db$flow_network <- factor(flow_network_db$flow_network, levels = levels(flow_network_db$flow_network)[c(3,4,1,2)])


plot(max_flow_time_per_network_plot(flow_network_db, title="Runtime per network and flow algorithm in ms") + facet_grid(num_hypernodes ~ type, scales="free")  )

types <- c("SAT14", "Dual", "Primal", "ISPD98", "SPM")
for (t in types) {
  flow_network_subset <- subset(flow_network_db, flow_network_db$type == t)
  plot_title <- paste("Runtime of max flow algorithm for ",t," instances in ms",sep="")
  plot(max_flow_time_per_network_plot(flow_network_subset, title=plot_title) + facet_wrap(~ num_hypernodes, scales="free"))
}

gmean_network_algorithm_table(flow_network_db)

for( num in levels(factor(flow_network_db$num_hypernodes))) {
  for (type in levels(factor(flow_network_db$type))) {
    print(paste("Type:",type," - Num HNs:", num, sep=" "))
    print(gmean_network_algorithm_table(flow_network_db[flow_network_db$num_hypernodes == num & flow_network_db$type == type & flow_network_db$flow_algorithm == "goldberg_tarjan",]))
    print("")
  }
}

for( num in levels(factor(flow_network_db$num_hypernodes))) {
  for (type in levels(factor(flow_network_db$type))) {
    print(paste("Type:",type," - Num HNs:", num, sep=" "))
    print(gmean_network_algorithm_table(flow_network_db[flow_network_db$num_hypernodes == num & flow_network_db$type == type & flow_network_db$flow_network == "hybrid",]))
    print("")
  }
}
