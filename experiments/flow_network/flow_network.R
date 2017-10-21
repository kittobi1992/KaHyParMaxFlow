working_dir="/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments"
source(paste(working_dir, "flow_network_functions.R", sep="/"))


db_name=paste(working_dir,"flow_network/flow_network_test.db",sep="/")
flow_network_db = dbGetQuery(dbConnect(SQLite(), dbname=db_name), "select * from experiments")

flow_network_db$type <- as.factor(apply(flow_network_db, 1, function(x) graphclass(x)))

flow_network_db$cut <- as.numeric(as.character(flow_network_db$cut))
flow_network_db$max_flow <- as.numeric(as.character(flow_network_db$max_flow))
flow_network_db$num_hypernodes <- as.numeric(as.character(flow_network_db$num_hypernodes))
flow_network_db$flow_network_num_nodes <- as.numeric(as.character(flow_network_db$flow_network_num_nodes))
flow_network_db$flow_network_num_edges <- as.numeric(as.character(flow_network_db$flow_network_num_edges))
flow_network_db$min_network_build_time <- as.numeric(as.character(flow_network_db$min_network_build_time))
flow_network_db$avg_network_build_time <- as.numeric(as.character(flow_network_db$avg_network_build_time))
flow_network_db$min_max_flow_time <- as.numeric(as.character(flow_network_db$min_max_flow_time))
flow_network_db$avg_max_flow_time <- as.numeric(as.character(flow_network_db$avg_max_flow_time))

flow_network_db$flow_network <- factor(flow_network_db$flow_network)
flow_network_db$flow_network <- factor(flow_network_db$flow_network, levels = levels(flow_network_db$flow_network)[c(3,4,1,2)])


plot(max_flow_time_per_network_plot(flow_network_db, title="Runtime per network and flow algorithm in ms"))

alphas <- c(1,2,4,8,16)
#Runtime per network and flow algorithm for each alpha in {1,2,4,8,16}
for (a in alphas) {
  flow_network_alpha <- subset(flow_network_db, flow_network_db$alpha == a)
  plot_title <- paste("Runtime of max flow algorithm per benchmark type in ms (alpha=",toString(a),")",sep="")
  plot(max_flow_time_per_benchmark_type_plot(flow_network_alpha, title=plot_title))
}

flow_network_small_edge_size_small_hn_degree <- subset(flow_network_db, flow_network_db$avgHyperedgeSize < 3 & flow_network_db$avgHypernodeDegree < 4)
flow_network_small_edge_size_large_hn_degree <- subset(flow_network_db, flow_network_db$avgHyperedgeSize < 3 & flow_network_db$avgHypernodeDegree >= 4)
flow_network_large_edge_size_small_hn_degree <- subset(flow_network_db, flow_network_db$avgHyperedgeSize >= 3 & flow_network_db$avgHypernodeDegree < 4)
flow_network_large_edge_size_large_hn_degree <- subset(flow_network_db, flow_network_db$avgHyperedgeSize >= 3 & flow_network_db$avgHypernodeDegree >= 4)

plot_title <- "Runtime per network and flow algorithm in ms (Small Avg HE Size (< 3) and Small Avg HN Degree (< 4))"
plot(max_flow_time_per_network_plot(flow_network_small_edge_size_small_hn_degree, title=plot_title))

plot_title <- "Runtime per network and flow algorithm in ms (Small Avg HE Size (< 3) and Large Avg HN Degree (>= 4))"
plot(max_flow_time_per_network_plot(flow_network_small_edge_size_large_hn_degree, title=plot_title))

plot_title <- "Runtime per network and flow algorithm in ms (Large Avg HE Size (>= 3) and Small Avg HN Degree (< 4))"
plot(max_flow_time_per_network_plot(flow_network_large_edge_size_small_hn_degree, title=plot_title))

plot_title <- "Runtime per network and flow algorithm in ms (Large Avg HE Size (>= 3) and Large Avg HN Degree (>= 4))"
plot(max_flow_time_per_network_plot(flow_network_large_edge_size_large_hn_degree, title=plot_title))