working_dir="/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow"
#working_dir="C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments"
setwd(working_dir)
source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))


aggreg = function(df) data.frame(avg_hn_degree=mean(df$avgHypernodeDegree),
                                 avg_he_size=mean(df$avgHyperedgeSize),
                                 avg_num_nodes=mean(df$flow_network_num_nodes), 
                                 avg_num_edges=mean(df$flow_network_num_edges), 
                                 avg_cut=mean(df$cut), 
                                 avg_max_flow=mean(df$max_flow), 
                                 min_network_build_time=min(df$min_network_build_time), 
                                 avg_network_build_time=mean(df$avg_network_build_time), 
                                 min_max_flow_time=min(df$min_max_flow_time), 
                                 avg_max_flow_time=mean(df$avg_max_flow_time))

db_name=paste(working_dir,"experiments/flow_network_experiment/global_relabeling_test.db",sep="/")
flow_network_db = dbGetQuery(dbConnect(SQLite(), dbname=db_name), "select * from experiments")
flow_network_db <- flow_network_db[flow_network_db$num_hypernodes != 0,]
flow_network_db$num_hypernodes <- ifelse(flow_network_db$num_hypernodes > 10000, 25000, flow_network_db$num_hypernodes)
flow_network_db$num_hypernodes <- ifelse(flow_network_db$num_hypernodes > 5000 & flow_network_db$num_hypernodes <= 10000, 10000, flow_network_db$num_hypernodes)
flow_network_db <- ddply(flow_network_db, c("hypergraph","flow_network","flow_algorithm","num_hypernodes","global_relabeling"), aggreg)
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
flow_network_db$global_relabeling <- as.numeric(as.character(flow_network_db$global_relabeling))

flow_network_db$num_hypernodes <- factor(flow_network_db$num_hypernodes)
flow_network_db$type <- factor(flow_network_db$type)
flow_network_db$flow_network <- factor(flow_network_db$flow_network)
flow_network_db$flow_algorithm <- factor(flow_network_db$flow_algorithm)
flow_network_db$type <- factor(flow_network_db$type, levels = levels(factor(flow_network_db$type))[c(1,3,2,5,4,6)])

##############################################################################################################################

speedup <- function(db) {
  aggreg <- function(df) data.frame(avg_max_flow_time=mean(df$avg_max_flow_time))
  df <- ddply(db, c("num_hypernodes", "type", "global_relabeling"), aggreg) 
  for( num_hn in levels(factor(df$num_hypernodes)) ) {
    for( type in levels(factor(df$type))) {
      df[df$num_hypernodes == num_hn & df$type == type,]$avg_max_flow_time <- (df[df$num_hypernodes == num_hn & df$type == type & df$global_relabeling == 1,]$avg_max_flow_time /
        df[df$num_hypernodes == num_hn & df$type == type,]$avg_max_flow_time - 1.0) * 100.0
    }
  }
  return(df)
}


max_flow_time_plot <- function(db, title="") {
  df <- speedup(db)
  
  plot <- ggplot(df, aes( x= global_relabeling, y = avg_max_flow_time)) + 
    geom_bar(stat = "identity", position="dodge") +
    facet_grid(type ~ num_hypernodes) +
    ggtitle(title) +
    ylab("Speed-Up in t[%]") +
    xlab("Global Relabeling Parameter") +
    theme_complete_bw()
  
  return(plot)
}

##############################################################################################################################

plot(max_flow_time_plot(flow_network_db))
