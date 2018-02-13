working_dir="/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow"
#working_dir="C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments"
setwd(working_dir)
source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
source(paste(working_dir, "experiments/plot_functions.R", sep="/"))


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

aggreg2 = function(df) data.frame(min_network_build_time=min(df$min_network_build_time), 
                                  avg_network_build_time=gm_mean(df$avg_network_build_time), 
                                  min_max_flow_time=min(df$min_max_flow_time), 
                                  avg_max_flow_time=gm_mean(df$avg_max_flow_time))

db_name=paste(working_dir,"experiments/flow_network_experiment/flow_network_test.db",sep="/")
flow_network_db = dbGetQuery(dbConnect(SQLite(), dbname=db_name), "select * from experiments")
flow_network_db <- flow_network_db[flow_network_db$flow_network != "node_degree",]
flow_network_db <- flow_network_db[flow_network_db$num_hypernodes != 0,]
flow_network_db$num_hypernodes <- ifelse(flow_network_db$num_hypernodes > 10000, 25000, flow_network_db$num_hypernodes)
flow_network_db$num_hypernodes <- ifelse(flow_network_db$num_hypernodes > 5000 & flow_network_db$num_hypernodes <= 10000, 10000, flow_network_db$num_hypernodes)
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

flow_network_db$num_hypernodes <- factor(flow_network_db$num_hypernodes)
flow_network_db$type <- factor(flow_network_db$type)
flow_network_db$flow_network <- factor(flow_network_db$flow_network)
flow_network_db$flow_algorithm <- factor(flow_network_db$flow_algorithm)
flow_network_db$type <- factor(flow_network_db$type, levels = levels(factor(flow_network_db$type))[c(1,3,2,5,4,6)])
flow_network_db$flow_network <- factor(flow_network_db$flow_network, levels = levels(flow_network_db$flow_network)[c(3,1,2)])
flow_network_db$flow_algorithm <- factor(flow_network_db$flow_algorithm, levels = levels(flow_network_db$flow_algorithm)[c(2,3,1,4)])

##############################################################################################################################


library(gridExtra)
library(grid)

flow_network_running_time <- function(db, title="") {
  df <- ddply(db, c("flow_algorithm","num_hypernodes"), aggreg2)
  plot <- ggplot(df, aes( x= as.numeric(as.character(num_hypernodes)), y = avg_max_flow_time, color = flow_algorithm)) + 
    geom_line(size=1) +
    geom_point(size=3, alpha=0.8) +
    ggtitle(title) +
    ylab("Time [ms]") +
    xlab("Num Hypernodes") +
    scale_y_continuous(trans=log10_trans()) +
    theme_complete_bw()
  return(plot)
}

flow_network_db_hybrid <- flow_network_db[flow_network_db$flow_network == "hybrid",]
plot <- flow_network_running_time(flow_network_db_hybrid)
print(plot)

speed_up_plot <- function(db, title="") {
  speed_up_db <- speedup_relative_to_lawler(db)
  #speed_up_db <- speed_up_db[speed_up_db$flow_network != "lawler"]
  speed_up_db <- revalue_columns_to_latex(speed_up_db)
  print(speed_up_db)
  speed_up_db <- speed_up_db[speed_up_db$flow_network != "$\\ExpLawler$",]
  
  speed_up_db$speedup1 <- 1.0
  speed_up_db$speedup2 <- 2.0
  speed_up_db$speedup3 <- 3.0
  
  plot <- ggplot(speed_up_db, aes( x= flow_network, y = avg_max_flow_time)) + 
    geom_bar(aes(fill = flow_algorithm), stat = "identity", position="dodge") +
    geom_hline(aes(yintercept = speedup1), color="red", linetype="dashed") +
    geom_hline(aes(yintercept = speedup2), color="blue", linetype="dashed") +
    facet_grid(~ num_hypernodes) +
    ggtitle(title) +
    ylab("Speed up relative to $\\ExpLawler$") +
    xlab("Flow Network") +
    theme_complete_bw()
  
  return(plot)
}

degree_thres <- 3.5
num_hn_thres <- 25000
flow_network_db_low_degree <- flow_network_db[flow_network_db$avg_hn_degree <= degree_thres,]
flow_network_db_high_degree <- flow_network_db[flow_network_db$avg_hn_degree > degree_thres,]
plot1 <- speed_up_plot(flow_network_db_low_degree, title="Avg. d(v) <= 3.5")
plot2 <- speed_up_plot(flow_network_db_high_degree, title="Avg. d(v) > 3.5")
grid.arrange(plot1,plot2,ncol=1)

##############################################################################################################################

source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
table_file <- paste(working_dir, "master_thesis/experiments/flow_network/flow_network_max_flow_summary_table.tex", sep="/")
sink(table_file)
create_flow_network_max_flow_table_hybrid(flow_network_db, showType=FALSE)
sink()

table_file <- paste(working_dir, "master_thesis/experiments/flow_network/flow_network_max_flow_table.tex", sep="/")
sink(table_file)
create_flow_network_max_flow_table_hybrid(flow_network_db, showType=TRUE)
for( type in levels(factor(flow_network_db$type))) {
  cat("\\midrule% \n")
  create_flow_network_max_flow_table_hybrid(flow_network_db[flow_network_db$type == type,], instance_type=as.character(type), showType=TRUE)
}
sink()


##############################################################################################################################

source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))

flow_network_subset_db <- flow_network_db[flow_network_db$num_hypernodes==25000 & flow_network_db$flow_algorithm=="goldberg_tarjan",]
node_edge_file <- paste(working_dir, "master_thesis/experiments/flow_network/node_edge_distribution.tex", sep="/")
tikz(node_edge_file, width=6.5, height=4.5, pointsize = 12)
source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
plot(node_edge_distribution_plot2(flow_network_subset_db) )
dev.off()

flow_network_lawler <- flow_network_subset_db[flow_network_subset_db$flow_network == "lawler",]
flow_network_edge_size <- flow_network_subset_db[flow_network_subset_db$flow_network == "edge_size",]
flow_network_hybrid <- flow_network_subset_db[flow_network_subset_db$flow_network == "hybrid",]

flow_network_hybrid$ratio <- flow_network_edge_size$avg_num_edges / flow_network_hybrid$avg_num_edges
flow_network_hybrid_2 <- flow_network_hybrid[flow_network_hybrid$ratio >= 1.25,]

# Total instances 159
# 61 == 1.0
# 69 >= 1.01
# Rest 29

filter <- "*"
plot <- cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_num_edges", min_obj = "avg_num_edges",
                                          UsePenalty = FALSE,
                                          kahypar = flow_network_lawler,
                                          flow_network_edge_size = flow_network_edge_size,
                                          flow_network_hybrid = flow_network_hybrid
                                          )$avg_ratios, 
                     title="", 
                     xbreaks=pretty_breaks(5),
                     showLegend = TRUE)

createRatioDFsFilter(filter = filter,
                     avg_obj = "avg_num_edges", min_obj = "avg_num_edges",
                     UsePenalty = FALSE,
                     kahypar = flow_network_lawler,
                     flow_network_edge_size = flow_network_edge_size,
                     flow_network_hybrid = flow_network_hybrid
)$avg_ratios

##############################################################################################################################

source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
table_file <- paste(working_dir, "master_thesis/experiments/flow_network/instance_type_properties.tex", sep="/")
sink(table_file)
instance_db <- create_instance_type_table(flow_network_db)
for( type in factor(levels(instance_db$type)) ) {
  cat(paste(type,"&",instance_db[instance_db$type == type,]["avg_hn_degree"],"&",instance_db[instance_db$type == type,]["avg_he_size"], "\\\\ \n", sep=" "))
}
sink()

##############################################################################################################################

source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
speed_up_file <- paste(working_dir, "master_thesis/experiments/flow_network/speed_up_flow_network.tex", sep="/")
tikz(speed_up_file, width=7, height=8)
plot(speed_up_plot(flow_network_db))
dev.off()

##############################################################################################################################

source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
edmond_karp_goldberg_tarjan_comparison(flow_network_db,"hybrid")
hybrid <- build_hybrid_flow_algo(flow_network_db, 2^15)
gmean_network_algorithm_table(hybrid)

for(num_hn in levels(factor(hybrid$num_hypernodes))) {
  print(num_hn)
  print(gmean_network_algorithm_table(hybrid[hybrid$num_hypernodes == num_hn,]))
}

sanityCheck(flow_network_db)

##############################################################################################################################

flow_network_gt <- flow_network_db[flow_network_db$flow_algorithm == "goldberg_tarjan" & flow_network_db$flow_network == "hybrid",]
flow_network_bk <- flow_network_db[flow_network_db$flow_algorithm == "boykov_kolmogorov" & flow_network_db$flow_network == "hybrid",]
flow_network <- merge(flow_network_gt, flow_network_bk, by=c("hypergraph","flow_network","num_hypernodes"))
flow_network$ratio <- flow_network$avg_max_flow_time.y / flow_network$avg_max_flow_time.x
flow_network$density <- flow_network$avg_num_edges.x / flow_network$avg_num_nodes.x
flow_network <- flow_network[c("hypergraph","flow_network","num_hypernodes","ratio","density")]
flow_network$type <- as.factor(apply(flow_network, 1, function(x) graphclass(x)))

threshold <- 8.5
plot <- ggplot(flow_network) + 
        geom_point(aes( x= density, y = ratio, color = type)) +
        geom_hline(aes(yintercept = 1), color="red", linetype="dashed") +
        geom_vline(aes(xintercept = threshold), color="red", linetype="dashed") +
        scale_x_log10() +
        scale_y_log10() +
        #ylim(0,25) +
        ylab("Ratio") +
        xlab("Density") +
        theme_complete_bw()
print(plot)

flow_network_summary <- flow_network_db[flow_network_db$flow_algorithm != "edmond_karp" & flow_network_db$flow_network == "hybrid",]
flow_network_summary$density <- flow_network_summary$avg_num_edges / flow_network_summary$avg_num_nodes
flow_network_low_density <- flow_network_summary[flow_network_summary$density < threshold, ]
flow_network_high_density <- flow_network_summary[flow_network_summary$density >= threshold, ]
gmean_network_algorithm_table(flow_network_low_density)
gmean_network_algorithm_table(flow_network_high_density)

##############################################################################################################################

