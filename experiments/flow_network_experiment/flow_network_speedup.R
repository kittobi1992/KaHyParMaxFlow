working_dir="/home/heuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow"
#working_dir="C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments"
setwd(working_dir)
source(paste(working_dir, "experiments/flow_network_functions.R", sep="/"))
source(paste(working_dir, "experiments/plot_functions.R", sep="/"))

library(highlight)

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

db_name=paste(working_dir,"experiments/flow_network_experiment/flow_network_test.db",sep="/")
flow_network_db = dbGetQuery(dbConnect(SQLite(), dbname=db_name), "select * from experiments")
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
flow_network_db$flow_network <- factor(flow_network_db$flow_network, levels = levels(flow_network_db$flow_network)[c(2,1,4,3)])
flow_network_db$flow_algorithm <- factor(flow_network_db$flow_algorithm, levels = levels(flow_network_db$flow_algorithm)[c(2,3,1,4)])

##############################################################################################################################

full_instance_stats = dbGetQuery(dbConnect(SQLite(), dbname="experiments/common_dbs/hgr_stats.db"),
                                 "Select * from experiments")
full_instance_stats$hypergraph = full_instance_stats$graph
flow_network_db = merge(flow_network_db, full_instance_stats, by='hypergraph')
flow_networks = flow_network_db[flow_network_db$flow_algorithm == "ibfs",]
flow_networks$flow_network <- factor(flow_networks$flow_network,
                       levels = c('lawler','edge_size','node_degree','hybrid'),ordered = TRUE)

fntsize=18

theme_complete_bw <- function(base_size = 11, base_family = "") {
        theme(aspect.ratio = 2/(1+sqrt(5)),
        legend.background = element_rect(),
        legend.title = element_text(face="bold",size=fntsize),
        legend.text=element_text(size=fntsize),
        legend.position = "bottom",
        legend.key.height =  unit(1, "cm"),
        legend.key.width =   unit(1, "cm"),
        strip.background = element_rect(fill = "white",colour = "white"),
        #strip.placement = "outside",
        legend.margin=unit(-0.15,"cm"),
        panel.grid.major = element_line(linetype="dotted",size = 0.5, color = "grey"),
        panel.grid.minor = element_line(),
        panel.border = element_rect(colour = "black"),
        axis.text=element_text(size = fntsize),
        axis.line = element_line(size = 0.2, color = "black"),
        axis.title.y = element_text(vjust=1.5, size = fntsize),
        axis.title.x = element_text(vjust=1, size = fntsize),
        plot.title = element_text(size=fntsize, face="bold", hjust = 0.5))}


ibfs = flow_networks_ibfs = flow_network_db[flow_network_db$flow_algorithm == "ibfs",]

ibfs_lawler =flow_networks_ibfs[flow_networks_ibfs$flow_network == "lawler",]

ibfs_lawler <- rbind(ibfs_lawler, ibfs_lawler)
ibfs_lawler <- rbind(ibfs_lawler, ibfs_lawler)

ibfs_lawler = ibfs_lawler[with(ibfs_lawler, order(graph,num_hypernodes)), ]
flow_networks_ibfs = flow_networks_ibfs[with(flow_networks_ibfs, order(graph,num_hypernodes)), ]

flow_networks_ibfs$speedup = ibfs_lawler$avg_max_flow_time / flow_networks_ibfs$avg_max_flow_time

flow_networks_ibfs$flow_network <- factor(flow_networks_ibfs$flow_network,
                                        levels = c('lawler','edge_size','node_degree','hybrid'),ordered = TRUE)
flow_networks_ibfs = flow_networks_ibfs[flow_networks_ibfs$num_hypernodes == 25000,]

flow_networks_ibfs<- flow_networks_ibfs[!(flow_networks_ibfs$flow_network=="lawler"),]
flow_networks_ibfs<- flow_networks_ibfs[!(flow_networks_ibfs$flow_network=="node_degree"),]
flow_networks_ibfs$flow_network =  revalue(flow_networks_ibfs$flow_network, c("edge_size"="Wong" , "hybrid"="Hybrid"))


ibfs = ggplot(flow_networks_ibfs, aes(x = type, y = speedup, fill=flow_network))  +
  scale_y_continuous(trans="log10",  breaks = c(1,2,5,10)) +
  geom_boxplot(notch=F,outlier.size=.5)  +
  scale_fill_manual(values=c("#00BA38","#619CFF")) +
  stat_boxplot(geom ='errorbar') + labs(fill = "Network")+
  ylab("Speedup over Lawler Network") +
  xlab("") + theme_bw() + theme_complete_bw() + ggtitle("IBFS")

print(ibfs)

