setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments")
source("plot_functions.R")

library(gridExtra)
library(grid)


dbs <- c( "flow_speed_up_heuristics/db/flow-000.db",
          "flow_speed_up_heuristics/db/flow-001.db",
          "flow_speed_up_heuristics/db/flow-011.db",
          "flow_speed_up_heuristics/db/flow-111.db")

algo <- c( "flow_000",
           "flow_001",
           "flow_011",
           "flow_111")

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

kahypar_sea <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="common_dbs/kahypar_sea.db"),
                                select_soed), c("graph","k"), aggreg)

hmetis_r = ddply(dbGetQuery(dbConnect(SQLite(), dbname="common_dbs/subset_hmetis-r.db"),
                            select_soed), c("graph","k"), aggreg)

full_instance_stats = dbGetQuery(dbConnect(SQLite(), dbname="common_dbs/hgr_stats.db"),
                                 "Select * from experiments")

flow_dbs <- list()
for(i in 1:length(dbs)) {
  flow_dbs[[i]] <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=dbs[i]),
                                   select_km1_soed), c("graph","k"), aggreg)
}


# consistently name hypergraphs
changename = function(row) {
  if(grepl("*primal*", row['graph'])){
    return(paste("sat14_",row['graph'],sep=""))
  } else if(grepl("*dual*", row['graph'])){
    return(paste("sat14_",row['graph'],sep=""))
  } else {
    return(row['graph'])
  }
}

hmetis_r$graph <- as.factor(apply(hmetis_r, 1, function(x) changename(x)))

#extract graph classes from graph names
kahypar_sea$type <- as.factor(apply(kahypar_sea, 1, function(x) graphclass(x)))
hmetis_r$type <- as.factor(apply(hmetis_r, 1, function(x) graphclass(x)))
for(i in 1:length(dbs)) {
  flow_dbs[[i]]$type <- as.factor(apply(flow_dbs[[i]], 1, function(x) graphclass(x)))
}


# restrict benchmark set to all instances for which we currently have results
semi_join_filter = semi_join(kahypar_sea, hmetis_r, by=c('graph','k'))
for(i in 1:length(dbs)) {
  semi_join_filter = semi_join(semi_join_filter, flow_dbs[[i]], by=c('graph','k'))
}


# apply the semi_join_filter to all data frames
kahypar_sea = semi_join(kahypar_sea, semi_join_filter, by=c('graph','k'))
hmetis_r = semi_join(hmetis_r, semi_join_filter, by=c('graph','k'))
for(i in 1:length(dbs)) {
  flow_dbs[[i]]= semi_join(flow_dbs[[i]], semi_join_filter, by=c('graph','k'))
}

# give each DF a name to identify the algorithm
kahypar_sea$algorithm = "kahypar_sea"
hmetis_r$algorithm = "hmetis_r"
for(i in 1:length(dbs)) {
  flow_dbs[[i]]$algorithm <- algo[i]
}

kahypar_sea = merge(kahypar_sea,full_instance_stats,by='graph')
for(i in 1:length(dbs)) {
  flow_dbs[[i]] = merge(flow_dbs[[i]],full_instance_stats,by='graph')
}
hmetis_r = merge(hmetis_r,full_instance_stats,by='graph')

flow_000_ <- flow_dbs[[1]]
flow_001 <- flow_dbs[[2]]
flow_011 <- flow_dbs[[3]]
flow_111 <- flow_dbs[[4]]


get_legend_as_seperate_plot <- function(plot) {
  g <- ggplotGrob(plot + 
                    theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)
}


##############################
# geometric mean calculation
##############################
# filter can be: *, SPM, ISPD98, SAT14
instance_classes = c("*","DAC","Primal", "Dual", "Literal", "ISPD", "SPM")
improvement_table <- calculateGmeansFilter(filter = "*",
                                           avg_obj = "avg_km1", min_obj = "min_km1",
                                           kahypar = kahypar_sea,
                                           flow_000 = flow_000,
                                           flow_001 = flow_001,
                                           flow_011 = flow_011,
                                           flow_111 = flow_111)[c("algo","gmean_avg","avg_imp","gmean_min","min_imp","gmean_time")]
improvement_table$gmean_flow_time <- improvement_table$gmean_time - improvement_table$gmean_time[1]

revalue_algo <- c("baseline" = "baseline",
                  "flow_000" = "\\KaHyPar{MF}",
                  "flow_001" = "\\KaHyParConfig{MF}{R1}",
                  "flow_011" = "\\KaHyParConfig{MF}{R1,R2}",
                  "flow_111" = "\\KaHyParConfig{MF}{R1,R2,R3}")
improvement_table$algo <- revalue(as.character(improvement_table$algo), revalue_algo) 
algo <- c("\\KaHyPar{MF}", "\\KaHyParConfig{MF}{R1}", "\\KaHyParConfig{MF}{R1,R2}", "\\KaHyParConfig{MF}{R1,R2,R3}")

speedup_table <- "../master_thesis/experiments/speed_up_heuristics/heuristic_table.tex"
sink(speedup_table)
flow_algo <- improvement_table[improvement_table$algo == "baseline",]
cat(paste("\\KaHyPar{CA}",
          to_latex_math_mode(round(flow_algo["gmean_avg"][1,], digits = 2)),
          to_latex_math_mode(round(flow_algo["gmean_min"][1,], digits = 2)),
          "-",
          to_latex_math_mode(round(flow_algo["gmean_time"][1,], digits = 2)),
          sep=" & "))
cat(" \\\\ \n")
for(flow_heuristic in algo) {
  flow_algo <- improvement_table[improvement_table$algo == flow_heuristic,]
  cat(paste(flow_heuristic,
            to_latex_math_mode(round(flow_algo["avg_imp"][1,], digits = 2)),
            to_latex_math_mode(round(flow_algo["min_imp"][1,], digits = 2)),
            to_latex_math_mode(round(flow_algo["gmean_flow_time"][1,], digits = 2)),
            to_latex_math_mode(round(flow_algo["gmean_time"][1,], digits = 2)),
            sep=" & "))
  cat(" \\\\ \n")
}
sink()

##############################
# performance plots
##############################


kahypar_sea$algorithm <- "\\KaHyPar{CA}"
flow_000$algorithm <- "\\KaHyPar{MF}"
flow_001$algorithm <- "\\KaHyParConfig{MF}{R1}"
flow_011$algorithm <- "\\KaHyParConfig{MF}{R1,R2}"
flow_111$algorithm <- "\\KaHyParConfig{MF}{R1,R2,R3}"

instance_classes <- c("*","DAC","ISPD","Primal","Dual","Literal","SPM")
i <- 1
type_plots <- list()
for(type in instance_classes) {
  filter <- type
  plot <- cuberootplot(createRatioDFsFilter(filter = filter,
                                            avg_obj = "avg_km1", min_obj = "min_km1",
                                            UsePenalty = TRUE,
                                            kahypar = kahypar_sea,
                                            flow_000 = flow_000,
                                            flow_001 = flow_001,
                                            flow_011 = flow_011,
                                            flow_111 = flow_111)$min_ratios, 
                       if(type == "*") "\\ALL" else paste("\\",filter,sep=""), 
                       pretty_breaks(n=7),
                       showLegend = FALSE)
  type_plots[[i]] <- plot
  i <- i + 1
}

speed_up_file <- paste("../master_thesis/experiments/speed_up_heuristics/subset.tex", sep="")
tikz(speed_up_file, width=7, height=8.5, pointsize=12)
grid.arrange(type_plots[[1]],type_plots[[2]],type_plots[[3]],type_plots[[4]],type_plots[[5]],type_plots[[6]],type_plots[[7]],get_legend_as_seperate_plot(type_plots[[1]]),ncol=2)
dev.off()

####################### Running Time ####################### 

to_latex_math_mode <- function(x) {
  return(paste("$",x,"$", sep=""))
}

calculateGmeanRunningTime <- function(..., type="ALL") {
  df <- rbind(...)
  df$algorithm <- factor(df$algorithm, levels = levels(factor(df$algorithm))[c(1,5,2,3,4)])
  if(type != "ALL") {
    df <- df[df$type == type,]
  }
  aggreg = function(df) data.frame(time=to_latex_math_mode(round(gm_mean(df$avg_time), digits = 2)))
  df <- ddply(df, c("algorithm"), aggreg)
}

instance_classes <- c("DAC","ISPD","Primal","Literal","Dual","SPM")
running_time <- calculateGmeanRunningTime(kahypar_sea, flow_000, flow_001, flow_011, flow_111, type="ALL")
for(type in instance_classes) {
  running_time <- cbind(running_time, calculateGmeanRunningTime(kahypar_sea, flow_000, flow_001, flow_011, flow_111, type=type)["time"])
}

table_file <- "../master_thesis/experiments/speed_up_heuristics/subset_flow_running_time.tex"
sink(table_file)
for( algo in levels(factor(running_time$algorithm))) {
  algo_df <- running_time[running_time$algorithm == algo,]
  names(algo_df) <- NULL
  cat(paste(unlist(c(algo_df)), collapse=" & "))
  cat("\\\\ \n")
}
sink()
