setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
source("experiments/plot_functions.R")

library(gridExtra)
library(grid)

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

kahypar_ca <- read.csv("experiments/common_dbs/kahypar_ca_detailed.csv")
hmetis_r <- read.csv("experiments/common_dbs/hmetis_r_detailed.csv")
hmetis_k <- read.csv("experiments/common_dbs/hmetis_k_detailed.csv")
patoh_q <- read.csv("experiments/common_dbs/patoh_q_detailed.csv")
patoh_d <- read.csv("experiments/common_dbs/patoh_d_detailed.csv")

kahypar_mf = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/final_flow/db/kahypar_mf.db"),
                            select_soed), c("graph","k"), aggreg)

full_instance_stats = dbGetQuery(dbConnect(SQLite(), dbname="experiments/common_dbs/hgr_stats.db"),
                                 "Select * from experiments")



#extract graph classes from graph names
kahypar_ca$type <- as.factor(apply(kahypar_ca, 1, function(x) graphclass(x)))
kahypar_mf$type <- as.factor(apply(kahypar_mf, 1, function(x) graphclass(x)))
hmetis_r$type <- as.factor(apply(hmetis_r, 1, function(x) graphclass(x)))
hmetis_k$type <- as.factor(apply(hmetis_k, 1, function(x) graphclass(x)))
patoh_q$type <- as.factor(apply(patoh_q, 1, function(x) graphclass(x)))
patoh_d$type <- as.factor(apply(patoh_d, 1, function(x) graphclass(x)))

# restrict benchmark set to all instances for which we currently have results
semi_join_filter = semi_join(kahypar_ca, kahypar_mf, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, hmetis_r, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, hmetis_k, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, patoh_q, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, patoh_d, by=c('graph','k'))

# apply the semi_join_filter to all data frames
kahypar_ca = semi_join(kahypar_ca, semi_join_filter, by=c('graph','k'))
kahypar_mf = semi_join(kahypar_mf, semi_join_filter, by=c('graph','k'))
hmetis_r = semi_join(hmetis_r, semi_join_filter, by=c('graph','k'))
hmetis_k = semi_join(hmetis_k, semi_join_filter, by=c('graph','k'))
patoh_q = semi_join(patoh_q, semi_join_filter, by=c('graph','k'))
patoh_d = semi_join(patoh_d, semi_join_filter, by=c('graph','k'))

# give each DF a name to identify the algorithm
kahypar_ca$algorithm = "\\KaHyPar{CA}"
kahypar_mf$algorithm = "\\KaHyPar{MF}"
hmetis_r$algorithm = "\\hMetis{R}"
hmetis_k$algorithm = "\\hMetis{K}"
patoh_q$algorithm = "\\PaToH{Q}"
patoh_d$algorithm = "\\PaToH{D}"

#kahypar_ca = merge(kahypar_ca,full_instance_stats,by='graph')
#kahypar_mf = merge(kahypar_mf,full_instance_stats,by='graph')
#hmetis_r = merge(hmetis_r,full_instance_stats,by='graph')
#hmetis_k = merge(hmetis_k,full_instance_stats,by='graph')
#patoh_q = merge(patoh_q,full_instance_stats,by='graph')
#patoh_d = merge(patoh_d,full_instance_stats,by='graph')


get_legend_as_seperate_plot <- function(plot) {
  g <- ggplotGrob(plot + 
                  theme(legend.position = "right"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)
}

####################### Performance Plots for instance types ####################### 
instance_classes <- c("*","DAC","ISPD","Primal","Dual","Literal","SPM")
i <- 1
type_plots <- list()
for(type in instance_classes) {
  filter <- type
  plot <- cuberootplot(createRatioDFsFilter(filter = filter,
                                            avg_obj = "avg_km1", min_obj = "min_km1",
                                            UsePenalty = TRUE,
                                            kahypar = kahypar_ca,
                                            kahypar_mf = kahypar_mf,
                                            hmetis_r = hmetis_r,
                                            hmetis_k = hmetis_k,
                                            patoh_q = patoh_q,
                                            patoh_d = patoh_d)$min_ratios, 
                       if(type == "*") "\\ALL" else paste("\\",filter,sep=""), 
                       pretty_breaks(n=7),
                       showLegend = FALSE)
  type_plots[[i]] <- plot
  i <- i + 1
}

fullset_file <- paste("master_thesis/experiments/final_flow/fullset.tex", sep="")
tikz(fullset_file, width=7, height=9, pointsize=12)
grid.arrange(type_plots[[1]],type_plots[[2]],type_plots[[3]],type_plots[[4]],type_plots[[5]],type_plots[[6]],type_plots[[7]],get_legend_as_seperate_plot(type_plots[[1]]),ncol=2)
dev.off()

####################### Performance Plots per k ####################### 

K <- c(2,4,8,16,32,64,128)
k_plots <- list()
i <- 1
for(k in K) {
  filter <- "*"
  plot <- cuberootplot(createRatioDFsFilter(filter = filter,
                                            avg_obj = "avg_km1", min_obj = "min_km1",
                                            UsePenalty = TRUE,
                                            kahypar = kahypar_ca[kahypar_ca$k == k,],
                                            kahypar_mf = kahypar_mf[kahypar_mf$k == k,],
                                            hmetis_r = hmetis_r[hmetis_r$k == k,],
                                            hmetis_k = hmetis_k[hmetis_k$k == k,],
                                            patoh_q = patoh_q[patoh_q$k == k,],
                                            patoh_d = patoh_d[patoh_d$k == k,])$min_ratios, paste("$k=",k,"$",sep=""), pretty_breaks(n=7), showLegend=FALSE)
  k_plots[[i]] <- plot
  i <- i + 1
}

fullset_k_file <- paste("master_thesis/experiments/final_flow/fullset-k.tex", sep="")
tikz(fullset_k_file, width=7, height=9, pointsize=12)
grid.arrange(k_plots[[1]],k_plots[[2]],k_plots[[3]],k_plots[[4]],k_plots[[5]],k_plots[[6]],k_plots[[7]],get_legend_as_seperate_plot(k_plots[[1]]),ncol=2)
dev.off()

####################### Running Time per Instance Type ####################### 

kahypar_mf <- kahypar_mf[c("graph", "type", "k", "min_km1", "avg_km1", "avg_imbalance", "avg_time", "algorithm")]

to_latex_math_mode <- function(x) {
  return(paste("$",x,"$", sep=""))
}

calculateGmeanRunningTime <- function(..., type="ALL") {
  df <- rbind(...)
  df$algorithm <- factor(df$algorithm, levels = levels(factor(df$algorithm))[c(4,3,2,1,6,5)])
  if(type != "ALL") {
    df <- df[df$type == type,]
  }
  aggreg = function(df) data.frame(time=to_latex_math_mode(round(gm_mean(df$avg_time), digits = 2)))
  df <- ddply(df, c("algorithm"), aggreg)
}

instance_classes <- c("DAC","ISPD","Primal","Literal","Dual","SPM")
running_time <- calculateGmeanRunningTime(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, type="ALL")
for(type in instance_classes) {
  running_time <- cbind(running_time, calculateGmeanRunningTime(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, type=type)["time"])
}

table_file <- "master_thesis/experiments/final_flow/final_flow_running_time.tex"
sink(table_file)
for( algo in levels(factor(running_time$algorithm))) {
  algo_df <- running_time[running_time$algorithm == algo,]
  names(algo_df) <- NULL
  cat(paste(unlist(c(algo_df)), collapse=" & "))
  cat("\\\\ \n")
}
sink()

####################### Running Time per K ####################### 

kahypar_mf <- kahypar_mf[c("graph", "type", "k", "min_km1", "avg_km1", "avg_imbalance", "avg_time", "algorithm")]

calculateGmeanRunningTimePerK <- function(..., k) {
  df <- rbind(...)
  df$algorithm <- factor(df$algorithm, levels = levels(factor(df$algorithm))[c(4,3,2,1,6,5)])
  df <- df[df$k == k,]
  aggreg = function(df) data.frame(time=to_latex_math_mode(round(gm_mean(df$avg_time), digits = 2)))
  df <- ddply(df, c("algorithm"), aggreg)
}

K <- c(4,8,16,32,64,128)
running_time <- calculateGmeanRunningTimePerK(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, k=2)
for(k in K) {
  running_time <- cbind(running_time, calculateGmeanRunningTimePerK(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, k=k)["time"])
}

table_file <- "master_thesis/experiments/final_flow/final_flow_running_time_per_k.tex"
sink(table_file)
for( algo in levels(factor(running_time$algorithm))) {
  algo_df <- running_time[running_time$algorithm == algo,]
  names(algo_df) <- NULL
  cat(paste(unlist(c(algo_df)), collapse=" & "))
  cat("\\\\ \n")
}
sink()

####################### Gmean Km1 per Instance Type ####################### 

kahypar_mf <- kahypar_mf[c("graph", "type", "k", "min_km1", "avg_km1", "avg_imbalance", "avg_time", "algorithm")]

calculateGmeanAvgKm1 <- function(..., type="ALL") {
  df <- rbind(...)
  df$algorithm <- factor(df$algorithm, levels = levels(factor(df$algorithm))[c(4,3,2,1,6,5)])
  if(type != "ALL") {
    df <- df[df$type == type,]
  }
  aggreg = function(df) data.frame(km1=gm_mean(df$avg_km1))
  df <- ddply(df, c("algorithm"), aggreg)
  df$km1[2:6] <- round((df$km1[2:6]/df$km1[1] - 1.0)*100.0, digits = 2)
  df$km1[1] <- round(df$km1[1], digits = 2)
  df$km1[1:6] <- to_latex_math_mode(df$km1[1:6])
  return(df)
}

instance_classes <- c("DAC","ISPD","Primal","Literal","Dual","SPM")
km1_table <- calculateGmeanAvgKm1(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, type="ALL")
for(type in instance_classes) {
  km1_table <- cbind(km1_table, calculateGmeanAvgKm1(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, type=type)["km1"])
}

km1_table$algorithm <- as.character(km1_table$algorithm)
partitioner <- c("\\KaHyPar{MF}","\\KaHyPar{CA}","\\hMetis{R}","\\hMetis{K}","\\PaToH{Q}","\\PaToH{D}")

table_file <- "master_thesis/experiments/final_flow/final_flow_km1_per_instance.tex"
sink(table_file)
for( algo in partitioner) {
  algo_df <- km1_table[km1_table$algorithm == algo,]
  names(algo_df) <- NULL
  cat(paste(unlist(c(algo_df)), collapse=" & "))
  cat("\\\\ \n")
}
sink()

####################### Gmean Km1 per k ####################### 

kahypar_mf <- kahypar_mf[c("graph", "type", "k", "min_km1", "avg_km1", "avg_imbalance", "avg_time", "algorithm")]

calculateGmeanAvgKm1PerK <- function(..., k) {
  df <- rbind(...)
  df$algorithm <- factor(df$algorithm, levels = levels(factor(df$algorithm))[c(4,3,2,1,6,5)])
  df <- df[df$k == k,]
  aggreg = function(df) data.frame(km1=gm_mean(df$avg_km1))
  df <- ddply(df, c("algorithm"), aggreg)
  df$km1[2:6] <- round((df$km1[2:6]/df$km1[1] - 1.0)*100.0, digits = 2)
  df$km1[1] <- round(df$km1[1], digits = 2)
  df$km1[1:6] <- to_latex_math_mode(df$km1[1:6])
  return(df)
}

K <- c(4,8,16,32,64,128)
km1_table <- calculateGmeanAvgKm1PerK(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, k=2)
for(k in K) {
  km1_table <- cbind(km1_table, calculateGmeanAvgKm1PerK(kahypar_mf, kahypar_ca, hmetis_r, hmetis_k, patoh_q, patoh_d, k = k)["km1"])
}

km1_table$algorithm <- as.character(km1_table$algorithm)
partitioner <- c("\\KaHyPar{MF}","\\KaHyPar{CA}","\\hMetis{R}","\\hMetis{K}","\\PaToH{Q}","\\PaToH{D}")

table_file <- "master_thesis/experiments/final_flow/final_flow_km1_per_k.tex"
sink(table_file)
for( algo in partitioner) {
  algo_df <- km1_table[km1_table$algorithm == algo,]
  names(algo_df) <- NULL
  cat(paste(unlist(c(algo_df)), collapse=" & "))
  cat("\\\\ \n")
}
sink()



####################### Number of Best Instance ####################### 

absolut_number_of_best_partitions <- function(...) {
  df <- rbind(...)
  min_df <- data.frame(graph = character(), k = numeric(), algorithm = character())
  for(graph in levels(factor(df$graph))) {
    for(k in levels(factor(df[df$graph == graph,]$k))) {
      tmp_df <- df[df$graph == graph & df$k == k, ]
      min_idx <- which.min(tmp_df$min_km1)
      min_df <- rbind(min_df, tmp_df[min_idx,][c("graph","k","algorithm")])
    }
  }
  
  aggreg = function(df) data.frame(cnt=length(df$algorithm))
  min_df <- ddply(min_df, c("algorithm"), aggreg)
  min_df$ratio <- min_df$cnt / sum(min_df$cnt) 
  return(min_df)
}

print(absolut_number_of_best_partitions(kahypar_mf, kahypar_ca, patoh_q, patoh_d, hmetis_r, hmetis_k))
print(absolut_number_of_best_partitions(kahypar_mf, kahypar_ca))
print(absolut_number_of_best_partitions(kahypar_mf, patoh_q))
print(absolut_number_of_best_partitions(kahypar_mf, patoh_d))
print(absolut_number_of_best_partitions(kahypar_mf, hmetis_r))
print(absolut_number_of_best_partitions(kahypar_mf, hmetis_k))

####################### Missing Instances ####################### 

ca <- read.csv("experiments/common_dbs/kahypar_ca_detailed.csv")
mf <- ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/final_flow/db/kahypar_mf.db"),
                       select_soed), c("graph","k"), aggreg)
ca <- ca[c("graph","k")]
mf <- mf[c("graph","k")]


missing <- rbind(ca,mf)
missing <- missing[! duplicated(missing, fromLast = TRUE) & seq(nrow(missing)) <= nrow(ca),]
print(paste("Hypergraphs with Missing Instances:",length(levels(factor(missing$graph)))),sep=" ")
