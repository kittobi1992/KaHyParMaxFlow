setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
source("experiments/plot_functions.R")


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


##############################
# performance plots
##############################
# UsePenalty = TRUE offsets imbalanced solutions
# same filters can be used here: *, SPM, ISPD98, SAT14
# this plot shows the min km1 ratios

#Quality
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

source("experiments/plot_functions.R")

filter <- "DAC"
dac <- cuberootplot(createRatioDFsFilter(filter = filter,
                                           avg_obj = "avg_km1", min_obj = "min_km1",
                                           UsePenalty = TRUE,
                                           kahypar = kahypar_ca,
                                           kahypar_mf = kahypar_mf,
                                           hmetis_r = hmetis_r,
                                           hmetis_k = hmetis_k,
                                           patoh_q = patoh_q,
                                           patoh_d = patoh_d)$min_ratios, paste("\\",filter,sep=""), pretty_breaks(n=10))

filter <- "ISPD"
ispd <- cuberootplot(createRatioDFsFilter(filter = filter,
                                           avg_obj = "avg_km1", min_obj = "min_km1",
                                           UsePenalty = TRUE,
                                           kahypar = kahypar_ca,
                                           kahypar_mf = kahypar_mf,
                                           hmetis_r = hmetis_r,
                                           hmetis_k = hmetis_k,
                                           patoh_q = patoh_q,
                                           patoh_d = patoh_d)$min_ratios, paste("\\",filter,sep=""), pretty_breaks(n=10))

filter <- "Primal"
primal <- cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_km1", min_obj = "min_km1",
                                          UsePenalty = TRUE,
                                          kahypar = kahypar_ca,
                                          kahypar_mf = kahypar_mf,
                                          hmetis_r = hmetis_r,
                                          hmetis_k = hmetis_k,
                                          patoh_q = patoh_q,
                                          patoh_d = patoh_d)$min_ratios, paste("\\",filter,sep=""), pretty_breaks(n=5))

filter <- "Dual"
dual <- cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_km1", min_obj = "min_km1",
                                          UsePenalty = TRUE,
                                          kahypar = kahypar_ca,
                                          kahypar_mf = kahypar_mf,
                                          hmetis_r = hmetis_r,
                                          hmetis_k = hmetis_k,
                                          patoh_q = patoh_q,
                                          patoh_d = patoh_d)$min_ratios, paste("\\",filter,sep=""), pretty_breaks(n=5))

filter <- "Literal"
literal <- cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_km1", min_obj = "min_km1",
                                          UsePenalty = TRUE,
                                          kahypar = kahypar_ca,
                                          kahypar_mf = kahypar_mf,
                                          hmetis_r = hmetis_r,
                                          hmetis_k = hmetis_k,
                                          patoh_q = patoh_q,
                                          patoh_d = patoh_d)$min_ratios, paste("\\",filter,sep=""), pretty_breaks(n=5))


filter <- "SPM"
spm <- cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_km1", min_obj = "min_km1",
                                          UsePenalty = TRUE,
                                          kahypar = kahypar_ca,
                                          kahypar_mf = kahypar_mf,
                                          hmetis_r = hmetis_r,
                                          hmetis_k = hmetis_k,
                                          patoh_q = patoh_q,
                                          patoh_d = patoh_d)$min_ratios, paste("\\",filter,sep=""), pretty_breaks(n=5))

filter <- "*"
all <- cuberootplot(createRatioDFsFilter(filter = filter,
                                         avg_obj = "avg_km1", min_obj = "min_km1",
                                         UsePenalty = TRUE,
                                         kahypar = kahypar_ca,
                                         kahypar_mf = kahypar_mf,
                                         hmetis_r = hmetis_r,
                                         hmetis_k = hmetis_k,
                                         patoh_q = patoh_q,
                                         patoh_d = patoh_d)$min_ratios, "\\ALL", pretty_breaks(n=7), showLegend=TRUE)

####################### Performance Plots ####################### 

speed_up_file <- paste("master_thesis/experiments/final_flow/fullset.tex", sep="")
tikz(speed_up_file, width=7, height=9, pointsize=12)
multiplot(all,dac,ispd,primal,dual,literal,spm,cols=2)
dev.off()

print(calculateGmeansFilter(filter = "*",
                            avg_obj = "avg_km1", min_obj = "min_km1",
                            kahypar = kahypar_ca,
                            kahypar_mf = kahypar_mf,
                            hmetis_r = hmetis_r,
                            hmetis_k = hmetis_k,
                            patoh_q = patoh_q,
                            patoh_d = patoh_d
))

####################### Running Time ####################### 

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

print(running_time)

table_file <- "master_thesis/experiments/final_flow/final_flow_running_time.tex"
sink(table_file)
for( algo in levels(factor(running_time$algorithm))) {
  algo_df <- running_time[running_time$algorithm == algo,]
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