setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
source("experiments/plot_functions.R")

select_km1_soed = 'select graph,k,epsilon,flow_region_size_alpha AS alpha, seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_cut_km1 = 'select graph,k,epsilon,seed,cut,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'

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

kahypar = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/new_source_sink_set/db/kahypar_sea.db"),
                              select_cut_km1), c("graph","k"), aggreg)
flow_fm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/new_source_sink_set/db/flow_fm.db"),
                           select_km1_soed), c("graph","k","alpha"), aggreg)
flow_fm_st = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/new_source_sink_set/db/flow_fm_st.db"),
                           select_cut_km1), c("graph","k"), aggreg)
flow_fm_bk = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/new_source_sink_set/db/flow_fm_bk.db"),
                              select_cut_km1), c("graph","k"), aggreg)

flow_fm <- flow_fm[flow_fm$alpha == 16,]



#extract graph classes from graph names
kahypar$type <- as.factor(apply(kahypar, 1, function(x) graphclass(x)))
flow_fm$type <- as.factor(apply(flow_fm, 1, function(x) graphclass(x)))
flow_fm_st$type <- as.factor(apply(flow_fm_st, 1, function(x) graphclass(x)))
flow_fm_bk$type <- as.factor(apply(flow_fm_bk, 1, function(x) graphclass(x)))

semi_join_filter = semi_join(flow_fm, flow_fm_st, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, kahypar, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, flow_fm_bk, by=c('graph','k'))

# apply the semi_join_filter to all data frames
kahypar = semi_join(kahypar, semi_join_filter, by=c('graph','k'))
flow_fm = semi_join(flow_fm, semi_join_filter, by=c('graph','k'))
flow_fm_st = semi_join(flow_fm_st, semi_join_filter, by=c('graph','k'))
flow_fm_bk = semi_join(flow_fm_bk, semi_join_filter, by=c('graph','k'))

# give each DF a name to identify the algorithm
kahypar$algorithm = "kahypar"
flow_fm$algorithm = "fm_flow"
flow_fm_st$algorithm = "fm_flow_st"
flow_fm_bk$algorithm = "flow_fm_bk"


####################### Direct K-Way - Km1 ####################### 

instance_classes = c("*","Primal", "Dual", "Literal", "ISPD", "SPM")
for (class in instance_classes) {
  filter = class
  print(paste("filter=",filter))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = kahypar,
                              flow_fm = flow_fm,
                              flow_fm_st = flow_fm_st,
                              flow_fm_bk = flow_fm_bk
  ))
}

K <- c(2,4,8,16,32,64,128)
for (k in K) {
  filter = "*"
  print(paste("k=",k,sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = kahypar[kahypar$k == k,],
                              flow_fm = flow_fm[flow_fm$k == k,],
                              flow_fm_st = flow_fm_st[flow_fm_st$k == k,],
                              flow_fm_bk = flow_fm_bk[flow_fm_bk$k == k,]
  ))
}

filter <- "*"
print(cuberootplot(createRatioDFsFilter(filter = filter,
                                        avg_obj = "avg_km1", min_obj = "min_km1",
                                        UsePenalty = TRUE,
                                        kahypar = kahypar,
                                        flow_fm = flow_fm,
                                        flow_fm_st = flow_fm_st,
                                        flow_fm_bk = flow_fm_bk
                                        )$min_ratios, 
                   title="", xbreaks=pretty_breaks(7), showLegend = TRUE))

