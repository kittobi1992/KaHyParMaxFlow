#setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow")
setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
source("experiments/plot_functions.R")

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


fm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/direct_cut_experiment/db/cut_fm.db"),
                         select_cut_km1), c("graph","k"), aggreg)
flow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/direct_cut_experiment/db/cut_flow.db"),
                           select_cut_km1), c("graph","k"), aggreg)



#extract graph classes from graph names
fm$type <- as.factor(apply(fm, 1, function(x) graphclass(x)))
flow$type <- as.factor(apply(flow, 1, function(x) graphclass(x)))

# give each DF a name to identify the algorithm
fm$algorithm = "fm"
flow$algorithm = "flow"

fm <- fm[fm$graph != "mult_dcop_01.mtx.hgr",]
flow <- flow[flow$graph != "mult_dcop_01.mtx.hgr",]

####################### Direct K-Way - Cut ####################### 

instance_classes = c("*","Primal", "Dual", "Literal", "ISPD", "SPM")
for (class in instance_classes) {
  filter = class
  print(paste("filter=",filter))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_cut", min_obj = "min_cut",
                              kahypar = fm,
                              flow = flow
  ))
}

K <- c(2,4,8,16,32,64,128)
for (k in K) {
  filter = "*"
  print(paste("k=",k,sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_cut", min_obj = "min_cut",
                              kahypar = fm[fm$k == k,],
                              flow = flow[flow$k == k,]
  ))
}

filter <- "*"
print(cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_cut", min_obj = "min_cut",
                                          UsePenalty = TRUE,
                                          kahypar = fm,
                                          flow = flow)$min_ratios, 
                   title="", xbreaks=pretty_breaks(7), showLegend = TRUE))

