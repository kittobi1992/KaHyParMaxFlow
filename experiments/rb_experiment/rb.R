setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
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


rb_cut = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_experiment/db/rb-cut.db"),
                          select_cut_km1), c("graph","k"), aggreg)
rb_cut_flow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_experiment/db/rb-cut-flow.db"),
                               select_cut_km1), c("graph","k"), aggreg)
rb_km1 = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_experiment/db/rb-km1.db"),
                          select_cut_km1), c("graph","k"), aggreg)
rb_km1_flow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_experiment/db/rb-km1-flow.db"),
                               select_cut_km1), c("graph","k"), aggreg)



#extract graph classes from graph names
rb_cut$type <- as.factor(apply(rb_cut, 1, function(x) graphclass(x)))
rb_cut_flow$type <- as.factor(apply(rb_cut_flow, 1, function(x) graphclass(x)))
rb_km1$type <- as.factor(apply(rb_km1, 1, function(x) graphclass(x)))
rb_km1_flow$type <- as.factor(apply(rb_km1_flow, 1, function(x) graphclass(x)))

# give each DF a name to identify the algorithm
rb_cut$algorithm = "RB-Cut"
rb_cut_flow$algorithm = "RB-Cut-Flow"
rb_km1$algorithm = "RB-Km1"
rb_km1_flow$algorithm = "RB-Km1-Flow"


####################### RB - Cut ####################### 

instance_classes = c("*","DAC","Primal", "Dual", "Literal", "ISPD", "SPM")
for (class in instance_classes) {
  filter = class
  print(paste("filter=",filter))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_cut", min_obj = "min_cut",
                              kahypar = rb_cut,
                              rb_cut_flow = rb_cut_flow
  ))
}

K <- c(2,4,8,16,32,64,128)
for (k in K) {
  filter = "*"
  print(paste("k=",k,sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_cut", min_obj = "min_cut",
                              kahypar = rb_cut[rb_cut$k == k,],
                              rb_cut_flow = rb_cut_flow[rb_cut_flow$k == k,]
  ))
}

####################### RB - Km1 ####################### 

instance_classes = c("*","DAC","Primal", "Dual", "Literal", "ISPD", "SPM")
for (class in instance_classes) {
  filter = class
  print(paste("filter=",filter))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = rb_km1,
                              rb_km1_flow = rb_km1_flow
  ))
}

K <- c(2,4,8,16,32,64,128)
for (k in K) {
  filter = "*"
  print(paste("k=",k,sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = rb_km1[rb_km1$k == k,],
                              rb_km1_flow = rb_km1_flow[rb_km1_flow$k == k,]
  ))
}
