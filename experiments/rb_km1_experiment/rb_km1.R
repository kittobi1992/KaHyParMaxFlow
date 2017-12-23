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


twofm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_km1_experiment/db/2fm.db"),
                         select_cut_km1), c("graph","k"), aggreg)
twoflow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_km1_experiment/db/2flow.db"),
                           select_cut_km1), c("graph","k"), aggreg)
twofm_kfm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_km1_experiment/db/2fm_kfm.db"),
                             select_cut_km1), c("graph","k"), aggreg)
twoflow_kfm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_km1_experiment/db/2flow_kfm.db"),
                               select_cut_km1), c("graph","k"), aggreg)
twofm_kflow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_km1_experiment/db/2fm_kflow.db"),
                               select_cut_km1), c("graph","k"), aggreg)
twoflow_kflow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="experiments/rb_km1_experiment/db/2flow_kflow.db"),
                                 select_cut_km1), c("graph","k"), aggreg)



#extract graph classes from graph names
twofm$type <- as.factor(apply(twofm, 1, function(x) graphclass(x)))
twoflow$type <- as.factor(apply(twoflow, 1, function(x) graphclass(x)))
twofm_kfm$type <- as.factor(apply(twofm_kfm, 1, function(x) graphclass(x)))
twoflow_kfm$type <- as.factor(apply(twoflow_kfm, 1, function(x) graphclass(x)))
twofm_kflow$type <- as.factor(apply(twofm_kflow, 1, function(x) graphclass(x)))
twoflow_kflow$type <- as.factor(apply(twoflow_kflow, 1, function(x) graphclass(x)))

# give each DF a name to identify the algorithm
twofm$algorithm = "2fm"
twoflow$algorithm = "2flow"
twofm_kfm$algorithm = "2fm_kfm"
twoflow_kfm$algorithm = "2flow_kfm"
twofm_kflow$algorithm = "2fm_kflow"
twoflow_kflow$algorithm = "2flow_kflow"


####################### RB - Km1 ####################### 

instance_classes = c("*","Primal", "Dual", "Literal", "ISPD", "SPM")
for (class in instance_classes) {
  filter = class
  print(paste("filter=",filter))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = twofm,
                              twofm_kfm = twofm_kfm,
                              twofm_kflow = twofm_kflow,
                              twoflow = twoflow,
                              twoflow_kfm = twoflow_kfm,
                              twoflow_kflow = twoflow_kflow
  ))
}

K <- c(2,4,8,16,32,64,128)
for (k in K) {
  filter = "*"
  print(paste("k=",k,sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = twofm[twofm$k == k,],
                              twofm_kfm = twofm_kfm[twofm_kfm$k == k,],
                              twofm_kflow = twofm_kflow[twofm_kflow$k == k,],
                              twoflow = twoflow[twoflow$k == k,],
                              twoflow_kfm = twoflow_kfm[twoflow_kfm$k == k,],
                              twoflow_kflow = twoflow_kflow[twoflow_kflow$k == k,]
  ))
}

filter <- "*"
print(cuberootplot(createRatioDFsFilter(filter = filter,
                                        avg_obj = "avg_km1", min_obj = "min_km1",
                                        UsePenalty = TRUE,
                                        kahypar = twofm,
                                        twofm_kfm = twofm_kfm,
                                        twofm_kflow = twofm_kflow
                                        #twoflow = twoflow
                                        #twoflow_kfm = twoflow_kfm,
                                        #twoflow_kflow = twoflow_kflow
                                        )$min_ratios, 
                   title="", xbreaks=pretty_breaks(7), showLegend = TRUE))
