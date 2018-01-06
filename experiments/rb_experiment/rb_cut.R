setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow/experiments")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
source("plot_functions.R")

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


twofm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="rb_experiment/db/2fm_cut.db"),
                         select_cut_km1), c("graph","k"), aggreg)
twofm_kfm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="rb_experiment/db/2fm_kfm_cut.db"),
                             select_cut_km1), c("graph","k"), aggreg)
twofm_kflow = ddply(dbGetQuery(dbConnect(SQLite(), dbname="rb_experiment/db/2fm_kflow_cut.db"),
                               select_cut_km1), c("graph","k"), aggreg)
kfm = ddply(dbGetQuery(dbConnect(SQLite(), dbname="rb_experiment/db/direct_kfm_cut.db"),
                               select_cut_km1), c("graph","k"), aggreg)


#extract graph classes from graph names
twofm$type <- as.factor(apply(twofm, 1, function(x) graphclass(x)))
twofm_kfm$type <- as.factor(apply(twofm_kfm, 1, function(x) graphclass(x)))
twofm_kflow$type <- as.factor(apply(twofm_kflow, 1, function(x) graphclass(x)))
kfm$type <- as.factor(apply(kfm, 1, function(x) graphclass(x)))

# give each DF a name to identify the algorithm
twofm$algorithm = "2fm"
twofm_kfm$algorithm = "2fm_kfm"
twofm_kflow$algorithm = "2fm_kflow"
kfm$algorithm = "kfm"

semi_join_filter = semi_join(twofm, twofm_kfm, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, twofm_kflow, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, kfm, by=c('graph','k'))
twofm = semi_join(twofm, semi_join_filter, by=c('graph','k'))
twofm_kfm = semi_join(twofm_kfm, semi_join_filter, by=c('graph','k'))
twofm_kflow = semi_join(twofm_kflow, semi_join_filter, by=c('graph','k'))
kfm = semi_join(kfm, semi_join_filter, by=c('graph','k'))


####################### RB - Cut ####################### 

instance_classes = c("*","Primal", "Dual", "Literal", "ISPD", "SPM")
for (class in instance_classes) {
  filter = class
  print(paste("filter=",filter))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_cut", min_obj = "min_cut",
                              kahypar = twofm,
                              kfm = kfm,
                              twofm_kfm = twofm_kfm,
                              twofm_kflow = twofm_kflow
  ))
}

K <- c(2,4,8,16,32,64,128,256,512,1024,2048)
for (k in K) {
  filter = "*"
  print(paste("k=",k,sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_cut", min_obj = "min_cut",
                              kahypar = twofm[twofm$k == k,],
                              kfm = kfm[kfm$k == k,],
                              twofm_kfm = twofm_kfm[twofm_kfm$k == k,],
                              twofm_kflow = twofm_kflow[twofm_kflow$k == k,]
  ))
}

filter <- "*"
print(cuberootplot(createRatioDFsFilter(filter = filter,
                                        avg_obj = "avg_cut", min_obj = "min_cut",
                                        UsePenalty = TRUE,
                                        kahypar = twofm,
                                        kfm = kfm,
                                        twofm_kfm = twofm_kfm,
                                        twofm_kflow = twofm_kflow
                                        )$min_ratios, 
                   title="", xbreaks=pretty_breaks(7), showLegend = TRUE))


for (k in K) {
  filter <- "*"
  print(cuberootplot(createRatioDFsFilter(filter = filter,
                                          avg_obj = "avg_cut", min_obj = "min_cut",
                                          UsePenalty = TRUE,
                                          kahypar = twofm[twofm$k == k,],
                                          kfm = kfm[kfm$k == k,],
                                          twofm_kfm = twofm_kfm[twofm_kfm$k == k,],
                                          twofm_kflow = twofm_kflow[twofm_kflow$k == k,]
  )$min_ratios, 
  title=paste("k=",k,sep=""), xbreaks=pretty_breaks(7), showLegend = TRUE))
}


