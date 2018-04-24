setwd("/home/heuer/Dokumente/experiments")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow\\experiments")
source("plot_functions.R")


dbs <- c( "fixed_vertices/db/bubble_direct_ip.db",
          "fixed_vertices/db/bubble_rb_ip.db")

algo <- c( "flow_fm_ip_old",
           "flow_fm_ip")

select_km1_soed = 'select graph,k,epsilon,fixed_vertex_fraction AS fraction, seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_km1_soed_2 = 'select graph,k,epsilon,fixed_vertex_fraction AS fraction, seed,km1,soed,imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_km1 = 'select graph,k,epsilon,flow_region_size_alpha AS alpha,seed,1 AS soed, kMinusOne AS km1, imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'
select_soed = 'select graph,k,epsilon,fixed_vertex_fraction AS fraction,seed,soed-cut AS km1, soed, imbalance,coarseningTime,uncoarseningRefinementTime, totalPartitionTime from experiments'

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

scheme <- "bubble"

bubble_free_direct_ip <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_free_direct_ip.db", sep="")),
                                     select_km1_soed_2), c("graph","k","fraction"), aggreg)
bubble_free_rb_ip <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_free_rb_ip.db", sep="")),
                                 select_km1_soed_2), c("graph","k","fraction"), aggreg)
bubble_fixed_direct_ip <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_fixed_direct_ip.db", sep="")),
                                          select_km1_soed_2), c("graph","k","fraction"), aggreg)
bubble_fixed_rb_ip <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_fixed_rb_ip.db", sep="")),
                                      select_km1_soed_2), c("graph","k","fraction"), aggreg)
bubble_fixed_rb_ip_integration <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_fixed_rb_ip_integration.db", sep="")),
                                       select_km1_soed_2), c("graph","k","fraction"), aggreg)
bubble_equi_direct_ip <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_equi_direct_ip.db", sep="")),
                                          select_km1_soed_2), c("graph","k","fraction"), aggreg)
bubble_equi_rb_ip <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/",scheme,"_equi_rb_ip.db", sep="")),
                                      select_km1_soed_2), c("graph","k","fraction"), aggreg)
hmetis <- ddply(dbGetQuery(dbConnect(SQLite(), dbname=paste("fixed_vertices/db/hmetis.db", sep="")),
                                      select_soed), c("graph","k","fraction"), aggreg)

#extract graph classes from graph names
bubble_free_direct_ip$type <- as.factor(apply(bubble_free_direct_ip, 1, function(x) graphclass(x)))
bubble_free_rb_ip$type <- as.factor(apply(bubble_free_rb_ip, 1, function(x) graphclass(x)))
bubble_fixed_direct_ip$type <- as.factor(apply(bubble_fixed_direct_ip, 1, function(x) graphclass(x)))
bubble_fixed_rb_ip$type <- as.factor(apply(bubble_fixed_rb_ip, 1, function(x) graphclass(x)))
bubble_fixed_rb_ip_integration$type <- as.factor(apply(bubble_fixed_rb_ip_integration, 1, function(x) graphclass(x)))
bubble_equi_direct_ip$type <- as.factor(apply(bubble_equi_direct_ip, 1, function(x) graphclass(x)))
bubble_equi_rb_ip$type <- as.factor(apply(bubble_equi_rb_ip, 1, function(x) graphclass(x)))
hmetis$type <- as.factor(apply(hmetis, 1, function(x) graphclass(x)))


# give each DF a name to identify the algorithm
bubble_free_direct_ip$algorithm = "bubble_free_direct_ip"
bubble_free_rb_ip$algorithm = "bubble_free_rb_ip"
bubble_fixed_direct_ip$algorithm = "bubble_fixed_direct_ip"
bubble_fixed_rb_ip$algorithm = "bubble_fixed_rb_ip"
bubble_fixed_rb_ip_integration$algorithm = "bubble_fixed_rb_ip_integration"
bubble_equi_direct_ip$algorithm = "bubble_equi_direct_ip"
bubble_equi_rb_ip$algorithm = "bubble_equi_rb_ip"
hmetis$algorithm = "hmetis"

#bubble_direct_ip <- subset(bubble_direct_ip, bubble_direct_ip$graph != "sat14_dated-10-11-u.cnf.hgr")
#bubble_rb_ip <- subset(bubble_rb_ip, bubble_rb_ip$graph != "sat14_dated-10-11-u.cnf.hgr")

 ############################################################################################
 
fraction <- c(0.01, 0.05, 0.1, 0.25, 0.5)
filter <- "*"
for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_free_rb_ip[bubble_free_rb_ip$fraction == f,],
                              bubble_fixed_rb_ip = bubble_fixed_rb_ip[bubble_fixed_rb_ip$fraction == f,],
                              bubble_equi_rb_ip = bubble_equi_rb_ip[bubble_equi_rb_ip$fraction == f,],
                              bubble_free_direct_ip = bubble_free_direct_ip[bubble_free_direct_ip$fraction == f,],
                              bubble_fixed_direct_ip = bubble_fixed_direct_ip[bubble_fixed_direct_ip$fraction == f,],
                              bubble_equi_direct_ip = bubble_equi_direct_ip[bubble_equi_direct_ip$fraction == f,]
  ))
}

for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_free_rb_ip[bubble_free_rb_ip$fraction == f,],
                              bubble_fixed_rb_ip = bubble_fixed_rb_ip[bubble_fixed_rb_ip$fraction == f,],
                              bubble_equi_rb_ip = bubble_equi_rb_ip[bubble_equi_rb_ip$fraction == f,]
  ))
}

for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_free_direct_ip[bubble_free_direct_ip$fraction == f,],
                              bubble_fixed_direct_ip = bubble_fixed_direct_ip[bubble_fixed_direct_ip$fraction == f,],
                              bubble_equi_direct_ip = bubble_equi_direct_ip[bubble_equi_direct_ip$fraction == f,]
  ))
}

for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_free_rb_ip[bubble_free_rb_ip$fraction == f,],
                              bubble_free_direct_ip = bubble_free_direct_ip[bubble_free_direct_ip$fraction == f,]
  ))
}

for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_fixed_rb_ip[bubble_fixed_rb_ip$fraction == f,],
                              bubble_fixed_rb_ip_integration = bubble_fixed_rb_ip_integration[bubble_fixed_rb_ip_integration$fraction == f,]
  ))
}

for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_equi_rb_ip[bubble_equi_rb_ip$fraction == f,],
                              bubble_equi_direct_ip = bubble_equi_direct_ip[bubble_equi_direct_ip$fraction == f,]
  ))
}

for (f in fraction) {
  print(paste("Fixed Vertex Fraction=", f, sep=""))
  print(calculateGmeansFilter(filter = filter,
                              avg_obj = "avg_km1", min_obj = "min_km1",
                              kahypar = bubble_fixed_rb_ip[bubble_fixed_rb_ip$fraction == f,],
                              bubble_free_direct_ip = bubble_free_direct_ip[bubble_free_direct_ip$fraction == f,],
                              bubble_fixed_direct_ip = bubble_fixed_direct_ip[bubble_fixed_direct_ip$fraction == f,],
                              bubble_equi_direct_ip = bubble_equi_direct_ip[bubble_equi_direct_ip$fraction == f,]
  ))
}


type <- c("*", "ISPD", "Primal", "Literal", "Dual", "SPM")
for (f in fraction) {
  for (t in type) {
    print(paste("Fixed Vertex Fraction=", f, " Type=", t, sep=""))
    print(calculateGmeansFilter(filter = t,
                                avg_obj = "avg_km1", min_obj = "min_km1",
                                kahypar = bubble_fixed_rb_ip[bubble_fixed_rb_ip$fraction == f,],
                                bubble_fixed_direct_ip = bubble_fixed_direct_ip[bubble_fixed_direct_ip$fraction == f,],
                                hmetis = hmetis[hmetis$fraction == f,]
    )) 
  }
}