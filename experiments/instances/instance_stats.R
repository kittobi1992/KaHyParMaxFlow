setwd("/home/theuer/Dropbox/Studium Informatik/10. Semester/KaHyParMaxFlow")
#setwd("C:\\Users\\tobia\\Dropbox\\Studium Informatik\\10. Semester\\KaHyParMaxFlow")
source("experiments/plot_functions.R")

type_revalue = c("DAC" = "\\DAC",
                 "ISPD" = "\\ISPD",
                 "Dual" = "\\Dual",
                 "Primal" = "\\Primal",
                 "Literal" = "\\Literal",
                 "SPM"="\\SPM")

to_latex_math_mode <- function(x) {
  return(paste("$",x,"$", sep=""))
}

aggreg1 = function(df) data.frame(cnt = to_latex_math_mode(length(df$type)),
                                  minHN = to_latex_math_mode(round(min(df$HNs))),
                                  HN = to_latex_math_mode(round(gm_mean(df$HNs))),
                                  maxHN = to_latex_math_mode(round(max(df$HNs))),
                                  minHE = to_latex_math_mode(round(min(df$HEs))),
                                  HE = to_latex_math_mode(round(gm_mean(df$HEs))))

aggreg2 = function(df) data.frame(maxHE = to_latex_math_mode(round(max(df$HEs))),
                                  avgHEsize = to_latex_math_mode(round(gm_mean(df$avgHEsize), digits=2)),
                                  medHEsize = to_latex_math_mode(round(gm_mean(df$medHEsize), digits = 2)),
                                  avgHNdegree = to_latex_math_mode(round(gm_mean(df$avgHNdegree), digits=2)),
                                  medHNdegree = to_latex_math_mode(round(gm_mean(df$medHNdegree), digits = 2)),
                                  density = to_latex_math_mode(round(gm_mean(df$density), digits = 2)))

to_latex_instance_type_table <- function(db, file, aggreg) {
  df <- ddply(db, c("type"), aggreg)
  df$type <- revalue(as.character(df$type), type_revalue) 
  df$type <- factor(df$type)
  if(length(levels(df$type)) == 6) {
    df$type <- factor(df$type, levels = levels(df$type)[c(1,3,5,4,2,6)]) 
  } else {
    df$type <- factor(df$type, levels = levels(df$type)[c(2,4,3,1,5)]) 
  }
  sink(file)
  for( type in levels(factor(df$type))) {
    type_df <- df[df$type == type,]
    names(type_df) <- NULL
    cat(paste(unlist(c(type_df)), collapse=" & "))
    cat("\\\\ \n")
  }
  sink()
}

full <- read.csv("experiments/instances/full_benchmark_set.csv")
full$type <- as.factor(apply(full, 1, function(x) graphclass(x)))

subset <- full[full$graph %in% scan("experiments/instances/benchmark_subset.txt", character(), quote = ""),]
parameter_tunning <- full[full$graph %in% scan("experiments/instances/parameter_tunning_subset.txt", character(), quote = ""),]

to_latex_instance_type_table(full, file = "master_thesis/experiments/instances/full_benchmark_stats1.tex", aggreg = aggreg1)
to_latex_instance_type_table(full, file = "master_thesis/experiments/instances/full_benchmark_stats2.tex", aggreg = aggreg2)
to_latex_instance_type_table(subset, file = "master_thesis/experiments/instances/subset_benchmark_stats1.tex", aggreg = aggreg1)
to_latex_instance_type_table(subset, file = "master_thesis/experiments/instances/subset_benchmark_stats2.tex", aggreg = aggreg2)
to_latex_instance_type_table(parameter_tunning, file = "master_thesis/experiments/instances/parameter_tunning_stats1.tex", aggreg = aggreg1)
to_latex_instance_type_table(parameter_tunning, file = "master_thesis/experiments/instances/parameter_tunning_stats2.tex", aggreg = aggreg2)
