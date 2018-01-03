if ( !exists( "tikzDeviceLoaded" ) ) {  
  library(tikzDevice) #if not installed call install.packages("tikzDevice", repos="http://R-Forge.R-project.org")
  
  options(tikzLatexPackages = c(getOption('tikzLatexPackages')
                                , "\\usepackage[utf8]{inputenc}"
                                , "\\usepackage[T1]{fontenc}"
                                , "\\usepackage{preview} "
                                , "\\usepackage{latexsym,amsmath,amssymb,mathtools,textcomp}"
                                , "\\usepackage{xcolor}"
                                ,paste("\\input{/home/theuer/macros.tex}",sep="")
  )
  )
  tikzDeviceLoaded = T
}


library(ggplot2)
library(scales)
library(RSQLite)
library(DBI)
library(dbConnect)
library(sqldf)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(grid)

penalty_fuc = function(row) {
  if(as.numeric(row['imb']) > 0.03){
    return(as.numeric(-0.1))
  } else {
    return(as.numeric(row['ratio']))
  }
}

graphclass = function(row) {
  if(grepl("*dual*", row['graph'])){
    return("Dual")
  } else if (grepl("*primal*", row['graph'])) {
    return("Primal")
  } else if (grepl("sat14*", row['graph'])) {
    return("Literal")
  } else if (grepl("*mtx*", row['graph'])) {
    return("SPM")
  }  else if (grepl("*ISPD98*", row['graph'])) {
    return("ISPD")
  } else {
    return("DAC")
  }
}

to_latex_math_mode <- function(x) {
  return(paste("$",x,"$", sep=""))
}

gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    return(exp(mean(log(x), na.rm = na.rm)))
  } else {
    return(exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)))
  }
}


calculateGmeans = function(avg_obj, min_obj,kahypar, ...) {
  # We set all "0" km1s to "1" in order to be able to calculate ratios
  dataframes = list(...)
  for(name in names(dataframes)) {
    df = dataframes[[name]]
    df[[avg_obj]][df[[avg_obj]] == 0.0] = 1;              df[[min_obj]][df[[min_obj]] == 0] = 1
  }
  kahypar[[avg_obj]][kahypar[[avg_obj]] == 0.0] = 1;      kahypar[[min_obj]][kahypar[[min_obj]] == 0] = 1
  
  result = data.frame(algo= as.factor("baseline"), 
                      gmean_avg=gm_mean(kahypar[[avg_obj]]), avg_imp=(gm_mean(kahypar[[avg_obj]]) - gm_mean(kahypar[[avg_obj]])) / gm_mean(kahypar[[avg_obj]]) *100,
                      gmean_min=gm_mean(kahypar[[min_obj]]), min_imp=(gm_mean(kahypar[[min_obj]]) - gm_mean(kahypar[[min_obj]])) / gm_mean(kahypar[[min_obj]]) *100,
                      avg_imbalance=mean(kahypar$avg_imbalance), gmean_time=gm_mean(kahypar$avg_time))
  
  for(name in names(dataframes)) {
    df = dataframes[[name]]
    result = rbind(result,   data.frame(algo = as.factor(name),
                                        gmean_avg = gm_mean(df[[avg_obj]]),
                                        avg_imp = (gm_mean(df[[avg_obj]]) - gm_mean(kahypar[[avg_obj]])) / gm_mean(kahypar[[avg_obj]]) *100,
                                        gmean_min = gm_mean(df[[min_obj]]),
                                        min_imp = (gm_mean(df[[min_obj]]) - gm_mean(kahypar[[min_obj]])) / gm_mean(kahypar[[min_obj]]) *100,
                                        avg_imbalance = mean(df$avg_imbalance),
                                        gmean_time = gm_mean(df$avg_time)))
  }
  return(result)
}


calculateGmeansFilter = function(filter,avg_obj, min_obj, kahypar, ...) {
  # We set all "0" km1s to "1" in order to be able to calculate ratios
  dataframes = list(...)
  for(name in names(dataframes)) {
    df = dataframes[[name]]
    df[[avg_obj]][df[[avg_obj]] == 0.0] = 1;              df[[min_obj]][df[[min_obj]] == 0] = 1
  }
  kahypar[[avg_obj]][kahypar[[avg_obj]] == 0.0] = 1;      kahypar[[min_obj]][kahypar[[min_obj]] == 0] = 1
  
  kahypar = kahypar[grep(filter,kahypar$type),]
  
  result = data.frame(algo= as.factor("baseline"), 
                      gmean_avg=gm_mean(kahypar[[avg_obj]]), avg_imp=(gm_mean(kahypar[[avg_obj]]) - gm_mean(kahypar[[avg_obj]])) / gm_mean(kahypar[[avg_obj]]) *100,
                      gmean_min=gm_mean(kahypar[[min_obj]]), min_imp=(gm_mean(kahypar[[min_obj]]) - gm_mean(kahypar[[min_obj]])) / gm_mean(kahypar[[min_obj]]) *100,
                      avg_imbalance=mean(kahypar$avg_imbalance), 
                      gmean_time=gm_mean(kahypar$avg_time),
                      avg_time = mean(kahypar$avg_time))
  
  for(name in names(dataframes)) {
    df = dataframes[[name]]
    df = df[grep(filter,df$type),]
    
    result = rbind(result,   data.frame(algo = as.factor(name),
                                        gmean_avg = gm_mean(df[[avg_obj]]),
                                        avg_imp = (gm_mean(df[[avg_obj]]) - gm_mean(kahypar[[avg_obj]])) / gm_mean(kahypar[[avg_obj]]) *100,
                                        gmean_min = gm_mean(df[[min_obj]]),
                                        min_imp = (gm_mean(df[[min_obj]]) - gm_mean(kahypar[[min_obj]])) / gm_mean(kahypar[[min_obj]]) *100,
                                        avg_imbalance = mean(df$avg_imbalance),
                                        gmean_time = gm_mean(df$avg_time),
                                        avg_time = mean(df$avg_time)))
  }
  return(result)
}




cuberoot_trans = function() trans_new('cuberoot',
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ids_trans = function() trans_new('ids',
                                      transform = function(x) x,
                                      inverse = function(x) x)

# colors
# kahypar-k = #e41a1c
# hmetis-r = #377eb8
# hmetis-k = #4daf4a
# patoh-d = #984ea3
# patoh-q = #ff7f00
# kahypar-R = #a65628
# kahypar-r* = #999999

cuberootplot = function(data, title, xbreaks,yexpand=c(0.0,0.1), legendPos=c(0.285,0.2), colors = c("#377eb8","#e41a1c","#4daf4a", "#984ea3","#a65628","#ff7f00"), showLegend = FALSE) {
  fntsize = 11
  return(ggplot(data, aes(x=x, y=1-ratio, color=algo)) +
           geom_point( size=0.75 ) + 
           geom_hline(yintercept = 1) +
           annotate("text", Inf,Inf,  label="infeasible solutions", size=2, hjust = 1.05, vjust = 1.5)+
           xlab("\\# Instances") +
           ggtitle(title) +
           ylab(paste("1-(Best/Algorithm)")) +
           scale_y_continuous(trans="cuberoot",  breaks = c(0.0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,1.0,2), expand=yexpand) +
           scale_x_continuous(trans="cuberoot", breaks = xbreaks) +
           scale_color_manual(values=colors) +
           theme_bw() +
           theme(aspect.ratio = 2/(1+sqrt(5)),
                 legend.position = if(showLegend) legendPos else "none",
                 legend.background = element_blank(),
                 legend.title = element_text(face="bold",size=10),
                 legend.text=element_text(size=10),
                 legend.key=element_blank(),
                 panel.grid.major = element_line(linetype="dotted",size = 0.5, color = "grey"),
                 panel.grid.minor = element_line(),
                 panel.border = element_rect(colour = "black"),
                 axis.text=element_text(size = 5),
                 axis.text.x=element_text(angle = 50,hjust = 1),
                 axis.line = element_line(size = 0.2, color = "black"),
                 axis.title.y = element_text(vjust=1.5, margin = margin(10,10,10,10), size = 8),
                 axis.title.x = element_text(vjust=1, size = 8),
                 plot.title = element_text(size=12, vjust=.5)) +
                 guides(colour = guide_legend(title="Algorithm", override.aes = list(size=2), ncol = 1, byrow = F, keywidth = .5, keyheight = 0.85, legend.margin =-.5)))
  
}

createRatioFilter = function(filter, column, mimima, UsePenalty, ...) {
  # We set all "0" km1s to "1" in order to be able to calculate ratios
  dataframes = list(...)
  for(name in names(dataframes)) {
    dataframes[[name]][[column]][dataframes[[name]][[column]] == 0.0] = 1;
    dataframes[[name]] = dataframes[[name]][grep(filter,dataframes[[name]]$type),]
  }
  
  result = data.frame(ratio= numeric(0), algo= character(0), x = integer(0))
  
  if (UsePenalty == FALSE) {
    for(name in names(dataframes)) {
      df = dataframes[[name]]
      result = rbind(result,   data.frame(ratio = sort(mimima/df[, column], decreasing=F), 
                                          algo = as.factor(df$algorithm),
                                          x = 1:length(df[, column])))
    }
    return(result)
  } else {
    for(name in names(dataframes)) {
      df = dataframes[[name]]
      
      temp = data.frame(ratio = mimima/df[, column], 
                        algo = as.factor(df$algorithm),
                        imb = df$avg_imbalance)
      
      temp$ratio = apply(temp, 1, function(x) penalty_fuc(x))
      temp = temp[with(temp, order(ratio)), ]
      temp$x = seq.int(nrow(temp))
      
      result = rbind(result,   temp)
    }
    return(result)
    
  }
}

createRatioDFsFilter = function(filter, avg_obj, min_obj, UsePenalty, ...) {
  # We set all "0" km1s to "1" in order to be able to calculate ratios
  dataframes = list(...)
  for(name in names(dataframes)) {
    dataframes[[name]][[avg_obj]][dataframes[[name]][[avg_obj]] == 0.0] = 1;      dataframes[[name]][[min_obj]][dataframes[[name]][[min_obj]] == 0] = 1
    dataframes[[name]] = dataframes[[name]][grep(filter,dataframes[[name]]$type),]
  }
  # find minima
  global_mins_avg = dataframes[[1]][[avg_obj]]
  global_mins_min = dataframes[[1]][[min_obj]]
  for(i in 2:length(dataframes)) {
    global_mins_avg = pmin(global_mins_avg, dataframes[[i]][[avg_obj]])
    global_mins_min = pmin(global_mins_min, dataframes[[i]][[min_obj]])
  }
  
  avg_global_ratios = createRatioFilter(filter=filter,column=avg_obj, mimima=global_mins_avg, UsePenalty = UsePenalty, ...)
  min_global_ratios = createRatioFilter(filter=filter,column=min_obj, mimima=global_mins_min, UsePenalty = UsePenalty, ...)
  
  return(list(avg_ratios=avg_global_ratios,min_ratios=min_global_ratios))
  
}

createRatioDFsForRunningTimeFilter = function(filter,column_name, ...) {
  dataframes = list(...)
  # find minima
  dataframes[[1]] = dataframes[[1]][grep(filter,dataframes[[1]]$type),]
  global_mins_min = dataframes[[1]][[column_name]]
  for(i in 2:length(dataframes)) {
    dataframes[[i]] = dataframes[[i]][grep(filter,dataframes[[i]]$type),]
    global_mins_min = pmin(global_mins_min, dataframes[[i]][[column_name]])
  }
  
  result = data.frame(ratio= numeric(0), algo= character(0), x = integer(0))
  
  for(name in names(dataframes)) {
    df = dataframes[[name]]
    result = rbind(result,   data.frame(ratio = sort(df[, column_name]/global_mins_min, decreasing=F), 
                                        algo = as.factor(df$algorithm),
                                        x = 1:length(df[, column_name])))
  }
  return(result)
}

plotRunningTime = function(data, title,xbreaks,xlimits, ybreaks, legendpos, ytrans="log10", colors = c("#e41a1c","#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) {
  fntsize = 11
  return(ggplot(data, aes(x=x, y=ratio, color=algo)) +
           geom_point(size=1) + 
           xlab("\\# Instances") +
           ggtitle(title) +
           ylab(paste("T(Algorithm)/T(Best)")) +
           scale_y_continuous(trans=ytrans, breaks=ybreaks) +
           scale_x_continuous(breaks=xbreaks, limits=xlimits) +
           theme_bw() +
           scale_color_manual(values=colors) +
           theme(aspect.ratio = 2/(1+sqrt(5)),
                 legend.position = legendpos,
                 legend.background = element_rect(),
                 legend.title = element_text(face="bold",size=8),
                 legend.text=element_text(size=8),
                 panel.grid.major = element_line(linetype="dotted",size = 0.5, color = "grey"),
                 panel.grid.minor = element_line(),
                 panel.border = element_rect(colour = "black"),
                 axis.text=element_text(size = fntsize),
                 axis.text.x=element_text(angle = 50,hjust = 1),
                 axis.line = element_line(size = 0.2, color = "black"),
                 axis.title.y = element_text(vjust=1.5, size = fntsize),
                 axis.title.x = element_text(vjust=1, size = fntsize),
                 plot.title = element_text(size=fntsize, vjust=.5, face="bold")) +
           #scale_color_brewer(name="Algorithm", palette = "Set1") +
           guides(colour = guide_legend(override.aes = list(size=2), ncol = 2, byrow = F, keywidth = .5, keyheight = .5, legend.margin =-.5)))
  
}