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


num_hn_revalue = c("500" = "$500$",
                   "1000" = "$1000$",
                   "5000" = "$5000$",
                   "10000" = "$10000$",
                   "25000" = "$25000$")
type_revalue = c("DAC" = "\\DAC",
                 "ISPD" = "\\ISPD",
                 "Dual" = "\\Dual",
                 "Primal" = "\\Primal",
                 "Literal" = "\\Literal",
                 "SPM"="\\SPM")
algo_revalue = c("goldberg_tarjan" = "\\GoldbergTarjan",
                 "edmond_karp" = "\\EdmondKarp",
                 "boykov_kolmogorov" = "\\BoykovKolmogorov",
                 "ibfs" = "\\IBFS")
network_revalue = c("lawler" = "$\\ExpLawler$",
                    "node_degree" = "$\\ExpNodeDegree$",
                    "edge_size" = "$\\ExpEdgeSize$",
                    "hybrid" = "$\\ExpHybrid$")
indicator_revalue = c("num_nodes" = "Number of Nodes",
                      "num_edges" = "Number of Edges")




theme_complete_bw <- function(base_size = 11, base_family = "") {
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      colour = "black", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                      margin = margin(), debug = FALSE),
    axis.text =          element_text(size = rel(0.8), colour = "grey50"),
    strip.text =         element_text(size = base_size * 0.7),
    axis.line =          element_blank(),
    axis.text.x =        element_text(size = base_size * 0.8, 
                                      lineheight = 0.9, angle = 0, colour = "black",
                                      vjust = 0.5,margin=margin(3,0,0,0)),
    axis.text.y =        element_text(size = base_size * 0.8 , 
                                      lineheight = 0.9, colour = "black",
                                      hjust = 1,margin=margin(0,3,0,0)),
    axis.ticks =         element_line(colour = "black"),
    axis.title.x =       element_text(size = base_size * 0.9,
                                      hjust = 0.5, 
                                      margin = margin(10,0,0,0)),
    axis.title.y =       element_text(size = base_size * 0.9, angle = 90,
                                      vjust = 0.5, margin = margin(5,10,5,0)),
    axis.ticks.length =  unit(0.05, "cm"),
    #axis.ticks.margin =  unit(0.1, "cm"),
    
    legend.background =  element_blank(),
    legend.margin =      unit(0, "cm"),
    legend.key.height =  unit(0.5, "cm"),
    legend.key.width =   unit(0.5, "cm"),
    legend.text =        element_text(size = rel(0.75),margin=margin(3,20,3,10)),
    legend.text.align =  NULL,
    legend.title =       element_blank(),
    legend.title.align = NULL,
    legend.direction =   "horizontal",
    legend.justification = "center",
    legend.box =         NULL,
    legend.position =   "bottom",
    
    #panel.background =   element_rect(fill = NA, colour = "grey", size = 1.3),
    panel.background =   element_rect(fill = NA, colour = "grey", size = 1.3),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "grey90", size = 0.7),
    panel.grid.minor =   element_line(colour = "grey90", size = 0.3),
    panel.margin =       unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = "grey90", colour = "grey90"),
    strip.text.x =       element_text(colour = "black", size = base_size * 1.0, margin=margin(5,10,5,10)),
    strip.text.y =       element_text(colour = "black", size = base_size * 1.0, angle = -90, margin=margin(5,10,5,10)),
    
    plot.background =    element_rect(colour = NA, fill = "white"),
    plot.title =         NULL,#element_text(size = base_size * 1.2),
    plot.margin=         unit(c(0,1,3,0),"mm"),
    complete = TRUE
  )
}

graphclass = function(row) {
  if(grepl("*dual*", row['hypergraph'])){
    return("Dual")
  } else if (grepl("*primal*", row['hypergraph'])) {
    return("Primal")
  } else if (grepl("sat14*", row['hypergraph'])) {
    return("Literal")
  } else if (grepl("dac*", row['hypergraph'])) {
    return("DAC")
  }  else if (grepl("*ISPD98*", row['hypergraph'])) {
    return("ISPD")
  } else {
    return("SPM")
  }
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


revalue_columns_to_latex <- function(db) {
  if("num_hypernodes" %in% colnames(db)) {
    db$num_hypernodes <- revalue(as.character(db$num_hypernodes), num_hn_revalue)
    db$num_hypernodes <- factor(db$num_hypernodes)
    db$num_hypernodes <- factor(db$num_hypernodes, levels = levels(db$num_hypernodes)[c(4,1,5,2,3)])
  }
  if("type" %in% colnames(db)) {
    db$type <- revalue(as.character(db$type), type_revalue) 
    db$type <- factor(db$type)
    db$type <- factor(db$type, levels = levels(db$type)[c(1,3,2,5,4,6)])
  }
  if("flow_algorithm" %in% colnames(db)) {
    db$flow_algorithm <- revalue(as.character(db$flow_algorithm), algo_revalue) 
    db$flow_algorithm <- factor(db$flow_algorithm)
    db$flow_algorithm <- factor(db$flow_algorithm, levels = levels(db$flow_algorithm)[c(2,3,1,4)])
  }
  if("flow_network" %in% colnames(db)) {
    db$flow_network <- revalue(as.character(db$flow_network), network_revalue) 
    db$flow_network <- factor(db$flow_network)
    db$flow_network <- factor(db$flow_network, levels = levels(db$flow_network)[c(3,4,1,2)])
  }
  if("ind" %in% colnames(db)) {
    db$ind <- revalue(as.character(db$ind), indicator_revalue) 
    db$ind <- factor(db$ind)
    db$ind <- factor(db$ind, levels = levels(db$ind)[c(2,1)])
  }
  
  return(db)
}

speed_up_plot <- function(db) {
  speed_up_db <- speedup_relative_to_lawler(db)
  #speed_up_db <- speed_up_db[speed_up_db$flow_network != "lawler"]
  speed_up_db <- revalue_columns_to_latex(speed_up_db)
  
  speed_up_db <- speed_up_db[speed_up_db$flow_network != "$\\ExpLawler$",]
  
  speed_up_db$speedup1 <- 1.0
  speed_up_db$speedup2 <- 2.0
  speed_up_db$speedup3 <- 3.0
    
  plot <- ggplot(speed_up_db, aes( x= flow_network, y = avg_max_flow_time)) + 
    geom_bar(aes(fill = flow_algorithm), stat = "identity", position="dodge") +
    #geom_text(aes(label=round(avg_max_flow_time, digits=1), group = flow_algorithm), vjust=0, position=position_dodge(width = 1), check_overlap = TRUE ) +
    facet_grid(type ~ num_hypernodes, scales = "free") +
    geom_hline(aes(yintercept = speedup1), color="red", linetype="dashed") +
    geom_hline(aes(yintercept = speedup2), color="blue", linetype="dashed") +
    ylab("Speed up relative to $\\ExpLawler$") +
    xlab("Flow Network") +
    theme_complete_bw()
  
  return(plot)
}

speed_up_plot_relative_to <- function(db, relative_algo="edmond_karp", relative_network="lawler") {
  speed_up_db <- speedup_relative_to(db,relative_algo,relative_network)
  speed_up_db <- revalue_columns_to_latex(speed_up_db)
  
  speed_up_db$speedup1 <- 1.0
  speed_up_db$speedup2 <- 2.0
  speed_up_db$speedup3 <- 3.0
  
  plot <- ggplot(speed_up_db, aes( x= flow_network, y = avg_max_flow_time)) + 
    geom_bar(aes(fill = flow_algorithm), stat = "identity", position="dodge") +
    #geom_text(aes(label=round(avg_max_flow_time, digits=1), group = flow_algorithm), vjust=0, position=position_dodge(width = 1), check_overlap = TRUE ) +
    facet_grid(type ~ num_hypernodes, scales = "free") +
    geom_hline(aes(yintercept = speedup1), color="red", linetype="dashed") +
    geom_hline(aes(yintercept = speedup2), color="blue", linetype="dashed") +
    ylab("Speed up relative to $\\ExpLawler$") +
    xlab("Flow Network") +
    theme_complete_bw()
  
  return(plot)
}


max_flow_time_per_network_plot <- function(db, aggreg_by=c("flow_network", "flow_algorithm", "num_hypernodes", "type"), title="") {
  aggreg <- function(df) data.frame(objective=mean(df$avg_max_flow_time))
  df <- ddply(db, aggreg_by, aggreg)
  
  plot <- ggplot(df, aes( x= flow_network, y = objective)) + 
          geom_bar(aes(fill = flow_algorithm), stat = "identity", position="dodge") +
          geom_text(aes(label=round(objective, digits=1), group = flow_algorithm), vjust=0, position=position_dodge(width = 1), check_overlap = TRUE ) +
          ggtitle(title) +
          ylab("Time [ms]") +
          xlab("Flow Network") +
          theme_complete_bw()
  
  return(plot)
}


node_edge_distribution_plot <- function(db) {
  aggreg <- function(df) data.frame(num_nodes=gm_mean(df$avg_num_nodes),
                                    num_edges=gm_mean(df$avg_num_edges))
  df <- ddply(db, c("flow_network", "flow_algorithm", "num_hypernodes","type"), aggreg)
  df <- revalue_columns_to_latex(df)

  plot <- ggplot(df, aes( x= num_nodes, y = num_edges)) + 
    geom_line(aes(fill = type), alpha=0.25) + 
    geom_point(aes(color = flow_network, shape = type), size = 7) +
    geom_vline(aes(xintercept = 25000), color="red", linetype="dashed") +
    scale_x_sqrt() +
    scale_y_sqrt() +
    #xlim(0,600000) +
    #ylim(0,1200000) +
    ylab("Number of Edges") +
    xlab("Number of Nodes") +
    theme_complete_bw()
  
  return(plot)
}

node_edge_distribution_plot2 <- function(db) {
  aggreg <- function(df) data.frame(num_nodes=gm_mean(df$avg_num_nodes),
                                    num_edges=gm_mean(df$avg_num_edges))
  df <- ddply(db, c("flow_network", "flow_algorithm", "num_hypernodes","type"), aggreg)
  df <- cbind(df[1:4],stack(df[5:6]))
  df <- revalue_columns_to_latex(df)
  
  plot <- ggplot(df) + 
          geom_bar(aes( x= flow_network, y = values, fill = ind), stat = "identity", position="dodge") +
          geom_hline(aes(yintercept = 25000), color="red", linetype="dashed") +
          facet_wrap( ~ type ) + 
          ylab("Number of Nodes/Edges") +
          xlab("Flow Network") +
          
          theme_complete_bw()
  
  return(plot)
}


gmean_network_algorithm_table <- function(db) {
  algorithms <- levels(factor(db$flow_algorithm))
  networks <- levels(factor(db$flow_network))
  
  aggreg = function(df) data.frame(avg_num_nodes=gm_mean(df$avg_num_nodes),
                                   avg_num_edges=gm_mean(df$avg_num_edges),
                                   min_network_build_time=gm_mean(df$min_network_build_time), 
                                   avg_network_build_time=gm_mean(df$avg_network_build_time), 
                                   min_max_flow_time=gm_mean(df$min_max_flow_time), 
                                   avg_max_flow_time=gm_mean(df$avg_max_flow_time))
  
  df <- data.frame()
  for (algorithm in algorithms) {
    for(network in networks) {
      df <- rbind(df, ddply(db[db$flow_network == network & db$flow_algorithm == algorithm,],
                            c("flow_network", "flow_algorithm"),aggreg))
    }
  }
  
  df <- df[order(df$avg_max_flow_time),]
  
  for(i in c(2:nrow(df))) {
    df[i,3:ncol(df)] <- (df[i, 3:ncol(df)]/df[1,3:ncol(df)] - 1.0)*100.0
  }
  
  return(df)
}

to_latex_math_mode <- function(x) {
  sign <- '+'
  if(substring(x,1,1) == "-") {
    sign <- '-'
  }
  return(paste("$",sign,abs(as.numeric(x)),"$", sep=""))
}

to_latex_bold_math_mode <- function(x) {
  # Input is a string of the form "$+2.34$"
  return(paste("$","\\mathbf{",substr(x,2,nchar(x)-1),"}$",sep=""))
}

create_flow_network_max_flow_table <- function(df, instance_type = "ALL") {
  aggreg <- function(df) data.frame(build_time=gm_mean(df$avg_network_build_time),
                                    max_flow_time=gm_mean(df$avg_max_flow_time))
  db <- ddply(df, c("num_hypernodes", "flow_algorithm", "flow_network"), aggreg)
  
  for( num_hn in levels(factor(db$num_hypernodes)) ) {
    column <- db[db$num_hypernodes == num_hn,]["max_flow_time"][c(1:12),]
    column <- c(column[9:12],column[5:8],column[1:4])
    min_idx <- which.min(column)
    column[2:12] <- (column[2:12]/column[1] - 1.0)*100.0
    column <- c(num_hn, as.character(round(column, digits = 2)))
    type <- ""
    if(num_hn == 500) {
      type <- paste("\\multirow{5}{*}{\\rotatebox{90}{\\", instance_type, "}}", sep="")
    }
    column <- c(type, paste("$",column[1],"$",sep=""), paste("$",column[2],"$",sep=""), sapply(column[3:13], to_latex_math_mode))
    column[min_idx+2] <- to_latex_bold_math_mode(column[min_idx+2])
    cat(paste(column, collapse = " & "))
    cat(" \\\\ \n")
  }
}

create_flow_network_max_flow_table_hybrid <- function(df) {
  aggreg <- function(df) data.frame(build_time=gm_mean(df$avg_network_build_time),
                                    max_flow_time=gm_mean(df$avg_max_flow_time))
  df <- df[df$flow_network == "hybrid",]
  db <- ddply(df, c("num_hypernodes", "flow_algorithm", "flow_network"), aggreg)
  
  for( num_hn in levels(factor(db$num_hypernodes)) ) {
    column <- db[db$num_hypernodes == num_hn,]["max_flow_time"][c(1:4),]
    column <- rev(column)
    min_idx <- which.min(column)
    column[2:4] <- (column[2:4]/column[1] - 1.0)*100.0
    column <- c(num_hn, as.character(round(column, digits = 2)))
    column <- c(paste("$",column[1],"$",sep=""), paste("$",column[2],"$",sep=""), sapply(column[3:5], to_latex_math_mode))
    column[min_idx+1] <- to_latex_bold_math_mode(column[min_idx+1])
    cat(paste(column, collapse = " & "))
    cat(" \\\\ \n")
  }
}

speedup_relative_to_lawler <- function(db) {
  aggreg <- function(df) data.frame(avg_max_flow_time=gm_mean(df$avg_max_flow_time))
  df <- ddply(db, c("num_hypernodes", "type", "flow_algorithm", "flow_network"), aggreg) 
  for( num_hn in levels(factor(df$num_hypernodes)) ) {
    for( type in levels(factor(df$type))) {
      for(algo in levels(factor(df$flow_algorithm))) {
        lawler_algo <- df[df$num_hypernodes == num_hn & df$type == type & df$flow_network == "lawler" & df$flow_algorithm == algo,]
        df[df$num_hypernodes == num_hn & df$type == type & df$flow_algorithm == algo,]$avg_max_flow_time <- lawler_algo$avg_max_flow_time / df[df$num_hypernodes == num_hn & df$type == type & df$flow_algorithm == algo,]$avg_max_flow_time 
      }
    }
  }
  return(df)
}

speedup_relative_to <- function(db, algo, network) {
  aggreg <- function(df) data.frame(avg_max_flow_time=gm_mean(df$avg_max_flow_time))
  df <- ddply(db, c("num_hypernodes", "type", "flow_algorithm", "flow_network"), aggreg) 
  for( num_hn in levels(factor(df$num_hypernodes)) ) {
    for( type in levels(factor(df$type))) {
      df[df$num_hypernodes == num_hn & df$type == type,]$avg_max_flow_time <- df[df$num_hypernodes == num_hn & df$type == type & df$flow_network == network & df$flow_algorithm == algo,]$avg_max_flow_time /
                                                                              df[df$num_hypernodes == num_hn & df$type == type,]$avg_max_flow_time 
    }
  }
  return(df)
}

create_instance_type_table <- function(db) {
  aggreg <- function(df) data.frame(avg_hn_degree=mean(df$avg_hn_degree),
                                    avg_he_size=mean(df$avg_he_size))
  
  to_math <- function(x) {
    return(paste("$",x,"$",sep=""))
  }
  
  instance_db <- ddply(db, c("hypergraph"),aggreg)
  instance_db$type <- as.factor(apply(instance_db, 1, function(x) graphclass(x)))
  instance_db <- ddply(db, c("type"), aggreg)
  instance_db$avg_hn_degree <- sapply(round(instance_db$avg_hn_degree, digits = 2), to_math)
  instance_db$avg_he_size <- sapply(round(instance_db$avg_he_size, digits = 2), to_math)
  instance_db <- revalue_columns_to_latex(instance_db)
  return(instance_db)
}

edmond_karp_goldberg_tarjan_comparison <- function(db, network = "hybrid") {
  edmond_karp <- db[db$flow_algorithm == "edmond_karp" & db$flow_network == network,]
  goldberg_tarjan <- db[db$flow_algorithm == "goldberg_tarjan" & db$flow_network == network,]
  edmond_karp <- edmond_karp[order(edmond_karp$avg_num_nodes),]
  goldberg_tarjan <- goldberg_tarjan[order(goldberg_tarjan$avg_num_nodes),]
  
  prefix_gmean_max_flow_time <- function(df) {
    df$flow_size_type <- 0
    for(i in c(1:nrow(df))) {
      df$gmean_max_flow_time[i] <- gm_mean(df$avg_max_flow_time[c(1:i)])
    }
    return(df)
  }
  
  edmond_karp <- prefix_gmean_max_flow_time(edmond_karp)
  goldberg_tarjan <- prefix_gmean_max_flow_time(goldberg_tarjan)
  df <- edmond_karp
  df$gmean_max_flow_time <- goldberg_tarjan$gmean_max_flow_time - edmond_karp$gmean_max_flow_time
  df$ratio <- edmond_karp$avg_max_flow_time / goldberg_tarjan$avg_max_flow_time
  df$num_edges <- ifelse(edmond_karp$avg_num_edges <= 125000, "EdmondKarp", "GoldbergTarjan")
  
  plot <- ggplot(df, aes(x = avg_num_nodes, y = ratio )) + 
    geom_point(aes(color=num_edges)) +
    #geom_line(aes(x = avg_num_nodes, y = gmean_max_flow_time), size=2, color="pink") +
    geom_hline(aes(yintercept = 1), color="red") +
    #geom_hline(aes(yintercept = 0), color="red") +
    geom_vline(aes(xintercept = 2^15), color="blue") +
    xlab("Number of Nodes") +
    ylab("Edmond Karp / Goldberg Tarjan (in t[ms])") +
    #xlim(0,1000000) +
    scale_x_continuous(trans="log2") +
    ylim(0,3) +
    theme_complete_bw()
  
  return(plot)
}

build_hybrid_flow_algo <- function(db, num_nodes_threshold = 2^15) {
  edmond_karp <- db[db$flow_algorithm == "edmond_karp" & db$flow_network == "hybrid",]
  goldberg_tarjan <- db[db$flow_algorithm == "goldberg_tarjan" & db$flow_network == "hybrid",]
  hybrid <- rbind(edmond_karp[edmond_karp$avg_num_nodes <= num_nodes_threshold,],goldberg_tarjan[goldberg_tarjan$avg_num_nodes > num_nodes_threshold,])
  hybrid$flow_algorithm <- "hybrid"
  hybrid <- rbind(edmond_karp,goldberg_tarjan,hybrid)
  return(hybrid)
}

sanityCheck <- function(db) {
  for( num in levels(factor(db$num_hypernodes))) {
    for (hg in levels(factor(db$hypergraph))) {
      max_flow_values <- c()
      for( algo in levels(factor(db$flow_algorithm))) {
        for( network in levels(factor(db$flow_network))) {
          max_flow_value <- db[db$hypergraph == hg & 
                               db$num_hypernodes == num &
                               db$flow_algorithm == algo &
                               db$flow_network == network,]["avg_max_flow"][1,]
          if(!is.na(max_flow_value)) {
            max_flow_values <- c(max_flow_values, max_flow_value) 
          }
        }
      }
      if(!all(max_flow_values == max_flow_values[1])) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
