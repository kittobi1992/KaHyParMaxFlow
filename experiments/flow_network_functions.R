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
                                      vjust = 0.5, margin = margin(0,10,0,0)),
    axis.ticks.length =  unit(0.05, "cm"),
    #axis.ticks.margin =  unit(0.1, "cm"),
    
    legend.background =  element_blank(),
    legend.margin =      unit(0.25, "cm"),
    legend.key.height =  unit(1.0, "cm"),
    legend.key.width =   unit(1.0, "cm"),
    legend.text =        element_text(size = rel(0.85),margin=margin(3,3,3,10)),
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
    panel.margin =       unit(0.1, "lines"),
    
    strip.background =   element_rect(fill = "grey90", colour = "grey90"),
    strip.text.x =       element_text(colour = "black", size = base_size * 0.9, margin=margin(3,10,3,10)),
    strip.text.y =       element_text(colour = "black", size = base_size * 1.0, angle = -90),
    
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

max_flow_time_per_network_plot <- function(db, aggreg_by=c("flow_network", "flow_algorithm", "num_hypernodes", "type"), title="") {
  aggreg <- function(df) data.frame(objective=mean(df$avg_max_flow_time),
                                    num_nodes=gm_mean(df$avg_num_nodes))
  df <- ddply(db, aggreg_by, aggreg)
  
  plot <- ggplot(df, aes( x= flow_network, y = objective)) + 
          geom_bar(aes(fill = flow_algorithm), stat = "identity", position="dodge") +
          geom_text(aes(label=round(objective, digits=2), group = flow_algorithm), vjust=0, position=position_dodge(width = 1), check_overlap = TRUE ) +
          ggtitle(title) +
          ylab("Time [ms]") +
          xlab("Flow Network") +
          theme_complete_bw()
  
  return(plot)
}


node_edge_distribution_plot <- function(db, aggreg_by=c("flow_network", "num_hypernodes", "type"), title="") {
  aggreg <- function(df) data.frame(num_nodes=gm_mean(df$avg_num_nodes),
                                    num_edges=gm_mean(df$avg_num_edges))
  df <- ddply(db, aggreg_by, aggreg)
  
  plot <- ggplot(df, aes( x= num_nodes, y = num_edges)) + 
    geom_point(aes(color = flow_network, shape = type ), size = 10) +
    ggtitle(title) +
    coord_trans(x="sqrt", y="sqrt") +
    ylab("Number of Edges") +
    xlab("Number of Nodes") +
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

test <- "+ads"
test[1]

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
    column <- db[db$num_hypernodes == num_hn,]["max_flow_time"][c(1:8),]
    min_idx <- which.min(column)
    column[2:8] <- (column[2:8]/column[1] - 1.0)*100.0
    column <- c(num_hn, as.character(round(column, digits = 2)))
    type <- ""
    if(num_hn == 500) {
      type <- paste("\\multirow{5}{*}{\\rotatebox{90}{\\", instance_type, "}}", sep="")
    }
    column <- c(type, paste("$",column[1],"$",sep=""), paste("$",column[2],"$",sep=""), sapply(column[3:9], to_latex_math_mode))
    column[min_idx+2] <- to_latex_bold_math_mode(column[min_idx+2])
    cat(paste(column, collapse = " & "))
    cat(" \\\\ \n")
  }
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
