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
    return("SAT14")
  } else if (grepl("*ISPD98*", row['hypergraph'])) {
    return("ISPD98")
  } else {
    return("SPM")
  }
}

max_flow_time_per_network_plot <- function(db, title="") {
  plot <- ggplot(db, aes( x= factor(db$flow_network), y = db$avg_max_flow_time, fill = factor(db$flow_algorithm) )) + 
          geom_bar(stat = "identity", position = "dodge") +
          facet_wrap( ~ alpha, scales="free") + 
          ggtitle(title) +
          ylab("Time [ms]") +
          xlab("Flow Network") +
          theme_complete_bw()
  return(plot)
}

max_flow_time_per_benchmark_type_plot <- function(db, title="") {
  plot <- ggplot(db, aes( x = factor(db$flow_network), y = db$avg_max_flow_time, fill = db$flow_algorithm )) + 
          geom_bar(stat = "identity", position = "dodge")  +
          facet_wrap( ~ type, scales="free") + 
          ggtitle(title) +
          ylab("Time [ms]") +
          xlab("Flow Network") +
          theme_complete_bw()
  return(plot)
}

