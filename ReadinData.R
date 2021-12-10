# readin data
library(anytime)
library(tidyverse)
library(dplyr)
library(lubridate)
library(igraph)
## transf txt to graph
txt_graph <- function(dat){
  dat$t_1 <- anytime(dat$V1, tz = "GMT")
  dat$t_0 <- anytime((dat$V1-20), tz = "GMT")
  colnames(dat) <- c("t", "Node1", "Node2", "T1", "T0")
  Nodes_weight <- dat %>% select(c("Node1", "Node2")) %>% group_by(Node1, Node2) %>% mutate(weight = n()) %>% unique()
  N1_time <- dat %>% select(c("Node1", "T0")) %>% group_by(Node1) %>% arrange(T0) %>% slice(1)
  colnames(N1_time) <- c("Node", "T")
  N2_time <- dat %>% select(c("Node2", "T0")) %>% group_by(Node2) %>% arrange(T0) %>% slice(1)
  colnames(N2_time) <- c("Node", "T")
  Nodes_time <- rbind(N1_time,N2_time) %>% group_by(Node) %>% arrange(T) %>% slice(1)
  Nodes_time$time <- Nodes_time$T
  Nodes_time$hour <- hour(Nodes_time$T)
  Nodes_time$Node <- as.character(Nodes_time$Node)
  Nodes_time <- select(Nodes_time, c("Node", "hour","T"))
  gtime <- graph_from_data_frame(Nodes_weight, directed = F)
  hour <- Nodes_time$hour[Nodes_time$Node == names(V(gtime)[1])]
  time <- Nodes_time$T[Nodes_time$Node == names(V(gtime)[1])]
  for (i in 2:length(V(gtime))){
    hour <- c(hour, Nodes_time$hour[Nodes_time$Node == names(V(gtime)[i])])
  }
  for (i in 2:length(V(gtime))){
    time <- c(time, Nodes_time$T[Nodes_time$Node == names(V(gtime)[i])])
  }
  V(gtime)$hour <- hour
  V(gtime)$time <- time
  return(gtime)
}

txt_dt <- function(dat){
  dat$t_1 <- anytime(dat$V1, tz = "GMT")
  dat$t_0 <- anytime((dat$V1-20), tz = "GMT")
  colnames(dat) <- c("t", "Node1", "Node2", "T1", "T0")
  Nodes_time <- dat %>% select(c("Node1", "Node2","T0")) %>% group_by(Node1, Node2) %>% arrange(T0) %>% slice(1)
  return(Nodes_time)
}

txt_dtall <- function(dat){
  dat$t_1 <- anytime(dat$V1, tz = "GMT")
  dat$t_0 <- anytime((dat$V1-20), tz = "GMT")
  colnames(dat) <- c("t", "Node1", "Node2", "T1", "T0")
  Nodes_time <- dat %>% select(c("Node1", "Node2","T0"))
  return(Nodes_time)
}

## readin data
list <- list.files()
graph <- list()
data <- list()
data_all <- list()
for(i in list){
  path <- i
  datain <- data.frame(read.table(file = path, sep=""))
  data[[i]] <- txt_dt(datain)
  data_all[[i]] <- txt_dtall(datain)
  graph[[i]] <- txt_graph(datain)
}
## Save data
saveRDS(graph,"graphdata.rda")
saveRDS(data,"data.rda")
saveRDS(data_all,"data_all.rda")

