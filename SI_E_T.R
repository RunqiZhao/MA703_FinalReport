
infect_process_E2 <- function(dt, Seed, Time0){
TimeE <- Time0 + 60 * 35
# time point 1
data1 <- dt %>% filter(T0 < TimeE)
I <- Seed
nodes_SI <- filter(data1, T0 == Time0) %>% filter(Node1 == Seed | Node2 == Seed)
if(dim(nodes_SI)[1] > 0){
  nodes_SI$node <- ifelse(nodes_SI$Node1 == Seed, nodes_SI$Node2, nodes_SI$Node1)
  if (length((nodes_SI$node))>0){
    for (i in 1:length((nodes_SI$node))){
      if(rbern(1, p=beta)){
        I <- c(I, nodes_SI$node[i])
      }
    }
  }
}
I <- unique(I)
t1 <- Time0 + 20
while(t1 <= TimeE){
  nodes_SI <- filter(data1, T0 == t1) %>% filter(Node1 %in% I | Node2 %in% I)
  if(dim(nodes_SI)[1] > 0){
    nodes_SI$node <- ifelse(nodes_SI$Node1 %in% I, nodes_SI$Node2, nodes_SI$Node1)
  if (length((nodes_SI$node))>0){
    for (i in 1:length((nodes_SI$node))){
      if(rbern(1, p=beta)){
        I <- c(I, nodes_SI$node[i])
      }
    }
  }
  }
t1 <- t1 + 20
}
I <- unique(I)

# Time Point E - end
data2 <- dt %>% filter(T0 >= TimeE)
gama <- 1/20

t2 <- min(data2$T0)
nodes_E <- filter(data2, T0 == t2) %>% filter(!Node1 %in% I & !Node2 %in% I)
nodes_E <- c(nodes_E$Node1, nodes_E$Node2)

while (t2 <= max(data2$T0)){
  nodes_SI <- filter(data2, T0 == t2) %>% filter(Node1 %in% I | Node2 %in% I)
  if(dim(nodes_SI)[1] > 0){
    nodes_SI$node <- ifelse(nodes_SI$Node1 %in% I, nodes_SI$Node2, nodes_SI$Node1)
    if (length((nodes_SI$node))>0){
      for (i in 1:length((nodes_SI$node))){
        if(rbern(1, p=beta)){
          I <- c(I, nodes_SI$node[i])
        }
      }
    }
  }
  
  nodes_E_new <- filter(data2, T0 == t2) %>% filter(!Node1 %in% I & !Node2 %in% I)
  if(length(nodes_E)>0 & length(nodes_E_new)>0){
    nodes_E <- c(nodes_E, nodes_E_new$Node1, nodes_E_new$Node2)
  } else {nodes_E <-c(nodes_E_new$Node1, nodes_E_new$Node2)}
  nodes_E <- unique(nodes_E)
  
  # %>% unique()
  for (i in length(nodes_E)){
    if(rbern(1,p=gama)){
      I <- c(I, nodes_E[i])
      nodes_E <- nodes_E[-i]
    }
  }
  I <- unique(I)
  t2 <- t2+20
}

return(length(I))
}

graph <- readRDS("graphdata.rda")
data <- readRDS("data.rda")

sim <- 100
I <- rep(NA,sim)
Infected_Number <- data.frame(matrix(nrow=sim,ncol=69))
for (i in 1:69){
  graphdt <- as_data_frame(graph[[i]],what = "both")$vertices
  graphdt$time <- anytime(graphdt$time, tz = "GMT")
  dt <- data[[i]]
  Nodes <- unique(c(unique(dt$Node1),unique(dt$Node2)))
  for (j in 1:sim){
    Seed <- Nodes[sample(length(Nodes),1)]
    Time0 <- graphdt$time[graphdt$name==Seed]
    I[j] <- infect_process_E2(dt, Seed, Time0)
  }
  Infected_Number[,i] <- I
  filename <- paste0("InfectedNumber_E2_D", i, ".rda")
  saveRDS(I, filename)
}