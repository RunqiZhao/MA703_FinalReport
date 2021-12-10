infect_process_E <- function(dt, Seed, Time0){
Nodes <- unique(c(unique(dt$Node1),unique(dt$Node2)))
TimeE <- Time0 + 60 * 35
## Part 1: t0 - TE
## Time point 1
data1 <- dt %>% filter(T0 < TimeE)
Nodes_t <- data1[c(which(data1$Node1==Seed),which(data1$Node2==Seed)),] %>% filter(T0>=Time0)
Nodes_t$Node <- ifelse(Nodes_t$Node1 == Seed, Nodes_t$Node2, Nodes_t$Node1)
Nodes_t <- Nodes_t[,c(4,3)]
New_I <- Nodes_t$Node
I <- c(Seed,New_I)

### Time point 2
nt <- c(NA,NA)
for (i in 1:length(New_I)){
  seed <- New_I[i]
  time <- Nodes_t[i,2]
  nodes_t <- data1[c(which(data1$Node1==seed), which(data1$Node2==seed)),] %>% filter(T0>=time)
  nodes_t$node <- ifelse(nodes_t$Node1 == seed, nodes_t$Node2, nodes_t$Node1)
  nodes_t <- nodes_t[,c(4,3)]
  nt <- rbind(nt,nodes_t)
}

### Time point 3 - TE
while (dim(nt)[1]>1){
  nt <- nt[-1,]
  testdt <- nt %>% filter(!node %in% New_I)
  if (dim(testdt)[1]==0) {
    break
  }
  Nodes_t <- nt %>% filter(!node %in% New_I) %>% group_by(node) %>% slice(1)
  New_I <- nt %>% filter(!node %in% New_I) %>% select(node) %>% unique()
  New_I <- New_I$node
  I <- c(I,New_I)  
  if (length(New_I)==0) {
    break
  }
  nt <- c(NA,NA)
  for (i in 1:length(New_I)){
    seed <- New_I[i]
    time <- Nodes_t[i,2]
    nodes_t <- data1[c(which(data1$Node1==seed), which(data1$Node2==seed)),] %>% filter(T0>=time)
    nodes_t$node <- ifelse(nodes_t$Node1 == seed, nodes_t$Node2, nodes_t$Node1)
    nodes_t <- nodes_t[,c(4,3)]
    if(dim(nodes_t)[1]>0){
      nt <- rbind(nt,nodes_t)
    }
  }
  if(is.na(nt)){
    break
  }
}

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
  I <- c(I, nodes_SI$node)
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
  
t2 <- t2+20
}

return(length(I))
}

graph <- readRDS("graphdata.rda")
data <- readRDS("data.rda")

## Visitor numbers
vcount <- c(NA,NA,NA)
for(i in 1:69){
  vcount <- as.data.frame(rbind(vcount,cbind(i,vcount(graph[[i]]), clusters(graph[[i]])$no)))
}
vcount <- vcount[-1,]
vcount$V2 <- as.integer(vcount$V2)
vcount$V3 <- as.integer(vcount$V3) # connected

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
    I[j] <- infect_process_E(dt, Seed, Time0)
  }
  Infected_Number[,i] <- I
}
colnames(Infected_Number) <- as.character(1:69)

saveRDS(Infected_Number, "InfectedNumber_E.rda")
Infected_Number <- readRDS("InfectedNumber_E.rda")
