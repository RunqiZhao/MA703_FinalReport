## SI model
### Infect Number
infect_process <- function(data,Seed,Time){
### Time point 1
Nodes_t <- data[c(which(data$Node1==Seed),which(data$Node2==Seed)),] %>% filter(T0>=Time)
Nodes_t$Node <- ifelse(Nodes_t$Node1 == Seed, Nodes_t$Node2, Nodes_t$Node1)
Nodes_t <- Nodes_t[,c(4,3)]
New_I <- Nodes_t$Node
I <- c(Seed,New_I)
### Time point 2
nt <- c(NA,NA)
for (i in 1:length(New_I)){
  seed <- New_I[i]
  time <- Nodes_t[i,2]
  nodes_t <- data[c(which(data$Node1==seed), which(data$Node2==seed)),] %>% filter(T0>=time)
  nodes_t$node <- ifelse(nodes_t$Node1 == seed, nodes_t$Node2, nodes_t$Node1)
  nodes_t <- nodes_t[,c(4,3)]
  nt <- rbind(nt,nodes_t)
}

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
    nodes_t <- data[c(which(data$Node1==seed), which(data$Node2==seed)),] %>% filter(T0>=time)
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

### Seed & Simulation
sim <- 100
I <- rep(NA,sim)
Infected_Number <- data.frame(matrix(nrow=sim,ncol=69))
for (i in 1:69){
  graphdt <- as_data_frame(graph[[i]],what = "both")$vertices
  graphdt$time <- anytime(graphdt$time, tz = "GMT")
  dt <- data[[i]]
  Nodes <- unique(c(unique(dt$Node1),unique(dt$Node2)))
  for (j in 1:100){
    Seed <- Nodes[sample(length(Nodes),1)]
    Time <- graphdt$time[graphdt$name==Seed]
    I[j] <- infect_process(dt, Seed, Time)
  }
  Infected_Number[,i] <- I
}
colnames(Infected_Number) <- as.character(1:69)

saveRDS(Infected_Number, "InfectedNumber.rda")
Infected_Number <- readRDS("InfectedNumber.rda")

library(reshape2)
md <- as.data.frame(t(Infected_Number))
md$DtID <- 1:69
md <- melt(md,id="DtID")
md <- merge(md,vcount,by.x = "DtID", by.y = "i")
md <- md[,c(1,3:5)]
colnames(md) <- c("DtId","Sim1","Vistors","CC")

md$i_s <- md$Sim1/md$Vistors*100

box_SI <- ggplot(data = md)+
  geom_boxplot(aes(x=as.factor(Vistors), y = Sim1), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) + labs(x = "Number of Visitors", y = "Infected Count")
box_SI
# pct(infected)/pct(su)
# susceptible individual and an infected one
box_SI_PCT <- ggplot(data = md)+
  geom_boxplot(aes(x=as.factor(Vistors), y = i_s), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) + ylim(0,100) + 
  labs(x = "Number of Visitors", y = "Infected Rate(100%)")
box_SI_PCT

