# Readin
## readin data
# list <- list.files()
# datain <- data.frame(readRDS(list[1]))
# for(i in list){
#   path <- i
#   datain[,i] <- data.frame(readRDS(path))
# }
# datain <- datain[,-1]
# cnames <- gsub("InfectedNumber_E_D","", colnames(datain))
# cnames <- gsub(".rda","",cnames) %>% as.numeric()
# colnames(datain) <- cnames

# saveRDS(datain,"InfectedNumber_E.rda")

InfectedNumber_E <- readRDS("InfectedNumber_E.rda")
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

library(reshape2)
md <- as.data.frame(t(InfectedNumber_E))
md$DtID <- rownames(md) %>% as.numeric()
md <- melt(md,id="DtID")
md <- merge(md,vcount,by.x = "DtID", by.y = "i")
md <- md[,c(1,3:5)]
colnames(md) <- c("DtId","SimInfectedNumber","Vistors","CC")

md$i_s <- md$SimInfectedNumber/md$Vistors*100

box_SIE <- ggplot(data = md)+
  geom_boxplot(aes(x=as.factor(Vistors), y = SimInfectedNumber), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) + labs(x = "Number of Visitors", y = "Infected Count")
box_SIE

# pct(infected)/pct(su)
# susceptible individual and an infected one
box_SIE_PCT <- ggplot(data = md)+
  geom_boxplot(aes(x=as.factor(Vistors), y = i_s), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) + ylim(0,100) + 
  labs(x = "Number of Visitors", y = "Infected Rate(100%)")
box_SIE_PCT
