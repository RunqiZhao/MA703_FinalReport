# Readin
## readin data
list <- list.files()
datain <- data.frame(readRDS(list[1]))
for(i in list){
  path <- i
  datain[,i] <- data.frame(readRDS(path))
}
datain <- datain[,-1]
cnames <- gsub("InfectedNumber_E2_D","", colnames(datain))
cnames <- gsub(".rda","",cnames) %>% as.numeric()
colnames(datain) <- cnames

saveRDS(datain,"InfectedNumber_E2.rda")

InfectedNumber_E2 <- readRDS("InfectedNumber_E2.rda")
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
md <- as.data.frame(t(InfectedNumber_E2))
md$DtID <- rownames(md) %>% as.numeric()
md <- melt(md,id="DtID")
md <- merge(md,vcount,by.x = "DtID", by.y = "i")
md <- md[,c(1,3:5)]
colnames(md) <- c("DtId","SimInfectedNumber","Vistors","CC")

md$i_s <- md$SimInfectedNumber/md$Vistors*100

box_SIE2 <- ggplot(data = md, aes(x = as.factor(Vistors), y = SimInfectedNumber))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "red")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "pink") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = "", y = "")
box_SIE2

# pct(infected)/pct(su)
# susceptible individual and an infected one
box_SIE2_PCT <- ggplot(data = md, aes(x=as.factor(Vistors), y = i_s))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "blue")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "lightblue") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) + ylim(0,100) + 
  labs(x = "Number of Visitors", y = "") +
  scale_x_discrete(breaks = c("90","110","130","163", "195", "305"))
box_SIE2_PCT
