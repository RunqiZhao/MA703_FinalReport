library(anytime)
library(tidyverse)
library(dplyr)
library(lubridate)
library(igraph)

graph <- readRDS("graphdata.rda")
data <- readRDS("data.rda")

## Visitor numbers
vcount <- c(NA,NA,NA,NA)
for(i in 1:69){
  vcount <- as.data.frame(rbind(vcount,cbind(i,vcount(graph[[i]]), clusters(graph[[i]])$no, ecount(graph[[i]]))))
}
vcount <- vcount[-1,]
vcount$V2 <- as.integer(vcount$V2)
vcount$V3 <- as.integer(vcount$V3)
summary(vcount$V4)

ggplot(vcount)+
  geom_histogram(aes(x=V2)) + labs(x="Number of Visitors", y = "Frequency", title = "Visitors")

mind <- which.min(vcount$V2)
maxd <- which.max(vcount$V2)
q03d <- which(vcount$V2==round(quantile(vcount$V2, 0.2)))[1]
q05d <- which(vcount$V2==211)

minv <- graph[[mind]] #2009_06_02
maxv <- graph[[maxd]] #2009_07_15
q03v <- graph[[q03d]] #2009_06_05
q05v <- graph[[q05d]] #2009_05_01

# plot1--diameter plot
diamplot <- function(graph){
  diameter(graph, directed=F, weights=NA)
  diam <- get_diameter(graph, directed=F, weights=NA)
  as.vector(diam)
  
  vcol <- rep("gray80", vcount(graph))
  vcol[diam] <- "gold"
  ecol <- rep("gray90", ecount(graph))
  ecol[E(graph, path=diam)] <- "red"
  ew <- rep(1, ecount(graph))
  ew[E(graph, path=diam)] <- 3
  
  diamplot <- plot(graph, 
                   layout = layout_with_fr,
       vertex.color=vcol, 
       vertex.label = NA,
       vertex.size = 5 + 1*(vcol=="gold"),
       edge.color=ecol, 
       edge.arrow.mode=0, 
       edge.width=ew,
       )
  return(diamplot)
}

diamplot(minv) + title(list("June 2nd", cex = 1.5, font = 3))
diamplot(maxv) + title(list("July 15th", cex = 1.5, font = 3))
diamplot(q03v) + title(list("June 5th", cex = 1.5, font = 3))
diamplot(q05v) + title(list("May 1st", cex = 1.5, font = 3))

## Number of connected components
library(ggplot2)
ggplot(vcount) +
  geom_col(aes(x = V2, y=V3), width = 10) + labs(x = "Number of Vistors", y = "Number of Connected Components")

## degree dis
dg <- degree(maxv)
hist(dg)

dg <- degree(graph[[1]])
for(i in 2:69){
  dg <- c(dg, degree(graph[[i]]))
}
mean(dg)

graph[[1]]

## time plot with network diameter highlighted | Temporal features

timeplot <- function(graph){
  diameter(graph, directed=F, weights=NA)
  diam <- get_diameter(graph, directed=F, weights=NA)
  as.vector(diam)
  
  colrs <- c("red", "gray60", "lightblue", "green", "gold", "tomato", "yellow", "blue", "pink")
  V(graph)$color <- colrs[(V(graph)$hour-8)]
  ecol <- rep("gray90", ecount(graph))
  ecol[E(graph, path=diam)] <- "red" 
  ew <- rep(1, ecount(graph))
  ew[E(graph, path=diam)] <- 3
  
  timeplot <- plot(graph, 
                   layout = layout_with_fr,
       edge.color=ecol, 
       edge.width=ew, 
       vertex.size = 5,
       vertex.label = NA)
  # legend(x=-1, y=-0.2, c("9:00-10:00","10:00-11:00", "11:00-12:00", "12:00-13:00", "13:00-14:00", "14:00-15:00", "15:00-16:00", "16:00-17:00", "17:00-18:00"), pch=21,

         # col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  
  return(timeplot)
}

timeplot(minv) + title(list("June 2nd", cex = 1.5, font = 3))
timeplot(maxv) + title(list("July 15th", cex = 1.5, font = 3))
timeplot(q03v) + title(list("June 5th", cex = 1.5, font = 3))
timeplot(q05v) + title(list("May 1st", cex = 1.5, font = 3))

## SI model
### Distribution of contacts duration
maxdt_edg <- as_data_frame(maxv, what = "both")$edges
maxdt_edg$Duration <- maxdt_edg$weight*20/60
ggplot(maxdt_edg) + 
  geom_histogram(aes(x = Duration), bins = 10)

##############
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

## SI model
Infected_Number <- readRDS("InfectedNumber.rda")

library(reshape2)
md <- as.data.frame(t(Infected_Number))
md$DtID <- 1:69
md <- melt(md,id="DtID")
md <- merge(md,vcount,by.x = "DtID", by.y = "i")
md <- md[,c(1,3:5)]
colnames(md) <- c("DtId","Sim1","Vistors","CC")

md$i_s <- md$Sim1/md$Vistors*100

box_SI <- ggplot(data = md, aes(x=as.factor(Vistors), y = Sim1))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "red")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "pink") +
  # theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(x = "Number of Visitors", y = "") +
  scale_x_discrete(breaks = c("90","110","130","163", "195", "305")) + 
  coord_flip()
box_SI

# pct(infected)/pct(su)
# susceptible individual and an infected one
box_SI_PCT <- ggplot(data = md, aes(x=as.factor(Vistors), y = i_s))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "blue")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "lightblue") +
  ylim(0,100) + 
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(x = "", y = "") 
box_SI_PCT

## SI_E model
InfectedNumber_E <- readRDS("InfectedNumber_E.rda")

md <- as.data.frame(t(InfectedNumber_E))
md$DtID <- rownames(md) %>% as.numeric()
md <- melt(md,id="DtID")
md <- merge(md,vcount,by.x = "DtID", by.y = "i")
md <- md[,c(1,3:5)]
colnames(md) <- c("DtId","SimInfectedNumber","Vistors","CC")

md$i_s <- md$SimInfectedNumber/md$Vistors*100

box_SIE <- ggplot(data = md, aes(x = as.factor(Vistors), y = SimInfectedNumber))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "red")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "pink") +
  # theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(x = "Number of Visitors", y = "") +
  scale_x_discrete(breaks = c("90","110","130","163", "195", "305")) + coord_flip()
box_SIE

# pct(infected)/pct(su)
# susceptible individual and an infected one
box_SIE_PCT <- ggplot(data = md, aes(x=as.factor(Vistors), y = i_s))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "blue")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "lightblue") +
  ylim(0,100) + 
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(x = "", y = "")
box_SIE_PCT

InfectedNumber_E2 <- readRDS("InfectedNumber_E2.rda")
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
  # theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(x = "Number of Visitors", y = "Number of Infected") +
  scale_x_discrete(breaks = c("90","110","130","163", "195", "305")) + coord_flip()
box_SIE2

# pct(infected)/pct(su)
# susceptible individual and an infected one
box_SIE2_PCT <- ggplot(data = md, aes(x=as.factor(Vistors), y = i_s))+
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 1, alpha = 0.8, color = "blue")  + 
  stat_summary(fun=mean, geom="point", size = 1, alpha = 0.5, color = "lightblue") +
  ylim(0,100) + 
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = "", y = "Infected Rate(100%)") 
box_SIE2_PCT

