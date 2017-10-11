library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(cowplot)
library(kableExtra)
library(knitr)

setwd("~/Desktop/DS1001/Data/Indeed")
Indeed<-read.csv("Indeed.csv")
Indeed<-as.data.frame(Indeed)
setwd("~/Desktop/DS1001/Data/Glassdoor")
GlassDoor<-read.csv("GlassDoor.csv")
GlassDoor<-as.data.frame(GlassDoor)
colnames(GlassDoor)[19]<-'C++'
colnames(Indeed)[19]<-'C++'


#Skill set
glassdoor.com<-GlassDoor[,6:23] %>% colSums()/nrow(GlassDoor)
indeed.com<-Indeed[,5:22] %>% colSums()/nrow(Indeed)
df<-cbind(glassdoor.com,indeed.com)
df.long<-melt(df)
colnames(df.long)[2]<-'website'

skillbar<-ggplot(df.long,aes(reorder(Var1, value),value,fill=website))+
  geom_bar(stat="identity",position="dodge")+ 
  coord_flip() + labs(x = 'Skills', y = 'Occurrences (%)', 
                      title = 'Skills that data scientist needs according to job posting websites')+
  theme(plot.title = element_text(hjust = 0.5))

dfG<-df.long[1:18,c(1,3)]
a<-dfG[dfG$value%in%head(sort(dfG$value,decreasing=TRUE), n = 5),]
a<-arrange(a,desc(value))
colnames(a)<-c("Glassdoor","Occurrences")


dfI<-df.long[19:36,c(1,3)]
b<-dfI[dfI$value%in%head(sort(dfI$value,decreasing=TRUE), n = 5),]
b<-arrange(b,desc(value))
colnames(b)<-c("Indeed","Occurrences")

skillbar

kable(cbind(a,b))

testG<-GlassDoor[,c('R','Python','Math','C++','Statistics')]
testI<-Indeed[,c('R','Python','Math','C++','Statistics')]

#proportional test
proptest<-matrix(rep(NA,10),ncol=2)
for(i in 1:5){
  x<-prop.test(c(sum(testG[,i]),sum(testI[,i])),c(nrow(testG),nrow(testI)),correct=FALSE)
  proptest[i,]<-c(colnames(testG)[i],x$p.value)
}
proptest<-as.data.frame(proptest)
colnames(proptest)<-c("Skills","p-value")
## proportional test
kable(proptest)







####--------------------#####
###Map
library(ggmap)
#location form city
city_geocode_G<-as.character(GlassDoor$city)
city_geocode_I<-as.character(Indeed$city)
setwd("~/Desktop/DS1001/Data")
loc_G<-read.csv("geoforlgassdoor.csv")
loc_I<-read.csv("geoforlindeed.csv")

#loc_G<-cbind(city_geocode_G,geocode(city_geocode_G))
#write.csv(loc_G,"geoforlgassdoor.csv")
#loc_I<-cbind(city_geocode_I,geocode(city_geocode_I))
#write.csv(loc_I,"geoforlindeed.csv")

map <- get_map(location = 'United States',zoom=4, maptype="roadmap",color = "bw")



G1<-ggmap(map) + borders(database="state")+
    geom_point(aes(x = lon, y = lat,color='dark red',alpha=.2),size=3, 
                        data = loc_G)+theme(legend.position="none")+
ggtitle("Glassdoor.com (n=616)")+
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())


I1<-ggmap(map) + borders(database="state")+
   geom_point(aes(x = lon, y = lat,alpha=.5),size=3,color='green', 
                        data = loc_I)+theme(legend.position="none")+ 
  ggtitle("Indeed.com (n=338)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


GMap<-ggmap(map, extent = "device",legend = "bottomright") + borders(database="state") + 
  stat_density2d(data = loc_G, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 2, 
                 bins = 25, geom = "polygon") + 
  scale_alpha(range = c(0, 0.8), guide = FALSE)+ ggtitle("Glassdoor.com") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")+
  scale_fill_gradient2("Density",low = "yellow", high = "red",mid="yellow")
  

IMap<-ggmap(map, extent = "device",legend = "bottomright") + borders(database="state") + 
  stat_density2d(data = loc_I, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 2, 
                 bins = 25, geom = "polygon") + 
  scale_alpha(range = c(0, 0.8), guide = FALSE)+ ggtitle("Indeed.com") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")+
  scale_fill_gradient2("Density",low = "yellow", high = "red",mid="yellow")

plot_grid(G1,I1)

####Density
#plot_grid(GMap,IMap)

##Bar chart location

Glocation<-paste(GlassDoor[-which(is.na(GlassDoor$state)),"city"],
                 GlassDoor[-which(is.na(GlassDoor$state)),"state"],sep = ",")
barchartG<-as.data.frame(head(sort(table(Glocation),decreasing=TRUE), n = 10))
barchartG$Freq<-barchartG$Freq/nrow(GlassDoor)


Ilocation<-paste(Indeed[-which(is.na(Indeed$state)),"city"],
                 Indeed[-which(is.na(Indeed$state)),"state"],sep = ",")
barchartI<-as.data.frame(head(sort(table(Ilocation),decreasing=TRUE), n = 10))
barchartI$Freq<-barchartI$Freq/nrow(Indeed)

barG<-ggplot(barchartG,
                 aes(x =reorder(Glocation, Freq) , y = Freq)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  coord_flip() + ggtitle("Job posting in Glassdoor (n=616)") +xlab("")+ylab("Count(#) / n")+
  theme_bw()

barI<-ggplot(barchartI,
           aes(x =reorder(Ilocation, Freq) , y = Freq)) + 
           geom_bar(position = "dodge", stat = "identity") + 
           coord_flip() + ggtitle("Job posting in Indeed (n=338)") +xlab("")+ylab("Count(#) / n")+
           theme_bw()

plot_grid(GMap,barG,IMap,barI)



###By state
Gstate<-as.character(GlassDoor[-which(is.na(GlassDoor$state)),"state"])
Istate<-as.character(Indeed[-which(is.na(Indeed$state)),"state"])
totalstate<-c(Gstate,Istate)
as.data.frame(head(sort(table(totalstate),decreasing=TRUE), n = 8))

##Industry
indus<-as.data.frame(head(sort(table(GlassDoor$industry),decreasing=TRUE), n = 8))
barIndus<-ggplot(indus,
             aes(x =reorder(Var1, Freq) , y = Freq)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  coord_flip() + ggtitle("Industy with the most job posting  (n=616)") +xlab("")+ylab("Count(#)")+
  theme_bw()
colnames(indus)<-c("Industry","count")
kable(indus[1:5,])
plot_grid(barIndus,kable(indus[1:5,]))
barIndus



