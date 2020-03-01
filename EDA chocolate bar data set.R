library(ggplot2)
library(dplyr)
library(tidyverse)
chocolate_dataset<-read.csv("flavors_of_cacao.csv")
head(chocolate_dataset)
chocolate_dataset<-chocolate_dataset[,-3]
head(chocolate_dataset)
sum(is.na(chocolate_dataset))
tail(chocolate_dataset,4)
chocolate_dataset$Cocoa.Percent<-as.numeric(chocolate_dataset$Cocoa.Percent)
head(chocolate_dataset)
dim(chocolate_dataset)
glimpse(chocolate_dataset)
names(chocolate_dataset)<-c("companymaker","localbeanorigin","Year","cocoaPercent","ManufacturerClocation","Rating","beantype","countrybeanOrigin")
head(chocolate_dataset)         
ggplot(chocolate_dataset,aes(x = ManufacturerClocation,y = countrybeanOrigin,col = Year))+
  geom_tile(alpha = 0.6,span = 0.7)

chocolate_dataset %>% group_by(companymaker) %>% summarise(Count= n())%>%
  top_n(10, wt = Count)%>%arrange(desc(Count))
origin<-chocolate_dataset%>%group_by(companymaker)%>%summarise(count = n())%>%
  top_n(10,wt = count)%>%arrange(desc(count))
print(origin)
ggplot(data = origin)+
  geom_col(mapping = aes(x = reorder(companymaker,-count), y = count,fill = count))+
  labs(title = "first ten countries where the beans originate")+
  scale_fill_continuous(trans = 'reverse')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust=1))


origin<-chocolate_dataset%>%group_by(countrybeanOrigin,ManufacturerClocation)%>%summarise(count())%>%filter(n>5)

ggplot(data = origin)+
  geom_tile(mapping = aes(y = countrybeanOrigin, x = ManufacturerClocation))+
  labs(title = "first ten countries where the beans originate")+
  scale_fill_distiller(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust=1))
chocolate_dataset %>% group_by(Year) %>% 
  ggplot(aes(x=as.factor(Year),y=Rating))+
           geom_boxplot()+theme(legend.position = "none")+
           labs(x="year",title="Rating change stats over years")
chocolate_dataset %>%
  group_by(Year) %>%
  summarise(averagerating=mean(Rating),sdrating=sd(Rating))%>%
  ggplot(aes(x=Year,y=averagerating,color=averagerating, group =1))+
  geom_line()+xlab("Year") +ylab("Rating_Average")+labs(title="Average chocolate bar rating over the years")

chocolate_dataset %>% group_by(Year) %>% summarise(Count= n())%>%
  ggplot(aes(x =as.factor(Year), y = Count, fill = Count)) + geom_bar(stat = "identity") + 
  scale_fill_continuous(trans = 'reverse')+
  labs(title="Rating count distribution over the years", subtitle = "Chocolate bars rated")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1)) #Rating Year
ggplot(chocolate_dataset,aes(cocoaPercent,Rating)) + 
  geom_point(alpha = 0.3,span = 0.9)+
  geom_smooth(method = "lm",se = F)+
  xlim(10,40)+
  labs(title=" Cocoa Percentage vs Rating", x="Cocoa Percentage (%)", y="Rating (/5)")#Cocoa percentage versus Ratings
  
