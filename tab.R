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
glimpse(chocolate_dataset)
names(chocolate_dataset)<-c("companymaker","localbeanorigin","Year","cocoaPercent","ManufacturerClocation","Rating","beantype","countrybeanOrigin")
head(chocolate_dataset)         
ggplot(chocolate_dataset,aes(x = ManufacturerClocation,y = countrybeanOrigin))+
  geom_tile()

chocolate_dataset %>% group_by(companymaker) %>% summarise(Count= n())%>%
  top_n(10, wt = Count)%>%arrange(desc(Count))%>%
  ggplot(chocolate_dataset,aes(companymaker,-count))+
  geom_col()+
  theme_bw()+
  scale_x_reverse()




