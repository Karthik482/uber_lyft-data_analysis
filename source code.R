#-----------------------------------------------------------------------------------------------------------------------------------------
# Group Number:       2
# Group Members: Athul Sony , Karthik Cheuvuru
# Section:    04
# Course ID   IE6600 18587
#-----------------------------------------------------------------------------------------------------------------------------------------



library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(ggalt)


setwd("C:/Users/athul/OneDrive - Northeastern University/Desktop/Computation and Visualisation/Project")

data <- read.csv('Rideshare.csv',stringsAsFactors = F,na.strings = c('',' ','-','NA'))

data <- data %>% 
  mutate(Day = wday(mdy(str_sub(datetime,1,10)),label=T))

data<- data[,c(1:6,58,7:57)]


# distribution of prices per cab type

data %>% 
  filter(name != 'Taxi') %>% 
  select(cab_type,name,price) %>% 
  mutate(Type = ifelse(cab_type == 'Uber',1,0)) %>% 
  ggplot(aes(x = reorder(name,Type),y = price,fill=cab_type)) + geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom = 'errorbar',width = 0.2)+
  stat_summary(fun.y=mean, geom="point", size=1)+
  xlab('Cab Type') +
  ylab('Fare($)') +
  labs(fill='Carrier') +
  scale_y_continuous('Price',breaks = seq(0,100,10),limits = c(0,50))+
  scale_fill_manual(values=c("#FF3399","#009999")) +
  ggtitle('Distribution of Prices per cab category')+
  theme(plot.title = element_text(hjust=0.5,size =16),
        axis.text.x = element_text(size = 8))



#  Performance on Sunday 



data %>% 
  filter(Day == 'Sun') %>% 
  group_by(cab_type,hour,day,month) %>% 
  summarise(Count = n()) %>% 
  group_by(cab_type,hour) %>% 
  summarise(Average = round(mean(Count),2)) %>% 
  ggplot(aes(x = hour,y = Average,group = cab_type,color = cab_type))+ geom_line() +
  scale_x_continuous(name = 'Hour of the day',breaks = seq(0,23,1))+
  scale_y_continuous(name = 'Hour of the day',limits = c(700,1200),breaks = seq(700,1200,50))+
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values=c("#FF3399","#009999"))+
  labs(color='Carrier') +
  ggtitle('Average number of rides by time of day on Sunday')+
  theme(plot.title = element_text(hjust=0.5,size =16))


# Number of rides per weather category


data %>% 
  filter(name != 'Taxi') %>% 
  group_by(short_summary,cab_type) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = reorder(short_summary,Count),y=Count,fill=cab_type)) + geom_bar(stat = 'identity')+ 
  geom_text(aes(label = Count),size = 3,vjust = 1,position = position_stack()) +
  scale_fill_manual(values=c("#FF3399","#009999"))+
  ylab('Number of rides') +
  xlab('Weather') +
  labs(fill = 'Carrier') +
  ggtitle('Dependency of ride bookings on weather') +
  theme(plot.title = element_text(hjust=0.5,size =16))

# Chances of surge multiplier

data %>% 
  filter(name!='Taxi') %>% 
  group_by(surge_multiplier) %>% 
  summarise(Count = n()) %>% 
  mutate(Percent = paste(as.character(round(Count/637976 * 100,4)),'%')) %>% 
  ggplot(aes(x = paste(as.character(surge_multiplier),'x'),y = Count)) + geom_bar(stat = 'identity',fill = 'steelblue') +
  scale_y_continuous('Number of times',trans='log10',
                     breaks=trans_breaks('log10',function(x)10^x),labels=trans_format('log10',math_format(10^.x))) +
  geom_text(aes(label = Percent),vjust = 1) +
  xlab('Surge Multiplier') +
  ggtitle('Chances of Surge Multiplication to the fare') +
  theme(plot.title = element_text(hjust=0.5,size =16))


x <- data %>% 
  filter(name != 'Taxi') %>% 
  group_by(surge_multiplier,short_summary) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = as.character(surge_multiplier),y = Count,fill= short_summary)) + geom_bar(stat = 'identity',position = 'dodge') +
  scale_y_continuous('Number of times',trans='log10',
                     breaks=trans_breaks('log10',function(x)10^x),labels=trans_format('log10',math_format(10^.x))) +
  scale_fill_brewer(palette = 'Paired')


# surge 3

surge3 <- data %>% 
  filter(name != 'Taxi') %>% 
  group_by(surge_multiplier,short_summary) %>% 
  summarise(Count = n()) %>% 
  filter(surge_multiplier=='3') %>% 
  mutate(Proportion = round(Count/sum(Count) * 100,digits = 1)) %>% 
  arrange(desc(Proportion)) %>% 
  ggplot(aes(x = '', y = Proportion, fill=short_summary)) + 
  geom_bar(width = 1, stat='identity',color=NA) +
  coord_polar('y',start=0) +
  geom_text(aes(label = Proportion),position = position_stack(vjust=0.5),color = 'black',size = 4) +
  theme_void() +
  scale_fill_brewer(palette = 'Set2')


count_of_rides<-ride%>%group_by(cab_type)%>%count
count_of_rides
pct<-round(MS$n/sum(MS$n)*100)
label<-paste(MS$cab_type,pct)
label<-paste(label,"%",sep="")
pie(MS$n,labels = label,col=c("orange","red"),border="brown",clockwise=TRUE,main="Market Share of Uber & Lyft")


#2nd
#Most sought after cab type
p1<-ride %>%
  select(name,cab_type,month) %>%
  filter(cab_type=="Uber",month==12) %>%
  group_by(name) %>%
  count%>%
  ggplot(aes(x=reorder(name,-n),y=n,fill=name))+geom_bar(stat = "identity")+
  labs(y="Number",x="",title="Most sought after cab in uber ") +
  theme(plot.title = element_text(family="Times New Roman", face="bold.italic",color="red",size=16), 
        axis.title.x = element_text(family="Arial",size=16,color="red"),
        axis.title.y = element_text(family="Arial",size=16,color="red"))
p2<-ride %>%
  select(name,cab_type,month) %>%
  filter(cab_type=="Lyft",month==11) %>%
  group_by(name) %>%
  count%>%
  ggplot(aes(x=reorder(name,-n),y=n,fill=name))+geom_bar(stat = "identity")+
  labs(y="Number",x="",title="Most sought after cab in Lyft ") +
  theme(plot.title = element_text(family="Times New Roman", face="bold.italic",color="red",size=16), 
        axis.title.x = element_text(family="Arial",size=16,color="red"),
        axis.title.y = element_text(family="Arial",size=16,color="red"))
grid.arrange(p1,p2,ncol=2,nrow=1)


#3rd
#Most sought after ride network by neu students

p1<-ride %>%
  select(source,destination,cab_type,month) %>%
  filter(source=="Northeastern University"&cab_type=="Uber",month==12) %>%
  group_by(destination) %>%
  summarise(n=n()) %>% 
  top_n(3)%>%
  ggplot(aes(x=reorder(destination,-n),y=n,fill=destination))+
  geom_bar(stat = "identity")+
  labs(y="Number",x="",title="Top 3 destinations from NEU (Uber) ") +
  theme(plot.title = element_text(family="Times New Roman", face="bold.italic",color="red",size=16), 
        axis.title.x = element_text(family="Arial",face="bold.italic",size=16,color="red"),
        axis.title.y = element_text(family="Arial",face="bold.italic",size=16,color="red"))+
  theme(legend.text = element_text(family="Times New Roman", size=13, face="bold"))+
  theme(legend.title = element_text(family="Times New Roman", size=13, face="bold"))+
  scale_fill_discrete(name = "Destination")
p2<-ride %>%
  select(source,destination,cab_type,month) %>%
  filter(source=="Northeastern University"&cab_type=="Lyft",month==11) %>%
  group_by(destination) %>%
  summarise(n=n()) %>% 
  top_n(3)%>%
  ggplot(aes(x=reorder(destination,-n),y=n,fill=destination))+
  geom_bar(stat = "identity")+
  labs(y="Number",x="",title="Top 3 destinations from NEU (Lyft) ") +
  theme(plot.title = element_text(family="Times New Roman", face="bold.italic",color="red",size=16), 
        axis.title.x = element_text(family="Arial",face="bold.italic",size=16,color="red"),
        axis.title.y = element_text(family="Arial",face="bold.italic",size=16,color="red"))+
  theme(legend.text = element_text(family="Times New Roman", size=13, face="bold"))+
  theme(legend.title= element_text(family="Times New Roman", size=13, face="bold"))+
  scale_fill_discrete(name = "Destination")
grid.arrange(p1,p2,nrow=1,ncol=2)

#At which time period maxium bookings happened

ride %>%
  select(hour,cab_type,name) %>%
  group_by(hour) %>%
  count %>%
  ggplot(aes(x=hour,y=n)) +
  geom_line(size=1) + 
  geom_point(size=2,color="orange")  +
  ggtitle("Bookings across different times") + 
  scale_x_discrete(limits=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"
  ))


#Distributions of distance for cab types

new<-subset(ride,name=="Black SUV"|name=="UberPool"|name=="UberX"|name=="UberXL")
pri<-new%>%filter(distance<=7&price<=50)
new1<-subset(ride,name=="Lyft XL"|name=="Shared"|name=="Lux Black XL"|name=="Lux")
re<-new1%>%filter(price<=60)

p1<-pri%>%
  select(distance,name,cab_type,price,month)%>%
  filter(cab_type=="Uber")%>%
  group_by(name)%>%
  ggplot(aes(x=distance, y=price, color=name)) +
  geom_point() +
  labs(title="Distributions of distance for cab types",x="Distance in Miles",y="Price in USD")


p2<-re %>%
  select(distance,name,cab_type,price,month)%>%
  filter(cab_type=="Lyft")%>%
  group_by(name)%>%
  ggplot(aes(x=distance, y=price, color=name)) +
  geom_point() +
  labs(title="Distributions of distance for cab types",x="Distance in Miles",y="Price in USD")

grid.arrange(p1,p2,ncol=2,nrow=1)



