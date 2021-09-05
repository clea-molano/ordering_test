library(dplyr)
library(lubridate)
library(openxlsx)
library(gtools)
library(tidyr)
library(feather)
library(plotly)

df <- read.csv("data/orders_lines.csv", stringsAsFactors = FALSE)


SKUs <- filter(df,Date<floor_date(today(),"months"),Date<>=floor_date(floor_date(today(),"months")-1,"months"),
               grepl("Countdown",Customer),!grepl("Vineonline",Customer))%>%
  mutate(Customer=gsub("Countdown | CD","",Customer))%>%
  mutate(Customer=gsub("Street West","St West",Customer))%>%
  mutate(Customer=gsub("Avenue","Ave",Customer))%>%
  mutate(Customer=gsub("CENTRAL - 9217","Central",Customer))%>%
  mutate(Customer=gsub("Aviemore","Aviemore Drive",Customer))%>%
  group_by(Customer,SKU)%>%
  summarise(n=sum(Quantity))%>%
  mutate(ordered="Y")
write.csv(SKUs,"output/orders_CD.csv",row.names=F)

CD <- filter(df,Date<floor_date(today(),"months"),Date<>=floor_date(floor_date(today(),"months")-1,"months"),
             grepl("Countdown",Customer))%>%
  mutate(week=floor_date(as.Date(Date),"week",week_start=1),
         ordering=ifelse(Date>="2021-03-01","after","before"))

FYCD <- filter(df,grepl("Countdown",Customer))%>%
  mutate(FY=paste("FY",ifelse(MonthNum>3,Year+1,Year)))%>%
  group_by(FY)%>%
  summarise(s=sum(Sale))


cdSlabs <- group_by(CD,week)%>%
  summarise(slabs=sum(Quantity))%>%
  spread(week,slabs)

cdOrders <- group_by(CD,Order,week)%>%
  summarise(slabs=sum(Quantity))%>%
  mutate(slabs=min(slabs,5))%>%
  group_by(week,slabs)%>%
  summarise(orders=n_distinct(Order))%>%
  spread(week,orders)

cdOrders <- group_by(CD,Order,ordering,week)%>%
  summarise(slabs=sum(Quantity))%>%
  mutate(slabs=min(slabs,5))%>%
  group_by(ordering,week,slabs)%>%
  summarise(orders=n_distinct(Order))%>%
  group_by(ordering,slabs)%>%
  summarise(orders=mean(orders))%>%
  spread(ordering,orders)

cdSlabs <- group_by(CD,Customer,ordering,week)%>%
  summarise(slabs=sum(Quantity))%>%
  group_by(ordering,week)%>%
  summarise(slabs=mean(slabs))%>%
  group_by(ordering)%>%
  summarise(slabs=mean(slabs))



cdOrders <- group_by(CD,Customer,ordering,week)%>%
  summarise(n=n_distinct(Order))%>%
  mutate(m=max(n))%>%
  group_by(Customer,ordering)%>%
  summarise(n=mean(n),m=max(m))%>%
  mutate(m=max(m))%>%
  spread(ordering,n)%>%
  arrange(desc(m))

cdOrdersSKU <-  group_by(CD,Customer,SKU,ordering,week)%>%
  summarise(n=n_distinct(Order))%>%
  mutate(m=max(n))%>%
  group_by(Customer,SKU,ordering)%>%
  summarise(n=mean(n),m=max(m))%>%
  mutate(m=max(m))%>%
  spread(ordering,n)%>%
  arrange(desc(m))

cdOrdersALL<-full_join(cdOrders,cdOrdersSKU,by="Customer")
write.csv(cdOrdersALL,"output/CD.csv",row.names=F)

cdOrders2 <- group_by(CD,)%>%
  summarise(n=n_distinct(Order))%>%
  group_by(week)%>%
  summarise(n=mean(n))

spendWeekly <- group_by(CD,week,Order)%>%
  summarise(s=sum(Sale))%>%
  group_by(week)%>%
  summarise(m=mean(s))%>%
  spread(weeks,m)

spendWeekly <- group_by(CD,week,Customer)%>%
  summarise(s=sum(Sale))%>%
  group_by(week)%>%
  summarise(t=sum(s),m=mean(s))
plot_ly(spendWeekly,x=~week,y=~m,mode='lines')  



ordersCD <-  group_by(CD2,Customer,week)%>%
  summarise(orders=n_distinct(Date))%>%
  group_by(Customer)%>%
  summarise(totalOrders=sum(orders),maxOrders=max(orders))%>%
  left_join(filter(summarise(group_by(summarise(group_by(CD2,Customer,SKU,week),orders=n_distinct(Date),q=sum(Quantity),a=mean(Quantity)),Customer,SKU),
                             totalOrdersSKU=sum(orders),maxOrdersSKU=max(orders),slabs=sum(q),a=mean(a),suggested=ceiling(slabs/4)),
                   totalOrdersSKU>4|maxOrdersSKU>1))%>%
  filter(totalOrders>4|maxOrders>1|totalOrdersSKU>4|maxOrdersSKU>1)%>%
  arrange(desc(totalOrders),Customer,desc(totalOrdersSKU))

write.csv(ordersCD,"output/orders_CD.csv",row.names=F)

