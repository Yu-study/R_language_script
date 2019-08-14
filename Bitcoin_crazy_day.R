library(jsonlite)
library(tidyverse)
library(lubridate)
library(reshape2)
library(grid)
library(zoo)
library(scales)

## == The data is stored in JSON  == ##
## == We can only download 1000 trades each time == ##
## == The while is to download all trades in the day == ##
## == Dates are in UNIX time == ##
## == Data from the last 24h (now it is 23:17 - 2017-11-29) == ##
start = 1512004273 - 86400
end = 1512004273
datalist = list()
while(start < end){
  file = paste("https://www.mercadobitcoin.net/api/BTC/trades/",start,"/",sep="")
  data = fromJSON(file, flatten = TRUE)
  start = max(data$date) + 1
  datalist[[length(datalist) + 1]] = data
}
df_present = Reduce("rbind", datalist)
df_present = df_present %>% filter(date<=end)

## == Data from the same time 2 days before == ##
start = 1512004273 - 86400 * 3
end = 1512004273 - 86400 * 2
datalist = list()
while(start < end){
  file = paste("https://www.mercadobitcoin.net/api/BTC/trades/",start,"/",sep="")
  data = fromJSON(file, flatten = TRUE)
  start = max(data$date) + 1
  datalist[[length(datalist) + 1]] = data
}
df_past = Reduce("rbind", datalist)
df_past = df_past %>% filter(date <= end)

## = adjust date = ##
df_past$date = as.POSIXct(df_past$date, origin = "1970-01-01")
df_present$date = as.POSIXct(df_present$date, origin = "1970-01-01")

# = statistics = #
past = c(nrow(df_past), sum(df_past$amount, na.rm = TRUE), mean(df_past$amount, na.rm = TRUE), mean(df_past$price), range(df_past$price))
present = c(nrow(df_present), sum(df_present$amount, na.rm = TRUE), mean(df_present$amount, na.rm = TRUE), mean(df_present$price), range(df_present$price))
stat = round(data.frame("Nov.29" = present,"Nov.27" = past), 3)
stat = data.frame(stat = c("Transactions","Amount","Avg.Amount","Avg.Price","Min.Price","Max.Price"),
                  stat)

## =  make data by minute = ##
df_present_min = df_present %>%
  mutate(day = 1 + day(date) - min(day(date)), hour = hour(date), min = minute(date)) %>%
  mutate(date = make_datetime(day = day, hour = hour,min = min,tz = "BRST")) %>%
  group_by(date) %>% summarise(price = tail(price, 1) ,vol = sum(amount))

df_past_min=df_past %>%
  mutate(day = 1 + day(date) - min(day(date)), hour = hour(date) ,min = minute(date)) %>%
  mutate(date = make_datetime(day = day, hour = hour,min = min, tz="BRST")) %>%
  group_by(date)%>% summarise(price = tail(price, 1), vol = sum(amount))

df_min = full_join(df_present_min, df_past_min,by=c("date")) %>% arrange(date)
df_min$price.x = na.locf(df_min$price.x, na.rm = FALSE)
df_min$price.y = na.locf(df_min$price.y, na.rm = FALSE)

## = Plots = ##
df1 = melt(df_min[, c(1, 2, 4)], id.vars = "date")
df2 = melt(df_min[, c(1, 3, 5)], id.vars = "date")

p0 = ggplot() +
  geom_boxplot(data = df1, aes(variable, value)) + labs(x="Day", y="Price") +
  scale_x_discrete(labels = c("Nov. 29", "Nov. 27"))

p1 = ggplot() +
  geom_line(data = df1, aes(x = date, y = value, color = factor(variable, labels = c("Nov. 29", "Nov. 27")))) +
  labs(x = "Time",y = "Price") + guides(color = guide_legend(title = "Day")) +
  scale_x_datetime(labels = date_format("%H:%M"))

p2 = ggplot() +
  geom_line(data = df2,aes(x = date,y = value,color = factor(variable, labels=c("Nov. 29", "Nov. 27")))) +
  labs(x = "Time", y = "Volume") + guides(color = guide_legend(title = "Day")) +
  scale_x_datetime(labels = date_format("%H:%M"))

p0
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "first"))