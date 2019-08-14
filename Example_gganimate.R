#We need a ImageMagick software
devtools::install_github("dgrtwo/gganimate")
library(gapminder)
#library(gganimate)
library(ggplot2)
theme_set(theme_bw())
data(gapminder)
gc<-ggplot(gapminder,aes(gdpPercap,lifeExp,size=pop,color=continent,frame=year))+
  geom_point()+scale_x_log10()

#gc
library(gganimate)
gganimate(gc)

#save 
gganimate(gc,"gc.gif")

#more interesting setting
gc2<-ggplot(gapminder,aes(gdpPercap,lifeExp,size=pop))+
  geom_point()+geom_point(aes(frame=year),color="red")+
  scale_x_log10()
gganimate(gc2)