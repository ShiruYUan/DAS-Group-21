group21_new$category<-as.factor(group21_new$category)
group21_new$sellable_online<-as.factor(group21_new$sellable_online)
group21_new$newprice<-as.factor(group21_new$newprice)
group21_new$other_colors<-as.factor(group21_new$other_colors)

#summary statistic
summary(group21_new[,2:7])

#histogram
library(ggplot2)
library(gridExtra)
h1<-ggplot()+ 
  geom_bar(data =group21_new,
           aes(x = factor(category),fill = factor(newprice)),
           position = "fill")
h2<-ggplot()+ 
  geom_bar(data=group21_new,
           aes(x=factor(sellable_online),fill = factor(newprice)),
           position="fill")
h3<-ggplot()+ 
  geom_bar(data = group21_new,
           aes(x = factor(other_colors),fill = factor(newprice)),
           position = "fill")
grid.arrange(h1,h2,h3,nrow=3)

#box plot
p1<-ggplot(group21_new, aes(x=width, y=newprice)) + 
  geom_boxplot()
p2<-ggplot(group21_new, aes(x=height, y=newprice)) + 
  geom_boxplot()
p3<-ggplot(group21_new, aes(x=depth, y=newprice)) + 
  geom_boxplot()
grid.arrange(p1,p2,p3,nrow=2)

#correlation plot
library(GGally)
pairs(group21_new)
pairs(~width+height+depth, data=group21_new)

11111