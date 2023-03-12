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


library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(kableExtra)
library(gridExtra)
library(skimr)
library(knitr)
library(GGally)
library(tidyverse)
install.packages("sjPlot")
library(sjPlot)
library(stats)
library(jtools)
library(janitor)




#Calculates the column median, replacing the missing value with the median
median_depth<- median(group21_new$depth,na.rm = TRUE)
median_depth
group21_new <- group21_new %>%
  mutate(depth = ifelse(is.na(depth), median_depth, depth))

median_height <- median(group21_new$height,na.rm = TRUE)
group21_new <- group21_new %>%
  mutate(height = ifelse(is.na(height), median_height, height))

median_width <- median(group21_new$width,na.rm = TRUE)
median_width
group21_new <- group21_new %>%
  mutate(width = ifelse(is.na(width), median_width, width))

# final dataset
W <- rep(0,500)
group21_new$newprice <- W
group21_new$newprice[group21_new$price>1000]=1
group21_new <- group21_new %>%
  dplyr:::select.data.frame(category,sellable_online,other_colors,depth,height,width,newprice)
 

group21_new
dim(group21_new)
View(group21_new)
sum(is.na(group21_new))

####analysis
group21_new$newprice <- as.factor(group21_new$newprice)
levels(group21_new$newprice) <- c("more than", "no more than")
group21_new$sellable_online <- as.factor(group21_new$sellable_online)
group21_new$category <- as.factor(group21_new$category)
group21_new$other_colors <- as.factor(group21_new$other_colors)

#boxplot
ggplot(data = group21_new, aes(x = newprice, y = depth , fill = newprice))+
  geom_boxplot() +
  labs(x = "More than 1000?", y = "Depth")+ 
  theme(legend.position = "none")

ggplot(data = group21_new, aes(x = newprice, y = height , fill = newprice))+
  geom_boxplot() +
  labs(x = "More than 1000?", y = "Height")+ 
  theme(legend.position = "none")

ggplot(data = group21_new, aes(x = newprice, y = width , fill = newprice))+
  geom_boxplot() +
  labs(x = "More than 1000?", y = "Width")+ 
  theme(legend.position = "none")

#barplot
group21_new %>%
  tabyl(category, newprice) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() 
ggplot(data = group21_new, aes(x = newprice, group = category) +
  geom_bar(aes(y = after_stat(prop), fill = category), stat = "count", position = "dodge") +
  labs(x = "More than 1000?", y = "Proportion"))

group21_new %>%
  tabyl(sellable_online, newprice) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts
ggplot(data = group21_new, aes(x = newprice, group = sellable_online)) +
  geom_bar(aes(y = after_stat(prop), fill = sellable_online), stat = "count", position = "dodge") +
  labs(x = "More than 1000?", y = "Proportion")

group21_new %>%
  tabyl(other_colors, newprice) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts
ggplot(data = group21_new, aes(x = newprice, group = other_colors)) +
  geom_bar(aes(y = after_stat(prop), fill = other_colors), stat = "count", position = "dodge") +
  labs(x = "More than 1000?", y = "Proportion")

#model
model_1 <- glm(newprice ~ category+sellable_online+other_colors+depth+height+width, data = group21_new,
               family = binomial(link = "logit"))
model_1 %>%
  summary()


#odds
model_2 <- glm(newprice ~ depth+height+width, data = group21_new,
               family = binomial(link = "logit"))
model_2 %>%
  summary()

plot_model(model_2, show.values = TRUE, transform = NULL,
           title = "Log-Odds", show.p = FALSE)

model_3 <- glm(newprice ~ sellable_online+other_colors, data = group21_new,
               family = binomial(link = "logit"))
model_3 %>%
  summary()
plot_model(model_3, show.values = TRUE, transform = NULL,
           title = "Log-Odds_attach", show.p = FALSE)

model_4 <- glm(newprice ~ sellable_online+other_colors+depth+height+width, data = group21_new,
               family = binomial(link = "logit"))
model_4 %>%
  summary()
plot_model(model_4, show.values = TRUE, transform = NULL,
           title = "Log-Odds_attach", show.p = FALSE)
