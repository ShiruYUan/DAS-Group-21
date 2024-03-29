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
library(sjPlot)
library(stats)
library(jtools)
library(janitor)


group21_new <- read.csv("C:/Users/DELL/Desktop/dataset21.csv")



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
  select(category,sellable_online,other_colors,depth,height,width,price,newprice)

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
  adorn_ns() # To show original counts,有点问题
ggplot(data = group21_new, aes(x = newprice, group = category)) +
  geom_bar(aes(y = after_stat(prop), fill = category), stat = "count", position = "dodge") +
  labs(x = "More than 1000?", y = "Proportion")

str(group21_new)
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

step_model_1 <- step(model_1,direction = "both")


model_2 <- glm(newprice ~ category+height+width, data = group21_new,
               family = binomial(link = "logit"))
model_2 %>%
  summary()
plot_model(model_2, show.values = TRUE, transform = NULL,
           title = "Log-Odds_attach", show.p = FALSE)


data_1 <- filter(group21_new, category!="Caf\xe9 furniture")
data_2 <- filter(data_1, category!="Chairs")
data_3 <- filter(data_2, category!="Chests of drawers & drawer units")
data_4 <- filter(data_3, category!="Children's furniture")
data_5 <- filter(data_4, category!="Nursery furniture")
data_6 <- filter(data_5, category!="Outdoor furniture")
data_7 <- filter(data_6, category!="Room dividers")
data_8 <- filter(data_7, category!="Sideboards, buffets & console tables")
data_9 <- filter(data_8, category!="Sofas & armchairs")
data_10 <- filter(data_9, category!="Tables & desks")
data_11 <- filter(data_10, category!="Trolleys")

model_3 <- glm(newprice ~ category+height+width, data = data_11,
               family = binomial(link = "logit"))
model_3 %>%
  summary()

data_12 <- filter(data_11, category!="Beds")
model_4 <- glm(newprice ~ category+height+width, data = data_12,
               family = binomial(link = "logit"))
model_4 %>%
  summary()
plot_model(model_4, show.values = TRUE, transform = NULL,
           title = "Log-Odds_attach", show.p = FALSE)

plot_model(model_4, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.25)

1
