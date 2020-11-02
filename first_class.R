library(tidyverse)
library(readxl)
library(ggplot2)

df <- tibble(read.csv("train.csv"))
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df %>% group_by (Pclass, Sex, Survived) %>% count() %>%
  ggplot(aes(x=Pclass, y=n, fill=Sex, mapping))+
  geom_col()+
  facet_wrap(~Survived)

df$Sex[which(is.na(df$Sex))] = 0

y <- df$Sex
df <- df %>% mutate(cat_sex = ifelse(y == 'male', 0, 1))

df$Age[which(is.na(df$Age))] = 0
x <- df$Age
df <- df %>% mutate(cat_age = ifelse(x<10,0,ifelse(x<20,1,ifelse(x<30,2,ifelse(x>+30,3,100)))))


