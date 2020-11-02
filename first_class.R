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
#-------------------------------------------------------------------------------
#if
df$Sex[which(is.na(df$Sex))] = 0

y <- df$Sex
df <- df %>% mutate(cat_sex = ifelse(y == 'male', 0, 1))

df$Age[which(is.na(df$Age))] = 0
x <- df$Age
df <- df %>% mutate(cat_age = ifelse(x<10,0,ifelse(x<20,1,ifelse(x<30,2,ifelse(x>+30,3,100)))))
#-------------------------------------------------------------------------------

seq_along(df$Survived)

#loops
x <- c(1,2,3,4)

for (i in seq_along(x)){
  for(j in seq_along(x)){
    print(i * j)
  }
}
#-------------------------------------------------------------------------------

#function in R
sq_x <- function(x,y){
  return (x * y)
}

sq_x(2,4)

#-------------------------------------------------------------------------------
#apply function (like map in python)

df_new <- df %>% select(Age,SibSp, Parch)
apply(df_new, 2, mean, na.rm = TRUE)

#mean(df_new$Age, na.rm = TRUE)
