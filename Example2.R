
library(tidyverse)
library(readxl)
library(ggplot2)
df <- tibble(read.csv("train.csv"))
df$Pclass <- as.factor(df$Pclass)
df_new <- df %>% group_by(Pclass) %>% count() %>%
  mutate(per_no = (n / 891) * 100) %>% select(-n)

ggplot(data = df_new, aes(x=Pclass, y= per_no, fill = Pclass, label = round(per_no, 1)))+
  geom_col()+
  geom_text()+
  labs(x="Classes", y = "Percentage", title = "Percentage of people")+
  theme(plot.title = element_text(hjust = 0.5))
  
  
  