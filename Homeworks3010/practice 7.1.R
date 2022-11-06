library (psych)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(tidyr)
library(y)

rairuoho<-read.delim('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt')
rairuoho
rairuoho<-rairuoho%>%pivot_longer(day3:day8,names_to = "day",values_to = "length")
head(rairuoho)

rairuoho<- dplyr::select(rairuoho, -spatial1, -spatial2, -bed, -row, -column, -germinate)
head(rairuoho)

rairuoho_1 <- rairuoho %>%  unite("day treatment", day:treatment, sep = "_")
head(rairuoho_1)

?t.test.default

t.test(length~treatment, data = rairuoho,
       alternative = "greater",
       mu=0)
