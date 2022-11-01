
library (psych)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(dplyr)


# students data set url 
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') 
students # to write it:
summary(students) #we can see the distribution of my data max and min

 
write.table(students, file = "https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt", sep = "\t", row.names=T)

library(psych) #funtion discribe beter summarry
psych::describe(students) #si se corta trimmmed se puede teren la media, describe more informatcion de als variables, podemos encontrar mas funciones para hacer un sumario

psych::describeBy (students,students$gender) #a cuando hay asterisco se debe de tenre cudiado porque puede haber un errror en la variable, pero solo lo da la describeBy, qui se extrajo el genero para ver mujeres y hombres , student$gender es un factor

psych::describeBy (students, list(students$gender,students$population)) # se oyeden separar la estadistica descriptiva para cada population

prop.table (table(students$gender)) #proporcion table funtion

# two variables
table(students$gender, students$shoesize)

prop.table (table(students$gender, students$shoesize)) #proporcion de la tabla

# three variables, with nicer formatting
ftable(students$gender, students$shoesize,students$population)

mean(students$height) #extraccion d emedia

ind.male <- students$gender == 'male'
mean(students$height[ind.male]) #only male information
ind.male <- students$gender == 'female'
mean(students$height[ind.female]) #check this one

aggregate(students$height,list (students$gender),median) # here we need to use the list, female en un lado y hombres de otro
#42 get the data frame
tapply(students$height,students$gender, median) #get the list

#funtion apply evita crear loops loops and loops, leer mas sobre eso.

library(iris)
data("iris")
head(iris)
summary(iris)
psych::describeBy(iris,iris$species)

psych::describeBy(iris,list(iris$species,iris$sepal.len))
rm(iris,plot1,plot2,plot3,plot4)
iris

plot1 <-ggplot(iris, aes(x=species, y=sepal.len)) + 
  geom_boxplot()

plot2 <-ggplot(iris, aes(x=species, y=sepal.wid)) + 
  geom_boxplot() 

plot3 <-ggplot(iris, aes(x=species, y=petal.len)) + 
  geom_boxplot() 

plot4 <-ggplot(iris, aes(x=species, y=petal.wid)) + 
  geom_boxplot() 

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
describeBy (iris, iris$species)
library(dplyr)

iris %>% group_by(Species) %>% summarise(across(c(1:4), length))  #follow thos when have a funtions
aggregate(iris[,1:4],by=list(iris$Species), median)
tapply(iris$Sepal.Length , iris$Species, mean)


# dataset hypotheses?
x<-students$height
y<-students$shoesize
s<-students[,1:2] # a matrix
# Pearson correlation
# cor(x,y)
# cor(s)
cor.test(x,y)

ggplot(students, aes(x = height, y = shoesize)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

cor.test(x,y, method ='spearman')
w<-(1:100)
z<-exp(x)
cor.test(w,z,method='pearson') # super low
cor.test(w,z,method='spearman') #super high

#Cast 240 times a die. We counted occurrence of 1,2,3,4,5,6
die<-data.frame(obs=c(55,44,35,45,31,30), row.names=c('Cast1','Cast2','Cast3','Cast4','Cast5','Cast6'))
die #Is this die fair? Define H0 and H1.  

chisq.test(die)

F <- matrix(nrow=4,ncol=2,data=c(33,14, 8,18,31,25,14,12))
chisq.test(F) #

obs <- c(750, 50, 200)
exp <- c(0.60, 0.35, 0.05)
chisq.test (x=obs, p=exp)

t.test (students$height, mu=170)

#paired data will compare two dependent, is goog when you have some dependents

# Two sample (with equal variances)
t.test (students$height~students$gender, var.equal = TRUE)
#elasticity esto e pra no parametrico para no parametrical statistics
students$height[6]<-132
students$height[10]<-310
students$height[8]<-132
students$height[9]<-210
boxplot(height~gender, students)

qqnorm(students$height) 
qqline(students$height) 

shapiro.test(students$height)

wilcox.test (students$height~students$gender)
# ~significa versus
fligner.test (students$height ~ students$gender)

tg<-ToothGrowth
tg$dose<-factor(tg$dose)
boxplot(len~dose*supp, data=tg)


#estos son test

bartlett.test(len~interaction (supp,dose),data=ToothGrowth) #compare lents vr    interaction , and the data

leveneTest(len~interaction (supp,dose),data=ToothGrowth)

fligner.test(len~interaction (supp,dose),data=ToothGrowth)
