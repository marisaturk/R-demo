#Set working directory (in your file explorer, locate the folder your files are in and copy path)
setwd("C:\\Users\\maris\\Biometry")

#Loading data
moth.data <- read.csv("spruce_moth_traps.csv")
moth.data

#Checking the structure of our data
str(moth.data)

#Extracting columns from your dataset
moth.data[,1]
moth.data$Scent

#Extracting rows from your dataset
moth.data[1,]

#Extracting entries of certain values from your dataset
moth.data[moth.data$Scent=="29",]
moth.data[moth.data$Bait=="Pheromone",]
subset(moth.data$Bait,moth.data$Scent=="29")
barplot(subset)

barplot(height=table(subset(moth.data$Bait,moth.data$Scent=="29")))





#Creating vectors from scratch
Treatment <- c(90,91,68,90,167,26,38,89,88,99,41,123,76,88,79)
Control <- c(52,104,146,27,46,120,5,15,11,48,30,40,8,42,74)
Pooled <- c(Treatment,Control)




### PERMUTATION TEST ###
#Permutation tests can be used to test hypotheses about the relationship between two variables. 
#This is done by repeatedly permuting (randomly reassigning) values from your dataset.
#From each permuted dataset, you can calculate a test statistic.
#Running many permutations produces a distribution of test statistics under the null hypothesis.
#We can then compare the test statistic from our actual observed dataset to this null distribution.
#If our observed test statistic is sufficiently extreme (p < 0.05) we can reject the null.

#create an empty vector to fill within your loop
T.distrib <- c()

#For loops are a way to run many simulated tests
for (i in 1:1000) {
  null.pooled <- sample(x=Pooled, size=30, replace=FALSE)   #Sampling from pooled data to permute labels
  null.treat <- null.pooled[1:15]   #First 15 assigned treatment label
  null.cont <- null.pooled[16:30]   #Next 15 assigned control label
  T.stat <- mean(null.treat) - mean(null.cont)   #Our estimation of the test statistic under null
  T.distrib[i] <- T.stat   #Resulting distribution of test statistics under null
}

hist(T.distrib)

T.star <- mean(Treatment) - mean(Control)   #Actual test statistic from our observed dataset

(sum(as.numeric(T.distrib >= T.star)) + 
    sum(as.numeric(T.distrib <= -abs(T.star)))) / 1000   #Calc p-value



### T-test ###
var.1 <- var(Treatment)
var.2 <- var(Control)
Fs <- var.2/var.1
(1-pf(Fs,df1=(length(Treatment)-1),df2=(length(Control)-1)))*2
#F-test is not significant (p=0.435), so we proceed with t-test assuming equal variances

t.test(Treatment, Control, var.equal=TRUE)



### ANOVA ###
model <- lm(moth.data$Scent~moth.data$Location*moth.data$Bait)
summary(model)
anova(model)



### Correlation ###
A <- rnorm(n = 100, mean = 5, sd = 2)
B <- A - rnorm(n = 100, mean = 0, sd = 1)
cor.test(A,B,method="pearson") 
#Pearson's r is the default (assumes data are normally distributed)

C <- c(2, 3, 5, 4)
D <- c(5, 6, 2, 3)
cor.test(C,D,method="spearman")
#Spearman's rank correlation does not require normality but assumes C and D have a monotonic relationship



### Data Visualization ###
hist(T.distrib)
barplot(table(subset(moth.data$Bait,moth.data$Scent=="29")))
boxplot(moth.data$Scent~moth.data$Bait)

#ggplot for advanced plotting
library(ggplot2)
library(ggpubr)
Vessel_data <- read.csv("Vessel_data.csv")
plot <- ggplot(Vessel_data, aes(x=length_meters, y=max_speed_knots)) + 
  geom_point() + 
  geom_smooth(method="lm", color="darkslategray", size=0.4) + 
  stat_regline_equation(label.x=0, label.y=30) +
  stat_cor(label.x=0, label.y=28)
plot

