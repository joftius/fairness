#setwd("~/Dropbox/work/turing/fairness/")
library(ggplot2)
library(dplyr)

data <- read.csv("stopandfrisk_stan_results_force.csv", header=T)
df <- data
df$X <- NULL

df$race <- factor(as.matrix(df[,1:6]) %*% 1:6, labels = c("Black", "BlackHisp", "Hispanic", "White", "AsPI", "NaAm"))
df$sex <- factor(df$female == 1, labels = c("female", "male"))

df$criminality <- df$criminality - mean(df$criminality)
df$perception <- df$perception - mean(df$perception)

# Make sure they have the right sign
#df$perception <- df$perception * sign(cor(df$perception, df$frisked))
#df$criminality <- df$criminality * sign(cor(df$criminality, df$weapon))
#cor(df[,c("perception", "criminality", "frisked", "searched", "arrested", "force")])

ggplot(df, aes(race, criminality)) + geom_boxplot() +
  ggtitle("Estimated criminality") + theme_bw()

ggplot(df, aes(race, perception)) + geom_boxplot() +
  ggtitle("Estimated perception") + theme_bw()

options(digits = 2)
df %>% group_by(race) %>% summarise(criminality = mean(criminality), perception = mean(perception))
df %>% group_by(race) %>% summarise(arrested = mean(arrested), weapon = mean(weapon))

summary(lm(criminality ~ race -1, df))
summary(lm(perception ~ race -1, df))

summary(glm(arrested ~ race, family="binomial", df))
summary(glm(weapon ~ race, family="binomial", df))
