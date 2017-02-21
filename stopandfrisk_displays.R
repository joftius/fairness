#setwd("~/Dropbox/work/turing/fairness/")
library(ggplot2)
library(dplyr)
library(reshape2)

data <- read.csv("stopandfrisk_stan_results_full.csv", header=T)
df <- data
df$X <- NULL

df$race <- factor(as.matrix(df[,1:6]) %*% 1:6, labels = c("Black", "BlackHisp", "Hispanic", "White", "AsPI", "NaAm"))
#df$sex <- factor(df$female == 1, labels = c("female", "male"))

df$criminality <- df$criminality - mean(df$criminality)
df$perception <- df$perception - mean(df$perception)

# Make sure they have the right sign
#df$perception <- df$perception * sign(cor(df$perception, df$frisked))
#df$criminality <- df$criminality * sign(cor(df$criminality, df$weapon))
#cor(df[,c("perception", "criminality", "frisked", "searched", "arrested", "force")])

df2 <- melt(df[,c("race", "criminality", "perception")])

ggplot(df2, aes(race,  value)) + geom_boxplot() +
  facet_wrap(~variable) + ylab("Estimated latent factor") +
  ggtitle("Results for stop and frisk example") + xlab("Race") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("writeup/stopandfrisk_output.pdf", width = 8, height = 5)

df2 <- df %>% filter(race != "AsPI", race != "NaAm") #%>% mutate(race = race == "White")
df2 %>% group_by(race) %>% summarise(count = n())


ggplot(df2, aes(criminality, colour = race)) + stat_ecdf(alpha = .3) +
  ggtitle("Estimated criminality") + theme_bw()

ggplot(df2, aes(perception, colour = race)) + stat_ecdf(alpha = .3) +
  ggtitle("Estimated perception") + theme_bw()



options(digits = 2)
df %>% group_by(race) %>% summarise(criminality = median(criminality), perception = median(perception))
#df %>% group_by(race) %>% summarise(criminality = mean(criminality), perception = mean(perception))
#df %>% group_by(race) %>% summarise(arrested = mean(arrested), weapon = mean(weapon))

summary(lm(criminality ~ race -1, df))
summary(lm(perception ~ race -1, df))

summary(glm(arrested ~ race, family="binomial", df))
summary(glm(weapon ~ race, family="binomial", df))
