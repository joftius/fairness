
library(dplyr)
library(caret)

raw_data <- read.csv("law_data.csv")
df2 <- dplyr::select(raw_data, race, sex, LSAT, UGPA, region_first, ZFYA, sander_index, first_pf) 


#set.seed(0)
#trainIndex <- createDataPartition(df2$first_pf, p = .8, 
#                                  list = FALSE, 
#                                  times = 1)
#lawTrain <- df2[trainIndex,]
#lawTest  <- df2[-trainIndex,]

n <- nrow(df2)
#n <- nrow(lawTrain)
#ne <- nrow(lawTest)

race <- rep(1, n); race[df2$race == "White"] <- 0
#race_te <- rep(1, ne); race_te[lawTest$race == "White"] <- 0


gender <- df2$sex - 1
#gender_te <- lawTest$sex - 1

a <- cbind(gender, race)

z <- df2$ZFYA
t <- as.integer(df2$LSAT)
g <- lawTrain$UGPA
l <- as.numeric(df2$region_first) - 1

y <- df2$first_pf

#a_te <- cbine(gender_te, race_te)
#z_te <- lawTest$ZFYA
#t_te <- as.integer(lawTest$LSAT)
#g_te <- lawTest$UGPA
#l_te <- as.numeric(lawTest$region_first) - 1
#y_te <- lawTest$first_pf

k <- ncol(a)


# Priors for coefficients
mu_g0 <- 0
sigma_g0 <- 1

mu_u_g <- 0
sigma_u_g <- 1

mu_l0 <- 0
sigma_l0 <- 1

mu_u_t <- 0
sigma_u_t <- 1

mu_u_z <- 0
sigma_u_z <- 1

#mu_yp0 <- 0
#sigma_yp0 <- 1

#mu_u_yp <- 0
#sigma_u_yp <- 1

mu_y0 <- 0
sigma_y0 <- 1

#mu_yp_y <- 0
#sigma_yp_y <- 1

#mu_a_y <- 0
#sigma_a_y <- 1

#mu_l_y <- 0
#sigma_l_y <- 1

mu_u_y <- 0
sigma_u_y <- 1


library(rstan)

law_stan_dat <- list(N = n, K = k, a = a, z = z, t = t, g = g, l = l, y = y,
                        mu_g0 = mu_g0, sigma_g0 = sigma_g0, mu_u_g = mu_u_g, sigma_u_g = sigma_u_g,
                        mu_l0 = mu_l0, sigma_l0 = sigma_l0,
                        mu_u_t = mu_u_t, sigma_u_t = sigma_u_t,
                        mu_u_z = mu_u_z, sigma_u_z = sigma_u_z,
                        #mu_yp0 = mu_yp0, sigma_yp0 = sigma_yp0, mu_u_yp = mu_u_yp, sigma_u_yp = sigma_u_yp,
                        #mu_y0 = mu_y0, sigma_y0 = sigma_y0, mu_yp_y = mu_yp_y, sigma_yp_y = sigma_yp_y,
                        #mu_a_y = mu_a_y, sigma_a_y = sigma_a_y, mu_l_y = mu_l_y, sigma_l_y = sigma_l_y,
                        mu_y0 = mu_y0, sigma_y0 = sigma_y0,
                        mu_u_y = mu_u_y, sigma_u_y = sigma_u_y)
                        
                
fit <- stan(file = 'law_school.stan', data = law_stan_dat, iter = 2000, chains = 1, verbose = TRUE)

# Extract information

la <- extract(fit, permuted = TRUE)
u_hat <- colMeans(la$u)

# Predict Y

######################################################################################################
# Store data for prediction module

output <- data.frame(bar_pass_fair = y,
                     location = l,
                     UGPA = g, 
                     LSAT = t, 
                     ZFYA = z,
                     race = race,
                     gender = gender, 
                     u_hat = u_hat)

write.csv(output, file = "law_school_stan_results.csv", row.names = FALSE)

#########################################

x_unfair <- cbind(a, z, t, g, l)
x_fair <- cbind(u_hat, l)

model_u <- glm(y ~ x_unfair,family=binomial(link='logit'))
model_f <- glm(y ~ x_fair,family=binomial(link='logit'))

pred_u <- predict.glm(model_u, type = "response")
pred_u_t <- function(t) ifelse(pred_u > t , 1,0)
confusionMatrix(pred_u_t(0.5),y)

pred_f <- predict.glm(model_f, type = "response")
pred_f_t <- function(t) ifelse(pred_f > t , 1,0)
confusionMatrix(pred_f_t(0.5),y)


