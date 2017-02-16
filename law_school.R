
library(dplyr)
library(caret)

raw_data <- read.csv("law_data.csv")
law <- dplyr::select(raw_data, race, sex, LSAT, UGPA, region_first, ZFYA, sander_index, first_pf) 
law <- law[law$region_first != "PO",]
law$region_first <- factor(law$region_first)


#race <- rep(1, n); race[lawTrain$race == "White"] <- 0
#race_te <- rep(1, ne); race_te[lawTest$race == "White"] <- 0

law$amerind <- as.numeric(law$race == "Amerindian")
law$asian   <- as.numeric(law$race == "Asian")
law$black   <- as.numeric(law$race == "Black")
law$hisp    <- as.numeric(law$race == "Hispanic")
law$mexican <- as.numeric(law$race == "Mexican")
law$other   <- as.numeric(law$race == "Other")
law$puerto  <- as.numeric(law$race == "Puertorican")
law$white   <- as.numeric(law$race == "White")

law$female    <- as.numeric(law$sex == 1)
law$male      <- as.numeric(law$sex == 2)

sense_cols <- c("amerind", "asian", "black", "hisp", "mexican", "other", "puerto", "white", "male", "female")

set.seed(0)
trainIndex <- createDataPartition(law$first_pf, p = .8, 
                                  list = FALSE, 
                                  times = 1)
lawTrain <- law[trainIndex,]
lawTest  <- law[-trainIndex,]

#n <- nrow(df2)
n <- nrow(lawTrain)
ne <- nrow(lawTest)



#gender <- lawTrain$sex - 1
#gender_te <- lawTest$sex - 1

#a <- cbind(gender, race)

z <- lawTrain$ZFYA
t <- as.integer(lawTrain$LSAT)
g <- lawTrain$UGPA
l <- as.numeric(lawTrain$region_first) - 1

y <- lawTrain$first_pf

#a_te <- cbind(gender_te, race_te)
z_te <- lawTest$ZFYA
t_te <- as.integer(lawTest$LSAT)
g_te <- lawTest$UGPA
l_te <- as.numeric(lawTest$region_first) - 1
y_te <- lawTest$first_pf

k <- length(sense_cols) #ncol(a)


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

mu_a_g <- 0
sigma_a_g <- 1

mu_a_t <- 0
sigma_a_t <- 1

mu_a_z <- 0
sigma_a_z <- 1

mu_a_y <- 0
sigma_a_y <- 1


library(rstan)


law_stan_dat <- list(N = n, K = k, a = data.matrix(lawTrain[,sense_cols]), z = z, t = t, g = g, #l = l, 
                        y = y,
                        N_TE = ne, a_TE = data.matrix(lawTest[,sense_cols]), z_TE = z_te, t_TE = t_te, g_TE = g_te, #l_TE = l_te,
                        mu_g0 = mu_g0, sigma_g0 = sigma_g0, mu_u_g = mu_u_g, sigma_u_g = sigma_u_g,
                        mu_l0 = mu_l0, sigma_l0 = sigma_l0,
                        mu_u_t = mu_u_t, sigma_u_t = sigma_u_t,
                        mu_u_z = mu_u_z, sigma_u_z = sigma_u_z,
                        #mu_yp0 = mu_yp0, sigma_yp0 = sigma_yp0, mu_u_yp = mu_u_yp, sigma_u_yp = sigma_u_yp,
                        #mu_y0 = mu_y0, sigma_y0 = sigma_y0, mu_yp_y = mu_yp_y, sigma_yp_y = sigma_yp_y,
                        #mu_a_y = mu_a_y, sigma_a_y = sigma_a_y,
                        #mu_l_y = mu_l_y, sigma_l_y = sigma_l_y,
                        mu_y0 = mu_y0, sigma_y0 = sigma_y0,
                        mu_u_y = mu_u_y, sigma_u_y = sigma_u_y,
                        mu_a_g = mu_a_g, sigma_a_g = sigma_a_g,
                        mu_a_t = mu_a_t, sigma_a_t = sigma_a_t,
                        mu_a_z = mu_a_z, sigma_a_z = sigma_a_z,
                        mu_a_y = mu_a_y, sigma_a_y = sigma_a_y)
                        
                
fit <- stan(file = 'law_school_l.stan', data = law_stan_dat, iter = 2000, chains = 1, verbose = TRUE)

# Extract information

la <- extract(fit, permuted = TRUE)
u_hat <- colMeans(la$u)
u_te_hat <- colMeans(la$u_TE)
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

output_te <- data.frame(bar_pass_fair = y_te,
                     location = l_te,
                     UGPA = g_te, 
                     LSAT = t_te, 
                     ZFYA = z_te,
                     race = race_te,
                     gender = gender_te, 
                     u_hat = u_te_hat)

write.csv(output, file = "law_school_l_stan_transductive_train.csv", row.names = FALSE)
write.csv(output_te, file = "law_school_l_stan_transductive_test.csv", row.names = FALSE)


#########################################

x_unfair <- cbind(a, z, t, g, l)
x_unfair_te <- cbind(a_te, z_te, t_te, g_te, l_te)

X_U <- as.data.frame(a)
X_U$z <- z
X_U$t <- t
X_U$g <- g
X_U$l <- l

model_u <- glm(y ~ gender + race + z + t + g + l + 1,family=binomial(link='logit'), data=X_U)
pred_u <- predict.glm(model_u, type = "response")

X_U_TE <- data.frame(gender=gender_te, race=race_te, z=z_te, t=t_te, g=g_te, l=l_te)
pred_u_te <- predict(model_u, newdata=X_U_TE, type = "response")

pred_u_thres <- function(t) ifelse(pred_u > t , 1,0)
pred_u_te_thres <- function(t) ifelse(pred_u_te > t , 1,0)

#confusionMatrix(pred_u_thres(0.5),y)
confusionMatrix(pred_u_te_thres(0.5),y_te)

x_fair <- cbind(u_hat, l)
x_fair_te <- cbind(u_te_hat, l_te)


X_F <- data.frame(u_hat=u_hat, l=l)
X_F_TE <- data.frame(u_hat=u_te_hat, l=l_te)

#model_f <- glm(y ~ x_fair,family=binomial(link='logit'))
model_f <- glm(y ~ u_hat + l + 1, family=binomial(link='logit'), data=X_F)

pred_f <- predict.glm(model_f, type = "response")
pred_f_te <- predict.glm(model_f, newdata=X_F_TE, type = "response")

pred_f_thres <- function(t) ifelse(pred_f > t , 1,0)
pred_f_te_thres <- function(t) ifelse(pred_f_te > t , 1,0)


confusionMatrix(pred_f_thres(0.5),y)
confusionMatrix(pred_f_te_thres(0.5),y_te)

fixed <- rep(1, 4358)
confusionMatrix(fixed,y_te)
