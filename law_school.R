
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




##########
###########gender <- lawTrain$sex - 1
###########gender_te <- lawTest$sex - 1
##########
###########a <- cbind(gender, race)
##########
##########z <- lawTrain$ZFYA
##########t <- as.integer(lawTrain$LSAT)
##########g <- lawTrain$UGPA
##########l <- as.numeric(lawTrain$region_first) - 1
##########
##########y <- lawTrain$first_pf
##########
###########a_te <- cbind(gender_te, race_te)
##########z_te <- lawTest$ZFYA
##########t_te <- as.integer(lawTest$LSAT)
##########g_te <- lawTest$UGPA
##########l_te <- as.numeric(lawTest$region_first) - 1
##########y_te <- lawTest$first_pf
##########
##########k <- length(sense_cols) #ncol(a)
##########
##########
########### Priors for coefficients
##########mu_g0 <- 0
##########sigma_g0 <- 1
##########
##########mu_u_g <- 0
##########sigma_u_g <- 1
##########
##########mu_l0 <- 0
##########sigma_l0 <- 1
##########
##########mu_u_t <- 0
##########sigma_u_t <- 1
##########
##########mu_u_z <- 0
##########sigma_u_z <- 1
##########
###########mu_yp0 <- 0
###########sigma_yp0 <- 1
##########
###########mu_u_yp <- 0
###########sigma_u_yp <- 1
##########
##########mu_y0 <- 0
##########sigma_y0 <- 1
##########
###########mu_yp_y <- 0
###########sigma_yp_y <- 1
##########
##########mu_a_y <- 0
##########sigma_a_y <- 1
##########
###########mu_l_y <- 0
###########sigma_l_y <- 1
##########
##########mu_u_y <- 0
##########sigma_u_y <- 1
##########
##########mu_a_g <- 0
##########sigma_a_g <- 1
##########
##########mu_a_t <- 0
##########sigma_a_t <- 1
##########
##########mu_a_z <- 0
##########sigma_a_z <- 1
##########
##########mu_a_y <- 0
##########sigma_a_y <- 1
##########
##########
##########library(rstan)
##########
##########
##########law_stan_dat <- list(N = n, K = k, a = data.matrix(lawTrain[,sense_cols]), z = z, t = t, g = g, #l = l, 
##########                        y = y,
##########                        N_TE = ne, a_TE = data.matrix(lawTest[,sense_cols]), z_TE = z_te, t_TE = t_te, g_TE = g_te, #l_TE = l_te,
##########                        mu_g0 = mu_g0, sigma_g0 = sigma_g0, mu_u_g = mu_u_g, sigma_u_g = sigma_u_g,
##########                        mu_l0 = mu_l0, sigma_l0 = sigma_l0,
##########                        mu_u_t = mu_u_t, sigma_u_t = sigma_u_t,
##########                        mu_u_z = mu_u_z, sigma_u_z = sigma_u_z,
##########                        #mu_yp0 = mu_yp0, sigma_yp0 = sigma_yp0, mu_u_yp = mu_u_yp, sigma_u_yp = sigma_u_yp,
##########                        #mu_y0 = mu_y0, sigma_y0 = sigma_y0, mu_yp_y = mu_yp_y, sigma_yp_y = sigma_yp_y,
##########                        #mu_a_y = mu_a_y, sigma_a_y = sigma_a_y,
##########                        #mu_l_y = mu_l_y, sigma_l_y = sigma_l_y,
##########                        mu_y0 = mu_y0, sigma_y0 = sigma_y0,
##########                        mu_u_y = mu_u_y, sigma_u_y = sigma_u_y,
##########                        mu_a_g = mu_a_g, sigma_a_g = sigma_a_g,
##########                        mu_a_t = mu_a_t, sigma_a_t = sigma_a_t,
##########                        mu_a_z = mu_a_z, sigma_a_z = sigma_a_z,
##########                        mu_a_y = mu_a_y, sigma_a_y = sigma_a_y)
##########                        
##########                
##########fit_law <- stan(file = 'law_school_l.stan', data = law_stan_dat, iter = 2000, chains = 1, verbose = TRUE)
##########
########### Extract information
##########
##########la_law <- extract(fit_law, permuted = TRUE)
##########u_hat <- colMeans(la_law$u)
##########u_te_hat <- colMeans(la_law$u_TE)
########### Predict Y












#get_performance <- function(pred, y) {
#  conf <- confusionMatrix(pred,y)
#  print(conf$byClass["Balanced Accuracy"])
#  F1 <- as.numeric(2*(conf$byClass["Precision"]*conf$byClass["Recall"])/(conf$byClass["Precision"]+conf$byClass["Recall"]))
#  print(F1)
#  return(list(conf$byClass["Balanced Accuracy"],F1))
#}
#res = get_perforamance(pred_UNA_te_thres(0.5),y_te)


# HERERERERERERE
lawTrain$LSAT <- round(lawTrain$LSAT)
lawTest$LSAT <- round(lawTest$LSAT)

# don't fit model transductively and don't use pass
# -------------------------------------------------
law_stan_train <- list(N = n, K = length(sense_cols), a = data.matrix(lawTrain[,sense_cols]), 
                          ugpa = lawTrain[,c("UGPA")], lsat = lawTrain[,c("LSAT")], zfya = lawTrain[,c("ZFYA")])


fit_law_train <- stan(file = 'law_school_train.stan', data = law_stan_train, iter = 2000, chains = 1, verbose = TRUE)
# Extract information

la_law_train <- extract(fit_law_train, permuted = TRUE)
#u_te_samp <- colMeans(la_law_train$u_TE)
U_TRAIN   <- colMeans(la_law_train$u)

ugpa0      <- mean(la_law_train$ugpa0)
eta_u_ugpa <- mean(la_law_train$eta_u_ugpa)
eta_a_ugpa <- colMeans(la_law_train$eta_a_ugpa)

lsat0      <- mean(la_law_train$lsat0)
eta_u_lsat <- mean(la_law_train$eta_u_lsat)
eta_a_lsat <- colMeans(la_law_train$eta_a_lsat)

SIGMA_G <- mean(la_law_train$sigma_g)

# get U_TEST by sampling from stan
#----------------------------------
law_stan_test <- list(N = ne, K = length(sense_cols), a = data.matrix(lawTest[,sense_cols]),
                      ugpa = lawTest[,c("UGPA")], lsat = lawTest[,c("LSAT")],
                      ugpa0 = ugpa0, eta_u_ugpa = eta_u_ugpa, eta_a_ugpa = eta_a_ugpa,
                      lsat0 = lsat0, eta_u_lsat = eta_u_lsat, eta_a_lsat = eta_a_lsat,
                      sigma_g = SIGMA_G)


fit_law_test <- stan(file = 'law_school_only_u.stan', data = law_stan_test, iter = 2000, chains = 1, verbose = TRUE)
la_law_test <- extract(fit_law_test, permuted = TRUE)
#u_te_samp <- colMeans(la_law_train$u_TE)
U_TEST   <- colMeans(la_law_test$u)

##lawTrainSmall <- lawTrain
##lawTrainSmall <- lawTrainSmall[lawTrainSmall$black==1 | lawTrainSmall$white==1,]
##
##lawTestSmall <- lawTest
##lawTestSmall <- lawTestSmall[lawTestSmall$black==1 | lawTestSmall$white==1,]
##
##n_small <- nrow(lawTrainSmall)
##law_stan_train <- list(N = n_small, K = length(sense_cols), a = data.matrix(lawTrain[,sense_cols]), 
##                       ugpa = lawTrain[,c("UGPA")], lsat = lawTrain[,c("LSAT")], zfya = lawTrain[,c("ZFYA")])




#outSampSwap <- outSampSwap[outSampSwap$black==1 | outSampSwap$white==1,]


######################################################################################################
# Store data for prediction module

output <- data.frame(bar_pass_fair = y,
                     location = l,
                     UGPA = g, 
                     LSAT = t, 
                     ZFYA = z,
                     amerind = lawTrain$amerind,
                     asian   = lawTrain$asian  ,
                     black   = lawTrain$black  ,
                     hisp    = lawTrain$hisp   ,
                     mexican = lawTrain$mexican,
                     other   = lawTrain$other  ,
                     puerto  = lawTrain$puerto ,
                     white   = lawTrain$white  ,
                     female  = lawTrain$female,
                     male    = lawTrain$male,
                     u_hat = u_hat)

output_te <- data.frame(bar_pass_fair = y_te,
                        location = l_te,
                        UGPA = g_te, 
                        LSAT = t_te, 
                        ZFYA = z_te,
                        amerind = lawTest$amerind,
                        asian   = lawTest$asian  ,
                        black   = lawTest$black  ,
                        hisp    = lawTest$hisp   ,
                        mexican = lawTest$mexican,
                        other   = lawTest$other  ,
                        puerto  = lawTest$puerto ,
                        white   = lawTest$white  ,
                        female  = lawTest$female,
                        male    = lawTest$male,
                        u_hat = u_te_hat)

write.csv(output, file = "law_school_l_stan_transductive_train.csv", row.names = TRUE)
write.csv(output_te, file = "law_school_l_stan_transductive_test.csv", row.names = TRUE)

save(la_law,file='law_school_l_stan_results.Rdata')
#########################################












# Classifiers on data
# -------------------

X_U <- as.data.frame(data.matrix(lawTrain[,sense_cols]))
X_U$ZFYA <- lawTrain$ZFYA
X_U$LSAT <- lawTrain$LSAT
X_U$UGPA <- lawTrain$UGPA

X_U_TE <- as.data.frame(data.matrix(lawTest[,sense_cols]))
X_U_TE$ZFYA <- lawTest$ZFYA
X_U_TE$LSAT <- lawTest$LSAT
X_U_TE$UGPA <- lawTest$UGPA



model_u <- lm(ZFYA ~ LSAT + UGPA + amerind + asian + black + hisp + mexican + other + puerto + white + male + female + 1, data=X_U)
pred_u <- predict.glm(model_u)


pred_u_te <- predict(model_u, newdata=X_U_TE)
rmse_u_te <- sqrt( sum( (pred_u_te - X_U_TE$ZFYA)^2 ) / nrow(X_U_TE) )

#pred_u_thres <- function(t) ifelse(pred_u > t , 1,0)
#pred_u_te_thres <- function(t) ifelse(pred_u_te > t , 1,0)

##confusionMatrix(pred_u_thres(0.5),y)
#conf_u <- confusionMatrix(pred_u_te_thres(0.5),y_te)
#conf_u$byClass["Balanced Accuracy"]
#F1_u <- as.numeric(2*(conf_u$byClass["Precision"]*conf_u$byClass["Recall"])/(conf_u$byClass["Precision"]+conf_u$byClass["Recall"]))
#F1_u


X_F <- data.frame(u=U_TRAIN, ZFYA=lawTrain$ZFYA)
X_F_TE <- data.frame(u=U_TEST, ZFYA=lawTest$ZFYA)

#model_f <- glm(y ~ x_fair,family=binomial(link='logit'))
model_f <- lm(ZFYA ~ u + 1, data=X_F)

pred_f <- predict.glm(model_f)
pred_f_te <- predict.glm(model_f, newdata=X_F_TE)

rmse_f_te <- sqrt( sum( (pred_f_te - X_F_TE$ZFYA)^2 ) / nrow(X_F_TE) )





model_un <- lm(ZFYA ~ LSAT + UGPA + 1, data=X_U)
pred_un <- predict.glm(model_un)


pred_un_te <- predict(model_un, newdata=X_U_TE)
rmse_un_te <- sqrt( sum( (pred_un_te - X_U_TE$ZFYA)^2 ) / nrow(X_U_TE) )




orthogonalize <- function(F, A) {
  
  mult <- sum(F * A) / sum(A*A)
  O <- F - A*mult
  
  return(O)
}

UGPA_O <- lawTrain$UGPA
UGPA_O_TE <- lawTest$UGPA

LSAT_O    <- lawTrain$LSAT
LSAT_O_TE <-  lawTest$LSAT
for (i in 1:length(sense_cols)) {
  UGPA_O = orthogonalize(UGPA_O,lawTrain[,sense_cols[i]])
  UGPA_O_TE = orthogonalize(UGPA_O_TE,lawTest[,sense_cols[i]])
  
  LSAT_O    = orthogonalize(LSAT_O,lawTrain[,sense_cols[i]])
  LSAT_O_TE = orthogonalize(LSAT_O_TE,lawTest[,sense_cols[i]])
}

lawTrain$UGPA_O <- UGPA_O
lawTrain$LSAT_O <- LSAT_O

lawTest$UGPA_O <- UGPA_O_TE
lawTest$LSAT_O <- LSAT_O_TE

model_o <- lm(ZFYA ~ LSAT_O + UGPA_O + 1, data=lawTrain)
pred_o <- predict(model_o)


pred_o_te <- predict(model_o, newdata=lawTest)
rmse_o_te <- sqrt( sum( (pred_o_te - lawTest$ZFYA)^2 ) / nrow(lawTest) )




# regress UGPA on race, sex
# regress LSAT on race, sex
model_ugpa <- lm(UGPA ~ amerind + asian + black + hisp + mexican + other + puerto + white + male + female + 1, data=lawTrain)
model_lsat <- lm(LSAT ~ amerind + asian + black + hisp + mexican + other + puerto + white + male + female + 1, data=lawTrain)

#resid
lawTrain$resid_UGPA = lawTrain$UGPA - predict(model_ugpa, newdata=lawTrain)
lawTrain$resid_LSAT = lawTrain$LSAT - predict(model_lsat, newdata=lawTrain)

model_f2 <- lm(ZFYA ~ resid_UGPA + resid_LSAT + 1, data=lawTrain)

model_ugpa_te <- lm(UGPA ~ amerind + asian + black + hisp + mexican + other + puerto + white + male + female + 1, data=lawTest)
model_lsat_te <- lm(LSAT ~ amerind + asian + black + hisp + mexican + other + puerto + white + male + female + 1, data=lawTest)

#resid
lawTest$resid_UGPA = lawTest$UGPA - predict(model_ugpa_te, newdata=lawTest)
lawTest$resid_LSAT = lawTest$LSAT - predict(model_lsat_te, newdata=lawTest)


pred_f2_te <- predict(model_f2, newdata=lawTest)
rmse_f2_te <- sqrt( sum( (pred_f2_te - lawTest$ZFYA)^2 ) / nrow(lawTest) )


mZFYA <- mean(lawTrain$ZFYA)


# ---------------------------------------
























  


# we'll try computing counterfactuals!!!!!!
# -----------------------------------------
# 1.  Estimate the parameters by taking the posterior expected value
ugpa0      <- mean(la_law_train$ugpa0)
eta_u_ugpa <- mean(la_law_train$eta_u_ugpa)
eta_a_ugpa <- colMeans(la_law_train$eta_a_ugpa)

lsat0      <- mean(la_law_train$lsat0)
eta_u_lsat <- mean(la_law_train$eta_u_lsat)
eta_a_lsat <- colMeans(la_law_train$eta_a_lsat)

eta_u_zfya <- mean(la_law_train$eta_u_zfya)
eta_a_zfya <- colMeans(la_law_train$eta_a_zfya)

SIGMA_G <- mean(la_law_train$sigma_g)

#G0 <- mean(la_law$g0)
#ETA_U_G <- mean(la_law$eta_u_g)
#ETA_A_G <- colMeans(la_law$eta_a_g)
#
#L0 <- mean(la_law$l0)
#ETA_U_T <- mean(la_law$eta_u_t)
#ETA_A_T <- colMeans(la_law$eta_a_t)
#
#ETA_U_Z <- mean(la_law$eta_u_z)
#ETA_A_Z <- colMeans(la_law$eta_a_z)
#
#Y0 <- mean(la_law$y0)
#ETA_U_Y <- mean(la_law$eta_u_y)
#ETA_A_Y <- colMeans(la_law$eta_a_y)
#
#SIGMA_G <- mean(la_law$sigma_g)

# 2. Simulate fake data out of it
# sample a new training set and test set
# fit vae on training set
# compute Z for training set and test set
# compute Z for counterfactual test set
ATR = data.matrix(lawTrain[,sense_cols])
#ATR = data.matrix(lawTest[,sense_cols])
ATR_swap_r <- ATR
ATR_swap_r2 <- ATR
ATR_swap_r3 <- ATR
ATR_swap_g <- ATR


temp <- ATR_swap_r[,8] # white
ATR_swap_r[,8] <- ATR_swap_r[,3] # black
ATR_swap_r[,3] <- temp

temp <- ATR_swap_g[,9] # male
ATR_swap_g[,9] <- ATR_swap_g[,10] # female
ATR_swap_g[,10] <- temp

ATR_swap_b <- ATR_swap_r
temp <- ATR_swap_b[,9] # male
ATR_swap_b[,9] <- ATR_swap_b[,10] # female
ATR_swap_b[,10] <- temp

temp <- ATR_swap_r2[,8] # white
ATR_swap_r2[,8] <- ATR_swap_r2[,2] # asian
ATR_swap_r2[,2] <- temp

temp <- ATR_swap_r3[,8] # white
ATR_swap_r3[,8] <- ATR_swap_r3[,5] # mexican
ATR_swap_r3[,5] <- temp


set.seed(0)
gpa_rand_tr <- rnorm(n, mean=0, sd=SIGMA_G)
#gpa_rand_te <- rnorm(ne,mean=0, sd=SIGMA_G)

lsat_rand_tr <- runif(n)
#lsat_rand_te <- runif(ne)

zfya_rand_tr <- rnorm(n)
#zfya_rand_te <- rnorm(ne)

pass_rand_tr <- runif(n)
#pass_rand_te <- runif(ne)

u_rand_tr <- rnorm(n)
#u_rand_te <- rnorm(ne)


sample_GPA <- function(u, a, eps) {
  GPA_m <- ugpa0 + eta_u_ugpa * u + a %*% eta_a_ugpa
  #eps <- rnorm(1,mean=0,sd=SIGMA_G)
  #return(list(GPA_m,eps))
  return(GPA_m + eps)
}

sample_LSAT <- function(u, a, eps) {
  #eps <- runif(1)
  #LSAT_m <- qpois( eps, exp( L0 + ETA_U_T * u + a %*% ETA_A_T ) )
  LSAT_m <- exp( lsat0 + eta_u_lsat * u + a %*% eta_a_lsat )
  sample <- qpois( eps, LSAT_m )
  return(sample)#list(LSAT_m,eps))
}

sample_ZFYA <- function(u, a, eps) {
  ZFYA_m <- eta_u_zfya * u + a %*% eta_a_zfya
  #eps <- rnorm(1,mean=0,sd=1)
  return(ZFYA_m + eps)#list(ZFYA_m,eps))
}

#sample_PASS <- function(u, a, eps) {
#  PASS_m <- 1.0/(1.0 + exp(-(Y0 + ETA_U_Y * u + a %*% ETA_A_Y)))
#  #eps <- runif(1)
#  # qbinom( eps, 1, PASS_m)
#  sample <- qbinom( eps, 1, PASS_m )
#  return(sample) #(list(PASS_m,eps))
#}

lawSample     <- lawTrain
lawSampleSwapR <- lawTrain
lawSampleSwapR2 <- lawTrain
lawSampleSwapR3 <- lawTrain
lawSampleSwapG <- lawTrain
lawSampleSwapB <- lawTrain
for (i in 1:n) {
  lawSample$UGPA[i] = sample_GPA(u_rand_tr[i], ATR[i,], gpa_rand_tr[i])
  lawSample$LSAT[i] = sample_LSAT(u_rand_tr[i], ATR[i,], lsat_rand_tr[i])
  lawSample$ZFYA[i] = sample_ZFYA(u_rand_tr[i], ATR[i,], zfya_rand_tr[i])
  #lawSample$first_pf[i] = sample_PASS(u_rand_tr[i], ATR[i,], pass_rand_tr[i])
  
  lawSampleSwapR$UGPA[i]     = sample_GPA(u_rand_tr[i],  ATR_swap_r[i,], gpa_rand_tr[i])
  lawSampleSwapR$LSAT[i]     = sample_LSAT(u_rand_tr[i], ATR_swap_r[i,], lsat_rand_tr[i])
  lawSampleSwapR$ZFYA[i]     = sample_ZFYA(u_rand_tr[i], ATR_swap_r[i,], zfya_rand_tr[i])
  #lawSampleSwap$first_pf[i] = sample_PASS(u_rand_tr[i], ATR_swap[i,], pass_rand_tr[i])
  lawSampleSwapR2$UGPA[i]     = sample_GPA(u_rand_tr[i],  ATR_swap_r2[i,], gpa_rand_tr[i])
  lawSampleSwapR2$LSAT[i]     = sample_LSAT(u_rand_tr[i], ATR_swap_r2[i,], lsat_rand_tr[i])
  lawSampleSwapR2$ZFYA[i]     = sample_ZFYA(u_rand_tr[i], ATR_swap_r2[i,], zfya_rand_tr[i])
  
  lawSampleSwapR3$UGPA[i]     = sample_GPA(u_rand_tr[i],  ATR_swap_r3[i,], gpa_rand_tr[i])
  lawSampleSwapR3$LSAT[i]     = sample_LSAT(u_rand_tr[i], ATR_swap_r3[i,], lsat_rand_tr[i])
  lawSampleSwapR3$ZFYA[i]     = sample_ZFYA(u_rand_tr[i], ATR_swap_r3[i,], zfya_rand_tr[i])

  lawSampleSwapG$UGPA[i]     = sample_GPA(u_rand_tr[i],  ATR_swap_g[i,], gpa_rand_tr[i])
  lawSampleSwapG$LSAT[i]     = sample_LSAT(u_rand_tr[i], ATR_swap_g[i,], lsat_rand_tr[i])
  lawSampleSwapG$ZFYA[i]     = sample_ZFYA(u_rand_tr[i], ATR_swap_g[i,], zfya_rand_tr[i])
  #lawSampleSwap$first_pf[i] = sample_PASS(u_rand_tr[i], ATR_swap[i,], pass_rand_tr[i])
  
  lawSampleSwapB$UGPA[i]     = sample_GPA(u_rand_tr[i],  ATR_swap_b[i,], gpa_rand_tr[i])
  lawSampleSwapB$LSAT[i]     = sample_LSAT(u_rand_tr[i], ATR_swap_b[i,], lsat_rand_tr[i])
  lawSampleSwapB$ZFYA[i]     = sample_ZFYA(u_rand_tr[i], ATR_swap_b[i,], zfya_rand_tr[i])
  #lawSampleSwap$first_pf[i] = sample_PASS(u_rand_tr[i], ATR_swap[i,], pass_rand_tr[i])
  
    
}
lawSampleSwapR[,sense_cols] = ATR_swap_r
lawSampleSwapR2[,sense_cols] = ATR_swap_r2
lawSampleSwapR3[,sense_cols] = ATR_swap_r3
lawSampleSwapG[,sense_cols] = ATR_swap_g
lawSampleSwapB[,sense_cols] = ATR_swap_b

#lawSampleTe <- lawTest
##lawSampleTeSwap <- lawTest
#for (i in 1:ne) {
#  lawSampleTe$UGPA[i] = sample_GPA(u_rand_te[i], ATE[i,], gpa_rand_te[i])
#  lawSampleTe$LSAT[i] = sample_LSAT(u_rand_te[i], ATE[i,], lsat_rand_te[i])
#  lawSampleTe$ZFYA[i] = sample_ZFYA(u_rand_te[i], ATE[i,], zfya_rand_te[i])
#  lawSampleTe$first_pf[i] = sample_PASS(u_rand_te[i], ATE[i,], pass_rand_te[i])
#  
#  #lawSampleTeSwap$UGPA[i] = sample_GPA(u_rand_te[i], ATE_swap[i,], gpa_rand_te[i])
#  #lawSampleTeSwap$LSAT[i] = sample_LSAT(u_rand_te[i], ATE_swap[i,], lsat_rand_te[i])
#  #lawSampleTeSwap$ZFYA[i] = sample_ZFYA(u_rand_te[i], ATE_swap[i,], zfya_rand_te[i])
#  #lawSampleTeSwap$first_pf[i] = sample_PASS(u_rand_te[i], ATE_swap[i,], pass_rand_te[i])
#}
##lawSampleTeSwap[,sense_cols] = ATE_swap


outSamp <- data.frame(bar_pass = lawSample$first_pf,
                     UGPA    = lawSample$UGPA, 
                     LSAT    = lawSample$LSAT, 
                     ZFYA    = lawSample$ZFYA,
                     amerind = lawSample$amerind,
                     asian   = lawSample$asian  ,
                     black   = lawSample$black  ,
                     hisp    = lawSample$hisp   ,
                     mexican = lawSample$mexican,
                     other   = lawSample$other  ,
                     puerto  = lawSample$puerto ,
                     white   = lawSample$white  ,
                     female  = lawSample$female,
                     male    = lawSample$male)

outSampSwap <- data.frame(bar_pass = lawSampleSwap$first_pf,
                      UGPA    = lawSampleSwap$UGPA, 
                      LSAT    = lawSampleSwap$LSAT, 
                      ZFYA    = lawSampleSwap$ZFYA,
                      amerind = lawSampleSwap$amerind,
                      asian   = lawSampleSwap$asian  ,
                      black   = lawSampleSwap$black  ,
                      hisp    = lawSampleSwap$hisp   ,
                      mexican = lawSampleSwap$mexican,
                      other   = lawSampleSwap$other  ,
                      puerto  = lawSampleSwap$puerto ,
                      white   = lawSampleSwap$white  ,
                      female  = lawSampleSwap$female,
                      male    = lawSampleSwap$male)


#outSampTe <- data.frame(bar_pass = lawSampleTe$first_pf,
#                        UGPA    = lawSampleTe$UGPA, 
#                        LSAT    = lawSampleTe$LSAT, 
#                        ZFYA    = lawSampleTe$ZFYA,
#                        amerind = lawSampleTe$amerind,
#                        asian   = lawSampleTe$asian  ,
#                        black   = lawSampleTe$black  ,
#                        hisp    = lawSampleTe$hisp   ,
#                        mexican = lawSampleTe$mexican,
#                        other   = lawSampleTe$other  ,
#                        puerto  = lawSampleTe$puerto ,
#                        white   = lawSampleTe$white  ,
#                        female  = lawSampleTe$female,
#                        male    = lawSampleTe$male)

#outSampTeSwap <- data.frame(bar_pass = lawSampleTeSwap$first_pf,
#                        UGPA    = lawSampleTeSwap$UGPA, 
#                        LSAT    = lawSampleTeSwap$LSAT, 
#                        ZFYA    = lawSampleTeSwap$ZFYA,
#                        amerind = lawSampleTeSwap$amerind,
#                        asian   = lawSampleTeSwap$asian  ,
#                        black   = lawSampleTeSwap$black  ,
#                        hisp    = lawSampleTeSwap$hisp   ,
#                        mexican = lawSampleTeSwap$mexican,
#                        other   = lawSampleTeSwap$other  ,
#                        puerto  = lawSampleTeSwap$puerto ,
#                        white   = lawSampleTeSwap$white  ,
#                        female  = lawSampleTeSwap$female,
#                        male    = lawSampleTeSwap$male)

write.csv(outSamp, file = "law_school_sampled_train.csv", row.names = TRUE)
write.csv(outSampSwap, file = "law_school_sampled_swap.csv", row.names = TRUE)
#write.csv(outSampTe, file = "law_school_sampled_test.csv", row.names = TRUE)
#write.csv(outSampTeSwap, file = "law_school_sampled_swap_test.csv", row.names = TRUE)


# only white vs. black
outSamp <- outSamp[outSamp$black==1 | outSamp$white==1,]
outSampSwap <- outSampSwap[outSampSwap$black==1 | outSampSwap$white==1,]
#outSampTe <- outSampTe[outSampTe$black==1 | outSampTe$white==1,]
#outSampTeSwap <- outSampTeSwap[outSampTeSwap$black==1 | outSampTeSwap$white==1,]

outSamp$bar_pass <- NULL
outSamp$amerind <- NULL
outSamp$asian <- NULL
outSamp$hisp <- NULL
outSamp$mexican <- NULL
outSamp$other <- NULL
outSamp$puerto <- NULL

outSampSwap$bar_pass <- NULL
outSampSwap$amerind <- NULL
outSampSwap$asian <- NULL
outSampSwap$hisp <- NULL
outSampSwap$mexican <- NULL
outSampSwap$other <- NULL
outSampSwap$puerto <- NULL

#outSampTeSwap$bar_pass <- NULL
#outSampTeSwap$amerind <- NULL
#outSampTeSwap$asian <- NULL
#outSampTeSwap$hisp <- NULL
#outSampTeSwap$mexican <- NULL
#outSampTeSwap$other <- NULL
#outSampTeSwap$puerto <- NULL
#
#outSampTe$bar_pass <- NULL
#outSampTe$amerind <- NULL
#outSampTe$asian <- NULL
#outSampTe$hisp <- NULL
#outSampTe$mexican <- NULL
#outSampTe$other <- NULL
#outSampTe$puerto <- NULL

write.csv(outSamp, file = "law_school_sampled_train.csv", row.names = TRUE)
write.csv(outSampSwap, file = "law_school_sampled_swap.csv", row.names = TRUE)

#lawSample     <- lawTrain
#lawSampleSwapR <- lawTrain
#lawSampleSwapG <- lawTrain
#lawSampleSwapB



# fit unfair classifier to sampled data
model_u_samp <- lm(ZFYA ~ LSAT + UGPA + amerind + asian + black + hisp + mexican + other + puerto + white + male + female + 1, data=lawSample) #,family=binomial(link='logit'), data=outSamp)
model_una_samp <- lm(ZFYA ~ UGPA + LSAT + 1, data=lawSample)


UGPA_O <- lawSample$UGPA
LSAT_O    <- lawSample$LSAT

UGPA_OR <- lawSampleSwapR$UGPA
UGPA_OR2 <- lawSampleSwapR2$UGPA
UGPA_OR3 <- lawSampleSwapR3$UGPA
UGPA_OG <-   lawSampleSwapG$UGPA
UGPA_OB <-   lawSampleSwapB$UGPA

LSAT_OR <-   lawSampleSwapR$LSAT
LSAT_OR2 <- lawSampleSwapR2$LSAT
LSAT_OR3 <- lawSampleSwapR3$LSAT
LSAT_OG <-   lawSampleSwapG$LSAT
LSAT_OB <-   lawSampleSwapB$LSAT



for (i in 1:length(sense_cols)) {
  UGPA_O = orthogonalize(UGPA_O,lawSample[,sense_cols[i]])
  
  UGPA_OR  = orthogonalize(UGPA_OR, lawSampleSwapR[,sense_cols[i]])
  UGPA_OR2 = orthogonalize(UGPA_OR2,lawSampleSwapR2[,sense_cols[i]])
  UGPA_OR3 = orthogonalize(UGPA_OR3,lawSampleSwapR3[,sense_cols[i]])
  UGPA_OG  = orthogonalize(UGPA_OG, lawSampleSwapG[,sense_cols[i]])
  UGPA_OB  = orthogonalize(UGPA_OB, lawSampleSwapB[,sense_cols[i]])

  LSAT_O    = orthogonalize(LSAT_O,lawSample[,sense_cols[i]])
  
  LSAT_OR  = orthogonalize(LSAT_OR, lawSampleSwapR[,sense_cols[i]])
  LSAT_OR2 = orthogonalize(LSAT_OR2,lawSampleSwapR2[,sense_cols[i]])
  LSAT_OR3 = orthogonalize(LSAT_OR3,lawSampleSwapR3[,sense_cols[i]])
  LSAT_OG  = orthogonalize(LSAT_OG, lawSampleSwapG[,sense_cols[i]])
  LSAT_OB  = orthogonalize(LSAT_OB, lawSampleSwapB[,sense_cols[i]])
}

lawSample$UGPA_O <- UGPA_O
lawSample$LSAT_O <- LSAT_O

lawSampleSwapR$UGPA_O <- UGPA_OR
lawSampleSwapR$LSAT_O <- LSAT_OR

lawSampleSwapR2$UGPA_O <- UGPA_OR2
lawSampleSwapR2$LSAT_O <- LSAT_OR2

lawSampleSwapR3$UGPA_O <- UGPA_OR3
lawSampleSwapR3$LSAT_O <- LSAT_OR3

lawSampleSwapG$UGPA_O <- UGPA_OG
lawSampleSwapG$LSAT_O <- LSAT_OG

lawSampleSwapB$UGPA_O <- UGPA_OB
lawSampleSwapB$LSAT_O <- LSAT_OB


model_o_samp <- lm(ZFYA ~ LSAT_O + UGPA_O + 1, data=lawSample)






predict_and_fair <- function(model, data, dataSwap) {
  pred      <- predict(model, newdata=data)#, type="response")
  pred_swap <- predict(model, newdata=dataSwap)#, type="response")
  
  data$pred_zfya <- pred
  data$type <- 0
  
  dataSwap$pred_zfya <- pred_swap
  dataSwap$type <- 1
  
  total <- rbind(data, dataSwap)
  total$type <- factor(total$type, labels=c("original", "swapped"))
  
#  ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
#    geom_density(alpha = 0.1) +
#    xlim(55, 70)
  
  p <- ggplot(total, aes(pred_zfya, color = type, fill = type)) + geom_density(alpha=0.1) #stat_density(position="identity",geom="line")## + theme_bw()
  rmse <- sqrt( sum( (data$pred_zfya - data$ZFYA)^2 ) / nrow(data) )
  return(list("plot"=p,"rmse"=rmse))
}
resR = predict_and_fair(model_u_samp, lawSample, lawSampleSwapR)
resR2 = predict_and_fair(model_u_samp, lawSample, lawSampleSwapR2)
resR3 = predict_and_fair(model_u_samp, lawSample, lawSampleSwapR3)
resG = predict_and_fair(model_u_samp, lawSample, lawSampleSwapG)
resB = predict_and_fair(model_u_samp, lawSample, lawSampleSwapB)

res_unaR = predict_and_fair(model_una_samp, lawSample, lawSampleSwapR)
res_unaR2 = predict_and_fair(model_una_samp, lawSample, lawSampleSwapR2)
res_unaR3 = predict_and_fair(model_una_samp, lawSample, lawSampleSwapR3)
res_unaG = predict_and_fair(model_una_samp, lawSample, lawSampleSwapG)
res_unaB = predict_and_fair(model_una_samp, lawSample, lawSampleSwapB)


res_OR  = predict_and_fair(model_o_samp, lawSample, lawSampleSwapR)
res_OR2 = predict_and_fair(model_o_samp, lawSample, lawSampleSwapR2)
res_OR3 = predict_and_fair(model_o_samp, lawSample, lawSampleSwapR3)
res_OG  = predict_and_fair(model_o_samp, lawSample, lawSampleSwapG)
res_OB  = predict_and_fair(model_o_samp, lawSample, lawSampleSwapB)

# TRY VAE
vae_train_data <- read.csv("vae_fix_generated_law_train_samp_beta100.0.csv", header = FALSE, strip.white = TRUE)
vae_swap_data  <- read.csv("vae_fix_generated_law_swap_samp_beta100.0.csv", header = FALSE, strip.white = TRUE)



model_vae <- lm(V1 ~ V2 + 1, data=vae_train_data)
res_vae = predict_and_fair(model_vae, vae_train_data, vae_swap_data)


# HERE


















pred_u_samp  <- predict(model_u_samp)
pred_u_swap  <- predict(model_u_samp, newdata=outSampSwap)
#pred_u_samp_te   <- predict(model_u_samp, newdata=outSampTe)#, type="response")
#pred_u_samp_swap <- predict(model_u_samp, newdata=outSampTeSwap)#, type="response")
#pred_u_samp_te_t <- function(t) ifelse(pred_u_samp_te > t , 1,0)
#pred_u_samp_samp_t <- function(t) ifelse(pred_u_samp_swap > t , 1,0)

#outSamp$race <- factor(as.matrix(outSamp[,5:12]) %*% 1:8, labels = c("amerind", "asian", "black", "hisp", "mexican", "other", "puerto", "white"))
outSamp$race     <- factor(as.matrix(outSamp[,c("black","white")]) %*% 1:2, labels = c("black", "white"))
outSampSwap$race <- factor(as.matrix(outSampSwap[,c("black","white")]) %*% 1:2, labels = c("black", "white"))

#outSampTe$race <- factor(as.matrix(outSampTe[,c("black","white")]) %*% 1:2, labels = c("black", "white"))
#outSampTeSwap$race <- factor(as.matrix(outSampTeSwap[,c("black","white")]) %*% 1:2, labels = c("black", "white"))


outSampTe$pred <- pred_u_samp_te
outSampTe$type <- 0


outSampTeSwap$pred <- pred_u_samp_swap
outSampTeSwap$type <- 1

total <- rbind(outSampTe, outSampTeSwap)
total$type <- factor(total$type, labels=c("original", "swapped"))
#total$type <- fct_recode(factor(total$type), Original = "0", Swapped = "1")

ggplot(total, aes(pred, linetype = type)) + geom_density() + theme_bw()



###outSampTe$race <- NULL
###outSampTe$bar_pass <- NULL
###outSampTe$pred <- NULL
###outSampTe$data <- NULL
###outSampTe$type <- NULL
###outSampTe$pred_unaware <- NULL
###outSampTeSwap$race <- NULL
###outSampTeSwap$bar_pass <- NULL
###outSampTeSwap$pred <- NULL
###outSampTeSwap$data <- NULL
###outSampTeSwap$type <- NULL
###outSampTeSwap$pred_unaware <- NULL

# Is the unaware model counterfactually fair?
model_u_unaware <- lm(ZFYA ~ UGPA + LSAT + male + female + 1, data=outSamp)
pred_u_unaware   <- predict(model_u_unaware, newdata=outSampTe)#, type="response")
pred_u_unaware_swap <- predict(model_u_unaware, newdata=outSampTeSwap)#, type="response")

outSampTe$pred_unaware <- pred_u_unaware
outSampTe$type <- 0
outSampTeSwap$pred_unaware <- pred_u_unaware_swap
outSampTeSwap$type <- 1

total <- rbind(outSampTe, outSampTeSwap)
total$type <- factor(total$type, labels=c("original", "swapped"))

ggplot(total, aes(pred_unaware, linetype = type)) + geom_density() + theme_bw()



# What about if we fit a stan model to the sampled data and predict ZFYA??????
# THIS WILL BE A NEW WAY TO COMPUTE COUNTERFACTUALS
# NEW WAY
#G0 <- mean(la_law$g0)
#ETA_U_G <- mean(la_law$eta_u_g)
#ETA_A_G <- colMeans(la_law$eta_a_g)
#
#L0 <- mean(la_law$l0)
#ETA_U_T <- mean(la_law$eta_u_t)
#ETA_A_T <- colMeans(la_law$eta_a_t)
#
#ETA_U_Z <- mean(la_law$eta_u_z)
#ETA_A_Z <- colMeans(la_law$eta_a_z)
#
#Y0 <- mean(la_law$y0)
#ETA_U_Y <- mean(la_law$eta_u_y)
#ETA_A_Y <- colMeans(la_law$eta_a_y)
#
#SIGMA_G <- mean(la_law$sigma_g)
library(rstan)

n_samp <- nrow(outSamp)
ne <- nrow(lawTest)
#sense_cols_samp <- c("black","white","female","male")
k_samp = length(sense_cols) #sense_cols_samp)

law_stan_for_test <- list(N = ne, K = k_samp, a = data.matrix(lawTest[,sense_cols]), 
                          ugpa = lawTest[,c("UGPA")], lsat = round(lawTest[,c("LSAT")]),
                          #zfya = lawTest[,c("ZFYA")], pass = lawTest[,c("first_pf")],
                          ugpa0 = G0, eta_u_ugpa = ETA_U_G, eta_a_ugpa = ETA_A_G,
                          lsat0 = L0, eta_u_lsat = ETA_U_T, eta_a_lsat = ETA_A_T,
                          #            eta_u_zfya = ETA_U_Z, eta_a_zfya = ETA_A_Z,
                          #pass0 = Y0, eta_u_pass = ETA_U_Y, eta_a_pass = ETA_A_Y,
                          sigma_g = SIGMA_G)
                          # HERERE!!! Also include ZYFA and PASS!!!
                         
fit_for_test <- stan(file = 'law_school_only_u.stan', data = law_stan_for_test, iter = 2000, chains = 1, verbose = TRUE)

# Extract information

la_for_test <- extract(fit_for_test, permuted = TRUE)
u_for_test <- t(la_for_test$u)

# GET epsilons!
# G_epsilons
# ----------
G_EPS <- matrix(, nrow=nrow(data.matrix(lawTest[,c("UGPA")])), ncol=1000)
for(column in 1:1000) {
  G_EPS[,column] <- data.matrix(lawTest[,c("UGPA")]) - G0 - (ETA_U_G * u_for_test[,column]) - (data.matrix(lawTest[,sense_cols]) %*% data.matrix(ETA_A_G))
}

# L_epsilons
# ----------
L_EPS <- matrix(, nrow=nrow(data.matrix(lawTest[,c("UGPA")])), ncol=1000)
for(column in 1:1000) {
  L_EPS[,column] <- ppois(data.matrix(lawTest[,c("LSAT")]), exp(L0 + (ETA_U_T * u_for_test[,column]) +  (data.matrix(lawTest[,sense_cols]) %*% data.matrix(ETA_A_T))))
}


sample_GPA <- function(u, a, eps) {
  GPA_m <- G0 + (ETA_U_G * u) + (a %*% ETA_A_G)
  #eps <- rnorm(1,mean=0,sd=SIGMA_G)
  #return(list(GPA_m,eps))
  return(GPA_m + eps)
}

sample_LSAT <- function(u, a, eps) {
  #eps <- runif(1)
  #LSAT_m <- qpois( eps, exp( L0 + ETA_U_T * u + a %*% ETA_A_T ) )
  LSAT_m <- exp( L0 + ETA_U_T * u + a %*% ETA_A_T )
  sample <- qpois( eps, LSAT_m )
  return(sample)#list(LSAT_m,eps))
}


# HERERERERERERERERERERERERERERERER
ATE = data.matrix(lawTest[,sense_cols])
ATE_swap <- ATE
temp <- ATE_swap[,8] # white
ATE_swap[,8] <- ATE_swap[,3] # black
ATE_swap[,3] <- temp
NE <- nrow(data.matrix(lawTest[,c("UGPA")]))
NS <- 1000

GPA_dist <- matrix(, nrow=NE, ncol=1000)
LSAT_dist<- matrix(, nrow=NE, ncol=1000)
for (i in 1:NE) {
  set.seed(i)
  g_eps_swap <- rnorm(1000, mean=0, sd=SIGMA_G)
  l_eps_swap <- runif(1000)
  GPA_dist[i,] <- sample_GPA(u_for_test[i,], ATE_swap[i,], g_eps_swap) #G_EPS[i,])
  LSAT_dist[i,] <- sample_LSAT(u_for_test[i,], ATE_swap[i,], l_eps_swap)
}











# OLD WAY BELOW
# -------------
library(rstan)

n_samp <- nrow(outSamp)
ne_samp<- nrow(outSampTe)
sense_cols_samp <- c("black","white","female","male")
k_samp = length(sense_cols_samp)

law_stan_samp_dat <- list(N = n_samp, K = k_samp, a = data.matrix(outSamp[,sense_cols_samp]), 
                     ugpa = outSamp[,c("UGPA")], lsat = outSamp[,c("LSAT")],
                     N_TE = ne_samp, a_TE = data.matrix(outSampTe[,sense_cols_samp]),
                     ugpa_TE = outSampTe[,c("UGPA")], lsat_TE = outSampTe[,c("LSAT")])
                     

fit_law_samp_te <- stan(file = 'law_school_on_samp.stan', data = law_stan_samp_dat, iter = 2000, chains = 1, verbose = TRUE)
# Extract information

la_law_samp_te <- extract(fit_law_samp_te, permuted = TRUE)
u_te_samp <- colMeans(la_law_samp_te$u_TE)
u_samp1   <- colMeans(la_law_samp_te$u)

ne_samp_swap<- nrow(outSampTeSwap)
law_stan_samp_swap_dat <- list(N = n_samp, K = k_samp, a = data.matrix(outSamp[,sense_cols_samp]), 
                          ugpa = outSamp[,c("UGPA")], lsat = outSamp[,c("LSAT")],
                          N_TE = ne_samp_swap, a_TE = data.matrix(outSampTeSwap[,sense_cols_samp]),
                          ugpa_TE = outSampTeSwap[,c("UGPA")], lsat_TE = outSampTeSwap[,c("LSAT")])


fit_law_samp_swap <- stan(file = 'law_school_on_samp.stan', data = law_stan_samp_swap_dat, iter = 2000, chains = 1, verbose = TRUE)
la_law_samp_swap <- extract(fit_law_samp_swap, permuted = TRUE)
u_samp2        <- colMeans(la_law_samp_swap$u)
u_te_samp_swap <- colMeans(la_law_samp_swap$u_TE)

outSamp$uu         <- u_samp1
outSamp$uu         <- u_samp2
outSampTe$uu       <- u_te_samp
outSampTeSwap$uu   <- u_te_samp_swap

model_u1 <- lm(ZFYA ~ uu + 1, data=outSamp)
model_u2 <- lm(ZFYA ~ uu + 1, data=outSamp)

predict_and_fair <- function(model1, model2, dataTest, dataSwap) {
  pred_te   <- predict(model1, newdata=dataTest)#, type="response")
  pred_swap <- predict(model2, newdata=dataSwap)#, type="response")
  
  dataTest$pred_zfya <- pred_te
  dataTest$type <- 0
  
  dataSwap$pred_zfya <- pred_swap
  dataSwap$type <- 1
  
  total <- rbind(dataTest, dataSwap)
  total$type <- factor(total$type, labels=c("original", "swapped"))
  
  p <- ggplot(total, aes(pred_zfya, linetype = type)) + geom_density() + theme_bw()
  rmse_te <- sqrt( sum( (dataTest$pred_zfya - dataTest$zfya)^2 ) / nrow(dataTest) )
  return(list("plot"=p,rmse_te="rmse_te"))
}

res = predict_and_fair(model_u1, model_u2, outSampTe, outSampTeSwap)


# OLD WAY ABOVE
# -------------







# VAE #
# --- #
vae_train_data <- read.csv("vae_fix_generated_law_train_samp_beta100.0.csv", header = FALSE, strip.white = TRUE)
vae_test_data  <- read.csv("vae_fix_generated_law_test_samp_beta100.0.csv", header = FALSE, strip.white = TRUE)

vae_test_swap <- vae_test_data
vae_test_swap$V2 <- vae_test_swap$V3
vae_test_swap$V3 <- NULL
vae_test_data$V3 <- NULL

model_vae <- lm(V1 ~ V2 + 1, data=vae_train_data)
#pred_u_unaware   <- predict(model_u_unaware, newdata=outSampTe)#, type="response")
#pred_u_unaware_swap <- predict(model_u_unaware, newdata=outSampTeSwap)#, type="response")
#outSampTe$pred_unaware <- pred_u_unaware
#outSampTe$type <- 0
#outSampTeSwap$pred_unaware <- pred_u_unaware_swap
#outSampTeSwap$type <- 1
#total <- rbind(outSampTe, outSampTeSwap)
#total$type <- factor(total$type, labels=c("original", "swapped"))
#ggplot(total, aes(pred_unaware, linetype = type)) + geom_density() + theme_bw()



predict_and_fair_vae <- function(model, dataTest, dataSwap) {
  pred_te   <- predict(model, newdata=dataTest)#, type="response")
  pred_swap <- predict(model, newdata=dataSwap)#, type="response")
  
  dataTest$pred <- pred_te
  dataTest$type <- 0
  
  dataSwap$pred <- pred_swap
  dataSwap$type <- 1
  
  total <- rbind(dataTest, dataSwap)
  total$type <- factor(total$type, labels=c("original", "swapped"))
  
  p <- ggplot(total, aes(pred, linetype = type)) + geom_density() + theme_bw()
  rmse_te <- sqrt( sum( (dataTest$pred - dataTest$V1)^2 ) / nrow(dataTest) )
  return(list("plot"=p,rmse_te="rmse_te"))
}

res = predict_and_fair_vae(model_vae, vae_test_data, vae_test_swap)
res$rmse_te
res$p























######## DEPRICATED CODE BELOW ##########












ggplot(outSampTe, aes(race, pred)) + geom_boxplot()
ggplot(outSampTeSwap, aes(race, pred)) + geom_boxplot()




ggplot(outSampTe, aes(pred)) + geom_density()
ggplot(outSampTeSwap, aes(pred)) + geom_density()



model_u_samp <- glm(bar_pass ~ . + 1,family=binomial(link='logit'), data=outSamp)
pred_u_samp_te   <- predict(model_u_samp, newdata=outSampTe, type="response")
pred_u_samp_swap <- predict(model_u_samp, newdata=outSampTeSwap, type="response")
pred_u_samp_te_t <- function(t) ifelse(pred_u_samp_te > t , 1,0)
pred_u_samp_samp_t <- function(t) ifelse(pred_u_samp_swap > t , 1,0)






D <- data.frame(pred = pred_u_samp_te_t(0.5), prob=pred_u_samp_te)
D2<- data.frame(pred = pred_u_samp_samp_t(0.5), prob=pred_u_samp_swap)
ppp <- pred_u_samp_te
qqq <- pred_u_samp_swap
M <- data.frame(ppp = ppp[lawSampleTe$male == 1], qqq=qqq[lawSampleTe$male == 1])
F <- data.frame(ppp = ppp[lawSampleTe$female == 1], qqq=qqq[lawSampleTe$female == 1])

ggplot(M, aes(ppp)) + stat_density(position="identity",geom="line")
ggplot(M, aes(qqq)) + stat_density(position="identity",geom="line")


ggplot(D, aes(pred)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))
ggplot(D2, aes(pred)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))

ggplot(D, aes(prob)) + stat_density(position="identity",geom="line")
ggplot(D2, aes(prob)) + stat_density(position="identity",geom="line")

ggplot(D2, aes(diff)) + stat_density(position="identity",geom="line")

lawSampleTe$black == 1

lawSampleTe$male == 1

data <- data[data$sex == "Male",]

# fit unaware classifier to sampled data

sense_cols





X_UNAWARE <- data.frame(z=z, t=t, g=g, y=y)
X_UNAWARE_TE <- data.frame(z=z_te, t=t_te, g=g_te)
model_UNA <- glm(y ~ . + 1,family=binomial(link='logit'), data=X_UNAWARE)
pred_UNA <- predict.glm(model_UNA, type = "response")
pred_UNA_te <- predict(model_UNA, newdata=X_UNAWARE_TE, type = "response")
pred_UNA_thres <- function(t) ifelse(pred_UNA > t , 1,0)
pred_UNA_te_thres <- function(t) ifelse(pred_UNA_te > t , 1,0)
#confusionMatrix(pred_u_thres(0.5),y)
conf_UNA <- confusionMatrix(pred_UNA_te_thres(0.5),y_te)
conf_UNA$byClass["Balanced Accuracy"]
F1_UNA <- as.numeric(2*(conf_UNA$byClass["Precision"]*conf_u$byClass["Recall"])/(conf_u$byClass["Precision"]+conf_u$byClass["Recall"]))
F1_UNA













#---------------------#
# UNTESTED CODE BELOW #


fair_train <- read.csv("fair_class_law_train_samp.csv", header = FALSE, strip.white = TRUE)
fair_train$y <- lawSample$first_pf
fair_test <- read.csv("fair_class_law_test_samp.csv", header = FALSE, strip.white = TRUE)
fair_test$y <- lawSampleTe$first_pf

fair_swap <- fair_test
fair_swap$V1 <- fair_swap$V2
fair_swap$V2 <- NULL
fair_test$V2 <- NULL
ggplot(fair_test, aes(V1)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))
ggplot(fair_swap, aes(V1)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))






model_vae <- glm(y ~ V1 + 1, family=binomial(link='logit'), data=fair_train)


vae_train_data <- read.csv("vae_generated_law_train_samp_beta200.0.csv", header = FALSE, strip.white = TRUE)
vae_test_data  <- read.csv("vae_generated_law_test_samp_beta200.0.csv", header = FALSE, strip.white = TRUE)

vae_test_swap <- vae_test_data
vae_test_swap$V2 <- vae_test_swap$V3
vae_test_swap$V3 <- NULL
vae_test_data$V3 <- NULL

model_vae <- glm(V1 ~ V2 + 1, family=binomial(link='logit'), data=vae_train_data)

pred_vae <- predict.glm(model_vae, type = "response")
pred_vae_te <- predict.glm(model_vae, newdata=vae_test_data, type = "response")
pred_vae_swap <- predict.glm(model_vae, newdata=vae_test_swap, type = "response")


pred_vae_thres <- function(t) ifelse(pred_vae > t , 1,0)
pred_vae_te_thres <- function(t) ifelse(pred_vae_te > t , 1,0)
pred_vae_swap_thres <- function(t) ifelse(pred_vae_swap > t , 1,0)


#confusionMatrix(pred_f_thres(0.5),y)
conf_vae <- confusionMatrix(pred_vae_te_thres(0.5),vae_test_data$V2)#y_te)
conf_vae$byClass["Balanced Accuracy"]
F1_vae <- as.numeric(2*(conf_vae$byClass["Precision"]*conf_vae$byClass["Recall"])/(conf_vae$byClass["Precision"]+conf_vae$byClass["Recall"]))
F1_vae

conf_vae_swap <- confusionMatrix(pred_vae_swap_thres(0.5),vae_test_swap$V2)#y_te)
conf_vae_swap$byClass["Balanced Accuracy"]
F1_vae_swap <- as.numeric(2*(conf_vae_swap$byClass["Precision"]*conf_vae_swap$byClass["Recall"])/(conf_vae_swap$byClass["Precision"]+conf_vae_swap$byClass["Recall"]))
F1_vae_swap

vae_test_data$pred <- pred_vae_te_thres(0.5)
vae_test_swap$pred <- pred_vae_swap_thres(0.5)
vae_test_data$prob <- pred_vae_te
vae_test_swap$prob <- pred_vae_swap

ggplot(vae_test_data, aes(pred)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))
ggplot(vae_test_swap, aes(pred)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))

ggplot(vae_test_data, aes(prob)) + stat_density(position="identity",geom="line")
ggplot(vae_test_swap, aes(prob)) + stat_density(position="identity",geom="line")

#---------------------#








fixed <- rep(1, 4358)
confusionMatrix(fixed,y_te)



# use potential outcomes-like model to test how well model fits data, by generating Y(a')
# can just use means instead of sampling
# try just on test set for now

#####output_te <- data.frame(bar_pass_fair = y_te,
#####                        location = l_te,
#####                        UGPA = g_te, 
#####                        LSAT = t_te, 
#####                        ZFYA = z_te,
#####                        amerind = lawTest$amerind,
#####                        asian   = lawTest$asian  ,
#####                        black   = lawTest$black  ,
#####                        hisp    = lawTest$hisp   ,
#####                        mexican = lawTest$mexican,
#####                        other   = lawTest$other  ,
#####                        puerto  = lawTest$puerto ,
#####                        white   = lawTest$white  ,
#####                        female  = lawTest$female,
#####                        male    = lawTest$male,
#####                        u_hat = u_te_hat)
#####
#####lawTest[lawTest$amerind == 1,]
#####G0 <- mean(la_law$g0)
#####ETA_U_G <- mean(la_law$eta_u_g)
#####ETA_A_G <- colMeans(la_law$eta_a_g)
#####
#####L0 <- mean(la_law$l0)
#####ETA_U_T <- mean(la_law$eta_u_t)
#####ETA_A_T <- colMeans(la_law$eta_a_t)
#####
#####ETA_U_Z <- mean(la_law$eta_u_z)
#####ETA_A_Z <- colMeans(la_law$eta_a_z)
#####
#####Y0 <- mean(la_law$y0)
#####ETA_U_Y <- mean(la_law$eta_u_y)
#####ETA_A_Y <- colMeans(la_law$eta_a_y)
#####
#####G <- G0 + ETA_U_G * u_te_hat[lawTest$amerind == 1] +  ETA_A_G[2] # what if amerind -> asian
#####TT <- exp(L0 + ETA_U_T * u_te_hat[lawTest$amerind == 1] + ETA_A_T[2])
#####Z <- ETA_U_Z * u_te_hat[lawTest$amerind == 1] = ETA_A_Z[2]
#####Y_amerind_asian <- 1/(1+exp(-(Y0 + ETA_U_Y * u_te_hat[lawTest$amerind == 1] + ETA_A_Y[2])))
#####
#####fairpred_pen <- function(G0, ETA_U_G, ETA_A_G, L0, ETA_U_T, ETA_A_T, ETA_U_Z, ETA_A_Z, Y0, ETA_U_Y, ETA_A_Y, inds, ix) {
#####  
#####  
#####  G <- G0 + ETA_U_G * u_te_hat[inds] +  ETA_A_G[ix] # what if amerind -> asian
#####  TT <- exp(L0 + ETA_U_T * u_te_hat[inds] + ETA_A_T[ix])
#####  Z <- ETA_U_Z * u_te_hat[inds] = ETA_A_Z[ix]
#####  YY <- 1/(1+exp(-(Y0 + ETA_U_Y * u_te_hat[inds] + ETA_A_Y[ix])))
#####  return(YY)
#####  
#####}



