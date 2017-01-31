######################################################################################################
# A model for the Compas data
#
# Summary: predict recidivism from attributes "race", "age", "gender" and "prior_counts".
# We do this by first explaining "priors_count" by a latent trait of "criminal propensity",
# which we will call U. Attributes "race" etc wil be considered not to be causes of
# "prior_counts" and recidivism Y. Instead, the connection is through a selection variable
# S which is also equal to 1 in this data. The postulated causal structure is:
#
# X -> S <- U -> prior_counts, U -> Y.
# 
# This is of course a highly simplified model, for which we never observe S = 0 and for
# which untestable assumptions about the "true" nature of U are raised. Here, we have the
# model
#
# - U ~ N(0, 1)
# - S ~ Bernoulli(logit(eta_s[1] + eta_s[2] * race + eta_s[3] * age + eta_s[4] * gender + u_s * U))
# - prior_counts ~ Poisson(exp(u_y0 + u_y * u))
#
# Once we infer U from this, we try to see how much training error we get from predicting 
# Y from U against predicting Y from the raw measurements of race, age, gender and prior_counts.

######################################################################################################
# Process the data, as in the propublic page

# install.packages("rstan") # may take a long time
library(dplyr)

raw_data <- read.csv("compas-scores-two-years.csv")
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')

df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))

df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))

######################################################################################################
# Establish priors for the model and format data so that it can be used in Stan

n <- nrow(df)

race <- rep(1, n); race[df$race == "Caucasian"] <- 0
age <- (df$age - mean(df$age)) / sd(df$age)
gender <- as.numeric(df$gender_factor) - 1

x <- cbind(rep(1, n), race, age, gender)
y <- df$priors_count
s <- rep(1, n)

# Prior for coefficients of covariates on S: Gaussian with this mean and variance

p <- ncol(x)
mu_eta <- rep(0, p)
sigma_eta <- rep(1, p)

# Prior for coefficient of U on S: Gaussian with this mean and variance

mu_us <- 0
sigma_us <- 1

# Prior for coefficient of U on priors_count: Gaussian with this mean and variance

mu_uy <- 0
sigma_uy <- 1

# Prior for intercept on equation for priors_count: Gaussian with this mean and variance

mu_y0 <- 0
sigma_y0 <- 1

# Run Stan

library(rstan)

compas_stan_dat <- list(N = n, K = p, x = x, s = s, y = y, 
                     mu_eta = mu_eta, sigma_eta = sigma_eta,
                     mu_us = mu_us, sigma_us = sigma_us, 
                     mu_uy = mu_uy, sigma_uy = sigma_uy, mu_y0 = mu_y0, sigma_y0 = sigma_y0)

fit <- stan(file = 'compas.stan', data = compas_stan_dat, iter = 1000, chains = 1, verbose = TRUE)

# Extract information

la <- extract(fit, permuted = TRUE)
u_hat <- colMeans(la$u)

######################################################################################################
# Now, check classification performance

library(randomForest)

y <- factor(df$two_year_recid)
x_unfair <- cbind(x[, 2:ncol(x)], df$prior_counts)
x_fair <- cbind(u_hat, df$priors_count)

unfair <- randomForest(x_unfair, y)
fair <- randomForest(x_fair, y)

cat("Confusion matrix for unfair classifier:")
unfair$confusion

cat("Confusion matrix for fair classifier:")
fair$confusion

