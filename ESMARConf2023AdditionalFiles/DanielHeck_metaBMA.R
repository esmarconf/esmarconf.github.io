###########################################################
##  metaBMA: Bayesian Model Averaging for Meta-Analysis
##
##  Author:      Daniel W. Heck
##  Institution: Philipps-Universität Marburg
##  Email:       daniel.heck@uni-marburg.de
##  Date:        February, 2023
##
##  Download script: www.dwheck.de/files/metaBMA-tutorial.R
##
###########################################################

##  install metaBMA package from CRAN:
# install.packages("metaBMA")

##  install newest version of metaBMA from GitHub:
# install.packages("devtools")
# devtools:::install_github("danheck/metaBMA")

# load packages
library(metaBMA)
library(metafor)

# random seed for replicability
set.seed(123)

# load example data set
data(towels)
towels

# forest plot of the metafor package
forest(towels$logOR, sei = towels$SE)



###########################################################
##  SPECIFY PRIOR DISTRIBUTIONS
###########################################################

# What do we expect for the overall effect size ("mu")?


# prior under the alternative hypothesis:
prior_H1 <- prior(family = "norm",
                  param = c(mean = 0, sd = 0.3))
plot(prior_H1)


# one-sided alternative hypothesis:
prior_H1_positive <- prior(family = "norm",
                           param = c(mean = 0, sd = .3),
                           lower = 0)
plot(prior_H1_positive, from = -.5)


# informed prior for effect sizes in social psychology
prior_H1_informed <- prior(family = "t",
                           param = c(0.35, 0.102, 3),
                           lower = 0)
plot(prior_H1_informed, from = -.5)



###########################################################
##  FIT MODELS
###########################################################

# structure of the data set
towels

# fixed-effects model
fit_f <- meta_fixed(
  y = logOR,
  SE = SE,
  label = study,
  data = towels,
  d = prior(family = "norm",
            param = c(mean = 0, sd = 0.3),
            lower = 0)
)

# results
plot_forest(fit_f)
plot_posterior(fit_f)
fit_f


# random-effects model
fit_r <- meta_random(
  y = logOR,
  SE = SE,
  label = study,
  data = towels,
  d = prior(family = "norm",
            param = c(mean = 0, sd = 0.3),
            lower=0),
  tau = prior(family = "t",
              param = c(location = 0, scale = 0.3, nu = 1),
              lower = 0)
)

plot_forest(fit_r)
plot_posterior(meta = fit_r,
               main = "Average effect size (mu)")
plot_posterior(meta = fit_r,
               parameter = "tau",
               main = "Heterogeneity (tau)")
fit_r



###########################################################
##  BAYESIAN MODEL AVERAGING
###########################################################

# Bayesian model averaging across fixed- & random-effects
fit_bma <- meta_bma(
  y = logOR,
  SE = SE,
  label = study,
  data = towels,
  d = prior(family = "norm",
            param = c(mean = 0, sd = 0.3),
            lower=0),
  tau = prior(family = "t",
              param = c(location = 0, scale = 0.3, nu = 1),
              lower = 0)
)

plot_forest(fit_bma)
plot_posterior(meta = fit_bma,
               parameter = "d",
               from = -.1, to = 1.4)
fit_bma



###########################################################
##  SEQUENTIAL PLOTS & FUTURE STUDIES
###########################################################

# how informative would one additional, future study be?
# (only working with newest version of metaBMA from GitHub)


# simulate 20 additional data sets
meta_sequential <- predicted_bf(fit_bma,
                                SE = 0.2,
                                sample = 20)

# plot sequential process of evidence updating
plot(meta_sequential, "BF.inclusion")

