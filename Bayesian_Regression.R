library(rstan)
library(rstanarm)
library(ggplot2)
library(ggpubr)
library(bayesplot)
# library(bayestestR)

# this option uses multiple cores if they're available
options(mc.cores = parallel::detectCores())

qplot(speed, dist, data=cars) + geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))
test <- lm(dist~speed, data=cars)
summary(test)
par(mfrow = c(2, 2))
plot(test)


glm_post1 <- stan_glm(dist~speed, data=cars, family=gaussian)
stan_trace(glm_post1, pars=c("(Intercept)","speed","sigma"))
stan_plot(glm_post1, pars=c("(Intercept)","speed","sigma"))


summary(glm_post1)
pp_check(glm_post1)

# another way to look at posterior predictive checks
ppc_intervals(
 y = cars$dist,
 yrep = posterior_predict(glm_post1),
 x = cars$speed)

stan_hist(glm_post1, pars=c("(Intercept)","speed","sigma"), bins=50)

# (dens_sep <- stan_dens(glm_post1, separate_chains = TRUE, alpha = 0.3))
# dens_sep + scale_fill_manual(values = c("red", "blue", "green", "black"))
# (dens_sep_stack <- stan_dens(glm_post1, pars = "speed", alpha = 0.5,
#                              separate_chains = TRUE, position = "stack"))


post_samps_speed <- as.data.frame(glm_post1, pars=c("speed"))[,"speed"]
mn_speed <- mean(post_samps_speed) # posterior mean 
ci_speed <- quantile(post_samps_speed, probs=c(0.05, 0.95)) # posterior 90% interval 

glm_fit <- glm(dist~speed, data=cars, family=gaussian)
summary(glm_fit)

prior_summary(glm_post1)

posterior_vs_prior(glm_post1, group_by_parameter = TRUE, pars=c("(Intercept)"))
posterior_vs_prior(glm_post1, group_by_parameter = TRUE, pars=c("speed","sigma"))

glm_post2 <- stan_glm(dist~speed, data=cars, family=gaussian, prior=normal(2, 0.5, autoscale=FALSE))
posterior_vs_prior(glm_post2, pars=c("speed"), group_by_parameter = TRUE)
summary(glm_post2)