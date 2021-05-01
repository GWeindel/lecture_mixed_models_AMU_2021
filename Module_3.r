#To run this notebook :
#conda create -n brms r-rstan r-brms

#$ conda create -n brms r-rstan r-brms notebook
#$ conda activate brms
#$ R 
#> remove.packages(c("StanHeaders", "rstan")) #solution from https://discourse.mc-stan.org/t/error-s-during-compilation-with-rstan-in-hpc/21948/3

#># Compile packages using all cores 
#>Sys.setenv(MAKEFLAGS = paste0("-j",parallel::detectCores()))

#>install.packages(c("StanHeaders","rstan"),type="source")
#>install.packages("devtools")
#>devtools::install_github("IRkernel/IRkernel")
#>IRkernel::installspec()
#>q()
#$ conda install rise


n_rep = 100 #100 tests repeated
matrix_ci_values = NULL
for (i in 1:n_rep){
    grp1 = rnorm(10, 0, 1)
    grp2 = rnorm(10, -1, 1)#H1 is true
    matrix_ci_values <- rbind(matrix_ci_values,matrix(c(t.test(grp1,grp2)$conf.int[1],t.test(grp1,grp2)$conf.int[2]), ncol=2,nrow=1) )
}
ci_corr = matrix_ci_values[(1 >= matrix_ci_values[,1]) & (1 <= matrix_ci_values[,2]),]#true included in CI
ci_incorr = matrix_ci_values[(1 >= matrix_ci_values[,2]) | (1 <= matrix_ci_values[,1]),]#true not included in CI

#Graphical  parameters
plot.new()
plot.window(xlim=c(-5,5), ylim=c(0.5, 100+.5), ylab="a")
axis(1, at=seq(-3, 3, by=1))
axis(2, at=seq(0, 100, by=20))

#Plotting correct CI vs incorrect
arrows(ci_corr[,1], 1:nrow(ci_corr), ci_corr[,2], 1:nrow(ci_corr), code=3, length=0.05, angle=90)
arrows(ci_incorr[,1], nrow(ci_corr):(nrow(ci_incorr)+nrow(ci_corr)), ci_incorr[,2],  
       nrow(ci_corr):(nrow(ci_incorr)+nrow(ci_corr)), code=3, length=0.05, angle=90,col="red")
#Plotting true effect as a line
abline(v=1)

# Code adapted from R. McElreath's book (see ressources at the end of the module)
grid = 50
p_grid = seq(from=0, to=1, length.out = grid) #Discretizing the parameter space, aka grid approximation
prior = rep(1,grid)#Flat uninformative prior
likelihood = dbinom(28, size=40, prob=p_grid)
posterior = (likelihood*prior)/sum(likelihood*prior)



plot(p_grid, prior, type="b")

plot(p_grid, likelihood, type="b")

plot(p_grid, posterior, type="b")

# Code adapted from R. McElreath's book (see ressources at the end of the module)
grid = 50
p_grid = seq(from=0, to=1, length.out = grid)
prior = ifelse(p_grid <= .5, 0, 1)#informative prior we know coin is biased towards head, but agnostic on how much
likelihood = dbinom(28, size=40, prob=p_grid)
posterior = (likelihood*prior)/sum(likelihood*prior)



plot(p_grid, prior, type="b")

plot(p_grid, likelihood, type="b")

plot(p_grid, posterior, type="b")

# Code taken from R. McElreath's book (see ressources at the end of the module)
grid = 50
p_grid = seq(from=0, to=1, length.out = grid) #Discretizing the parameter space, aka grid approximation
prior = ifelse(p_grid > .5, 0, 1)#improper prior
likelihood = dbinom(28, size=40, prob=p_grid)
posterior = (likelihood*prior)/sum(likelihood*prior)

plot(p_grid, prior, type="b")

plot(p_grid, likelihood, type="b")

plot(p_grid, posterior, type="b")

set.seed(234)
options(mc.cores = parallel::detectCores())#Detects and stores the number of cores on your computer
library(brms)#Library for Bayesian estimation of LMMs (amongst other models)
library(shinystan)#Library we are going to use to inspect our MCMC procedures

dummy_data = data.frame(y=rnorm(100, 100,15))#e.g. Normal with known mean of 100 and sd of 15

priors = c(prior(normal(0, 1000), class = Intercept),#The prior we have on the mean
    prior(cauchy(0, 1000), class = sigma))#The prior we have on the SD a fat-tailed truncated distribution
values = seq(from=-5000, to=5000, length.out = 50)
plot(values,dnorm(values,0,1000),type="b",ylab="density")
lines(values[26:50],dcauchy(values[26:50],0,1000),type="b",col="red")

a_mean = brm(y ~ 1, data=dummy_data, 
             family="normal", prior = priors, chain = 2, warmup=500, iter=1000)#A shotgun to a knife fight

print(stancode(a_mean))

mcmc_plot(a_mean, type="trace")

summary(a_mean)

options(browser = "firefox")
launch_shinystan(a_mean)

a_mean_prior_only = brm(y ~ 1, data=dummy_data, sample_prior = "only",
             family="normal", prior = priors, chain = 4, cores=5, warmup=500, iter=1000)
pp_check(nsamples = 100, a_mean_prior_only)

priors = c(prior(normal(100, 10), class = Intercept),#The prior we have on the mean
    prior(cauchy(15, 5), class = sigma))#The prior we have on the SD a fat-tailed truncated distribution
a_mean_prior_only = brm(y ~ 1, data=dummy_data, sample_prior = "only",
             family="normal", prior = priors, chain = 4, cores=5, warmup=500, iter=1000)
pp_check(nsamples = 10, a_mean_prior_only)

a_mean = brm(y ~ 1, data=dummy_data, 
             family="normal", prior = priors, chain = 4, cores=5, warmup=500, iter=1000)
pp_check(nsamples = 100, a_mean)

improper_priors = c(prior(normal(-150, 1), class = Intercept),#The (wrong) prior we have on the mean
    prior(normal(1, 1), class = sigma))#Bad prior on variance
a_mean_improper = brm(y ~ 1, data=dummy_data, #Note that the model is slower to fit
             family="normal", prior = improper_priors, chain = 4, cores=5, warmup=500, iter=1000)
pp_check(nsamples = 100, a_mean_improper)

summary(a_mean_improper)

slightly_improper_priors = c(prior(normal(-150, 100), class = Intercept),#We increased the variance around our prior
    prior(cauchy(1, 1), class = sigma))#From normal to Cauchy
a_mean_slightly_improper = brm(y ~ 1, data=dummy_data, #Note that the model is slower to fit
             family="normal", prior = slightly_improper_priors, chain = 4, cores=5, warmup=500, iter=1000)
pp_check(nsamples = 100, a_mean_slightly_improper)

library(MASS)
mean_wm = 6
sd_wm = 1
n_participants = 15

varcovmat = matrix(c(1, .33, .33, .33, .33, 1, .33, .33, .33, .33,1,.33, .33, .33, .33,1), nrow=4)
data = data.frame()
for(i in 1:n_participants){
    re = mvrnorm(n=1, mu=c(0, 0, 0, 0), Sigma=varcovmat)
    mu_i = re[1] * sd_wm + mean_wm
    b_age = re[2] *(sd_wm*2) - (sd_wm*4) #b_age_j
    b_task = re[3] * sd_wm + -(sd_wm*2)#b_task_j
    b_age_task = re[4] * (sd_wm/2) - (sd_wm*2)#b_agetask_j
    age_tested = runif(runif(1, 5,15), 1, 99)
    for (age in age_tested){# for loop in for loops is especially dirty but more transparent
        age = age/100#We recode age for scale purpose
        for (task in c(0,1)){
            ntrials = runif(1, 50, 150)
            subdata = data.frame(trial=1:ntrials)
            mu = mu_i+b_age*age+b_task*task+b_age_task*age*task
            subdata$wm = rnorm(ntrials, mu, sd=sd_wm)
            subdata$participant = as.character(i)
            subdata$age = age 
            subdata$task = task
            data = rbind(data, subdata)
        }
    }
}
means_ = aggregate(data$wm, FUN=mean, 
          by=list(age=data$age, task=data$task, participant = data$participant))#pre-averaging at first

#We let brms use the default priors out of commodity
blm0 = brm(x ~ age * task, data=means_, 
             family="normal", chain = 4, cores=4, warmup=500, iter=1000)

print(stancode(blm0))

blm0

options(repr.plot.width=8, repr.plot.height=8, repr.plot.res = 200)#hidden code for display size
plot(blm0)
options(repr.plot.width=8, repr.plot.height=5, repr.plot.res = 200)#hidden code for display size

pp_check(nsamples = 100, blm0)

library(ggplot2)
fitted <- fitted(blm0)[, 1]
resid <- residuals(blm0)[, 1]

qplot(fitted, resid)

hist(resid)

pairs(blm0)

#install.packages('tidybayes') see http://mjskay.github.io/tidybayes/articles/tidy-brms.html
library(dplyr)
library(ggplot2)
library(tidybayes)
library(modelr)
means_ %>%
  group_by(task) %>%
  data_grid(age = seq_range(age, n = 100)) %>%
  add_fitted_draws(blm0, n = 50) %>%
  ggplot(aes(x = age, y = x, color = ordered(task))) +
  geom_line(aes(y = .value, group = paste(task, .draw)), alpha = .1) +
  geom_point(data = means_) 

get_variables(blm0)

intercept = blm0 %>% spread_draws(b_Intercept)
slope_age = blm0 %>% spread_draws(b_age)
wm_at_age_70 = intercept$b_Intercept + slope_age$b_age*.70
hist(wm_at_age_70,breaks=100)

priors = c(prior(normal(6, 2), class = Intercept),#The prior we have on the intercept
    prior(cauchy(1, 5), class = sigma),#The prior we have on the SD a fat-tailed truncated distribution
    prior(normal(0,10), class = b, coef=age),#prior on the age effect
    prior(normal(0,10), class = b, coef=task),#prior on the task effect
    prior(normal(0,10), class = b, coef=age:task),#prior on the interaction between age and task
    prior(lkj(2), class = cor))#prior on the correlation between RE

#The following is too slow to be run in class so I just recover it from a previous fit
blm1 = brm(wm ~ age * task + (age * task | participant), data=data, prior = priors,
             family="normal", chain = 8, cores=8, warmup=1000, iter=1200, file="blm1")

options(repr.plot.width=8, repr.plot.height=8, repr.plot.res = 200)#hidden code for display size
plot(blm1)
options(repr.plot.width=8, repr.plot.height=5, repr.plot.res = 200)#hidden code for display size

summary(blm1)

pp_check(nsamples = 100, blm1)

fitted <- fitted(blm1)[, 1]
resid <- residuals(blm1)[, 1]

qplot(fitted, resid)

hist(resid)

pairs(blm1, pars = "^sd_")#looking at the scatterplotes for the SD parameters

get_variables(blm1)

data = read.csv("data.csv", sep='')
data$rt = data$rt * 1000 #from s to ms
data$encoding = as.character(data$encoding)
data$participant = as.character(data$participant)
data$ease = data$ease-1
data$encoding = ifelse(data$encoding == "standard", 0, 1)
data$response = ifelse(data$response == "upper",1, 0)

require(lattice)#needed for histogram plot
#Histogram for first five participants
histogram( ~ rt | participant ,data = data[data$participant %in% unique(data$participant)[1:5],], breaks=20) 

means = aggregate(data$rt, FUN=mean, 
          by=list(participant=data$participant, ease=data$ease, encoding=data$encoding)) #getting the mean for each experimental cell X participant
grand_means = aggregate(means$x, FUN=mean, 
          by=list(ease=means$ease, encoding=means$encoding)) #getting the mean for each experimental cell
options(repr.plot.width=12, repr.plot.height=5, repr.plot.res = 400)
ggplot(grand_means, aes(x=ease, y=x, color=ordered(encoding))) +
  geom_line()+
  geom_point(size=4) +
  scale_size_area() +
  xlab("easiness")+
  ylab("RT")
options(repr.plot.width=8, repr.plot.height=5, repr.plot.res = 200)#hidden code for display size

priors = c(prior(normal(700, 200), class = Intercept),#The prior we have on the intercept
    prior(cauchy(100, 50), class = sigma),#The prior we have on the SD a fat-tailed truncated distribution
    prior(normal(0,100), class = b, coef=ease),#prior on the age effect
    prior(normal(0,100), class = b, coef=encoding),#prior on the task effect
    prior(normal(0,100), class = b, coef=ease:encoding),#prior on the task effect
    prior(lkj(2), class = cor))#prior on the correlation between RE

#The following is too slow to be run in class so I just recover it from a previous fit
blm2_rt = brm(rt ~ ease * encoding + (ease*encoding|participant), data=data, prior = priors,
             family="normal", chain = 4, cores=4, warmup=1000, iter=2000, file="blm2_rt")

blm2_rt

priors = c(prior(normal(6, 1), class = Intercept),#The prior we have on the intercept
    prior(cauchy(.5, 1), class = sigma),#The prior we have on the SD a fat-tailed truncated distribution
    prior(normal(0,1), class = b, coef=ease),#prior on the age effect
    prior(normal(0,1), class = b, coef=encoding),#prior on the task effect
    prior(normal(0,1), class = b, coef=ease:encoding),#prior on the task effect
    prior(lkj(2), class = cor))#prior on the correlation between RE

#The following is too slow to be run in class so I just recover it from a previous fit
blm2_lognorm_rt = brm(rt ~ ease * encoding + (ease*encoding|participant), data=data, prior = priors,
             family="lognormal", chain = 4, cores=4, warmup=1000, iter=2000, file="blm2_lognorm_rt")
# Equivalent to : brm(log(rt) ~ ease * encoding + (ease*encoding|participant),family="normal",...)

blm2_lognorm_rt

priors = c(prior(normal(1, 1), class = Intercept),#The prior we have on the intercept
    prior(normal(0,.5), class = b, coef=ease),#prior on the age effect
    prior(normal(0,.5), class = b, coef=encoding),#prior on the task effect
    prior(normal(0,.5), class = b, coef=ease:encoding),#prior on the task effect
    prior(lkj(2), class = cor))#prior on the correlation between RE

blm2_correct = brm(response ~ ease * encoding + (ease*encoding|participant), data=data, prior = priors,
  family = bernoulli(), chain = 4, cores=4, warmup=1000, iter=2000, file="blm1_acc")

summary(blm2_correct)

inv.logit = function(x){
    inv = exp(x)/(1+exp(x))
    return(inv)
}

intercept = blm2_correct %>% spread_draws(b_Intercept)
hist(inv.logit(intercept$b_Intercept),breaks=100)
