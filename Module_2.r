dnorm(100,100,15)#probability density function for an IQ of 100 given IQ~N(100,15)

dnorm(100,70,15)#Same but given IQ~N(70,15)

#This code was directly inspired by S. Kurz, see https://solomonkurz.netlify.app/post/make-model-diagrams-kruschke-style/
# But it's hidden to students because too complex and just for graphical purpose

library(tidyverse)
library(patchwork)
library(ggforce)
theme_set(theme_grey() +
            theme_void() +
            theme(plot.margin = margin(0, 5.5, 0, 5.5)))

# plot of a normal density
p1_1 <-
  tibble(x = seq(from = -3, to = 3, by = .01),
         d = (dnorm(x, 0, 1)) / max(dnorm(x, 0, 1))) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "indianred", size = 0) +
  annotate(geom = "text",
           x = .35, y = .2,
           label = "italic(mu)",
           size = 7, family = "Times", parse = TRUE) +
#  annotate(geom = "text",
#           x = 0, y = .6,
#           label = "italic(mu)*', '*italic(sigma)", 
#           size = 7, family = "Times", parse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))

# a chi-square density
p1_2 <-
  tibble(x = seq(from = 0, to = 5, by = .01),
         d = (dchisq(x, 3) / max(dchisq(x, 3)))) %>% 
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "indianred", size = 0) +
  annotate(geom = "text",
           x = 2.5, y = .2,
           label = "italic(sigma)",
           size = 7, family = "Times", parse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))

## an annotated arrow
# save our custom arrow settings
my_arrow <- arrow(angle = 20, length = unit(0.35, "cm"), type = "closed")
p2_1 <-
  tibble(x    = .5,
         y    = 1,
         xend = .5,
         yend = 0) %>%
  
  ggplot(aes(x = x, xend = xend,
             y = y, yend = yend)) +
  geom_segment(arrow = my_arrow) +
  xlim(0, 1)

p2_2 <-
  tibble(x    = 2.5,
         y    = 1,
         xend = 1,
         yend = 0) %>%
  
  ggplot(aes(x = x, xend = xend,
             y = y, yend = yend)) +
  geom_segment(arrow = my_arrow) +
  xlim(0, 4)

# density ofr indiv sub
p3 <-
  tibble(x = seq(from = -3, to = 3, by = .01),
         d = (dnorm(x, 0, 1)) / max(dnorm(x, 0, 1))) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "indianred", size = 0) +
  annotate(geom = "text",
           x = .35, y = .2,
           label = "Individual*' '*italic(j)",
           size = 7, parse=TRUE) +
  annotate(geom = "text",
           x = 0, y = .6,
           label = "italic(mu[j])*', '*italic(sigma[j])", 
           size = 7, family = "Times", parse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))


# another annotated arrow
p4 <-
  tibble(x     = c(.375, .625),
         y     = c(1/3, 1/3),
         label = c("'~'", "italic(i)")) %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = c(10, 7), parse = T, family = "Times") +
  geom_segment(x = .5, xend = .5,
               y = 1, yend = 0,
               arrow = my_arrow) +
  xlim(0, 1)

# some text
p5 <-
  tibble(x     = 1,
         y     = .5,
         label = "italic(y[ji])") %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = 7, parse = T, family = "Times") +
  xlim(0, 2)

layout <- c(
  area(t = 1, b = 2, l = 1, r = 1),
  area(t = 1, b = 2, l = 2, r = 2),
  area(t = 3, b = 3, l = 1, r = 1),
  area(t = 3, b = 3, l = 1, r = 2),
  area(t = 4, b = 5, l = 1, r = 1),
  area(t = 6, b = 6, l = 1, r = 1),
  area(t = 7, b = 7, l = 1, r = 1)
)

#code was directly inspired by S. Kurz, see https://solomonkurz.netlify.app/post/make-model-diagrams-kruschke-style/
(p1_1 + p1_2 + p2_1 + p2_2 + p3 + p4 + p5) + 
  plot_layout(design = layout) &
  ylim(0, 1)
theme_set(theme_grey())#Reset ggplot theme, 

###### Distributions mu parameter
x = seq(2, 10, length=1000)
plot(x, dnorm(x, mean=6, sd=1), type="l", ylab="density", xlab="mu value")


mean_wm = 6
sd_wm = 1
set.seed(234)
n_participants = 15
data = data.frame()
ntrial_v = NULL
mu_v = NULL
for(i in 1:n_participants){
    ntrials = runif(1, 5, 100)# Ntrials from 5 to 100
    subdata = data.frame(trial=1:ntrials)#trial number vector
    mu = rnorm(1, mean=mean_wm, sd=sd_wm)#random sample from the normal illustrated above
    subdata$wm = rnorm(ntrials, mu, sd=sd_wm)#generating data based on the drawn mu
    subdata$participant = as.character(i) #recording participant
    data = rbind(data, subdata)#pasting the new subject to the df
    ntrial_v[i] <- ntrials#SAving the number of trials per participant
}

tail(data)

hist(ntrial_v,breaks=30)

hist(data$wm, breaks=100, xlim=c(3,9))

require(lattice)#needed for histogram plot
options(repr.plot.width=10, repr.plot.height=10)
histogram( ~ wm | participant ,data = data, breaks=20) 
options(repr.plot.width=8, repr.plot.height=5, repr.plot.res = 200)

library(MASS)#we will sample from a multivariate normal distribution
data = data.frame()
varcovmat = matrix(c(1, 0, 0, 1), nrow=2)#See later for var-cov matrix
data = data.frame()
for(i in 1:n_participants){
    re = mvrnorm(n=1, mu=c(0, 0), Sigma=varcovmat)#Sampling from the mvn, rescaling according to DV distribution in next lines
    mu_i = re[1] * sd_wm + mean_wm#participant keeps the same mu during all the years
    b_age = re[2] *(sd_wm*2) - (sd_wm*4) ##Each participant get a single slope for age, the effect size is displayed as a unit of sd_wm
    age_tested = runif(runif(1, 5,15), 1, 99)#retesting participant at different random [1,99] ages [5,15] times
    for (age in age_tested){# A for loop in a for loop is especially dirty but more transparent
        age = age/100#We recode age for scale purpose
        ntrials = runif(1, 5, 100)
        subdata = data.frame(trial=1:ntrials)
        mu = mu_i+b_age*age#inducing age diff alpha + beta * IV
        subdata$wm = rnorm(ntrials, mu, sd=sd_wm)
        subdata$participant = as.character(i)
        subdata$age = age #storing age
        data = rbind(data, subdata)
    }
    
}

means_bysub = aggregate(data$wm, FUN=mean, 
          by=list(age=data$age, participant = data$participant))

library(ggplot2)
ggplot() +
  geom_point(data=means_bysub, aes(x=age, y=x, group = participant, colour = participant)) + 
  scale_colour_discrete('participant') 

m0 <- lm(x ~ age, data = means_bysub)#Syntax discussed later
means_bysub$random.intercept.preds <- predict(m0)

options(repr.plot.width=7, repr.plot.height=5, repr.plot.res = 300)
ggplot() +
  geom_point(data=means_bysub, aes(x=age, y=x, group = participant, colour = participant)) + 
  geom_line(data=means_bysub, aes(x=age, y=random.intercept.preds)) +
  labs(x="age", y="WM") +
  scale_colour_discrete('participant') 

mu_j = rnorm(1, mean=mean_wm, sd=sd_wm)#Random intercept a_j

b_age_j = rnorm(1, -4, 2)#Random slope

#install.packages(lme4)
library(lme4)

m1 <- lmer(x ~ age + (1|participant), data = means_bysub)#Syntax
means_bysub$random.intercept.preds <- predict(m1)

library(ggplot2)
options(repr.plot.width=7, repr.plot.height=5, repr.plot.res = 300)
ggplot() +
  geom_point(data=means_bysub, aes(x=age, y=x, group = participant, colour = participant)) + 
  geom_line(data=means_bysub, aes(x=age, y=random.intercept.preds, group = participant, colour = participant)) +
  labs(x="age", y="WM") +
  scale_colour_discrete('participant') 

m2 <- lmer(x ~ age + (0+age|participant), data = means_bysub)
means_bysub$random.intercept.preds <- predict(m2)

options(repr.plot.width=7, repr.plot.height=5, repr.plot.res = 300)
ggplot() +
  geom_point(data=means_bysub, aes(x=age, y=x, group = participant, colour = participant)) + 
  geom_line(data=means_bysub, aes(x=age, y=random.intercept.preds, group = participant, colour = participant)) +
  labs(x="age", y="WM") +
  scale_colour_discrete('participant') 

m3 <- lmer(x ~ age + (age||participant), data = means_bysub)#1+ is implicit, 0+ explicitely removes intercept
#In this model I specified || which removes the estimation of the correlation between intercept and slope, more later
means_bysub$random.intercept.preds <- predict(m3)

options(repr.plot.width=7, repr.plot.height=5, repr.plot.res = 300)
ggplot() +
  geom_point(data=means_bysub, aes(x=age, y=x, group = participant, colour = participant)) + 
  geom_line(data=means_bysub, aes(x=age, y=random.intercept.preds, group = participant, colour = participant)) +
  labs(x="age", y="WM") +
  scale_colour_discrete('participant') 

i_slope_values = NULL
i_intercept_values = NULL
for(i in 1:length(unique(means_bysub$participant))){
    m0_ = lm(x ~ age, data = means_bysub[means_bysub$participant == i,])
    i_slope_values[i] = m0_$coefficients["age"]
    i_intercept_values[i] = m0_$coefficients["(Intercept)"]
}


h_slope_values = coef(m3)$participant$age
h_intercept_values = coef(m3)$participant$'(Intercept)'

plot(i_intercept_values, i_slope_values, xlab="Intercept", ylab="Slope")#parameter of individual LM
points(h_intercept_values, h_slope_values, col="red")#FE parameters of LMM
abline(v=6)#True intercept
abline(h=-4)#True slope


summary(m0)

summary(m1)

summary(m2)

summary(m3)

#install.packages('lmerTest')
library('lmerTest')#provides estimation of DoF and Pvalues

m1 <- lmer(x ~ age + (1|participant), data = means_bysub)
m2 <- lmer(x ~ age + (0+age|participant), data = means_bysub)
m3 <- lmer(x ~ age + (age||participant), data = means_bysub)

summary(m1)

summary(m2)

summary(m3)

library(MASS)#we will sample from a multivariate normal distribution
varcovmat = matrix(c(1, .66, .66, 2), nrow=2)#here covar = 0.66 = correlation of .33
matrix_mvn = mvrnorm(10000, mu=c(6, -4), Sigma=varcovmat)
plot(matrix_mvn[,1],matrix_mvn[,2], xlab = "Intercept", ylab = "Slope")

data = data.frame()
varcovmat = matrix(c(1, .66, .66, 2), nrow=2)#Same covar matrix
data = data.frame()
for(i in 1:n_participants){
    re = mvrnorm(n=1, mu=c(6, -1), Sigma=varcovmat)#Sampling from the mvn, rescaling according to DV distribution in next lines
    mu_i = re[1]
    b_age = re[2]
    age_tested = runif(runif(1, 5,15), 1, 99)#retesting participant at different random [1,99] ages [5,15] times
    for (age in age_tested){# A for loop in a for loop is especially dirty but more transparent
        age = age/100#We recode age for scale purpose
        ntrials = runif(1, 5, 100)
        subdata = data.frame(trial=1:ntrials)
        mu = mu_i+b_age*age#inducing age diff alpha + beta * IV
        subdata$wm = rnorm(ntrials, mu, sd=sd_wm)
        subdata$participant = as.character(i)
        subdata$age = age #storing age
        data = rbind(data, subdata)
    }
    
}

m3_cov <- lmer(x ~ age + (age|participant), data = means_bysub)#One | only indicates we estimate the covariance between RE

summary(m3_cov)

#First we define our covariance matrix unitless, we later rescale to the values of the DV
varcovmat = matrix(c(1, .33, .33, .33, .33, 1, .33, .33, .33, .33,1,.33, .33, .33, .33,1), nrow=4)#everything is correlated at .33
data = data.frame()
for(i in 1:n_participants){
    re = mvrnorm(n=1, mu=c(0, 0, 0, 0), Sigma=varcovmat)
    mu_i = re[1] * sd_wm + mean_wm #Rescaling so that mean = 6 and sd = 1
    b_age = re[2] *(sd_wm*2) - (sd_wm*4) #b_age_j + rescaling
    b_task = re[3] * sd_wm + -(sd_wm*2)#b_task_j + rescaling
    b_age_task = re[4] * (sd_wm/2) - (sd_wm*2)#b_agetask_j + rescaling
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

m4 <- lmer(x ~ age * task + (1+age*task|participant), data = means_)

library(optimx)
m4 <- lmer(x ~ age * task + (1+age*task|participant), data = means_, 
           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
           optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#m4_wrong <- lmer(x ~ age * task + (1+age*task|participant), data = means_[means_$age < .1,])

summary(m4)

#install.packages("sjPlot")
library("sjPlot")
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(m4, type = "pred", terms = c("age", "task"), show.data = TRUE)

#install.packages('glmmTMB')
plot_model(m4, type = "est")

#install.packages('glmmTMB')
plot_model(m4, type = "re")

library(performance)

options(repr.plot.width=8, repr.plot.height=10, repr.plot.res = 100)#hidden code for display size
check_model(m4)
options(repr.plot.width=8, repr.plot.height=5, repr.plot.res = 200)#hidden code for display size

full_trial_fullFE <- lmer(wm ~ age * task + (age*task|participant), data = data, 
           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
           optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(full_trial_fullFE)

summary(m4)

plot_model(full_trial_fullFE, type = "pred", terms = c("age", "task"), show.data = FALSE)

plot_model(full_trial_fullFE)

plot_model(full_trial_fullFE, type="re")

#check_model(full_trial_fullFE)# eats too much RAM and therefore too slow so I saved it in advance

random_mu_trial = rnorm(50, 0, 1)#A simple random effect where each trial has its own mean

varcovmat = matrix(c(1, .33, .33, .33, .33, 1, .33, .33, .33, .33,1,.33, .33, .33, .33,1), nrow=4)
data = data.frame()
for(i in 1:n_participants){
    re = mvrnorm(n=1, mu=c(0, 0, 0, 0), Sigma=varcovmat)
    mu_i = re[1] * sd_wm + mean_wm
    b_age = re[2] *(sd_wm*2) - (sd_wm*4) #b_age_j
    b_task = re[3] * sd_wm + -(sd_wm*2)#b_task_j
    b_age_task = re[4] * (sd_wm/2) - (sd_wm*2)#b_agetask_j
    age_tested = runif(runif(1, 5,15), 1, 99)
    for (age in age_tested){
        age = age/100
        for (task in c(0,1)){
            ntrials = 50
            subdata = data.frame(trial=1:ntrials)
            mu = mu_i+b_age*age+b_task*task+b_age_task*age*task
            subdata$wm = rnorm(50, mu, sd=sd_wm)
            subdata$wm = subdata$wm + rnorm(50, random_mu_trial, 1)#drawing 1 samples for the 50 trials with the 50 mu
            subdata$participant = as.character(i)
            subdata$age = age 
            subdata$task = task
            subdata$trial = 1:50#trial n
            data = rbind(data, subdata)
        }
    }
}

full_trial_fullFERE <- lmer(wm ~ task * age + (task * age|participant) + (1|trial), data = data, 
           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
           optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(full_trial_fullFERE)

plot_model(full_trial_fullFERE, type = "pred", terms = c("age","task"), show.data = TRUE)

options(repr.plot.width=10, repr.plot.height=10, repr.plot.res = 100)#hidden code for display size
plot_model(full_trial_fullFERE, type = "re")
options(repr.plot.width=8, repr.plot.height=5, repr.plot.res = 200)#hidden code for display size
