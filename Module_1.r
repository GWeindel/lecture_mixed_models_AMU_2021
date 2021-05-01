plot(0,0)
abline(a=1,b=0)

set.seed(234)#I don't like randomness in my lectures

obs = 1000 #We sampled the WM of 100 persons
mean_wm = 6 #magical number 6
sd_wm = 1 #+/- 1
wm_v = rnorm(obs, mean_wm,sd_wm)#Random number generator
youngest = 1
oldest = 99
age_v = runif(obs, youngest,oldest)#Random number generator
data = data.frame(wm_v, age_v)

plot(data$age_v, data$wm_v, xlab= "Age",ylab="WM span")

b_age = -0.04 #aging of one unit (e.g. year) decreases the WM of 0.4 unit
data$wm_v = data$wm_v + b_age * data$age_v 

plot(age_v, wm_v + (age_v*b_age), xlab="Age (year)", ylab="WM span")
#above is an operation on vector ; 
# for each indiv i : wm_i = wm_v[i] + (age_v[i] * b_age)

model <- lm(wm_v ~ age_v, data=data)

summary(model)

model$coeff#accessing coefficients

#plot(data$age_v, data$wm_v)
#abline(model$coeff[1], model$coeff[2], lwd=3, col="blue")
#We can even extend the prediction :
plot(data$age_v, data$wm_v,xlim = c(1,150))
abline(model$coeff[1], model$coeff[2], lwd=3, col="blue")

resid = data$wm_v - (model$coeff[1] + model$coeff[2] * data$age_v)
hist(resid)

require("performance")#Useful automated assumption check package 
check_model(model)

data_exp = data
data_exp$wm_v = exp(data_exp$wm_v)
exp_model <- lm(wm_v ~ age_v, data=data_exp)

summary(exp_model)

plot(data_exp$age_v, data_exp$wm_v)
abline(exp_model$coeff[1], exp_model$coeff[2], lwd=3, col="blue")

check_model(exp_model)

mean_double_task = 2
wm_v = rnorm(obs, mean_double_task,sd_wm)#Different mean
data2 = data.frame(wm_v, age_v)#age is the same

# Inducing an interaction :
b_age_double_task = -0.02 #aging of one unit (e.g. year) decreases the WM of 0.2 unit when double
data2$wm_v = data2$wm_v + b_age_double_task * data2$age_v

#Merging 
data$condition = "single"
data2$condition = "double"
data_int = rbind(data, data2)

subset = data_int[data_int$condition == "single",]#lisibility/clarity purpose
plot(subset$age_v, subset$wm_v, xlab="Age (year)", ylab="WM span", col="red")

subset = data_int[data_int$condition == "double",]#lisibility/clarity purpose
points(subset$age_v, subset$wm_v, xlab="Age (year)", ylab="WM span", col="blue")

model_add <- lm(wm_v ~ age_v + condition, data=data_int) 

#install.packages("sjPlot")
library("sjPlot")
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(model_add, type = "pred", terms = c("age_v", "condition"), 
           show.data = TRUE, ci.lvl = NA, line.size=3)

summary(model_add)

model_int <- lm(wm_v ~ age_v * condition, data=data_int)#We switched to "*"
#Or more explicitely :
model_int <- lm(wm_v ~ age_v + condition + age_v:condition, data=data_int)#same as above

plot_model(model_int, type = "pred", terms = c("age_v", "condition"), 
           show.data = TRUE, ci.lvl = NA, line.size=3)

summary(model_int)

data_int$age_v_recoded = (data_int$age_v - 1)/100
data_int$condition_recoded = ifelse(data_int$condition == "single", 0, 1)

head(data_int)

tail(data_int)

model_int_recoded <- lm(wm_v ~ age_v_recoded * condition_recoded, data=data_int)
summary(model_int_recoded)

data_int$condition_treatment = ifelse(data_int$condition =="single",0,1)
data_int$condition_sum = ifelse(data_int$condition =="single",-1,1 )
summary(lm(wm_v ~ age_v * condition_treatment, data=data_int))$coefficients
summary(lm(wm_v ~ age_v * condition_sum, data=data_int))$coefficients

mean_double_task_same = 5
mean_double_task_diff = 2

wm_v = rnorm(obs, mean_double_task_same,sd_wm)#Different mean
data_same = data.frame(wm_v, age_v)

wm_v = rnorm(obs, mean_double_task_diff,sd_wm)#Different mean
data_diff = data.frame(wm_v, age_v)

#Merging 
data$condition = "a-single"
data_same$condition = "d-same"
data_diff$condition = "d-diff"
data_threemod = rbind(data, data_same, data_diff)
data_threemod$condition = as.factor(data_threemod$condition)#We are working with the factor class
print(levels(data_threemod$condition))

data_threemod$condition_sum = data_threemod$condition
contrasts(data_threemod$condition_sum) = contr.sum(3)#Defining the contrast matrix
print(contrasts(data_threemod$condition_sum)) #printing it
sum_cont = lm(wm_v ~ age_v * condition_sum, data=data_threemod)
summary(sum_cont)$coefficients

data_threemod$condition_treatment = data_threemod$condition
contrasts(data_threemod$condition_treatment) = contr.treatment(3)
print(contrasts(data_threemod$condition_treatment))
treatment_cont = lm(wm_v ~ age_v * condition_treatment, data=data_threemod)
summary(treatment_cont)$coefficients

data_threemod$condition_sum = data_threemod$condition
contrasts(data_threemod$condition_sum,how.many=2) = matrix(c(-1,0,1,-1,1,0),ncol=2)
print(contrasts(data_threemod$condition_sum))
sum_cont = lm(wm_v ~ age_v * condition_sum, data=data_threemod)
summary(sum_cont)$coefficients

library('MASS')
varcovmat = matrix(c(1, .97, .97, 1), nrow=2)
data = mvrnorm(n=100, mu=c(0, 0), Sigma=varcovmat, empirical=TRUE)
iq = data[, 1]*15 +100#Just rescaling for "natural" scale
mem = data[, 2]*15 +100 #same

plot(iq,mem)

rt = rnorm(100, 500,50) + -1*iq #Only relationship simulated is with IQ

plot(iq, rt)

model_rt = lm(rt ~ iq + mem)#Additive, we did not include an interaction

summary(model_rt)

check_collinearity(model_rt)

model_rt = lm(rt ~ iq)

summary(model_rt)
