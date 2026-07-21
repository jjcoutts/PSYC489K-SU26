# R script created by Jacob J. Coutts (Copyright 2025)
# Mediation Lecture 1 (PSYC489)

# load required packages 
library(ggplot2)
library(jtools)
source("~/PSYC489K-SU26/process_v5.R") # Make sure the source command above routes to where PROCESS is located in YOUR computer

# read in data and summarize - be sure to change file path to appropriate location on your computer
mentorship <- read.csv(file="~/PSYC489K-SU26/Datasets/mentor.csv")
summary(mentorship)

### simple mediation analysis 
## causal steps 
# c path 
cpath <- lm(conflict ~ mentor, data = mentorship)
summary(cpath); confint(cpath) # NOT significant

# a path
apath <- lm(workload ~ mentor, data = mentorship)
summary(apath); confint(apath) # significant

# b and c' paths
bcppaths <- lm(conflict ~ mentor + workload, data = mentorship)
summary(bcppaths);confint(bcppaths) # significant

## joint test -- see results for a and b paths above

## Sobel test - manual 
a <- coef(apath)["mentor"]; seasq <- diag(vcov(apath))["mentor"] # extract a path
b <- coef(lm(bcppaths))["workload"]; sebsq <- diag(vcov(bcppaths))["workload"] # extract b path
zstat <- a*b/(sqrt(b^2*seasq+a^2*sebsq+seasq*sebsq)) # obtain test statistic, here it's standard normal
2*pnorm(zstat,lower.tail=FALSE) # multiply by two because it's two tailed

## Sobel test - PROCESS (info for joint and causal steps on every process output)
process(data = mentorship, x = "mentor",m = "workload", y = "conflict", model = 4, normal = 1, boot = 0) # model 4 is simple/parallel mediation, seed is not needed here, normal=1 produces Sobel test

## bootstrap - manual 
set.seed(0529) # set seed to reproduce results
B = 5000 # set number of bootstrap samples
n = nrow(mentorship) # set the sample size
bootres <- rep(NA, B) # create empty vector of B length
# now we need to bootstrap the data B times and store the indirect effect in each resample
for(i in 1:B){
  bootdat <- mentorship[sample(1:n,n, replace=TRUE),] # randomly sample 5,000 rows with replacement and use the new dataset for the analyses
  boota <- coef(lm(workload ~ mentor, data = bootdat))["mentor"] # index the coefficient we want by name--here, the effect of X 
  bootb <- coef(lm(conflict ~ mentor + workload, data = bootdat))["workload"] # grab the coefficient for workload because we want the effect of M controlling for X here
  bootres[i] <- boota*bootb # compute indirect effect. Loop ends, adding one to the increment until it reaches B (5,000 here)
}
# create confidence intereval. 2.5 and 97.5th percentiles for 95% CI
bs_ci <- quantile(bootres, c(.025,.975))
bs_ci

# visualize the sampling distribution - fancy plot shown on slots, not the simpler code presented on the slides
bs <- data.frame(bootres = bootres)
ggplot() + 
  geom_histogram(data=bs, aes(x=bootres), col="black",fill="yellow3",bins=50,alpha=.3) +
  geom_vline(xintercept=.164, col="darkred",linetype="dashed") +
  geom_vline(xintercept=quantile(bs$bootres,.025),linetype="dashed") + 
  geom_vline(xintercept=quantile(bs$bootres,.975),linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        panel.background = element_rect(color="black"), # remove background
        axis.title.x = element_text(hjust=.5), # center x axis title
        axis.title.y = element_text(hjust=.5), # center y axis title
        axis.line = element_line(), # add axis lines
        plot.title = element_text(hjust=.5, color="grey0", face="plain")) 

## bootstrap - PROCESS
process(data = mentorship, x = "mentor",m = "workload", y = "conflict", seed = 489, model = 4) # model 4 is simple/parallel mediation, seed set at 489 means the results are reproducible

## Monte Carlo - manual 
set.seed(0705) # set seed to reproduce results
k <- 5000
avec <- rnorm(k, mean = coef(apath)["mentor"], sd = sqrt(diag(vcov(apath)))["mentor"]) # generate 5000 a paths
bvec <- rnorm(k, mean = coef(lm(bcppaths))["workload"], sd = sqrt(diag(vcov(bcppaths)))["workload"]) # generate 5000 b paths
mcres <- avec*bvec
mc_ci <- quantile(mcres, c(.025,.975)) # multiply vectors together and create 95% confidence interval
mc_ci

# visualize the sampling distribution - fancy plot shown on slides not code presented on slides
mc <- data.frame(mcres = mcres)
ggplot() + 
  geom_histogram(data=bs, aes(x=bootres), col="black",fill="lightblue3",bins=50,alpha=.3) +
  geom_vline(xintercept=.164, col="darkred",linetype="dashed") +
  geom_vline(xintercept=quantile(mc$mcres,.025),linetype="dashed") + 
  geom_vline(xintercept=quantile(mc$mcres,.975),linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        panel.background = element_rect(color="black"), # remove background
        axis.title.x = element_text(hjust=.5), # center x axis title
        axis.title.y = element_text(hjust=.5), # center y axis title
        axis.line = element_line(), # add axis lines
        plot.title = element_text(hjust=.5, color="grey0", face="plain"))

## Monte Carlo - PROCESS
process(data = mentorship, x = "mentor",m = "workload", y = "conflict", seed = 489, model = 4, mc = 1) # model 4 is simple/parallel mediation, seed set at 489 means the results are reproducible, mc creates monte carlo confidence intervals

## mediation adjusting for random measurement error
process(data=mentorship,y="conflict",x="mentor",m="workload",relx = .7, relm = .7, model=4, seed=489)


### end of script