# R script created by Jacob J. Coutts (Copyright 2026)
# Mediation Lecture 2 (PSYC489)

# load required packages 
library(ggplot2) # for visuals
library(jtools) # for apa formatting on graphs
source("~/PSYC489K-SU26/process_v5.R") # Make sure the source command above routes to where PROCESS is located in YOUR computer

# read in data and summarize - be sure to change file path to appropriate location on your computer
mentorship <- read.csv(file="~/PSYC489K-SU26/Datasets/mentor.csv")
summary(mentorship)


################### SIMPLE MEDIATION ################### 
###### Simple mediatioon through workload (PROCESS ONLY) 
process(data = mentorship, x = "mentor", m = "workload", y = "conflict", seed = 0529, model = 4, boot=10000, total = 1) 

###### Simple mediation through resources (PROCESS ONLY) 
process(data = mentorship, x = "mentor", m = "resource", y = "conflict", seed = 0529, model = 4, boot=10000, total = 1) 

################### END SIMPLE MEDIATION ################### 



################### SIMPLE MEDIATION THROUGH WORKLOAD WITH A COVARIATE ###################
#### manually, not in PROCESS (through workload)
# bootstrap
set.seed(0529) # set seed to reproduce results
B <- 10000 # set the number of resamples
ind <- rep(NA,B) # creates an empty vector of 5,000 NAs
n = nrow(mentorship) # set sample size

for(i in 1:B){  
  bootdat <- mentorship[sample(1:n,n,replace=TRUE),] # create a random dataset from our original
  boota <- coef(lm(workload ~ mentor + wforient, data = bootdat))["mentor"] # bootstrapped a path
  bootb <- coef(lm(conflict ~ mentor + workload + wforient, data = bootdat))["workload"] # bootstrapped b path
  ind[i] <- boota*bootb # bootstrapped indirect effect
}

# find the upper and lower endpoints of the confidence interval 
bs_ci <- quantile(ind, c(.025, .975))
paste("The CI for the indirect effect through workload is:");round(bs_ci,4)

### Now in PROCESS through workload
# seed makes results reproducible
# model 4 is simple or parallel mediation
# total = 1 prints total effect
process(data = mentorship, x = "mentor",m = "workload", y = "conflict", cov="wforient",seed = 0529, model = 4, boot=10000, total = 1) 

###### Mediation through resources with a covariate (PROCESS ONLY)
# seed makes results reproducible
# model 4 is simple or parallel mediation
# total = 1 prints total effect
process(data = mentorship, x = "mentor",m = "resource", y = "conflict", cov="wforient",seed = 0529, model = 4, boot=10000, total = 1) 

################### END SIMPLE MEDIATION THROUGH WORKLOAD WITH A COVARIATE ###################



################### PARALLEL MEDIATION ###################
###### parallel Mediation with contrasts, manually
# bootstrap
set.seed(0529) # set seed to reproduce results
B <- 10000 # set the number of resamples
ind1 <- rep(NA,B) # creates an empty vector of 5,000 NAs
ind2 <- rep(NA,B) # creates an empty vector of 5,000 NAs
contrast <- rep(NA, B) # empty results vector for comparisons
n = nrow(mentorship) # set sample size

# path estimates 
summary(lm(workload~mentor, data=mentorship)) # a1
summary(lm(resource~mentor, data=mentorship)) # a2
summary(lm(conflict ~ mentor + workload + resource, data = mentorship)) # b1, b2, and c'

for(i in 1:B){  
  bootdat <- mentorship[sample(1:n,n,replace=TRUE),] # create a random dataset from our original
  boota1 <- coef(lm(workload ~ mentor, data = bootdat))["mentor"] # bootstrapped a1 path
  boota2 <- coef(lm(resource ~ mentor, data = bootdat))["mentor"] # bootstrapped a2 path
  bootb1 <- coef(lm(conflict ~ mentor + workload + resource, data = bootdat))["workload"] # bootstrapped b1 path
  bootb2 <- coef(lm(conflict ~ mentor + workload + resource, data = bootdat))["resource"] # bootstrapped b2 path
  ind1[i] <- boota1*bootb1 # bootstrapped specific indirect effect a1b1
  ind2[i] <- boota2*bootb2 # bootstrapped specific indirect effect a2b2
  contrast[i] <- ind1[i]-ind2[i]  # bootstrapped contrast
}

# find the upper and lower endpoints of the confidence interval 
bs_ci1 <- quantile(ind1, c(.025, .975))
bs_ci2 <- quantile(ind2, c(.025,.975))
contrast_ci <- quantile(contrast, c(.025, .975))
paste("The CI for the indirect effect through workload is:");round(bs_ci1,4)
paste("The CI for the indirect effect through resource is:");round(bs_ci2,4)
paste("The CI for the difference between the indirect effects is:");round(contrast_ci,4)

# total effect
summary(lm(conflict ~ mentor, data = mentorship))

### parallel mediation with contrasts, PROCESS
# contrast = 2 produces the difference in absolute values between all indirect effects in model 
# seed makes results reproducible
# model 4 is simple or parallel mediation
# total = 1 prints total effect
process(data = mentorship, x = "mentor",m = c("workload", "resource"), y = "conflict", seed = 489, boot=10000, model = 4, contrast = 2, total = 1) 

################### END PARALLEL MEDIATION ###################



################### SERIAL MEDIATION ###################
###### serial Mediation with contrasts, manually
# bootstrap
set.seed(0529) # set seed to reproduce results
B <- 10000 # set the number of resamples
indwork <- rep(NA,B) # creates an empty vector of 5,000 NAs for workload
indres <- rep(NA,B) # creates an empty vector of 5,000 NAs for resource
indserial <- rep(NA,B) # creates an empty vector of 5,000 Nas for serial IE
contrast_workserial <- rep(NA, B) # contrast 1 results vector
contrast_workres <- rep(NA, B) # contrast 2 results vector
contrast_resserial <- rep(NA, B) # contrast 3 results vector
n = nrow(mentorship) # set sample size

for(i in 1:B){  
  bootdat <- mentorship[sample(1:n,n,replace=TRUE),] 
  boota1 <- coef(lm(workload ~ mentor, data = bootdat))["mentor"] # bootstrapped a1 path
  boota2 <- coef(lm(resource ~ mentor + workload, data = bootdat))["mentor"] # bootstrapped a2 path
  booti <- coef(lm(resource ~ mentor + workload, data = bootdat))["workload"] # bootstrapped i path
  bootb1 <- coef(lm(conflict ~ mentor + workload + resource, data = bootdat))["workload"] # bootstrapped b1 path
  bootb2 <- coef(lm(conflict ~ mentor + workload + resource, data = bootdat))["resource"] # bootstrapped b2 path
  indwork[i] <- boota1*bootb1 # bootstrapped specific indirect effect a1b1
  indres[i] <- boota2*bootb2 # bootstrapped specific indirect effect a2b2
  indserial[i] <- boota1*booti*bootb2 # bootstrapped serial indirect effect a1ib2
  contrast_workserial[i] <- indwork[i]-indserial[i] # bootstrapped contrast
  contrast_workres[i] <- abs(indwork[i])-abs(indres[i]) # bootstrapped contrast
  contrast_resserial[i] <- abs(indres[i])-abs(indserial[i]) # bootstrapped contrast
}

# find the upper and lower endpoints of the confidence interval 
workload_ci <- quantile(indwork, c(.025, .975))
resource_ci <- quantile(indres, c(.025,.975))
serial_ci <- quantile(indserial, c(.025, .975))

### create and print CIs
### specific IEs
paste("The 95% percentile bootstrap CI for the specific indirect effect through workload is:") ; workload_ci
paste("The 95% percentile bootstrap CI for the specific indirect effect through resources is:"); resource_ci
paste("The 95% percentile bootstrap CI for the serial indirect effect is:"); serial_ci

### contrasts 
paste("The 95% percentile bootstrap CI for the difference between the effect through workload and the serial indirect effect is:"); quantile(contrast_workserial, c(.025, .975))
paste("The 95% percentile bootstrap CI for the difference in absolute values between the effect through workload and resources is:"); quantile(contrast_workres, c(.025, .975))
paste("The 95% percentile bootstrap CI for the difference in aboslute values between the effect through resources and the serial indirect effect is:"); quantile(contrast_resserial, c(.025, .975))

# total effect
summary(lm(conflict ~ mentor, data = mentorship))

# serial mediation in PROCESS
# model = 6 is serial mediation
process(data = mentorship, x = "mentor", m = c("workload","resource"), y = "conflict", model = 6, seed = 0529, total = 1, boot = 10000)

# run it again with contrast = 2 to get all correct contrasts (i.e., difference in absolute values)
process(data = mentorship, x = "mentor", m = c("workload","resource"), y = "conflict", model = 6, seed = 0529, total = 1, boot = 10000, contrast =2)

################### END SERIAL MEDIATION ###################



################### COMPLEX MODELS ###################
###### parallel mediation with contrasts and covariates, (PROCESS ONLY)
process(data = mentorship, x = "mentor",m = c("workload","resource"), y = "conflict", cov = "wforient", seed = 489, boot=10000,contrast = 2, model = 4, total = 1)

###### serial mediation with contrasts and covariates, (PROCESS ONLY)
process(data = mentorship, x = "mentor",m = c("workload", "resource"),y = "conflict", cov = "wforient", seed = 489, boot=10000, model = 6, contrast = 1, total = 1)

# run it again with contrast = 2 to get proper contrasts between all indirect effects 
process(data = mentorship, x = "mentor",m = c("workload", "resource"),y = "conflict", cov = "wforient", seed = 489, boot=10000, model = 6, contrast = 2, total = 1)

################### END COMPLEX MODELS ###################


### end of script