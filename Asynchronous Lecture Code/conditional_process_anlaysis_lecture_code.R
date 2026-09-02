# R script created by Jacob J. Coutts (Copyright 2026)
# Conditional Process Analysis Lecture (PSYC489K)

# load required packages 
library(ggplot2) # for data visualization
library(jtools) # for APA figures
library(dplyr) # going to use between for CIs
library(MASS) # for multivariate normal (Monte Carlo)
source("~/PSYC489K-SU26/process_v5.R")
# make sure the source command above routes to where PROCESS is located in YOUR computer

# read in data and summarize - be sure to change file path to appropriate location on your computer
selfc <- read.csv(file = "~/PSYC489K-SU26/Datasets/self_compassion_l6.csv")
summary(selfc)

######################## JOINT TEST: Manaul ICIE ########################
# fit the model manually
m_mod <- lm(scc_TOT ~ self_TOT*cesd_TOT, data = selfc); summary(m_mod)
a1 = coef(m_mod)["self_TOT"]; a3 = coef(m_mod)["self_TOT:cesd_TOT"]
y_mod <- lm(swl_TOT ~ self_TOT*cesd_TOT + scc_TOT*cesd_TOT, data = selfc); summary(y_mod)
b1 = coef(y_mod)["scc_TOT"]; b3 = coef(y_mod)["cesd_TOT:scc_TOT"]
# both interaction effects sig, significant by way of joint test

######################## END JOINT TEST: Manaul ICIE ########################


######################## BOOTSTRAP: Manual ICIE ########################
B = 10000
set.seed(489)
bsICIE = rep(NA, B) # bs results vector
for(i in 1:B){
  boot_dat = selfc[sample(1:nrow(selfc), nrow(selfc), replace = TRUE),]
  a3_boot = coef(lm(scc_TOT ~ self_TOT*cesd_TOT, data = boot_dat))["self_TOT:cesd_TOT"]
  b3_boot = coef(lm(swl_TOT ~ self_TOT*cesd_TOT + scc_TOT*cesd_TOT, data = boot_dat))["cesd_TOT:scc_TOT"]
  bsICIE[i] = a3_boot*b3_boot
}
bsCI = quantile(bsICIE, c(.025, .975));bsCI

######################## ENDBOOTSTRAP: Manual ICIE ########################


######################## MONTE CARLO: Manual ICIE ######################## 
B = 10000
set.seed(489)
m_MC = MASS::mvrnorm(n = B, mu = coef(m_mod), Sigma = vcov(m_mod))
y_MC = MASS::mvrnorm(n = B, mu = coef(y_mod), Sigma = vcov(y_mod))
a1MC = m_MC[, "self_TOT"]
a3MC = m_MC[, "self_TOT:cesd_TOT"]
b1MC = y_MC[, "scc_TOT"]
b3MC = y_MC[, "cesd_TOT:scc_TOT"]

mcICIE = a3MC*b3MC
mcCI = quantile(mcICIE, c(.025,.975));mcCI
######################## END MONTE CARLO: Manual ICIE ######################## 


######################## BOOTSTRAP: Manual Probing ######################## 

### Bootstrap: Pick-a-point/simple slopes/spotlight analysis
# set bootstrap parameters
B = 10000 # number of resamples
set.seed(489) # to reproduce results
bsLOW = rep(NA, B) # for low values of moderator vis bootstrap
bsMED = rep(NA, B) # for med values of moderator vis bootstrap
bsHIGH = rep(NA, B) # for high values of moderator vis bootstrap
a1_boot = rep(NA, B) # for bootstrap a1 values
a3_boot = rep(NA, B) # for bootstrap a3 values
b1_boot = rep(NA, B) # for bootstrap b1 values
b3_boot = rep(NA, B) # for bootstrap b3 values

# moderator details
sdDEP = sd(selfc$cesd_TOT) # get SD of moderator
mDEP = mean(selfc$cesd_TOT) # get mean of moderator
mod_vals = seq(min(selfc$cesd_TOT), max(selfc$cesd_TOT), length.out = 100) # obtain large range of moderator for psuedo JN
region = 0 # for the pseudo JN

# generate bootstrap as and bs
for(i in 1:B){
  boot_dat = selfc[sample(1:nrow(selfc), nrow(selfc), replace = TRUE),]
  # store a1 results
  a1_boot[i] = coef(lm(scc_TOT ~ self_TOT*cesd_TOT, data = boot_dat))["self_TOT"]
  # store a3 results
  a3_boot[i] = coef(lm(scc_TOT ~ self_TOT*cesd_TOT, data = boot_dat))["self_TOT:cesd_TOT"]
  # store b1 results
  b1_boot[i] = coef(lm(swl_TOT ~ self_TOT*cesd_TOT + scc_TOT*cesd_TOT, data = boot_dat))["scc_TOT"]
  # store b3 results
  b3_boot[i] = coef(lm(swl_TOT ~ self_TOT*cesd_TOT + scc_TOT*cesd_TOT, data = boot_dat))["cesd_TOT:scc_TOT"]
}

# create CIs for the 3 point estimates of the conditional indirect effect
# remember, the formula is (a1 + a3W)(b1 + b3W)
bsLOW = (a1_boot + a3_boot*(mDEP-sdDEP))*(b1_boot + b3_boot*(mDEP-sdDEP))
bsLOWCI = quantile(bsLOW, c(.025, .975));bsLOWCI

bsMED = (a1_boot + a3_boot*mDEP)*(b1_boot + b3_boot*mDEP)
bsMEDCI = quantile(bsMED, c(.025, .975));bsMEDCI

bsHIGH = (a1_boot + a3_boot*(mDEP+sdDEP))*(b1_boot + b3_boot*(mDEP+sdDEP))
bsHIGHCI = quantile(bsHIGH, c(.025, .975));bsHIGHCI
### END Bootstrap: Pick-a-point/simple slopes/spotlight analysis

### Bootstrap: Pseudo JN
bs_simp_CI = data.frame(llci = rep(NA, length(mod_vals)), ulci = rep(NA, length(mod_vals)))
# loop through all mod values and point estimate at each, find if sig transfers at 0, 1, or 2 points
for(i in 1:length(mod_vals)){
  bs_simp_ie = (a1_boot + a3_boot*mod_vals[i])*(b1_boot + b3_boot*mod_vals[i]) # create individual conditional IEs
  bs_simp_CI[i, "llci"] = quantile(bs_simp_ie, .025) # create CIs
  bs_simp_CI[i, "ulci"] = quantile(bs_simp_ie, .975) # create CIs

  # fancy results things
  if(dplyr::between(0, bs_simp_CI[i,1], bs_simp_CI[i,2]) == FALSE & region == 0){
    region = 1
    r_vals = mod_vals[i]
    r_index = i
  } # if one region
  
  if(region == 1 & dplyr::between(0, bs_simp_CI[i,1], bs_simp_CI[i,2]) == TRUE){
    region = 2
    r_vals = c(r_vals, mod_vals[i])
    r_index = c(r_index, i)
  } # if two regions
}

# Nice results message (Bootstrap)
if(region == 0){
  paste("There were no significant simple conditional indirect effects in the range of the moderator via the Bootstrap.")
} else if (region == 1 & r_index == 1){
  paste("All the simple conditional indirect effects in the range of the moderator were significant via the Bootstrap.")
} else if (region == 1 & r_index != 1){
  paste0("The approximate region of significance (by way of the Bootstrap) started at a moderator value of ", r_vals, " and continued through the maximum observed value of the moderator ", max(selfc$cesd_TOT), ", where a total of ", mean(selfc$cesd_TOT>=r_vals)*100, "% of the data fell in the approximate region of significance.")
} else if (region == 2){
  paste0("The approximate region of significance (by way of the Bootstrap) started at a moderator value of ", r_vals[1], " and ended at a value of ", r_vals[2], " where a total of ", mean(selfc$cesd_TOT <= r_vals[2] & selfc$cesd_TOT >= r_vals[1])*100, "% of the data fell in the approximate region of significance.")
}

### END Bootstrap: Pseudo JN

######################## END BOOTSTRAP: Manual Probing ######################## 



######################## MONTE CARLO: Manual Probing ########################
### Monte Carlo: Pick-a-point/simple slopes/spotlight analysis
B = 10000 # number of resamples
set.seed(489) # to reproduce results
region = 0 # for the pseudo JN

# remember, the formula is (a1 + a3W)(b1 + b3W)
# create CIs for the 3 point estimates of the conditional indirect effect
mcLOW = (a1MC + a3MC*(mDEP-sdDEP))*(b1MC + b3MC*(mDEP-sdDEP))
mcLOWCI = quantile(mcLOW, c(.025,.975));mcLOWCI

mcMED = (a1MC + a3MC*(mDEP))*(b1MC + b3MC*(mDEP))
mcMEDCI = quantile(mcMED, c(.025, .975));mcMEDCI

mcHIGH = (a1MC + a3MC*(mDEP+sdDEP))*(b1MC + b3MC*(mDEP+sdDEP))
mcHIGHCI = quantile(mcHIGH, c(.025, .975));mcHIGHCI

### END Monte Carlo: Pick-a-point/simple slopes/spotlight analysis


### Monte Carlo: Pseudo JN
mc_simp_CI = data.frame(llci = rep(NA, length(mod_vals)), ulci = rep(NA, length(mod_vals)))
# loop through all mod values and point estimate at each, find if sig transfers at 0, 1, or 2 points
for(i in 1:length(mod_vals)){
  mc_simp_ie = (a1MC + a3MC*mod_vals[i])*(b1MC + b3MC*mod_vals[i])
  mc_simp_CI[i,"llci"] = quantile(mc_simp_ie, .025)
  mc_simp_CI[i,"ulci"] = quantile(mc_simp_ie, .975)
  
  if(dplyr::between(0, mc_simp_CI[i,1], mc_simp_CI[i,2]) == FALSE & region == 0){
    region = 1
    r_vals = mod_vals[i]
    r_index = i
  } # if one region
  
  if(region == 1 & dplyr::between(0, mc_simp_CI[i,1], mc_simp_CI[i,2]) == TRUE){
    region = 2
    r_vals = c(r_vals, mod_vals[i])
    r_index = c(r_index, i)
  } # if two regions
}

# Nice results message (Monte Carlo)
if(region == 0){
  paste("There were no significant simple conditional indirect effects in the range of the moderator via the Monte Carlo.")
} else if (region == 1 & r_index == 1){
  paste("All the simple conditional indirect effects in the range of the moderator were significant via the Monte Carlo.")
} else if (region == 1 & r_index != 1){
  paste0("The approximate region of significance (by way of the Monte Carlo) started at a moderator value of ", r_vals, " and continued through the maximum observed value of the moderator ", max(selfc$cesd_TOT), ", where a total of ", mean(selfc$cesd_TOT>=r_vals)*100, "% of the data fell in the approximate region of significance.")
} else if (region == 2){
  paste0("The approximate region of significance (by way of the Monte Carlo) started at a moderator value of ", r_vals[1], " and ended at a value of ", r_vals[2], " where a total of ", mean(selfc$cesd_TOT <= r_vals[2] & selfc$cesd_TOT >= r_vals[1])*100, "% of the data fell in the approximate region of significance.")
}

### END Monte Carlo: Pseudo JN

######################## END MONTE CARLO: Manual Probing ########################


######################## Everything in PROCESS - Can't do Monte Carlo ######################## 
# model 59 is a dual-stage conditional IE with the direct effect moderated
process(data = selfc, y = "swl_TOT", x = "self_TOT", m = "scc_TOT", w = "cesd_TOT", model = 59, boot = 10000, seed = 489, progress = 0, jn = 1, wmodval = mod_vals)
# compare to bootstrap
bs_simp_CI
# compare to Monte Carlo
mc_simp_CI

# manual pseudo JN in PROCESS - use wodval to input our range of the moderator
process(data = selfc, y = "swl_TOT", x = "self_TOT", m = "scc_TOT", w = "cesd_TOT", model = 59, boot = 10000, seed = 489, progress = 0, jn = 1, wmodval = mod_vals)


######################## Quick Visual Plots ########################
#################### a3 #################### 
# quick prediction data set we could get from PROCESS, but manual
pred_a3 <- expand.grid(
  # values for X (predictor)
  self_TOT = seq(min(selfc$self_TOT), max(selfc$self_TOT), length.out = 100),
  # values for W (moderator)
  cesd_TOT = c(mDEP - sdDEP, mDEP, mDEP + sdDEP)
)
# generate predictions 
pred_a3$scc_TOT <- predict(m_mod, pred_a3)

# prepare for plotting
pred_a3$W_level <- factor(
  rep(c("Low (-1 SD)", "Mean", "High (+1 SD)"), each = 100),
  levels = c("High (+1 SD)", "Mean", "Low (-1 SD)")
)

# create the plot
ggplot(data = selfc, aes(x = self_TOT, y = scc_TOT)) +
  geom_point(aes(color = cesd_TOT)) + 
  geom_line(data = pred_a3, aes(x = self_TOT, y = scc_TOT, linetype = W_level), linewidth = 1, color = "black") +
  geom_rug(position = "jitter", outside = TRUE, sides = "tr") + 
  coord_cartesian(clip = "off") +
  labs(x = "Self-Compassion Score (X)", 
       y = "Self-Concept Clarity Score (Y)") + 
  scale_color_gradient(low = "lightblue", 
                       high = "navy",
                       name = "Depression (W)", 
                       breaks = c(10,50), 
                       labels = c("Low","High")) +
  scale_linetype_manual(name = "Depression Level",
                        values = c("High (+1 SD)" = "solid", "Mean" = "dashed", "Low (-1 SD)" = "dotted")) +
  jtools::theme_apa() +
  theme(legend.title = element_text())

#################### b3 #################### 
# quick prediction data set we could get from PROCESS, but manual
pred_b3 <- expand.grid(
  # values for M (predictor)
  scc_TOT = seq(min(selfc$scc_TOT), max(selfc$scc_TOT), length.out = 100),
  # values for W (moderator)
  cesd_TOT = c(mDEP - sdDEP, mDEP, mDEP + sdDEP),
  # values for X (covariate)
  self_TOT = mean(selfc$self_TOT)
)
# generate predictions 
pred_b3$swl_TOT <- predict(y_mod, pred_b3)

# prepare for plotting
pred_b3$W_level <- factor(
  rep(c("Low (-1 SD)", "Mean", "High (+1 SD)"), each = 100),
  levels = c("High (+1 SD)", "Mean", "Low (-1 SD)")
)

# create the plot
ggplot(data = selfc, aes(x = scc_TOT, y = swl_TOT)) +
  geom_point(aes(color = cesd_TOT)) + 
  geom_line(data = pred_b3, aes(x = scc_TOT, y = swl_TOT, linetype = W_level), linewidth = 1, color = "black") + 
  geom_rug(position = "jitter", outside = TRUE, sides = "tr") + 
  coord_cartesian(clip = "off") +
  labs(x = "Self-Concept Clarity Score (M)", 
       y = "Satisfaction with Life Score (Y)") + 
  scale_color_gradient(low = "tan2", 
                       high = "darkred",
                       name = "Depression (W)", 
                       breaks = c(10,50), 
                       labels = c("Low","High")) +
  scale_linetype_manual(name = "Depression Level",
    values = c("High (+1 SD)" = "solid", "Mean" = "dashed", "Low (-1 SD)" = "dotted")) +
  jtools::theme_apa() +
  theme(legend.title = element_text())

#################### c' moderated #################### 
# quick prediction data set we could get from PROCESS, but manual
pred_cp3 <- expand.grid(
  # values for X (predictor)
  self_TOT = seq(min(selfc$self_TOT), max(selfc$self_TOT), length.out = 100),
  # values for W (moderator)
  cesd_TOT = c(mDEP - sdDEP, mDEP, mDEP + sdDEP),
  # values for M (covariate)
  scc_TOT = mean(selfc$scc_TOT)
)
# generate predictions 
pred_cp3$swl_TOT <- predict(y_mod, pred_cp3)

# prepare for plotting
pred_cp3$W_level <- factor(
  rep(c("Low (-1 SD)", "Mean", "High (+1 SD)"), each = 100),
  levels = c("High (+1 SD)", "Mean", "Low (-1 SD)")
)

# create the plot
ggplot(data = selfc, aes(x = self_TOT, y = swl_TOT)) +
  geom_point(aes(color = cesd_TOT)) + 
  geom_line(data = pred_cp3, aes(x = self_TOT, y = swl_TOT, linetype = W_level), linewidth = 1, color = "black") + 
  geom_rug(position = "jitter", outside = TRUE, sides = "tr") + 
  coord_cartesian(clip = "off") +
  labs(x = "Self-Compassion Score (X)", 
       y = "Satisfaction with Life Score (Y)") + 
  scale_color_gradient(low = "lightgreen", 
                       high = "darkgreen",
                       name = "Depression (W)", 
                       breaks = c(10,50), 
                       labels = c("Low","High")) +
  scale_linetype_manual(name = "Depression Level",
                        values = c("High (+1 SD)" = "solid", "Mean" = "dashed", "Low (-1 SD)" = "dotted")) +
  jtools::theme_apa() +
  theme(legend.title = element_text())


### end of script 