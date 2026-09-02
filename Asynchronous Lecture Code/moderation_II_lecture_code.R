# R script created by Jacob J. Coutts (Copyright 2026)
# Moderation Lecture II (PSYC489K)

# load required packages 
library(ggplot2)
library(jtools)

# Make sure the source command above routes to where PROCESS is located in YOUR computer
source("~/PSYC489K-SU26/process_v5.R")

# read in data and summarize - be sure to change file path to appropriate location on your computer
vgames <- read.csv(file="~/PSYC489K-SU26/Datasets/videogames_l4.csv")
summary(vgames)

# we need to mean center the perceived performance of the other play so it has a meaningful interpretation
vgames$perform_o_cen <- vgames$perform_o - mean(vgames$perform_o)
mean(vgames$perform_o_cen)
# see that it doesn't change anything about associations
cor(vgames)


############ Simple moderation with perceived performance (continuous moderator centered vs. not) - manual ############ 
### modeling
process(data = vgames, x = "comp_cond",w = "perform_o", y = "like", model = 1, plot =1, center=1, jn =1,moments =1) 
process(data = vgames, x = "comp_cond",w = "perform_o", y = "like", model = 1, plot =1, jn =1,moments =1) 
# the interaction effect does not change and is still significant


############ Multiple moderation with perceived performance and losing disposition - Additive (two two-way interactions) ############ 
additive_mod <- lm(like ~ comp_cond*perform_o_cen + comp_cond*hate_lose, data = vgames)
summary(additive_mod)
# one of the interactions is significant. Now, let's probe to see what is going on. In a model like this, you can't really do the Johnson-Neyman. You instead do two sets of simple slope plots. We will ignore the non-significant interaction for probing. 

#### obtain mean +/- one Sd #### 
wlow = mean(vgames$perform_o_cen)-sd(vgames$perform_o_cen)
wmed = mean(vgames$perform_o_cen)
whigh = mean(vgames$perform_o_cen)+sd(vgames$perform_o_cen)

# new, adjusted variables
vgames$perform_o_low = vgames$perform_o_cen-wlow
vgames$perform_o_high = vgames$perform_o_cen-whigh
vgames$hate_lose_INV = as.numeric(vgames$hate_lose == 0)
# we already have the centered variable from mean centering 

# tests of the simple effects   
# low performers - sore losers
low_mod_sl = lm(like ~ comp_cond*hate_lose + comp_cond*perform_o_low, data = vgames)
summary(low_mod_sl)
# low performers - not sore losers (use flipped variable so other is reference)
low_mod_Nsl = lm(like ~ comp_cond*hate_lose_INV + comp_cond*perform_o_low, data = vgames)
summary(low_mod_Nsl)
# sig and neg for sore losers only
# positive, but not sig for either group

# average performers - sore losers
summary(additive_mod) # we already fit the centered model simple effect
# average performers - not sore losers (remove first interaction because hate lose will be at 0)
med_mod_Nsl = lm(like ~ comp_cond*hate_lose_INV + comp_cond*perform_o_cen, data = vgames)
summary(med_mod_Nsl)
# sig and neg for sore losers only

# high performers - sore losers
high_mod_sl = lm(like ~ comp_cond*hate_lose + comp_cond*perform_o_high, data = vgames)
summary(high_mod_sl)
# high performers - note sore losers (remove first interaction because hate lose will be at 0)
high_mod_Nsl = lm(like ~ comp_cond*hate_lose_INV + comp_cond*perform_o_high, data = vgames)
summary(high_mod_Nsl)
# sig and neg regardless of whether sore loser

#### now, let's plot the pick-a-point approach ####
# create a dataframe with values for X, W, and Z
temp = data.frame(comp_cond = rep(c(0,1), 6), hate_lose = c(rep(0, 6), rep(1,6)), perform_o_cen = rep(c(wlow,wmed,whigh), 4))

# generate predictions
ypred = predict(additive_mod, temp)

# final probing dat
probe_add_dat = data.frame(x = rep(c("Cooperative","Competitive"), 6), w = rep(c("Low Performance","Average Performance","High Performance"),4), wraw = temp$perform_o_cen, z = c(rep("Sore Loser",6),rep("Non-sore Loser", 6)), y = ypred)

# change X values to factors
# game type
probe_add_dat$x = factor(probe_add_dat$x, levels = c("Cooperative", "Competitive"))
# perceived performance (centered)
probe_add_dat$w = factor(probe_add_dat$w, levels = c("Low Performance","Average Performance","High Performance"))
# sore loser status
probe_add_dat$z = factor(probe_add_dat$z, levels = c("Sore Loser", "Non-sore Loser"))

# create the plot
ggplot(data=probe_add_dat) + # lets ggplot know the data frame we're working with
  geom_line(aes(x=x,y=y,color=w, group = w)) + # create lines
  facet_grid(~z) + # put plots side by side
  geom_point(aes(x=x, y=y, color=w)) + # add points to end of lines
  theme_bw() + # this makes it neat and tidy  
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        axis.title.x = element_text(hjust=.5), # center x-axis
        axis.title.y = element_text(hjust=.5), # center y-axis
        panel.background = element_rect(color="black"), # box around legend elements
        legend.background = element_rect(color="black"), # box around legend 
        plot.title = element_text(hjust = 0.5)) + # center plot title
  guides(color=guide_legend(title="Perceive Performance Centered (W)")) + # add legend title
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) + # change colors manually
  labs(x="Game Type (X)",y="Affective Attraction (Y)", title = "Losing Type (Z)") # label axes


############## now do all of the above in PROCESS
process(data = vgames,x = "comp_cond",w = "perform_o_cen",z= "hate_lose",y = "like",model = 2,plot =1,moments = 1) 

# from the PROCESS output, can rerun figure with this instead to see it's the same
probe_add_dat2 = data.frame(x = c(0,1,0,1,0,1,0,1,0,1,0,1), w = c(-1.8389,-1.8389,-1.8389,-1.8389,0,0,0,0, 1.8389,1.8389,1.8389,1.8389), z = c(0,0,1,1,0,0,1,1,0,0,1,1), y = c(4.3337,4.3601,4.3717,4.6169, 4.4605, 3.9870, 4.4985, 4.2438, 4.5873, 3.6139, 4.6253, 3.8707))

# create the plot
ggplot(data=probe_add_dat2) + # lets ggplot know the data frame we're working with
  geom_line(aes(x=x,y=y,color=w, group = w)) + # create lines
  facet_grid(~z) + # put plots side by side
  geom_point(aes(x=x, y=y, color=w)) + # add points to end of lines
  theme_bw() + # this makes it neat and tidy  
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        axis.title.x = element_text(hjust=.5), # center x-axis
        axis.title.y = element_text(hjust=.5), # center y-axis
        panel.background = element_rect(color="black"), # box around legend elements
        legend.background = element_rect(color="black"), # box around legend 
        plot.title = element_text(hjust = 0.5)) + # center plot title
  guides(color=guide_legend(title="Perceive Performance Centered (W)")) + # add legend title
  #scale_color_discrete(values=c("#F8766D","#00BA38","#619CFF")) + # change colors manually
  labs(x="Game Type (X)",y="Affective Attraction (Y)", title = "Losing Type (Z)") # label axes

############ Multiple moderation with perceived performance and losing disposition - Multiplicative (three-way interaction) ############ 
mutiply_mod <- lm(data = vgames, like~comp_cond*hate_lose*perform_o_cen) # fit the model 
summary(mutiply_mod) # check the results

# now fit it and probe in PROCESS
process(data=vgames, y="like", x="comp_cond", w="perform_o_cen", z="hate_lose", model=3, plot = 1, moments = 1)

# create probing data
mod_mod_probe = data.frame(x = rep(c("Cooperative","Competitive"), 6), w = c(rep("Low Performance", 4), rep("Average Performance", 4), rep("High Performance", 4)), z = rep(c("Sore Loser", "Sore Loser", "Not Sore Loser", "Not Sore Loser"),3), y = c(4.3916, 4.0655, 4.3003, 4.9879, 4.4618, 3.9862, 4.5041, 4.2564, 4.5320, 3.9070, 4.7079, 3.5249))

# make factors for plotting
# predictor - condition
mod_mod_probe$x = factor(mod_mod_probe$x, levels = c("Cooperative","Competitive"))
# moderator - losing disposition
mod_mod_probe$z = factor(mod_mod_probe$z, levels = c("Sore Loser","Not Sore Loser")) 

# create the plot
ggplot(data=mod_mod_probe) + # lets ggplot know the data frame we're working with
  geom_line(aes(x=x,y=y,color=w, group = w)) + # create lines
  facet_grid(~z) + # put plots side by side
  geom_point(aes(x=x, y=y, color=w)) + # add points to end of lines
  theme_bw() + # this makes it neat and tidy  
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        axis.title.x = element_text(hjust=.5), # center x-axis
        axis.title.y = element_text(hjust=.5), # center y-axis
        panel.background = element_rect(color="black"), # box around legend elements
        legend.background = element_rect(color="black"), # box around legend 
        plot.title = element_text(hjust = 0.5)) + # center plot title
  guides(color=guide_legend(title="Perceive Performance Centered (W)")) + # add legend title
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) + # change colors manually
  labs(x="Game Type (X)",y="Affective Attraction (Y)", title = "Losing Type (Z)") # label axes

# run the model so the jn will work
process(data=vgames, y="like", x="comp_cond", w="hate_lose", z="perform_o_cen", model=3, jn = 1, plot = 1, moments = 1)



### end of script 