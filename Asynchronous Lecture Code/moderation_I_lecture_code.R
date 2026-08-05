# R script created by Jacob J. Coutts (Copyright 2026)
# Moderation Lecture 1 (PSYC489K)

# load required packages 
library(ggplot2)
library(jtools)
source("~/PSYC489K-SU26/process_v5.R")
# Make sure the source command above routes to where PROCESS is located in YOUR computer

# read in data and summarize - be sure to change file path to appropriate location on your computer
vgames <- read.csv(file="~/PSYC489K-SU26/Datasets/videogames.csv")
summary(vgames)

###### Simple moderation with perceived performance (continuous moderator) - manual 
### modeling
perform_mod <- lm(like ~ comp_cond*perform_o, data = vgames)
summary(perform_mod)

### visualizations and probing - simple slopes mean +/- 1 SD
x_values <- rep(c(0,1),3) # predictor values for probing via simple slopes
mod_values <- c(rep((mean(vgames$perform_o) - sd(vgames$perform_o)),2),rep(mean(vgames$perform_o),2),rep((mean(vgames$perform_o) + sd(vgames$perform_o)),2)) # moderator values for probing via simple slopes
y_values <- predict(perform_mod, data.frame(comp_cond = x_values, perform_o=mod_values)) # outcome values for probing via simple slopes
simp_slope_perform <- data.frame(x = x_values, w = factor(c("Low (-1SD)","Low (-1SD)","Medium (Mean)","Medium (Mean)","High (+1SD)","High (+1SD)"), levels = c("Low (-1SD)","Medium (Mean)","High (+1SD)"), ordered=TRUE), y = y_values)

### old simple slopes continuous moderator ggplot
ggplot(data=simp_slope_perform) + # lets ggplot know the data frame we're working with
  geom_line(aes(x=x,y=y,color=w)) + # create lines
  geom_point(aes(x=x, y =y, color=w)) + # add dots at end of lines
  theme_bw() + # this makes it neat and tidy
  theme(panel.grid.major = element_blank(), # start theme code, remove grid
        panel.grid.minor = element_blank(), # remove grid
        panel.background = element_rect(color="black"), # remove background
        axis.title.x = element_text(hjust=.5), # center x axis title
        axis.title.y = element_text(hjust=.5), # center y axis title
        axis.line = element_line(), # add axis lines
        legend.background = element_rect(color="black")) + # end theme code
  guides(color=guide_legend(title="Performance (Other)")) +
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  scale_x_continuous(breaks = c(0,1), labels = c("Cooperative","Competitive")) + # change values on x-axis
  labs(x="Game Type",y="Affective Attraction") # relabel axes

### probing
# create centered moderator values at mean +/- 1 SD
vgames$low_per <- vgames$perform_o - mean(vgames$perform_o) - sd(vgames$perform_o)
vgames$med_per <- vgames$perform_o - mean(vgames$perform_o)
vgames$high_per <- vgames$perform_o - mean(vgames$perform_o) + sd(vgames$perform_o)
# fit the models with the centered variables as the moderator
low_w_mod <- lm(like~low_per*comp_cond, data = vgames); summary(low_w_mod)
med_w_mod <- lm(like~med_per*comp_cond, data = vgames); summary(med_w_mod)
high_w_mod <- lm(like~high_per*comp_cond, data = vgames); summary(high_w_mod)

## JN (floodlight analysis) - manual
tcrit <- qt(.975,nrow(vgames)-4)
jna <- tcrit^2 * vcov(perform_mod)[4,4]-coef(perform_mod)[4]^2
jnb <- 2*(tcrit^2 *vcov(perform_mod)[2,4]-coef(perform_mod)[2]*coef(perform_mod)[4])
jnc <- tcrit^2 * vcov(perform_mod)[2,2]-coef(perform_mod)[2]^2
# regions of significance - manual
r1 <- (-jnb + sqrt(jnb^2 - 4*jna*jnc))/(2*jna);r1
r2 <- (-jnb - sqrt(jnb^2 - 4*jna*jnc))/(2*jna);r2
percb1 <- sum(as.numeric(vgames$perform_o <= r1))/nrow(vgames)*100;percb1
percb2 <- sum(as.numeric(vgames$perform_o >= r2))/nrow(vgames)*100;percb2
totperc <- percb1 + percb2; totperc

###### simple moderation with perceived performance (continuous moderator) - PROCESS
process(data = vgames, x = "comp_cond",w = "perform_o", y = "like", model = 1, plot =1, jn =1,moments =1) 

# JN via process
jnres <- process(data = vgames, x = "comp_cond",w = "perform_o", y = "like", model = 1, plot =1, jn =1,moments =1, save =2) 

perfx <- jnres[12:33,1];effex <- jnres[12:33,2];llciex <- jnres[12:33,6];ulciex <- jnres[12:33,7]
# J-N plot 
plot(x=perfx,y=effex,type="l",pch=19,lwd=3,col="black",ylab="Conditional Effect of Game Type",xlab="Perceived Performance (W)")
points(perfx,llciex,lwd=1.75,lty=2,type="l",col="red")
points(perfx,ulciex,lwd=1.75,lty=2,type="l",col="red")
abline(h=0,untf=FALSE,lty=3,lwd=1)
abline(v=1.144,untf=FALSE,lty=3,lwd=1)
text(.95,.1,"1.14",cex=.8)
abline(v=4.2010,untf=FALSE,lty=3,lwd=1)
text(4.5,.1,"4.20",cex=.8)

## new graph, combination of scatterplot, simple slopes, and J-N
simpmod <- lm(data=vgames,like~comp_cond*perform_o)
b0 <- coef(simpmod)[1];b1 <- coef(simpmod)[2];b2 <- coef(simpmod)[3];b3 <- coef(simpmod)[4]
sintlow <- b0 + b2*(mean(vgames$perform_o)-sd(vgames$perform_o));sslolow <- b1 + b3*(mean(vgames$perform_o)-sd(vgames$perform_o))
sintmed <- b0 + b2*(mean(vgames$perform_o));sslomed <- b1 + b3*(mean(vgames$perform_o))
sinthigh <- b0 + b2*(mean(vgames$perform_o)+sd(vgames$perform_o));sslohigh <- b1 + b3*(mean(vgames$perform_o)+sd(vgames$perform_o))
sintsig1 <- b0 + b2*(1.144);sslosig1 <- b1 + b3*(1.144)
sintsig2 <- b0 + b2*(4.201);sslosig2 <- b1 + b3*(4.201)
vgames$modinROS = as.numeric(vgames$perform_o <= 1.1440 | vgames$perform_o >=4.2010)

# new visual with all the bells and whistles
ggplot() + 
  geom_point(data=vgames,aes(y=like,x=comp_cond,color=as.factor(modinROS)), position=position_jitterdodge(dodge=0.6) ) + # add points and color by factor
  geom_rug(data=vgames, aes(y=like,x=comp_cond),outside=TRUE,sides="tr", position = "jitter") + # allows for rug plot
  coord_cartesian(clip = "off") + # allows for rug plot
  geom_abline(slope=sslolow,intercept=sintlow,color="#031D38", size = 1) + # slope at low levels
  geom_abline(slope=sslomed,intercept=sintmed,color="#346E9D", size = 1) + # slope at medium levels
  geom_abline(slope=sslohigh,intercept=sinthigh,color="#55AEF3", size = 1) + # slope at high levels
  geom_abline(slope=sslosig1,intercept=sintsig1,color="black",linetype="dotted", size =1) + # add region 1
  geom_abline(slope=sslosig2,intercept=sintsig2,color="black",linetype="dotted", size =1) + # add region 2
  theme_bw() +
  scale_color_manual(name="Performance Value (W)", labels = c("Not in ROS","ROS"), values = c("red2","green4")) + 
  scale_x_continuous(expand = expansion(mult=.005)) +
  scale_y_continuous(expand = expansion(mult=.005)) + 
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        panel.background = element_rect(color="black"), # remove background
        axis.title.x = element_text(hjust=.5), # center x axis title
        axis.title.y = element_text(hjust=.5), # center y axis title
        axis.line = element_line(),
        legend.background = element_rect(color="black")) + # add axis lines, end theme adjustments
  labs(x="Game Type",y="Affective Attraction") + # relabel axes
  scale_x_continuous(breaks = c(0,1), labels = c("Cooperative","Competitive")) + # change values on x-axis
  annotate(geom="text",size=4, x=.5,y=1.7,label="65.03% of data in region of significance") # add in ROS info

###### Simple moderation with losing disposition (dichotomous moderator) - manual 
soreLmod <- lm(like~hate_lose*comp_cond, data = vgames) # fit the model
summary(soreLmod) # check the results

# simple slopes
x_values_d <- rep(c(0,1),2) # predictor values for probing via simple slopes
mod_values_d <- c(rep(0,2),rep(1,2)) # moderator values for probing via simple slopes
y_values_d <- predict(soreLmod, data.frame(comp_cond = x_values_d, hate_lose=mod_values_d)) # outcome values for probing via simple slopes
simp_slope_hate_lose <- data.frame(x = x_values_d, w = factor(c("Yes","Yes","No","No")), y = y_values_d)

### old simple slopes dichotomous moderator ggplot
ggplot(data=simp_slope_hate_lose) + 
  geom_point(mapping=(aes(x=x, y = y, color=w))) + 
  geom_line(mapping=aes(x=x,y=y, color=w)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        panel.background = element_rect(color="black"), # remove background
        axis.title.x = element_text(hjust=.5), # center x axis title
        axis.title.y = element_text(hjust=.5), # center y axis title
        axis.line = element_line(), # add axis lines
        plot.title = element_text(hjust=.5, color="grey0", face="plain"), 
        legend.background = element_rect(color="black")) + 
  guides(color=guide_legend(title="Sore Loser")) +
  scale_color_discrete(labels=c("Yes","No")) +
  scale_x_continuous(breaks = c(0,1), labels = c('Cooperative','Competitive')) + 
  labs(x = "Game Type", y="Affective Attraction")

###### simple moderation with losing disposition (dichotomous moderator) - PROCESS
process(data = vgames, x = "comp_cond",w = "hate_lose", y = "like", model = 1, plot = 1) 

### new visualization
ggplot(data=simp_slope_hate_lose) + # lets ggplot know the data frame we're working with
  geom_boxplot(vgames, mapping=aes(x= comp_cond, y = like, color = as.factor(hate_lose))) + # optional for new vis
  geom_line(aes(x=x,y=y,color=as.factor(mod_values_d))) + # create lines
  geom_point(aes(x=x, y =y, color=as.factor(mod_values_d))) + # add dots at end of lines
  theme_bw() + # this makes it neat and tidy
  theme(panel.grid.major = element_blank(), # start theme code, remove grid
        panel.grid.minor = element_blank(), # remove grid
        panel.background = element_rect(color="black"), # remove background
        axis.title.x = element_text(hjust=.5), # center x axis title
        axis.title.y = element_text(hjust=.5), # center y axis title
        axis.line = element_line(), # add axis lines
        legend.background = element_rect(color="black")) + # end theme code
  scale_color_discrete(labels=c("Yes","No")) +
  guides(color=guide_legend(title="Sore Loser")) +
  scale_x_continuous(breaks = c(0,1), labels = c("Cooperative","Competitive")) + # change values on x-axis
  labs(x="Game Type",y="Affective Attraction") # relabel axes


### end of script