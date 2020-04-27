library(dplyr)
library(data.table)
library(tibble)
library(ggplot2)
library(tidyr)
library(grid)
library(beepr)
library(lattice)
library(nlme)

setwd("C:/Users/Vladimir/YandexDisk/work/Joe/round 3 rev")
g <- fread("pregraph_reduced.csv")
g <- as.data.frame(g) %>% 
  mutate(fix1 = ifelse(order == 's', 317, 292), fix2 = ifelse(order == 'p', 786, 777)) %>% 
  mutate(order = recode(order, 's' = "Sequential planning", 'p'  = "Parallel planning"))

############################################################################################
####### I. Graphs

#1. graph with vlines

gg <- g %>% filter(participant == 18)
ggplot() + 
  geom_rect(data=data.frame(xmin = -80, ymin = -Inf, xmax = 0, ymax = Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill="[-0.08, 0]"), colour=NA, alpha=0.5) + 
  facet_grid(~order) +
  geom_vline(xintercept = 276, size = 1.5, color = "#48d1cc") + geom_vline(xintercept = 778, size = 1.5, color = "#eceabe") +
  geom_smooth(data = gg, aes(x=timecut, y=mean1, color = sound), stat = "identity", size = 1) +
  scale_x_continuous(name="Time (sec)", labels=c("-2000" = "-2", "-1000" = "-1", "0" = "0\nstart of\nsaccade", 
                                                 "1000" = "1", "2000" = "2")) + 
  ggtitle("Pupil size change in different conditions")+
  scale_y_continuous(name="Pupil size normalized")+
  scale_fill_manual('Remapping\nperiod', values = "#FFDB6D",  
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(name  ="Eye lands on:", labels=c("Light", "Dark"), values=c("#000000",  "#56B4E9")) + 
  theme_bw(base_size = 14, base_family = "Times New Roman")
beep(sound = 4)
ggsave("beloved1.png", width = 7, height = 5)

#2. with confidence interval

gg <- g 
ggplot() +
  geom_rect(data=data.frame(xmin = -80, ymin = -Inf, xmax = 0, ymax = Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="[-0.08, 0]"), 
            colour=NA, alpha=0.5) + 
  geom_line(data = g, aes(x=timecut, y=mean1, color = sound), size = 1.2)+ 
  geom_vline(xintercept = 276, size = 1.5, color = "#48d1cc") + 
  geom_vline(xintercept = 778, size = 1.5, color = "#eceabe") +
  geom_smooth(data = g, aes(x=timecut, y=mean1, ymin = CI_lower, ymax = CI_upper, color = sound), 
              stat = "identity") + 
  scale_color_manual(name  ="Eye lands on:", 
                     labels=c("Light", "Dark"), values=c("#000000",  "#56B4E9"), 
                     breaks=c("light", "dark")) + 
  facet_wrap(~order) + 
  scale_x_continuous(name="Time (sec)") + 
  ggtitle("Pupil size change in different conditions")+
  scale_y_continuous(name="Pupil size normalized")+
  scale_fill_manual('Remapping\nperiod', values = "#FFDB6D",  
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  theme_set(theme_bw(base_size = 28)) 

#3. confidence + vlines + other colors

ggplot() + 
  geom_rect(data=data.frame(xmin = -80, ymin = -Inf, xmax = 0, ymax = Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill="[-0.08, 0]"), colour=NA, alpha=0.5) + 
  facet_grid(~order) +
  geom_line(data = g, aes(x=timecut, y=pupl_med, color = sound), size = 1.2)+ 
  geom_smooth(data = g, aes(x=timecut, y=pupl_med, 
                            color = sound), stat = "identity") + 
  scale_x_continuous(name="Time (sec)", 
                     labels=c("-2000" = "-2", "-1000" = "-1", 
                     "0" = "0\nstart of\nsaccade", "1000" = "1", "2000" = "2")) + 
  ggtitle("Pupil size change in different conditions")+
  scale_y_continuous(name="Pupil size normalized")+
  scale_fill_manual('Remapping\nperiod', values = "#c96029",  
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(name  ="Eye lands on:", labels=c("Dark", "Light"), 
                     values=c("#0b1711", "#56B4E9")) + 
  geom_vline(data = g, aes(xintercept = fix1, linetype = "3 - dot 1"), size = 1, colour = "#56B4E9") + 
  geom_vline(data = g, aes(xintercept = fix2, linetype = "4 - dot 2"), size = 1, colour = "red") +
  geom_vline(data = g, aes(xintercept = a, linetype = "1 - start of cue"), size = 1, colour = "#999999") +
  geom_vline(data = g, aes(xintercept = b, linetype = "2 - end of cue"), size = 1, colour = "#E69F00") +
  scale_linetype_manual(name = "Events:", values = c(2, 2, 2, 2), 
      guide = guide_legend(override.aes = list(color = c("#999999", "#E69F00", "#56B4E9", "red")))) +
  theme_bw(base_size = 18, base_family = "Times New Roman") +
  theme(plot.title = element_text(hjust = 0.5)) 


a <- -1468
b <- -690


ggsave("gr-3.png", width = 7, height = 5)
 ##do it with all participants, not only with one 



# trying to find negativity
gwide <- g %>% group_by(timecut) %>% 
  mutate(gl = mean(pn[sound == "l"]), gr = mean(pn[sound == "r"])) %>% 
  select(gl, gr, order, timecut) %>% mutate(delta = gl-gr) %>% slice(1) %>% ungroup() 



ggplot() + 
  geom_smooth(data = gwide, aes(x=timecut, y=delta, color = order), stat = "identity")
ggsave("2.delta.png", width = 7, height = 5)
beep(sound = 4)

#######################################################################################################

###### II. beautiful stat

dw  <- g %>% group_by(trial) %>% 
  mutate(bw = mean(pn[timecut > - 500 & timecut < -100]), rw = mean(pn[timecut > - 80 & timecut < 0])) %>% 
  select(participant, trial, bw, rw, order, sound) %>% mutate(delta = rw-bw) %>% ungroup() %>% 
  group_by(participant, trial) %>% slice(1)
# stat

model1 <- t.test(data = dw, delta ~ order)
model1

# effect size


t1 = model1$statistic[[1]]
df1 = model1$parameter[[1]]
r1 <- sqrt(t1^2/(t1^2+df1))
r1




m3.nlme = lme(delta ~ sound*order,
              random = ~ 1|participant,
              data = dw)

summary(m3.nlme)
anova(m3.nlme)

#numDF denDF  F-value p-value
#(Intercept)     1  4366 40990.34  <.0001
#sound           1  4366     9.86  0.0017
#order           1  4366     0.68  0.4110
#sound:order     1  4366     1.20  0.2728

tab_model(m3.nlme, digits = 4)
stargazer(m3.nlme, type = "html", title="Descriptive statistics", ci = TRUE, 
          digits=5, 
          column.labels='substraction of pupil size<br>in two bins: [-500; -100] and [-80; 0]', 
          out="table1.htm")

#### extrmeties



extr1 <- g %>% group_by(participant, trial) %>% mutate(d = mean(pn[timecut > -80 & timecut <  100])) %>% 
  ungroup() %>% 
  group_by(participant, trial) %>% slice(1) %>% na.exclude()


#land            1  3767   4.7387  0.0296
#order           1  3767   0.2054  0.6505
#land:order      1  3767   0.9791  0.3225

m3.nlme = lme(d ~ sound*order,
              random = ~ 1|participant,
              data = extr1)


summary(m3.nlme)
anova(m3.nlme)

extr2 <- g %>% group_by(trial) %>% mutate(d = mean(pn[timecut == 430])) %>% 
  ungroup() %>% 
  group_by(participant, trial) %>% slice(1) %>% na.exclude()

m3.nlme = lme(d ~ sound*order,
              random = ~ 1|participant,
              data = extr2)


summary(m3.nlme)
anova(m3.nlme)

extr3 <- w_mean %>% group_by(trial) %>% mutate(d = mean(pn[timecut > 1700 & timecut <  2500])) %>% 
  ungroup() %>% 
  group_by(participant, trial) %>% slice(1) %>% na.exclude()

#land            1  4135   8.8967  0.0029
#order           1  4135   1.3227  0.2502
#land:order      1  4135   1.1993  0.2735

extr4 <- w_mean %>% group_by(trial) %>% mutate(d = mean(pn[timecut > 1700 & timecut <  2000])) %>% 
  ungroup() %>% 
  group_by(participant, trial) %>% slice(1) %>% na.exclude()

#land            1  4135  11.2298  0.0008
#order           1  4135   2.3998  0.1214
#land:order      1  4135   1.4049  0.2360

extr5 <- w_mean %>% group_by(trial) %>% mutate(d = mean(pn[timecut == 1700])) %>% 
  ungroup() %>% 
  group_by(participant, trial) %>% slice(1) %>% na.exclude()

model1 <- t.test(data = extr, delta ~ land)
model1

m3.nlme = lme(d ~ sound*order,
              random = ~ 1|participant,
              data = extr1)
m3.nlme = lme(d ~ land*order,
              random = ~ 1|participant,
              data = extr4)

summary(m3.nlme)
anova(m3.nlme)







model1 <- t.test(data = extr1, d ~ sound)
model1
model2 <- t.test(data = extr1, d ~ order)
model2
# effect size


t1 = model1$statistic[[1]]
df1 = model1$parameter[[1]]
r1 <- sqrt(t1^2/(t1^2+df1))  
r1
#[1] 0.07609039
t2 = model2$statistic[[1]]
df2 = model2$parameter[[1]]
r2 <- sqrt(t2^2/(t2^2+df2))  
r2
# [1] 0.0009980507
###
write.csv(w, file = "bins.csv", row.names=FALSE)
write.csv(ec2, file = "bins2.csv", row.names=FALSE)






############################### LOOOOP###############################

for(i in c(17:20))
{
  ww <- w %>% filter(participant == i)
  
  w_mean <- ww %>% select(x, pn, timecut, land, participant, trial, order) %>% 
    filter(!is.na(x))  %>% 
    group_by(timecut, land, order) %>% 
    mutate(mean1 = mean(pn)) %>% 
    ungroup() %>% mutate(order = recode(order, 'seq' = "Sequential planning", 'par'  = "Parallel planning"))
  
  ggplot() + 
    geom_line(data = w_mean, aes(x = timecut, y = mean1, color = land)) + facet_grid(~order) 
  name = sprintf("myplot%s.png", i)
  ggsave(name, width = 6, height = 4.18)
  
  beep(sound = 2)
  
}
beep(sound = 4)




i <-3
ww <- w %>% filter(participant == i)

w_mean <- ww %>% select(x, pn, timecut, land, participant, trial, order) %>% 
  filter(!is.na(x))  %>% 
  group_by(timecut, land, order) %>% 
  mutate(mean1 = mean(pn)) %>% 
  ungroup() %>% mutate(order = recode(order, 'seq' = "Sequential planning", 'par'  = "Parallel planning"))


name = sprintf("myplot%s.png", i)


beep(sound = 2)