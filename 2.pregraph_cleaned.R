library(dplyr)
library(data.table)
library(tibble)
library(ggplot2)
library(tidyr)
library(grid)
library(beepr)
library(lattice)
library(nlme)
library(readr)

setwd("C:/Users/Vladimir/YandexDisk/work/Joe/round 3 rev")
etc <- fread("preproc.csv")

#1. preproc
#1.1

#baseline correction for pupil 
# pn - pupil normalized
etc1 <- etc %>% 
  group_by(participant, trial)  %>% # time of souns command
  mutate(esound = min(time[epoch  == "3"])) %>% mutate(ssound = min(time[epoch  == "2"])) %>% 
  mutate(bl = ifelse(time < ssound, pupil, NA))  %>% #normalization
  ungroup %>% 
  mutate(baseline = mean(bl, na.rm=TRUE))  %>% group_by(participant, trial, order) %>% 
  mutate(pn = ((pupil - baseline)/baseline)*100) 

#1.2
# saccade start
etc2 <- etc1 %>% group_by(trial, participant) %>% 
  mutate(ssac = min(time[epoch  == "3"  & saccade == 3])) %>%  filter(ssac != Inf) %>% mutate(RT1 = ssac - ssound, RT2 = ssac - esound)

##### timecut
#ec <- etc2 %>% mutate(timecut = time - ssac) %>%  filter(timecut < 0, timecut > -1500) %>% ungroup()  
w <- etc2 %>% group_by(trial, participant) %>%  mutate(timecut = time - ssac)  %>%
  mutate(esound = min(timecut[epoch  == "3"])) %>% mutate(ssound = min(timecut[epoch == "2"])) %>% 
  filter(timecut < 2200) 

#1.2.1 Comparison of fixations in different orders

## SFIX = 4, SSAC = 3
rm(etc)
rm(etc1)
rm(etc2)


# 1.5 permutation test

d_reduced <- w %>% select(x, pn, timecut, sound, participant, trial, order) %>% 
  filter(!is.na(x), timecut > -2000, timecut < 2000) %>% 
  group_by(timecut, order, sound)

d_reduced1 <- d_reduced %>% 
  mutate(n = n(),
         pupl_med = mean(pn)) %>% 
  select(pupl_med, n, timecut, sound, order) %>% slice(1) %>%  ungroup()


#write_csv(d_reduced1, path = "pregraph_reduced.csv")

g <-  as.data.frame(d_reduced1) %>% 
  mutate(fix1 = ifelse(order == 's', 317, 292), fix2 = ifelse(order == 'p', 786, 777)) %>% 
  mutate(order = recode(order, 's' = "Sequential planning", 'p'  = "Parallel planning"))


a <- -1468
b <- -690
ggplot() + 
  geom_rect(data=data.frame(xmin = -100, ymin = -Inf, xmax = 0, ymax = Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill="[-0.1, 0]"), colour=NA, alpha=0.5) + 
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
    
  theme(plot.title = element_text(hjust = 0.5)) 


#########################################
# order check
d_reduced4 <- w %>% select(x, pn, timecut, sound, participant, trial, order) %>% 
  filter(!is.na(x), timecut > -2000, timecut < 2000)  
d_reduced4 <- d_reduced4 %>% ungroup  %>% select(pn, order, participant, trial, timecut)
d_reduced4$order <- as.factor(d_reduced4$order)
d_reduced4$pn <- round(d_reduced4$pn, digits = 3)  


time = -1999
pmt <- function(d_reduced4, time){
   p1 <- d_reduced4 %>%  filter(timecut >= time, timecut <= time + 10) %>% group_by(participant, trial, order) %>% 
        mutate(pnm = round(mean(pn), digits = 3)) %>% select(pnm, pn, participant, trial, order) %>% slice(1) %>% ungroup %>% 
        mutate(n = 1:length(order)) %>% 
        select(-trial, -participant)
  
  p1$order <- as.factor(p1$order)
  p1$pnm <- round(p1$pnm, digits = 3)
  median(p1$pnm[p1$order=="p"])
  median(p1$pnm[p1$order=="s"])
  # lets calculate the absolute diff in means
  test.stat2 <- abs(median(p1$pnm[p1$order =="p"]) - 
                      median(p1$pnm[p1$order =="s"])) 
  set.seed(1979)  
  n <- length(p1$pnm)  
  P <- 1000
  variable <- p1$pnm 
  PermSamples <- matrix(0, nrow=n, ncol=P)
  for(i in 1:P){
    PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
  }
  Perm.test.stat1 <- Perm.test.stat2 <- rep(0, P)
  for (i in 1:P){
    Perm.test.stat2[i] <- abs( median(PermSamples[p1$order =="p",i]) - 
                                 median(PermSamples[p1$order =="s",i]) )
  }
  out <- mean( Perm.test.stat2 >= test.stat2)
  print(time)
  return(out)
}

pmatrix <- matrix(0, nrow=401, ncol=2)
i = 1
for (time in -200:200)
{
  pmatrix[i, 1] <- pmt(d_reduced, time*10)
  pmatrix[i, 2] <- time*10
  print(pmatrix[i,1])
  print(pmatrix[i,2])
  i = i + 1

}
pmatrix <-as.data.frame(pmatrix[1:400, 1:2])
ggplot() + geom_line(data = pmatrix, aes(x = V2, y = V1)) + 
  geom_hline(aes(yintercept = 0.05, linetype = "0.05"), color = "red") + 
  scale_linetype_manual(name = "alpha level", values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  scale_x_continuous(name="Time (sec)", 
                     labels=c("-2000" = "-2", "-1000" = "-1", 
                              "0" = "0\nstart of\nsaccade", "1000" = "1", "2000" = "2")) +
  ggtitle("Result of cluster mass test:\npupil size ~ saccadic order")+
  scale_y_continuous(name="Monte Carlo p-values") +
  theme_bw(base_size = 18, base_family = "Times New Roman")+
  theme(plot.title = element_text(hjust = 0.5)) 
beep(sound = 4)

pmatrix %>% filter(V1 <= 0.05)
######################################################
p
# sound (light-dark) check

d_reduced <- w %>% select(x, pn, timecut, sound, participant, trial, order) %>% 
  filter(!is.na(x), timecut > -2000, timecut < 2000)  
d_reduced <- d_reduced %>% ungroup %>% select(pn, sound, participant, trial, timecut)
d_reduced$sound <- as.factor(d_reduced$sound)
d_reduced$pn <- round(d_reduced$pn, digits = 3)  
d_reduced5 <- d_reduced 
time = -1999
pmt_s <- function(d_reduced5, time)
{
   p1 <- d_reduced5 %>%  filter(timecut >= time, timecut <= time + 10) %>% group_by(participant, trial, sound) %>% 
        mutate(pnm = round(mean(pn), digits = 3)) %>% select(pnm, pn, participant, trial, sound) %>% slice(1) %>% ungroup %>% 
        mutate(n = 1:length(sound)) %>% 
        select(-trial, -participant)
  
  median(p1$pnm[p1$sound=="l"])
  median(p1$pnm[p1$sound=="r"])
  # lets calculate the absolute diff in means
  test.stat2 <- abs(median(p1$pnm[p1$sound =="l"]) - 
                      median(p1$pnm[p1$sound =="r"])) 
  set.seed(1979)  
  n <- length(p1$pnm)  
  P <- 1000
  variable <- p1$pnm 
  PermSamples <- matrix(0, nrow=n, ncol=P)
  for(i in 1:P){
    PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
  }
  Perm.test.stat1 <- Perm.test.stat2 <- rep(0, P)
  for (i in 1:P){
    Perm.test.stat2[i] <- abs( median(PermSamples[p1$sound =="l",i]) - 
                                 median(PermSamples[p1$sound =="r",i]) )
  }
  out <- mean( Perm.test.stat2 >= test.stat2)
  print(time)
  rm(p1)
  return(out)
}

pmatrix_s <- matrix(0, nrow=401, ncol=2)
i = 1
for (time in -200:200)
{
  pmatrix_s[i, 1] <- pmt_s(d_reduced5, time*10)
  pmatrix_s[i, 2] <- time*10
  print(pmatrix_s[i,1])
  print(pmatrix_s[i,2])
  i = i + 1

}

pmatrix_s <-as.data.frame(pmatrix_s[1:400, 1:2])
ggplot() + geom_line(data = pmatrix_s, aes(x = V2, y = V1)) + 
  geom_hline(aes(yintercept = 0.05, linetype = "0.05"), color = "red") + 
  scale_linetype_manual(name = "alpha level", values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  scale_x_continuous(name="Time (sec)", 
                     labels=c("-2000" = "-2", "-1000" = "-1", 
                     "0" = "0\nstart of\nsaccade", "1000" = "1", "2000" = "2")) +
  ggtitle("Result of cluster mass test:\npupil size ~ luminance")+
  scale_y_continuous(name="Monte Carlo p-values") +
  theme_bw(base_size = 18, base_family = "Times New Roman")+
  theme(plot.title = element_text(hjust = 0.5)) 
  




beep(sound = 4)
