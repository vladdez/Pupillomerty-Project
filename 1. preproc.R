library(dplyr)
library(data.table)
library(tibble)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(stringr)
library(gtools) 

setwd("C:/Users/Vladimir/YandexDisk/work/Joe/round 3 rev")


### import
#1 folder managment
common_path = "C:/Users/Vladimir/YandexDisk/work/Joe/round 3 rev"

list.filenames1<-list.files("3. r", pattern=".csv$")
list.filenames1 
list.data1<-list()
list.data1
for (i in 1:length(list.filenames1)) {
  list.data1[[i]]<-fread(file = paste("3. r/", list.filenames1[i], sep = ""))
}
list.data1



##### loop1 - from eyetracker

for (i in 1:length(list.data1)) {
  list.data1[[i]] <-  data.frame(list.data1[[i]]$trial, list.data1[[i]]$time, list.data1[[i]]$x, list.data1[[i]]$y, list.data1[[i]]$pupil, list.data1[[i]]$epoch, list.data1[[i]]$saccade, 
                                 list.data1[[i]]$error, list.data1[[i]]$sound,  list.data1[[i]]$order,  list.data1[[i]]$circle)
  list.data1[[i]]$participant <- i
  
}


big_etc <- data.frame()
i = 1
while (i != 21) {
  et <-  list.data1[[i]]
  et <- et %>% rename(trial = list.data1..i...trial, time = list.data1..i...time, x = list.data1..i...x,  
                      y = list.data1..i...y, pupil = list.data1..i...pupil, epoch = list.data1..i...epoch, saccade = list.data1..i...saccade, 
                      error = list.data1..i...error, sound = list.data1..i...sound, order = list.data1..i...order, circle = list.data1..i...circle) 

  big_etc <- rbind(big_etc, et)
  i = i + 1
}
# last 3-4 minutes

big <- big_etc %>% group_by(participant, trial) %>% mutate(er = sum(error)) %>% filter(er == 0) 

big_etc <- big %>% select(-error, -er) %>% filter(participant != 2)  %>%  filter(epoch != 0)

big <- big_etc %>% group_by(participant, trial) %>% mutate(order = order[time == 50], circle = circle[time == 50], sound = sound[time == 50])
what <- big_etc %>% group_by(participant, trial) %>%  filter(epoch == 0) %>% summarise(max(time))
what2 <- big_etc %>% group_by(participant, trial) %>%  filter(epoch == 3) %>% summarise(max(time))
# changing epochs

##################
write.csv(big, file = "preproc.csv", row.names=FALSE)











