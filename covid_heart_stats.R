# Title: COVID-Heart Stats

# Description: includes viz, lvef t-test (one tail? I think),  and eventually
# whatever else BG has asked for

# Author: Victoria Thomas (MiCOR, USUHS)
# Source file: /Users/victoriathomas/Downloads/MiCORHeartCOIVDDatab-1019figs_DATA_LABELS_2022-10-25_1409.csv
# Output file: fill_in_blank_output.csv
# Created: 10-25-2022
# Updated: 11-22-2022
# Updated: 11-29-2022
###########################

#load package(s)
library(dplyr)

#import data
#only V2 arm 1
lvef_data <- read.csv("/Users/victoriathomas/Documents/MiCOR/COVID-Heart/Analysis/lvef_data.csv")

#import data
#V0-V2, both arms?
lvef_plus <- read.csv("/Users/victoriathomas/Downloads/MiCORHeartCOIVDDatab-1019figs_DATA_LABELS_2022-10-25_1409.csv",
                      stringsAsFactors = FALSE)



#create v1 data.frame
v1 <- lvef_plus %>%
  filter(Event.Name == "V1")

#create v2 data.frame
v2 <- lvef_plus %>%
  filter(Event.Name == "V2")
class(v2)

#filter only participants  with rest echo value 
v2 <- v2 %>%
  filter(!is.na(Rest.echocardiogram..LVEF))
#or
lvef_plus %>%
  group_by(Record.ID) %>%
  filter(!is.na(Age))
#or
lvef_plus %>%
  filter(!is.na(Age))

#merge v1 and v2 datasets by "Record.ID"
merge_v1_v2 <-  merge(v1, v2, by = c("Record.ID"))

#remove blank columns
library(tidyverse)
other <- merge_v1_v2 %>%
  discard(~all(is.na(.) | . ==""))

#filter only participants w/ stress echo
other <- other %>%
  filter(!is.na(LV.systole..LVEF.y))

#get rid of event name columns 

other <- other %>%
  select(!c("Event.Name.x","Event.Name.y" ))

final <-  other
final_lvef_data <- final

#try this write pdf file date thing
write.csv(final_lvef_data, file = paste("/Users/victoriathomas/Documents/MiCOR/COVID-Heart/Analysis/final_lvef_data",
                               Sys.Date(), ".csv"), na="")

final_lvef_data %>%
  count(Gender.y, Are.you.active.duty..x)

final_lvef_data %>%
  count(Gender.y)

final_lvef_data %>%
  count(Are.you.active.duty..x)

final_lvef_data %>%
  count(ICU.admission..x)

final_lvef_data %>%
  count(Have.you.received.your.COVID.19.vaccine..x)

wut <- v1 %>%
  subset(Record.ID == c(1,3,4,5,8,9,10,12,13,14,16,19,20))

wut <- lvef_plus %>%
  filter(Record.ID %in% c(1,3,4,5,8,9,10,12,13,14,16,19,20)) %>%
  

wut_v2 <- wut%>%
  filter(Event.Name == "V2")


#silly histo viz, not a good fit
#perhaps box and whisker plot
stress_exercise_histo <- ggplot(lvef_data, aes(x=strecho_lvsys_lvef)) + geom_histogram(binwidth=1)
stress_rest_histo <- ggplot(lvef_data, aes(x=strecho_rest_lvef)) + geom_histogram(binwidth=1)


rm(list=ls())

#generate avg steps

#download complete activpal from google drive

#read csv - screen
screen_master_activpal <- read.csv("/Users/victoriathomas/Documents/MiCOR/ASSET/Data Processing/ActivPal/Data/Screen/activpal_complete_screen.csv")

#separate out valid days (df$valid.day == 1)
screen_valid <- screen_master_activpal %>%
  filter(Valid.Day == 1)

#mean of step count

screen_sum_stat <- screen_valid %>% summarise(mean = mean(step.count),
                           sd = sd(step.count))

#read csv - ptx
ptx_master_activpal <- read.csv("/Users/victoriathomas/Documents/MiCOR/ASSET/Data Processing/ActivPal/Data/PTX/activpal_complete_ptx.csv")

#separate out valid days (df$valid.day == 1)
ptx_valid <- ptx_master_activpal %>%
  filter(Valid.Day == 1)

#mean of step count

ptx_sum_stat <- ptx_valid %>% summarise(mean = mean(step.count),
                                              sd = sd(step.count))
#viz sensor glucose
ggplot(sample_plot, aes(timestamp, sensorglucose)) +
  geom_line()


