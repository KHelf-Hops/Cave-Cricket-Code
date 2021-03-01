#Most often the mistakes made during image scoring data entry are transcription or mis-typing EventID or Date.  These
#mistakes can prevent importation of the image scoring .csv file into the cave cricket database.  A quick and easy way to
#look for these mistakes is to use the data.table package

#This short script can detect transcription errors by yielding a table with counts of correctly and *incorrectly* typed
#attributes.

# Clear the console
cat("\014")

# Clear the environment
rm(list = ls())

#load data.table to check for consistent data
library(data.table)
library(tidyverse)

#set working directory to where Merged .csv files from the Bout in question are contained
#For example, setwd("D:/My Book File Backup/CUPN_MACA_Cave_Cricket_Images/Summer2019/Summer2019_QC_Merged") 
setwd("D:/My Book File Backup/CUPN_MACA_Cave_Cricket_Images/Summer2019/SlCr3/Training Images/Huck")

#read in data from CC_EventID_Merged .csv file
CC_60_Merged<-read.csv("D:/My Book File Backup/CUPN_MACA_Cave_Cricket_Images/Summer2019/SlCr3/Training Images/Huck/CC_60_Merged.csv")

#########IS A FOR LOOP POSSIBLE FOR ALL EVENTS IN A BOUT?  HOW WOULD WE GET THE EVENTIDS INTO THE CODE? IS THAT POSSIBLE?

View(CC_60_Merged)

#look for mis-typed Dates or EventIDs
setDT(CC_60_Merged)[,.N,by=Date]
setDT(CC_60_Merged)[,.N,by=EventID]
setDT(CC_60_Merged)[,.N,by=ClusterN]

#setDT(CC_44_Merged)[,.N,by=EventID]
#EventID    N
#1:   CC_44 1781  
#2:   CC-44    1  

#setDT(CC_44_Merged)[,.N,by=Date]
#Date    N
#1: 6/19/2018 1728  CORRECT
#2: 6/29/2018   54  INCORRECT

###Ellen's script for renaming Strip Pos from "-1" to "StripPos"### 
template_StripPos <- unique(dat[c("ClusterId", "StripPos")]) %>%
  dplyr::filter(StripPos != -1) %>%
  dplyr::rename(StripPosX = StripPos)
dat %<>%
  left_join(template_StripPos, by = c("ClusterId"))
dat$InStrip <- dat$StripPos!=-1

write.csv(Summer2019_Merged_Deason, "D:/My Book File Backup/CUPN_MACA_Cave_Cricket_Images/Summer2019/Summer2019_Merged
          /Summer2019_Merged_Deason.csv") #save changes to specific image scorer's merged file
