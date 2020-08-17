rm(list=ls())

##More indirect method below
#SET THE WORKING DIRECTORY  
CCMerge.dir<-("Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018")

setwd(CCMerge.dir)

library(tidyverse)
library(janitor)

#List file names of all files, filtered by .csv, including subfolders
csvList<-dir(path=CCMerge.dir, pattern="*\\.csv$", recursive=TRUE, full.names=TRUE)

head(csvList)

#Check for correct number of .csv files; as of 1/15/2020 should be 15
csvList

#show file contents of first file, maximum five lines. 
#May also be used to examine .csv files that possibly do not belong with Merged cluster files 
read_lines(csvList[1], n_max=5)

###Code taken from: kieranhealy.org/blog/archives/2019/11/09/reading-in-data/
#Grab event number 
#get_event_no<-function(x){
# read_lines(x,n_max=2) %>% 
#    str_extr
#}

#####Use merged .csv file from all events in a bout#####

MrgShpBout.dir<-("Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_Merged")
setwd(MrgShpBout.dir)
#Read in .csv file June2018_Merged
June2018_Merged<-read_csv("June2018_Merged.csv")

#Check to see if EventIDs match those listed in Bout Plan
EventList<-unique(June2018_Merged$EventID)
View(EventList)

#look at number of clusters per event
ClustEvent<-ggplot(June2018_Merged, aes(x=EventID,y=ClusterN))
ClustEvent + geom_bar(stat="identity")

######Create list of Image Scorers#####
EventScorer<-data.frame("scorer"=c("Helf","Hammond","Deering","Scoggins"))
View(EventScorer)
#EventIDs already in EventList
#####Match scorers with specific EventID and ClusterN#####
##########################################################
#Merge EventID and ClusterN
June2018_Merged<-June2018_Merged%>%unite("EventClustN", EventID:ClusterN, remove=FALSE)

