rm(list=ls())

#SET THE WORKING DIRECTORY  
RndImgDraw.dir<-("Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018")
#####   Alt. paths below are folders for original and rescored data, respectively  #####
#Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_Merged
#Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_QC_Rescore

setwd(RndImgDraw.dir)

library(tidyverse)
library(data.table)

#Read in .csv file June2018_Merged
June2018_Merged<-read_csv("Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_Merged/June2018_Merged.csv")

#Order by EventID and ClusterN
June2018_Merged<-June2018_Merged[
  with(June2018_Merged, order(EventID,ClusterN)),
  ]

#Check to see if EventIDs match those listed in Bout Plan (i.e., correctly entered)
#Could be looped to check ClusterN & Date; inelegant since create df rather than checking for correct EventIDs/ClusterNs/Dates; could use Philippi's pullShapefiles.R code for this
EventList<-data.frame(unique(June2018_Merged$EventID))
colnames(EventList)<-"EventID"
View(EventList)

##########################################################################################
#####  Need code to create list of scored images with scorers; use Hs.ClusterCode.R(?) ###
##########################################################################################

######Create list of Image Scorers#####
EventScorer<-data.frame("scorer"=c("Helf","Hammond","Deering","Scoggins"))
View(EventScorer)
#EventIDs already in EventList

#Merge Events/Clusters with scorer name by EventID/ClusterN

#############I could NOT figure out how to do this######
#####################SOOOOOOOO##########################
#########I created the list by brute force in excel#####

#Derived from ordered SRS draw and June 2018 Bout data ordered by EventID and ClusterN#
#File contains EventID/Date/OrigClustN/OrigScorer/RescoreClustN
#RescoreClustN is SRS draw (see line 92) no. based on number of total ClusterN from June2018 bout (i.e.,880). This step is necessary because the cluster numbers in the final merged data set are not sequential as a whole.  For example, in the 2018 data, Austin entrance had 27 clusters (i.e. cluster 1-27), Carmichael had 31 clusters (i.e., cluster 1-31), Crockpot had 53 clusters (i.e. cluster 1-53), etc. 
June2018_QC_Rescore_Draw<-read_csv("Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_QC_Rescore/June2018_QC_Rescore_Draw.csv")

View(June2018_QC_Rescore_Draw)

#Derived from June2018_Merged; all cricket data from clusters drawn by SRS
#Read in .csv file June2018_QC_Rescored_Merged
June2018_QC_Rescored_Merged<-read_csv("Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_QC_Rescore/June2018_QC_Rescored_Merged.csv")

#Order by EventID
June2018_QC_Rescored_Merged<-June2018_QC_Rescored_Merged[
  with(June2018_QC_Rescored_Merged, order(EventID,ClusterN)),
  ]

#Measurement Quality Objectives of Image Scoring 

#Stated goal in DRAFT cave cricket Data Quality Standards doc is the following: "Significant portion of cave crickets in analyzed images are properly identified to species, sex, and life stage.  Determination is made on a case by case basis.  95% accuracy in a 5% random subsample."
#DQS needs to be updated based on work with Ellen Cheng

#Two primary goals:

#1. To determine efficacy of image scoring training, new scorers that might need further training, or areas where training might be enhanced, by examining variation among seasoned and new scorers in the cave crickets demographic characteristics (i.e., species, size, sex, and total cave crickets scored) at several cave entrances.

#New scorers work through a set of "standardized" images and their scores compared to "answer key" of scores by PI. 

#####

#2. As a final quality control measure determine accuracy of 5% random subsample of all scored images with the goal of 95% accuracy of identification to species, size, and sex relative to rescore by PI.

#In addition, as discussed with Ellen Cheng, direction of scoring bias and which demographic characteristics are most difficult to score.

#Random selection of 5% subset of scored images from cave cricket sampling bout for QC

#Questions 

#1 The selection of 5% of images is largely based on convenience because the image scoring process can be time consuming.  Is this a large enough sample size to meet the goals of QC?  For example, in the 2018 data there are 880 images scored in the Summer 2019 sampling bout; 5% of 880 = 44 images.  However, there are 370 individuals among those 44 images? Issue of confounding of scorer and entrance.

#2.	Since the PI is performing the rescoring of the random selection should the pool of scored images be restricted to individuals other than the PI?  Given the images will be stripped of scorer ID it could be a good QC check on the PI's scoring consistency.

#3.	Should a random sample (without replacement) be generated based simply on the total number of images from a sampling bout?  
 
#This will bias the selection process in that events with the greater number of scored images will be rescored. In fact, it is possible events with a small number of scored images, e.g., Austin Entrance, would not be rescored at all.

#4.	Or should the total number of scored images be divided by the number of caves and, based on the result, the number of scored images apportioned equally among all events with each event randomized separately?

#  For example, 44 rescored images/15 events = 2.93 or approximately 3 images per event.  

sample(1:880, 44, replace=FALSE)
#[1] 365 383 16 817 545 498 662 621 608 299 793 108 182 94 230 67 748 355 32 742 100 728 584
#[24] 595 561 808 834 17 632 448 350 652 719 261 353 192 835 86 791 534 852 45 514 62

#List should be created as a df or dt and ordered 

#Take SRS draw above and create df
June2018_Rescore<-data.frame(ClustImgSRS=c(365, 383, 16, 817, 545, 498, 662, 621, 608, 299, 
793, 108, 182, 94, 230, 67, 748, 355, 32, 742, 100, 728, 584, 595, 561, 808, 834, 17, 632, 448, 350, 652, 719, 261, 353, 192, 835, 86, 791, 534, 852, 45, 514, 62)) 

#Order by number
June2018_Rescore$ClustImgSRS<-sort(June2018_Rescore$ClustImgSRS)

sample(1:26, 3, replace=FALSE)
#[1] 15  4  1

#Option 2 has the advantage that each event has an equal number of images rescored.  
#However, since the goal is quality control on the image scoring process event may not be an important variable.  There can be a range of difficulty scoring images at any given cave entrance.  Further, the order in which cave entrances are visited is randomized each sampling bout so each entrance will have its images rescored at some point.

	
#####Failed attempts to get total clusters imaged and percentage scored by scorers#####
#June2018_Merged %>% 
 #group_by(EventID, Scorer) %>% 
 #summarise(n=n()) %>% 
 #mutate(freq=n/sum(n))

#Scorer not in df
June2018_Merged %>% 
  group_by(Scorer) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))

#Works but is a total of crickets for EventID
June2018_Merged %>% 
  as.tibble() %>% 
  count(EventID)
#######################################################################################

##################################################################################
#####     Create data frames for SRS and Clusters/Event and Merge     ############
##################################################################################

June2018_EvID<-data.frame(EventID=c("CC_31","CC_32","CC_33","CC_34","CC_35","CC_35","CC_36", "CC_37","CC_38","CC_39","CC_40","CC_41","CC_42","CC_43","CC_44","CC_45"))
                            
June2018_Totals<-data.frame(ClustImages=c(1:70,71:152,153:180,181:216,217:244,245:302,303:356,357:426,427:458,459:484,485:539,540:601,602:648,649:737,738:880))

June2018_EvID31<-data.frame(EventID=c("CC_31"), ClustImgs=c(1:70))
June2018_EvID32<-data.frame(EventID=c("CC_32"), ClustImgs=c(71:152))
June2018_EvID33<-data.frame(EventID=c("CC_33"), ClustImgs=c(153:179))
June2018_EvID34<-data.frame(EventID=c("CC_34"), ClustImgs=c(180:244))
June2018_EvID35<-data.frame(EventID=c("CC_35"), ClustImgs=c(245:271))
June2018_EvID36<-data.frame(EventID=c("CC_36"), ClustImgs=c(272:328))
June2018_EvID37<-data.frame(EventID=c("CC_37"), ClustImgs=c(329:381))
June2018_EvID38<-data.frame(EventID=c("CC_38"), ClustImgs=c(382:450))
June2018_EvID39<-data.frame(EventID=c("CC_39"), ClustImgs=c(451:481))
June2018_EvID40<-data.frame(EventID=c("CC_40"), ClustImgs=c(482:506))
June2018_EvID41<-data.frame(EventID=c("CC_41"), ClustImgs=c(507:560))
June2018_EvID42<-data.frame(EventID=c("CC_42"), ClustImgs=c(561:621))
June2018_EvID43<-data.frame(EventID=c("CC_43"), ClustImgs=c(622:667))
June2018_EvID44<-data.frame(EventID=c("CC_44"), ClustImgs=c(668:755))
June2018_EvID45<-data.frame(EventID=c("CC_45"), ClustImgs=c(756:880))

#Bind creates df with an ordered list of all CLUSTERS for each Event in 2018 Bout
#Purpose is to match SRS of 1:880 with orderd list of clusters from 2018 Bout and thereby
#pick which clusters to rescore. 
June2018_Totals<-rbind(June2018_EvID31,June2018_EvID32,June2018_EvID33,June2018_EvID34,June2018_EvID35,June2018_EvID36,June2018_EvID37,June2018_EvID38,June2018_EvID39,June2018_EvID40,June2018_EvID41,June2018_EvID42,June2018_EvID43,June2018_EvID44,June2018_EvID45)

#Check to see if EventIDs match those listed in Bout Plan
EventList<-unique(June2018_Merged$EventID)
View(EventList)

######Create list of Image Scorers#####
EventScorer<-data.frame("scorer"=c("Helf","Hammond","Deering","Scoggins"))
View(EventScorer)

#EventList merged with EventScorer then merged with June2018_Totals then targeted replacement for Events with two scorers
#As it is, brute force file was created in Excel (i.e., "June2018_QC_Rescore_Draw") by hand #entry of scorer names

##############Combine SRS of Cluster Images with data frame containing previously scored data



June2018_Rescore_ClustN<-data.frame(c(dplyr::semi_join(June2018_Totals, June2018_Rescore, by = c("ClustImgs"= "ClustImgSRS"))))

#Rename col 2 to show its SRS draw
colnames(June2018_Rescore_ClustN)[2]<-"ClustImgSRS"

#Reorder 
June2018_Rescore_ClustN<-June2018_Rescore_ClustN[
  with(June2018_Rescore_ClustN, order(EventID,ClustImgSRS)),
  ]

ClustImgSRS<-data.frame(c(dplyr::semi_join(June2018_Merged, June2018_Rescore, by = "EventID")))

ClustImgSRS<-ClustImgSRS[
  with(ClustImgSRS, order(EventID,ClusterN)),
  ]

#Remove columns containing data from previously scored data
ClustImgSRS<-select (ClustImgSRS, -c(1,5,6,7))

#Rename ClusterN to ClustRescore
colnames(ClustImgSRS) = c("EventID", "Date", "OrigScorer", "Rescore")

#Rename Scorer to OrigScorer and SRSID to Rescore
colnames(June2018_QC_Rescore_Draw) = c("EventID", "Date", "OrigClustN", "OrigScorer", "RescoreClustN")

#Reorder columns
June2018_QC_Rescore_Draw<-data.frame(c(June2018_QC_Rescore_Draw %>% select(EventID, Date, OrigClustN, OrigScorer, Rescore)))

write_csv(June2018_QC_Rescore_Draw, path ="Z:/Vital_Signs/Vital_Signs_Prj/Cave_Crickets/Images/June2018/June2018_QC_Rescore/June2018_QC_Rescore_Draw.csv")

#######               FAILED Attempts                ########################
#Need to add simple row numbers and sort from there 
#Summarize SRS of Cluster Images from previously scored data
#ImgSRSUnique<-
  #ClustImgSRS %>% group_by(EventID, ClustRescore) %>% 
  #mutate(duplicate = n()) %>% 
  #filter(duplicate == 1) %>% 
  #select(-duplicate)

#ImgSRSFilt<-ClustImgSRS %>% distinct(EventID, ClustRescore, .keep_all=TRUE)
                     
#ImgSRSFilt<-dplyr(ClustImgSRS, .("EventID", "ClustRescore"), function(ClustImgSRS) if(nrow(ClustImgSRS) > 1) ClustImgSRS else c())
############################################################################

#####Collapse data frame by selecting first row from each ClusterRescore number
#June2018Clust<-June2018_Merged[!duplicated(June2018_Merged$EventID, June2018_Merged$ClusterN),]
 
#ClustImgSRS[!duplicated(ClustImgSRS$ClustRescore),]

#ClustSRS<-data.frame(ClustImgSRS=c(365, 383, 16, 817, 545, 498, 662, 621, 608, 299, 793, 108,
#182, 94, 230, 67, 748, 355, 32, 742, 100, 728, 584, 595, 561, 808, 834, 17, 632, 448, 350,
#652, 719, 261, 353, 192, 835, 86, 791, 534, 852, 45, 514, 62))

#ImgSRS<-subset(ClustImgSRS, June2018_Totals %in% c(365, 383, 16, 817, 545, 498, 662, 621, 608,
#299, 793, 108, 182, 94, 230, 67, 748, 355, 32, 742, 100, 728, 584, 595, 561, 808, 834, 17,
#632, 448, 350, 652, 719, 261, 353, 192, 835, 86, 791, 534, 852, 45, 514, 62))

#ImgRescrSRS<-ImgSRS[!duplicated(ImgSRS$ClustRescore),]


##########################################################
####################    THIS CODE WORKS    ###############
##########################################################

#####Collapse all cluster data to single, distinct clusters per event#####
ClustImgSRS<-June2018_Merged %>% distinct(EventID,ClusterN, .keep_all=TRUE)

#Remove columns containing data from previously scored data
ClustImgSRS<-select (ClustImgSRS, -c(1,2,5,6,7))

#####Create row ID to match with SRS#####
ClustImgSRS$ID<-1:nrow(ClustImgSRS)

#####Rename columns so can join with SRS for rescore#####
names(ClustImgSRS)[5]<-"ClustID"
names(June2018_Rescore)[1]<-"ClustID"

##############Combine SRS of Cluster Images with data frame containing previously scored data
June2018_Rescore_Draw<-data.frame(c(dplyr::semi_join(ClustImgSRS, June2018_Rescore, 
                                                     by = "ClustID")))

colnames(June2018_Rescore_Draw)[5]<-"SRSID"

write.csv(June2018_Rescore_Draw,
          "C:/Users/khelf/Desktop/R Files/CC_Files/June2018_Rescore_Draw.csv",
          row.names=FALSE)

#Check proportion of scorers sampled
June2018_Rescore_Draw %>% 
  group_by(Scorer) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))


##################################################################################
########      MUCH Simpler alternative: SRS from aggregated cluster data      ####
##################################################################################

#Random 5% sample
June2018_Rescore_Draw<-data.frame(c(ClustImgSRS %>% sample_frac(.05)))

##################################################################################
#############        OR sample proportionally by orig scorer       ###############
##################################################################################

June2018_Rescore_Draw<-data.frame(c(ClustImgSRS %>% group_by(Scorer) %>% sample_n(11)))

##################################################################################
########     Following code should work with either above 2 data frames     ######
##################################################################################

#Sort by EventID then ClusterN
June2018_Rescore_Draw<-June2018_Rescore_Draw[
  with(June2018_Rescore_Draw, order(EventID,ClusterN)),
  ]

#Remove unneeded ClustID column
June2018_Rescore_Draw<-select (June2018_Rescore_Draw, -c(5))

#Rename ClusterN to Rescore_ClustN
colnames(June2018_Rescore_Draw) = c("EventID", "Date", "OrigScorer", "Rescore_ClustN")

#save
write.csv(June2018_Rescore_Draw,"C:/Users/khelf/Desktop/R Files/CC_Files/June2018_Rescore_Draw.csv", row.names=FALSE)

#Check proportion of scorers sampled
June2018_Rescore_Draw %>% 
  group_by(OrigScorer) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))

#Should remove the 'OrigScorer' column, to prevent bias, from printed checklist but
#leave on original file

##################################################################################
####################   Post Rescore Comparison of Data     #######################
##################################################################################

#Strip 

June2018_QC_Rescore %>% inner_join(June2018_Merged,June2018_QC_Rescored_Merged.csv)

June2018_QC_Rescored_Test<-semi_join(June2018_Merged,June2018_QC_Rescored_Merged, 
                                    by =c('EventID','ClusterN'))
#Reorder by EventID, ClusterN
June2018_QC_Rescored_Test<-June2018_QC_Rescored_Test[
  with(June2018_QC_Rescored_Test, order(EventID,ClusterN)),
  ]
