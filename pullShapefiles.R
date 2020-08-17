# Notes to myself
# This is written against the shapefiles as sent by CUPN, so 10+ GB of images, too,
# and lots of mutliple shapefiles for the same cluster.  It is also written
# against the Access mdb files with varying old schema, and not populated with
# all data from the paper field datasheets.
# The one thing that has changed is that their directory structure had 
# entrance as the top level, then bout as folders within entrances, then mod/raw/misc within bout.
# I swaped the nesting of entrance and bout, so that each bout can be zipped up and archived somewhere,
# as opposed to new bouts being scattered across all entrances.
# I also copied the entire shapefile+image directory structure, then deleted all .jpg files,
# creating a structure with the shapefiles and the pdfs of the annotated images,
# but only ~1.5GB in size.

# Also, check whether you have 32-bit or 64-bit ODBC drivers.  If you only have
# 32-bit drivers (e.g., from 32-bit Office 2010), then run this in 32-bit R.


setwd("d:/networks/05_CUPN_CumberlandPiedmont/crickets")
#setwd('d:/Crickets')

options(stringsAsFactors=FALSE)

# where is the Access database CUPN_Cave_Cricket_be_20150424b.mdb
#    likely same as above, but maybe not 
db.dir <- "d:/networks/05_CUPN_CumberlandPiedmont/crickets"
#db.dir <- getwd()

db.base <- "CUPN_Cave_Cricket_be" # The name of the access file minus date
# used when my automatic backup/versioning of Access files is used.

#   The shapefiles of processed cluster photos are in bout-specific subdirectories 
#   of this location
sf.dir <- file.path(db.dir,'CC')
#


########################
pkg <- c("sp", "rgdal", "RODBC", "NCmisc", "plyr", "lubridate")
inst <- pkg %in% installed.packages()
if (length(pkg[!inst]) > 0) install.packages(pkg[!inst])
lapply(pkg, require, character.only = TRUE)



############################

# The accessory file needed when doing auto-backup of Access file
# source('AccessBackup2.R')

# This version for doing 1 Bout at a time
# Bout <- "June2014"
# sfList <- list.files(path=sf.dir,pattern=paste('.*',Bout,'.*\\.shp$',recursive=TRUE)

# This version for all Bouts with directories under sf.dir full of shapefiles
sfList <- list.files(path=sf.dir,pattern='.*\\.shp$',recursive=TRUE)
head(sfList)
xx <- strsplit(sfList,'/')
xxx <- do.call('rbind',xx)
sfDF <- as.data.frame(xxx,stringsAsFactors=FALSE)
names(sfDF) <- c('Bout','Entrance','subDir','fName')
#sfDF$fullFname <- sfList
table(sfDF$subDir) # yep, all Mod
sfDF$subDir <- NULL   # drop that variable
# add back in full paths
sfDF$path <- paste(xxx[,1], xxx[,2], xxx[,3], sep = "/")

# now deal with shapefile names
sfName <- sub('.shp', '', sfDF$fName, fixed = TRUE)

# further parsing of cluster names
yy <- gregexpr('_',sfName)
sapply(yy,length)
# so inconsistent file naming convention sometimes has 2 "_" separators sometimes 3.
# for now, remove CC_ prefix, then split once to parse off Event, then pull numeric of remainder for Cluster.
sfName <- sub('CC_', '', sfName, fixed = TRUE)
flag <- regexpr("_", sfName, fixed = TRUE)
sfDF$Event = paste0("CC_", substr(sfName,1,flag-1))
cruft <- substr(sfName,flag+1, length(sfName))
# regular expression to only keep numeric
sfDF$Cluster <- as.numeric(gsub("[^0-9]", "", cruft))
# sort into easier to see order
sfDF <- sfDF[order(sfDF$Bout, sfDF$Entrance, sfDF$Event, sfDF$Cluster, sfDF$path, sfDF$fName),
             c("Bout", "Entrance", "Event", "Cluster", "path", "fName")]
sfDF$fName <- sub(".shp", "", sfDF$fName, fixed = TRUE)

# Now, is there more than one record (file) for a given Event/Cluster?
if (anyDuplicated(sfDF[,c("Event", "Cluster")])) {
   cat("Some clusters have more than one shapefile\n")
   flags <- duplicated(sfDF[,c("Event", "Cluster")])
   nshp <- plyr::count(sfDF, vars = c("Event", "Cluster"))
   dups <- nshp[nshp$freq > 1, c("Event", "Cluster")]
   dups <- merge(sfDF, dups, by = c("Event", "Cluster"), all.y = TRUE) 
   print(dups)
} # if anyDuplicated


##############################################
######  Even minimally tested code stops here.  See the substantial email exchange about how to
# distinguish which shapefiles need to be read and which omitted to count every
# cricket in a cluster, and count each cricket exactly once.
# In theory, there will be a rule that can be coded into the regex in the sfList <- list.files()
# line above, but no such rule exists that works for the 2014 and 2015 files I have.


# For experimentation, assume that for clusters with mutiple shapefiles, all are needed,
# as in no individual cricket in that cluster appears in more than 1 shapefile.  We know
# that isn't true, but that's a fix on the CUPN end.


# grab temp tables from Access
# Connect to the CC database
# mdb2015 <- odbcConnectAccess2007(file.path(db.dir,db.name))
# test against sandbox version!

# most recently tested against CUPN_Cave_Cricket_be_2018-05-16.mdb
mdb <- odbcConnectAccess2007(file.choose())
Tlist <- sqlTables(mdb)[,3:4] # list of all tables
# all bouts
Bouts <- sqlFetch(mdb,'tbl_CC_Bouts', stringsAsFactors = FALSE)
# Events with data already processed
Events <- sqlFetch(mdb,'tbl_CC_Events', stringsAsFactors = FALSE)
# Strips with data already processed  not needed here
# Strips <- sqlFetch(mdb,'tbl_CC_Strips', stringsAsFactors = FALSE)
# Clusters with data already processed  not needed here
# Clusters <- sqlFetch(mdb,'tbl_CC_Clusters', stringsAsFactors = FALSE)



# for now, are there any bouts in the directory structure not in the tbl_CC_Bouts?
badBouts <- sfDF[!sfDF$Bout %in% Bouts$BoutName,]
if (nrow(badBouts) > 0) {
   cat("These shapefiles have bouts in their paths that are not in tbl_CC_Bouts\n")
   print(sfDF)
}
boutList <- unique(sfDF$Bout)



# at this point, let the user pick which bout to process, or to process more than one in a loop.
# for now, manually picking bout

bout <- boutList[1]  # June2014
bout <- boutList[2]  # June2015
working <- sfDF[sfDF$Bout == bout,]  # working subset of the info about shapefiles


tmpEvents <- sqlFetch(mdb,paste0("temp_Events_", bout), stringsAsFactors = FALSE)
tmpStrips <- sqlFetch(mdb,paste0("temp_Strips_", bout), stringsAsFactors = FALSE)
                    # n.b.: the version of the Access database I have does not have
                    #  Strip_Total, Strip_Juvenile, etc., populated from the data sheet.
                    # The protocol and "CUPN_CaveCricket_Protocol_SOPs_Appendix6_tp_markup.docx"
                    # have these fields/variables as light green (tan) filled from data sheets.
                    # If the intent is to add a flag to the shapefile to indicate whether each
                    # individual is in a strip (not just yes no but the Strip_ID, as an
                    # individual cricket can only be in 1 strip), then the code below needs
                    # changing.
                    
tmpClusters <- sqlFetch(mdb,paste0("temp_Clusters_", bout), stringsAsFactors = FALSE)
tmpxref <- sqlFetch(mdb, "xref_CC_Strips_Clusters")  # n.b. this table is blank in all
                                                     # Access databases I have copies of
                                                     # it should have the xref from the paper data sheets
                                                     # for cross-checking against the computed coordinate overlaps


# discard Events and Strips not sampled
tmpEvents <- tmpEvents[!is.na(tmpEvents$Start_Time),]

# odd syntax because sampled strips in Access file have NA as the value,
# 2014 has "Unused" for unsampled strips, but they may add other possible values
# like rejection reasons.  This needs to be revisited once the dust settles on the
# database and the values used for this variable.
tmpStrips <- tmpStrips[!tmpStrips$Rejected %in% c("Unused"),]  




# Consistency checks
# each Event should only occur once, and all records should have non-missing Event_Name
# Are there any events in tmpEvents that don't have any shapefiles for clusters?
# All events with a Start_Time should have

# shapefile events not in the Events table
nonEvents <- working[!working$Event %in% tmpEvents$Event_ID,]
if (nrow(nonEvents) > 0) {
   cat("These shapefile names have events not in table tmpEvents\n")
   cat("Or their record in tmpEvents was missing Start_Time\n")
   print(nonEvents)
} else {
   cat("All shapefile names & paths refer to events in table tmpEvents\n")
}


# Events in tmpEvents without any shapefiles for clusters (possible but should be flagged)
nonEvents2 <- tmpEvents[!tmpEvents$Event_ID %in% working$Event,]
if (nrow(nonEvents2) > 0) {
   cat("These events in table tmpEvents don't have any shapefiles of clusters, is this correct?\n")
   print(nonEvents2)
} else {
   cat("All events in table tmpEvents have at least one shapefile\n")
}

# consistency check on denormalized Bout Location_ID Event
#     For every shapefile *_FINAL.shp, the Bout & Location_ID parsed from the path and the Event_ID parsed
#     from the filename must match an entry in tbl_CC_Events.  [This is useful for file naming/subdirectory mistakes, 
#     which are plausible during the manual ArcMAP processing & naming process.]  
#     working Bout Entrance Event from path & filename vs tmpEvents Bout, Location_ID, Event_ID
f1 <- unique(paste(working$Bout, working$Entrance, working$Event, sep = "."))
f2 <- paste(tmpEvents$Bout, tmpEvents$Location_ID, tmpEvents$Event_ID, sep = ".")
cat("Bout Entrance Event_ID combinations from filenames not in tmpEvents:\n")
f1[!f1 %in% f2]
cat("Bout Entrance Event_ID combinations from tmpEvents not having shapefiles:\n")
f2[!f2 %in% f1]





cat("If any of these tables have TRUE, there are missing values in the database that shouldn't be there\n")
table(is.na(tmpStrips$Position))
table(is.na(tmpClusters$Xmin))
table(is.na(tmpClusters$Xmax))

# if so, uncomment these lines to see which ones
# tmpStrips[is.na(tmpStrips$Position),]
# tmpClusters[is.na(tmpClusters$Xmax) | is.na(tmpClusters$Xmin),]




# computed xref is compxref
# strip & cluster intersect if tmpStrips$Position <= tmpClusters$Xmax & 
#                              tmpStrips$Position + 10cm >= tmpClusters$Xmin

# NOTE THAT THIS IS CODED FOR WHEN POSITONS WERE IN UNITS OF METERS, SO 10CM == 0.1
# THE NEW DATABASE SHOULD BE IN UNITS OF CM, SO THIS NEEDS TO BE + 10, NOT + 0.1
compxref <- join(tmpClusters, tmpStrips, by = "Event_ID")
cat("should be all TRUE\n")
table(!is.na(compxref$Event_ID))

compxref <- compxref[!is.na(compxref$Event_ID) &
                     compxref$Position <= compxref$Xmax &
                     (compxref$Position + 0.1) >= compxref$Xmin,]

# This should be compared to the datasheet version in xref_CC_Strips_Clusters 
# (or the tmp equivalent of it) but as noted above I don't have a version of the Access database
# with that table populated.  There should be a 1 to 1 match on Event_ID, Cluster_ID, and Strip_ID
# with exactly 1 record with that triplet in both tables.



# 1a)  Each Cluster_ID in tbl_CC_Clusters must occur at least once in xref_CC_Strips_Clusters (it can occur more than once)
# Every Cluster in tmpClusters should occur at least once in compxref
orphanClusters <- tmpClusters[!tmpClusters$Cluster_ID %in% compxref$Cluster_ID,]
if (nrow(orphanClusters) > 0) {
   cat("These Clusters aren't intersected by Strips and shouldn't exist:\n")
   cat("perhaps a data entry error on tmpStrip Event_ID, Strip_ID, Position, or tmpClusters Event_ID Xmax Xmin?\n")
   print(orphanClusters[,c(1:10,13)])
#   cat("All strips for those events:\n")
#   xx <- tmpStrips[tmpStrips$Event_ID %in% orphanClusters$Event_ID,]
#   xx[order(xx$Event_ID, xx$Position),c(1:5, 7, 9, 10)]  
} else {
   cat("No orphaned clusters not intersected by at least 1 strip\n")
} # if else block orphanClusters


# Not every strip intersects a cluster, which strips get artificial clusters of 0s 
# (later during analysis)?
emptyStrips <- tmpStrips[!tmpStrips$Strip_ID %in% compxref$Strip_ID,]
# are they really empty?
# table(emptyStrips$Strip_Total, useNA = "always")
badStrips <- emptyStrips[emptyStrips$Strip_Total > 0,]
if (nrow(badStrips) > 0) {
   cat("These strips have crickets but don't intersect any clusters\n")
   cat("even if they are Unused they shouldn't have crickets in them\n")
      print(badStrips)
} else {
  cat("No strips with crickets but not clusters\n")
} # if else block badStrips

# Every strip that intersects at least 1 cluster must have at least 1 cricket in it
# for each cluster it intersects.
# These lines are for if the tmpStrips table gets populated form the field datasheet.
# Until then, it is tripped by all 0s for Strip_Total.
# If inStrip becomes a shapefile attribute, this check can happen after processing
# shapefiles to populate the counts.
stripHits <- count(compxref, vars = "Strip_ID")
stripHits <- merge(stripHits, tmpStrips, by = "Strip_ID", all.x = TRUE)
oopsStrips <- stripHits[stripHits$freq > stripHits$Strip_Total,]
if (nrow(oopsStrips) > 0) {
  cat("These strips don't have at least 1 cricket for each cluster they intersect\n")
  names(oopsStrips)[2] <- "clustersIntersected"
  print(oopsStrips)
} else {
  cat("All strips have at least as many crickets as intersected clusters\n")
} # if else block oopsStrips








#####################################################################################
# now grab from shapefiles
tmpstack <- vector("list", length = nrow(working)) # don't grow objects!
for (i in 1:nrow(working)) {
   print(working[i,])
   tmpshp <- readOGR(file.path(sf.dir,working$path[i]), working$fName[i], stringsAsFactors = FALSE)@data
   cat(nrow(tmpshp),"\n")
   tmpshp$Event <- working$Event[i]
   tmpshp$Cluster <- working$Cluster[i]
   tmpstack[[i]] <-  tmpshp
} # loop i over working
crickets <- do.call("rbind", tmpstack)
crickets$Cluster_ID <- paste(crickets$Event, crickets$Cluster, sep = "_")
cTot <- count(crickets, vars = "Cluster_ID")
cF <- count(crickets[crickets$Sex == "F",], vars = "Cluster_ID")
cM <- count(crickets[crickets$Sex == "M",], vars = "Cluster_ID")
cU <- count(crickets[crickets$Sex == "U",], vars = "Cluster_ID")
cJ <- count(crickets[crickets$Life_stage == "J",], vars = "Cluster_ID")
# note that Kurt probably wants another count variable Cluster_Adults, as many Sex == U 
# are not Life_stage == J
with(crickets, table(Sex, Life_stage))
# but, not yet in what was asked for...  and for once I'm following directions.
tmpClusters$Cluster_Total <- cTot$freq[match(tmpClusters$Cluster_ID, cTot$Cluster_ID)]
# only fill in 0s for clusters with totals
tmpClusters$Cluster_juvenile[!is.na(tmpClusters$Cluster_Total)] <- 0
tmpClusters$Cluster_Male[!is.na(tmpClusters$Cluster_Total)] <- 0
tmpClusters$Cluster_Female[!is.na(tmpClusters$Cluster_Total)] <- 0
tmpClusters$Cluster_Unknown[!is.na(tmpClusters$Cluster_Total)] <- 0

# now fill in counts
tmpClusters$Cluster_juvenile <- cJ$freq[match(tmpClusters$Cluster_ID, cJ$Cluster_ID)]
tmpClusters$Cluster_Male <- cM$freq[match(tmpClusters$Cluster_ID, cM$Cluster_ID)]
tmpClusters$Cluster_Female <- cF$freq[match(tmpClusters$Cluster_ID, cF$Cluster_ID)]
tmpClusters$Cluster_Unknown <- cU$freq[match(tmpClusters$Cluster_ID, cU$Cluster_ID)]


# At this point, assuming all QA flags above were ok, this table would be appended to the bottom of 
# the cumulative tbl_CC_Clusters table.  This can be done manually by the DM, or via this R code:
# note that the option test = TRUE just show what would happen, it deosn't change the table in
# the Access file.
cols <- sqlColumns(mdb, "tbl_CC_Clusters")
cols2 <- sqlColumns(mdb, paste0("temp_Clusters_", bout))
# annoyance.  The columns of the temp_Clusters_June2014 and tbl_CC_Clusters tables
# are not the same order.  And, capitalization doesn't match, but Access might not be
# case sensitive.
# reorder variables on the fly, because this is R, so there's always a way.  It would be
# simple if either capitalization was consistent, or variable order was consistent...

# first fix capitalization
names(tmpClusters) <- c("Cluster_ID" , "Event_ID", "Clustern", "Xmin", "Xmax",
                        "Cluster_Distance", "Cluster_Side", "Est_Crickets", "Cluster_Location",
                        "Cluster_Temp", "Cluster_RH", "Cluster_Airflow",  "Cluster_Notes",
                        "Cluster_Total", "Cluster_Juvenile", "Cluster_Male", "Cluster_Female",
                        "Cluster_Unknown")
# now order to match tbl_CC_Clusters                        
tmpClusters <- tmpClusters[,cols$COLUMN_NAME]



flag <- sqlSave(mdb, tmpClusters, "tbl_CC_Clusters", append = TRUE, 
                rownames = FALSE, 
                test = TRUE,
                verbose = TRUE)
                
# The equivalent would be done with tmpxref if it is not populated from the datasheets
# but rather from the shapefiles.  But without an attribute in the shapefiles for which 
# strip (if any) a cricket is in, that can;'t happen.

close(mdb)

