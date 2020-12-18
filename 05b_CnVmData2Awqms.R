# This script was written Jan 2018 to compile continuous data already processed using the Continuous Data Review Scripts
# used for volunteer data.  The compiled data is for upload to the AWQMS data managment system.

library(tidyverse)
library(stringi)
library(plyr)
library(dplyr)
library(writexl)



# Designate the folder where you are getting the .Rdata files.  Must end with '/'.
shiny_path <-  "//deqlab1/wqm/TMDL/aEastern Region/Deschutes/2020/20200521/SondeData/Rdata/FINALdataFiles/"


# Location for the saved files for the volunteer database
in_path <- "//deqlab1/wqm/TMDL/aEastern Region/Deschutes/2020/20200521/SondeData/Rdata/ROutputs/"

# Designate the folder where you will Save the outputs...this may be the same as above. Must end with '/'.

out_path <- "//deqlab1/wqm/TMDL/aEastern Region/Deschutes/2020/20200521/SondeData/Rdata/Test/"

# Enter VolWQdb.t_Submission Number as text
sbm <- '2006084'

# AWQMS Project 
aprj <- 'TMDL'

#  Get look up table for characteristics
load('//deqlab1/wqm/DataManagement/ContinuousDataRTool/ConCharInfo.RData')

#Set working directory
setwd(shiny_path)


######################################
###                                ###
#   CONTINUOUS LOGGED DATA UPLOAD    #
###                                ###
######################################



#Get the names for the .Rdata graded files
fnames <- list.files(path = shiny_path, pattern = ".R[Dd]ata")
datfls <- fnames[grep(paste0("^",sbm,".+_.Rdata"),fnames)] # data files
#datfls <- fnames[grep(paste0("^TMDL",".+_.Rdata"),fnames)] # data files
audfls <- fnames[grep(".AUDIT_INFO.Rdata",fnames)] # audit files

dbnames <- list.files(path = in_path, pattern = ".R[Dd]ata")
svdfls <- dbnames[grep(".RData", dbnames)] # Saved files for volunteer db.



###
##
# Start For loop through datfls
for ( i in seq_along(datfls)) {
  load(datfls[i])
  tmp_data <- tmp_data[!is.na(tmp_data$rDQL),]
  fileinfo <- stri_split_regex(datfls[i],pattern = '_')[[1]] # get file information
  tmp_data$Station <- paste0(fileinfo[2],'-ORDEQ') # Add Station from fileinfo
  tmp_data$Depth <- fileinfo[length(fileinfo)-1] # Add Depth fileinfo
  tmp_data$EquipID <- fileinfo[3] # Add equipment ID
  tmp_data$Project <- as.character(aprj) # Add project
  tmp_data$Unit <- ConCharInfo$Unit[which(ConCharInfo$charid == fileinfo[4])]
  
  # Select fields needed for AWQMS upload as temporary df
  atmp <- tmp_data[,c('Station', 'EquipID', 'Depth', 'Project', 'charid', 'DATETIME', 'r', 'Unit', 'rDQL' )]
  
  #Stack files
  if (i == 1) {
    awqmsCnDat <- atmp
  } else {
    awqmsCnDat <- rbind(awqmsCnDat, atmp)
  }
  
  }
# End For loop through datfls
##
###

# Remove time change duplicates for Fall time change to Standard Time
redundantdata<-awqmsCnDat[duplicated(awqmsCnDat[,c(names(awqmsCnDat[1:(length(names(awqmsCnDat))-3)]))]),] # pull out duplicated rows based on all columns except r, Unit & rDQL
write.csv(redundantdata, file = paste0(out_path,sbm,"RedundantCnData.csv")) # create csv of deleted rows of data
awqmsCnDat<-awqmsCnDat[!duplicated(awqmsCnDat[,c(names(awqmsCnDat[1:(length(names(awqmsCnDat))-3)]))]),] # clean duplicated rows from dataset

# Add Date column
awqmsCnDat$Date <- strftime(awqmsCnDat$DATETIME, format =  '%m-%d-%Y')

# Add Time column
awqmsCnDat$Time <- strftime(awqmsCnDat$DATETIME, format =  '%H:%M')

#  Add Time Zone
awqmsCnDat$TimeZone <- stri_sub(as.character(as.POSIXct(awqmsCnDat$DATETIME), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

# remove text NA's that are character "NA"
acdc <- apply(awqmsCnDat, 2, function(y) gsub(pattern ="NA", replacement = "", y))

# Export to csv removing real NA values

write.csv(acdc, file = paste0(out_path, aprj, sbm, 'ContinuousDataAwqmsUpload.csv'), na = "")

# Export to Excel file which AWQMS import configuration requires
write_xlsx(as.data.frame(acdc), path = paste0(out_path, aprj, sbm, 'ContinuousDataAwqmsUpload.xlsx'))



######################################
###                                ###
#    DATA SUBMISSION INFORMATION     #
###                                ###
######################################

DeployInfo <- ddply(awqmsCnDat, c('Station', 'EquipID'), summarise, 
  startdate = min(DATETIME),
  enddate = max(DATETIME)
 )

DeployInfo$startTZ <- stri_sub(as.character(as.POSIXct(DeployInfo$startdate), format = '%Y-%m-%d %H:%M:%S %Z') , -3) 
DeployInfo$endTZ <- stri_sub(as.character(as.POSIXct(DeployInfo$enddate), format = '%Y-%m-%d %H:%M:%S %Z') , -3) 

load(fnames[grep(".SiteMasterInfo.RData",fnames)]) # Site Master Information 
smi2 <- smi[,which(names(smi) %in% c('Logger_ID','LASAR_ID','Station_Description', 'Depth_m'))]
names(smi2) <- c('EquipID', 'Station', 'Station_Description', 'Depth_m')
smi2$Station <- paste0(smi2$Station, '-ORDEQ') 

#smi2[10,2] <- '38591-ORDEQ' # submission 0020 problem

DeployInfo <- merge(DeployInfo, smi2, all = T)
DeployInfo$Project <- aprj

#<<<<<<< HEAD
write.csv(DeployInfo, file = paste0(out_path, aprj, sbm, 'ContinuousDataAwqmsInfo.csv'), na = "")





######################################
###                                ###
#    DISCRETE AUDIT DATA UPLOAD      #
###                                ###
######################################

audDat <- dbnames[grep(".AuditResults.RData",dbnames)] # audit files
audAct <- dbnames[grep(".AuditActivity.RData",dbnames)] # audit files

setwd(in_path)

load(audDat)
load(audAct)

awAud <- merge(t_CnAudRslt, t_CnAudAct, by = 'ActivityID')

# Add Project
awAud$Project <- aprj

# Convert to AWQMS Monitoring Location
awAud$SiteID <- paste0(awAud$SiteID,'-ORDEQ')

# Add Date column
awAud$Date <- strftime(awAud$StartDateTime, format =  '%m-%d-%Y')

# Add Time column
awAud$Time <- strftime(awAud$StartDateTime, format =  '%H:%M')

#  Add Time Zone
awAud$TimeZone <- stri_sub(as.character(as.POSIXct(awAud$StartDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)


write.csv(awAud, file = paste0(out_path, aprj, sbm, 'ContinuousAuditDataAwqmsUpload.csv'), na = '')



######################################
###                                ###
#     DAILY SUMMARY DATA UPLOAD      #
###                                ###
######################################

#dyDat <- fnames[grep(paste0(sbm,"Results.RData"),fnames)] # audit files
#dyAct <- fnames[grep(paste0(sbm,"Activity.RData"),fnames)] # audit files

# Some old formats may be named by project rather than work order/submission ID.
dyDat <- dbnames[grep(paste0(sbm,"Results.RData"),dbnames)] # audit files
dyAct <- dbnames[grep(paste0(sbm,"Activity.RData"),dbnames)] # audit files

# Some old formats may be named by project rather than work order/submission ID.
#dyDat <- dbnames[grep(paste0(aprj,"Results.RData"),dbnames)] # audit files
#dyAct <- dbnames[grep(paste0(aprj,"Activity.RData"),dbnames)] # audit files

load(dyDat)
load(dyAct)

dySum <- merge(t_CnRslt, t_ConDatAct, by = 'ActivityID')

# Add Project
dySum$Project <- aprj

# Convert to AWQMS Monitoring Location
dySum$SiteID <- paste0(dySum$SiteID,'-ORDEQ')

# Add Activity Start Date column
dySum$ActStartDate <- strftime(dySum$StartDateTime, format =  '%m-%d-%Y')

# Add Activity Start Time column
dySum$ActStartTime <- strftime(dySum$StartDateTime, format =  '%H:%M')

#  Add Activity Start Time Zone
dySum$ActStartTimeZone <- stri_sub(as.character(as.POSIXct(dySum$StartDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

# Add Activity End Date column
dySum$ActEndDate <- strftime(dySum$EndDateTime, format =  '%m-%d-%Y')

# Add Activity End Time column
dySum$ActEndTime <- strftime(dySum$EndDateTime, format =  '%H:%M')

#  Add Activity End Time Zone
dySum$ActEndTimeZone <- stri_sub(as.character(as.POSIXct(dySum$EndDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

# Add Analytical Start Date column
dySum$AnaStartDate <- strftime(dySum$AnalyticalStartTime, format =  '%m-%d-%Y')

# Add Analytical Start Time column
dySum$AnaStartTime <- strftime(dySum$AnalyticalStartTime, format =  '%H:%M')

#  Add Analytical Start Time Zone
dySum$AnaStartTimeZone <- stri_sub(as.character(as.POSIXct(dySum$AnalyticalStartTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

# Add Analytical End Date column
dySum$AnaEndDate <- strftime(dySum$AnalyticalEndTime, format =  '%m-%d-%Y')

# Add Analytical End Time column
dySum$AnaEndTime <- strftime(dySum$AnalyticalEndTime, format =  '%H:%M')

#  Add Analytical End Time Zone
dySum$AnaEndTimeZone <- stri_sub(as.character(as.POSIXct(dySum$AnalyticalEndTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

# Concatenate all comment columns
dySum$ActComment <- paste0(dySum$Org_Comment,"; ", dySum$DEQ_Comment)
dySum$ActComment <- gsub('NA; |; NA', '', dySum$ActComment)

# Populate Equipment columns
dySum$SmplColEquip <- "Probe/Sensor" # Equipment type
dySum$SmplEquipID <- stri_split_regex(dySum$ActivityID,'_', simplify = T)[,4] # equipment serial number or ID


dySum <- dySum[, c("CharID", "Result", "Unit", "Method", "RsltType", "ORDEQ_DQL", "StatisticalBasis", "RsltTimeBasis",
                   "DEQ_RsltComment", "ActivityType", "SiteID", "SmplColMthd", "SmplColEquip", "SmplDepth", "SmplDepthUnit", "SmplColEquipComment", 
                   "Samplers", "SmplEquipID", "Project", "ActStartDate", "ActStartTime", "ActStartTimeZone", "ActEndDate", "ActEndTime", "ActEndTimeZone",
                   "AnaStartDate", "AnaStartTime", "AnaStartTimeZone", "AnaEndDate", "AnaEndTime", "AnaEndTimeZone", "ActComment")]  

####################
# Remove DO 30DMADMin if needed

dySum <- dySum[-which(dySum$StatisticalBasis == '30DMADMin'),]

# Actvity fix when a day has and activity end time that doesn't match for all charid need to just pick the latest time.  
# AWQMS- 	An Activity ID will be generated from the values provided for Monitoring Location ID, Activity Date, Activity Time, and Activity Type. 
#        This is only guaranteed to produce a unique Activity ID if you provide a unique Activity Time for each Activity at a particular Monitoring Location
# To keep AWQMS from having this error need to have unique combinations of: 
# "ActEndTime" when df is grouped by "SiteID", "ActStartDate", "ActStartTime", "ActivityType"
# with the same "ActEndTime". If there is more than one "ActEndTime" then assign the larger ActEndTime.

fndact <- select(dySum, SiteID, ActStartDate, ActStartTime, ActivityType, ActEndDate, ActEndTime, StatisticalBasis, CharID, Result, Unit ) # just makes it easier to work with
ActDiffETm <-fndact %>% 
  group_by(SiteID, ActStartDate, ActStartTime, ActivityType, ActEndDate) %>% # group by activity ID parts
  mutate(nofactid = n_distinct(ActEndTime)) %>% # this is not necessary but handy to look at
  mutate(usendtime = max(ActEndTime)) # gives the later time
#ActDiffETm <- filter(ActDiffETm, nofactid > 1) # this give a df of just the actid's with different times, don't want this for the correction

dySum <- left_join (dySum, ActDiffETm)
dySum$ActEndTime <- dySum$usendtime 
dySum <-  select(dySum, -c('nofactid', 'usendtime')) # remove unwanted columns

ETimeChng <- dySum[which(dySum$ActEndTime != dySumJoin$ActEndTime),]


# remove text NA's "NA"
dysm <- apply(dySum, 2, function(y) gsub(pattern ="NA", replacement = "", y))

write.csv(dysm, file = paste0(out_path, aprj, sbm,'DailySumStatCnDataAwqmsUpload.csv'), na = '')

#  Files to upload to AWQMS end in ContinuousDataAwqmsUpload.csv, ContinuousDataAwqmsInfo.csv (meta data), ContinuousAuditDataAwqmsUpload.csv,
# DailySumStatCnDataAwqmsUpload.csv.  
