# This script was written Jan 2018 to compile continuous data already processed using the Continuous Data Review Scripts
# used for volunteer data.  The compiled data is for upload to the AWQMS data managment system.

library(stringi)
library(plyr)
library(dplyr)



# Designate the folder where you are getting the .Rdata files.  Must end with '/'.
in_path <- '//deqlab1/Vol_Data/umpqua/2014/ReferenceTemp14/Rfiles/'

# Designate the folder where you will Save the outputs...this may be the same as above. Must end with '/'.
out_path <- '//deqlab1/Vol_Data/umpqua/2014/ReferenceTemp14/AWQMS/'

# Enter VolWQdb.t_Submission Number as text
sbm <- '0089'

# AWQMS Project 
aprj <- 'VmOdeqSdcwcWqm'

#  Get look up table for characteristics
load('//deqlab1/WQM/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/ConCharInfo.RData')

#Set working directory
setwd(in_path)



######################################
###                                ###
#   CONTINUOUS LOGGED DATA UPLOAD    #
###                                ###
######################################



#Get the names for the .Rdata graded files
fnames <- list.files(path = in_path, pattern = ".R[Dd]ata")
datfls <- fnames[grep(paste0("^",sbm,".+_.Rdata"),fnames)] # data files
audfls <- fnames[grep(".AUDIT_INFO.Rdata",fnames)] # audit files
svdfls <- fnames[grep(".RData", fnames)] # Saved files for volunteer db.



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

# Export to csv

write.csv(awqmsCnDat, file = paste0(out_path, aprj,'ContinuousDataAwqmsUpload.csv'))



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

smi2[10,2] <- '38591-ORDEQ' # submission 0020 problem

DeployInfo <- merge(DeployInfo, smi2, all = T)
DeployInfo$Project <- aprj

write.csv(DeployInfo, file = paste0(out_path, aprj,'ContinuousDataAwqmsInfo.csv'))



######################################
###                                ###
#    DISCRETE AUDIT DATA UPLOAD      #
###                                ###
######################################

audDat <- fnames[grep(".AuditResults.RData",fnames)] # audit files
audAct <- fnames[grep(".AuditActivity.RData",fnames)] # audit files

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

write.csv(awAud, file = paste0(out_path, aprj,'ContinuousAuditDataAwqmsUpload.csv'))


######################################
###                                ###
#     DAILY SUMMARY DATA UPLOAD      #
###                                ###
######################################

dyDat <- fnames[grep(paste0(sbm,"Results.RData"),fnames)] # audit files
dyAct <- fnames[grep(paste0(sbm,"Activity.RData"),fnames)] # audit files

load(dyDat)
load(dyAct)

dySum <- merge(t_CnRslt, t_ConDatAct, by = 'ActivityID')

# Add Project
dySum$Project <- aprj

# Convert to AWQMS Monitoring Location
dySum$SiteID <- paste0(dySum$SiteID,'-ORDEQ')

# Add Start Date column
dySum$StartDate <- strftime(dySum$StartDateTime, format =  '%m-%d-%Y')

# Add Start Time column
dySum$StartTime <- strftime(dySum$StartDateTime, format =  '%H:%M')

#  Add Start Time Zone
dySum$StartTimeZone <- stri_sub(as.character(as.POSIXct(dySum$StartDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

# Add End Date column
dySum$EndDate <- strftime(dySum$EndDateTime, format =  '%m-%d-%Y')

# Add End Time column
dySum$EndTime <- strftime(dySum$EndDateTime, format =  '%H:%M')

#  Add End Time Zone
dySum$EndTimeZone <- stri_sub(as.character(as.POSIXct(dySum$EndDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)

write.csv(dySum, file = paste0(out_path, aprj,'DailySumStatCnDataAwqmsUpload.csv'))

