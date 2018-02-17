# This script loads continuous data into the VolWQdb.
# It depends on the files being named consistently from previous R scripts 
# 1/23/2018 may need to have timezone added.

library(RODBC)

# Designate the folder where you are getting the .RData files, the save_path from the 04 Script.  Must end with '/'.
in_path <- '//deqlab1/Vol_Data/socoast/2013Submit/2013 Multiparameter Data Submit/Routputs/'

# Designate the folder where you will Save the outputs...this may be the same as above. Must end with '/'.
out_path <- '//deqlab1/Vol_Data/socoast/2013Submit/2013 Multiparameter Data Submit/Routputs/'

# Enter VolWQdb.t_Submission Number as text
sbm <- '0099'

# AWQMS Project 
aprj <- 'VmOdeqSdcwcWqm'

#  Get look up table for characteristics
load('//deqlab1/wqm/DataManagement/ContinuousDataRTool/ConCharInfo.RData')

#Set working directory
setwd(in_path)



######################################
###                                ###
#   CONTINUOUS LOGGED DATA UPLOAD    #
###                                ###
######################################



#Get the names for the .Rdata graded files
fnames <- list.files(path = in_path, pattern = ".R[Dd]ata")
svdfls <- fnames[grep(".RData", fnames)] # Saved files for volunteer db, note logged and audit data '.Rdata'.

lapply(svdfls, load, .GlobalEnv)


#####
  #
  #
  # IMEZONES FOR ACTVITIES AND RESULTS

#  WARNING- this code requires the column order to remain the same from the time of these edits

# Continuous data daily summary activities
nc <- ncol(t_ConDatAct) # Total number of columns before adding time zones, depends on specific location of datatime fields
t_ConDatAct$StartDateTimeZone <- stri_sub(as.character(as.POSIXct(t_ConDatAct$StartDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)
t_ConDatAct$EndDateTimeZone <- stri_sub(as.character(as.POSIXct(t_ConDatAct$EndDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)
t_ConDatAct <- t_ConDatAct[,c(1:7,nc+1,8,nc+2,9:nc)]

# Audit data actvities
nc <- ncol(t_CnAudAct) # Total number of columns before adding time zones, depends on specific location of datatime fields
t_CnAudAct$StartDateTimeZone <- stri_sub(as.character(as.POSIXct(t_CnAudAct$StartDateTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)
t_CnAudAct <- t_CnAudAct[,c(1:7,nc+1,8:nc)]

# Result Data
nc <- ncol(t_CnRslt) # Total number of columns before adding time zones, depends on specific location of datatime fields
t_CnRslt$AnalyticalStartTimeZone <- stri_sub(as.character(as.POSIXct(t_CnRslt$AnalyticalStartTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)
t_CnRslt$AnalyticalEndTimeZone <- stri_sub(as.character(as.POSIXct(t_CnRslt$AnalyticalEndTime), format = '%Y-%m-%d %H:%M:%S %Z') , -3)
t_CnRslt <- t_CnRslt[,c(1:8,nc+1,9,nc+2,10:nc)]



##
# #
# #
## atabase prep


ch <- odbcConnectAccess2007("//deqlab1/wqm/Volunteer Monitoring/datamanagement/VolWQdb.mdb", case="nochange")
odbcGetInfo(ch)
sqlTypeInfo(ch)

#    #
#  #
#   ariable Types from database used for saving dataframes in Access

# Activity Table variable types
vt<-sqlColumns(ch, 't_Activity')
vt_Activity <- as.character(vt$TYPE_NAME)
names(vt_Activity) <- as.character(vt$COLUMN_NAME)
vt_Activity <- gsub('LONGCHAR','VARCHAR',vt_Activity) # remove LONGCHAR memory hog variable type

# Results Table variable types
vt<-sqlColumns(ch, 't_Result')
vt_Result <- as.character(vt$TYPE_NAME)
names(vt_Result) <- as.character(vt$COLUMN_NAME)
vt_Result <- gsub('LONGCHAR','VARCHAR',vt_Result)
rm(vt)

# ActGrp table variable types
vt<-sqlColumns(ch, 't_ActGrp')
vt_ActGrp <- as.character(vt$TYPE_NAME)
names(vt_ActGrp) <- as.character(vt$COLUMN_NAME)
vt_ActGrp <- gsub('LONGCHAR','VARCHAR',vt_ActGrp)
rm(vt)

# ActGrp2Act table variable types
vt<-sqlColumns(ch, 'tjct_ActGrp2Act')
vt_ActGrp2Act <- as.character(vt$TYPE_NAME)
names(vt_ActGrp2Act) <- as.character(vt$COLUMN_NAME)
vt_ActGrp2Act <- gsub('LONGCHAR','VARCHAR',vt_ActGrp2Act)
rm(vt)



#########################################################
########################################################

#    
#    
#  
#### oad the data into the database

############################################################

#
#
###
#####
#Continuous to Activity table
sqlDrop(ch, 'TempCnAct', errors = FALSE)
sqlSave(ch, t_ConDatAct, tablename = "TempCnAct", append = FALSE, rownames = FALSE, colnames = FALSE, 
        safer = TRUE, varTypes = vt_Activity) # vt_Activity not limited here to only those in names(t_ConDatAct)
CDAqry <- 'INSERT INTO t_Activity ( ActivityID, ActivityType, SubID, SiteID, SiteID_Context, SiteDescription, StartDateTime, StartDateTimeZone, EndDateTime, EndDateTimeZone, Media, ActivityOrg, SmplColMthd, SmplColEquip, SmplEquipID, SmplColEquipComment, SmplDepth, SmplDepthUnit, Org_Comment, DEQ_Comment, Samplers )
SELECT TempCnAct.ActivityID, TempCnAct.ActivityType, TempCnAct.SubID, TempCnAct.SiteID, TempCnAct.SiteID_Context, TempCnAct.SiteDescription, TempCnAct.StartDateTime, TempCnAct.StartDateTimeZone, TempCnAct.EndDateTime, TempCnAct.EndDateTimeZone, TempCnAct.Media, TempCnAct.ActivityOrg, TempCnAct.SmplColMthd, TempCnAct.SmplColEquip, TempCnAct.SmplEquipID, TempCnAct.SmplColEquipComment, TempCnAct.SmplDepth, TempCnAct.SmplDepthUnit, TempCnAct.Org_Comment, TempCnAct.DEQ_Comment, TempCnAct.Samplers
FROM TempCnAct;'
sqlQuery(ch, CDAqry, max = 0, buffsize = length(t_ConDatAct$ActivityID))

#
###
#####
#Continuous to Activity Groups
vt_ActGrp <- vt_ActGrp[which(names(vt_ActGrp) %in% names(t_ActGrp))]
sqlDrop(ch,'TempCnActGrp', errors = FALSE)
sqlSave(ch, t_ActGrp, tablename = 'TempCnActGrp', append = FALSE, rownames = FALSE, colnames = FALSE, 
        safer = TRUE, varTypes = vt_ActGrp)
CAGqry <- 'INSERT INTO t_ActGrp (ActGrpID, ActGrpType)
SELECT TempCnActGrp.ActGrpID, TempCnActGrp.ActGrpType
FROM TempCnActGrp;'
sqlQuery(ch, CAGqry, max = 0, buffsize = length(t_ActGrp$ActGrpID))

#
###
#####
#Continuous to Activity Group to Activity junction table
vt_ActGrp2Act <- vt_ActGrp2Act[which(names(vt_ActGrp2Act) %in% names(t_ActGrp2Act))]
sqlDrop(ch,'TempCnActGrp2Act', errors = FALSE)
sqlSave(ch, t_ActGrp2Act, tablename = 'TempCnActGrp2Act', append = FALSE, rownames = FALSE, colnames = FALSE, 
        safer = TRUE, varTypes = vt_ActGrp2Act)
CAG2Aqry <- 'INSERT INTO tjct_ActGrp2Act (ActGrpID, ActivityID)
SELECT TempCnActGrp2Act.ActGrpID, TempCnActGrp2Act.ActivityID
FROM TempCnActGrp2Act;'
sqlQuery(ch, CAG2Aqry, max = 0, buffsize = length(t_ActGrp2Act$ActGrpID))



#
###
#####
# Continuous Daily Summary Stats to Results

# Trim the Variable Type vector to just include fields from file to upload
vt_Rslt <- vt_Result[which(names(vt_Result) %in% names(t_CnRslt))]
sqlDrop(ch, 'TempCnRslt', errors = FALSE)
sqlSave(ch, t_CnRslt, tablename = "TempCnRslt", append = FALSE, rownames = FALSE, colnames = FALSE, 
        safer = TRUE, varTypes = vt_Rslt)
CDRqry <- 'INSERT INTO t_Result ( ResultID, ActivityID, CharID, Result, Unit, Method, RsltType, AnalyticalStartTime, AnalyticalStartTimeZone, AnalyticalEndTime, AnalyticalEndTimeZone, ORDEQ_DQL, StatisticalBasis, RsltTimeBasis, RsltStatus, DEQ_RsltComment )
SELECT TempCnRslt.ResultID, TempCnRslt.ActivityID, TempCnRslt.CharID, TempCnRslt.Result, TempCnRslt.Unit, TempCnRslt.Method, TempCnRslt.RsltType, TempCnRslt.AnalyticalStartTime, TempCnRslt.AnalyticalStartTimeZone, TempCnRslt.AnalyticalEndTime, TempCnRslt.AnalyticalEndTimeZone, TempCnRslt.ORDEQ_DQL, TempCnRslt.StatisticalBasis, TempCnRslt.RsltTimeBasis, TempCnRslt.RsltStatus, TempCnRslt.DEQ_RsltComment
FROM TempCnRslt;'
sqlQuery(ch, CDRqry, max = 0, buffsize = length(t_CnRslt$ResultID))

#####
###
##
#Audits Activity to activity table
#remove duplicated activityID's when audits are redundant
t_CnAudAct <- t_CnAudAct[!duplicated(t_CnAudAct),]
# Trim the Variable Type vector to just include fields from file to upload
vt_CnAudAct <- vt_Activity[which(names(vt_Activity) %in% names(t_CnAudAct))]
sqlDrop(ch,'TempCnAudAct',errors = FALSE)
sqlSave(ch, t_CnAudAct, tablename = "TempCnAudAct", append = FALSE, rownames = FALSE, colnames = FALSE, safer = TRUE, varTypes = vt_CnAudAct)
CDAAqry <- 'INSERT INTO t_Activity ( ActivityID, ActivityType, SubID, SiteID, SiteID_Context, SiteDescription, StartDateTime, StartDateTimeZone, Media, ActivityOrg )
SELECT TempCnAudAct.ActivityID, TempCnAudAct.ActivityType, TempCnAudAct.SubID, TempCnAudAct.SiteID, TempCnAudAct.SiteID_Context, TempCnAudAct.SiteDescription, TempCnAudAct.StartDateTime, TempCnAudAct.StartDateTimeZone, TempCnAudAct.Media, TempCnAudAct.ActivityOrg
FROM TempCnAudAct;'
sqlQuery(ch, CDAAqry, max = 0, buffsize = length(t_CnAudAct$ActivityID))

#####
###
##
#Audits activity groups to activity #####   This turns out duplicates from continuous activity groups
vt_ActGrp <- vt_ActGrp[which(names(vt_ActGrp) %in% names(t_AudActGrp))]
sqlDrop(ch,'TempCnAudActGrp', errors = FALSE)
sqlSave(ch, t_AudActGrp, tablename = 'TempCnAudActGrp', append = FALSE, rownames = FALSE, colnames = FALSE, 
        safer = TRUE, varTypes = vt_ActGrp)
CAAGqry <- 'INSERT INTO t_ActGrp (ActGrpID, ActGrpType)
SELECT TempCnAudActGrp.ActGrpID, TempCnAudActGrp.ActGrpType
FROM TempCnAudActGrp;'
sqlQuery(ch, CAAGqry, max = 0, buffsize = length(t_AudActGrp$ActGrpID))

###
#
##
#
###rror?

#Need to compare t_AudActGrp$ActGrpID to t_ActGrp$ActGrpID and only load new t_AudActGrp$ActGrpID
t_AudActGrp$ActGrpID %in% t_ActGrp$ActGrpID
# If these are all true then the failed failed query is fine.  Otherwise new groupID's need to be created in t_ActGrp database



#####
###
##
# Audit activity groups to activity junction table
vt_AudActGrp2Act <- vt_ActGrp2Act[which(names(vt_ActGrp2Act) %in% names(t_AudActGrp2Act))]
sqlDrop(ch,'TempCnActGrp2Act', errors = FALSE)
sqlSave(ch, t_AudActGrp2Act, tablename = 'TempCnActGrp2Act', append = FALSE, rownames = FALSE, colnames = FALSE, 
        safer = TRUE, varTypes = vt_ActGrp2Act)
CAAG2Aqry <- 'INSERT INTO tjct_ActGrp2Act (ActGrpID, ActivityID)
SELECT TempCnActGrp2Act.ActGrpID, TempCnActGrp2Act.ActivityID
FROM TempCnActGrp2Act;'
sqlQuery(ch, CAAG2Aqry, max = 0, buffsize = length(t_AudActGrp2Act$ActGrpID))



#####
###
##
# Audit Results to results table
#remove duplicated audit results when audits are redundant
t_CnAudRslt <- t_CnAudRslt[!duplicated(t_CnAudRslt),]
# Trim the Variable Type vector to just include fields from file to upload

vt_CnAudRslt <- vt_Result[which(names(vt_Result) %in% names(t_CnAudRslt))]
sqlDrop(ch, 'TempCnAudRslt', errors = FALSE)
sqlSave(ch, t_CnAudRslt, tablename = "TempCnAudRslt", append = FALSE, rownames = FALSE, colnames = FALSE, safer = TRUE, varTypes = vt_CnAudRslt)
CDARqry <- 'INSERT INTO t_Result ( ResultID, ActivityID, CharID, Result, Unit, Method, RsltType, ORDEQ_DQL, RsltStatus, Org_RsltComment )
SELECT TempCnAudRslt.ResultID, TempCnAudRslt.ActivityID, TempCnAudRslt.CharID, TempCnAudRslt.Result, TempCnAudRslt.Unit, TempCnAudRslt.Method, TempCnAudRslt.RsltType, TempCnAudRslt.ORDEQ_DQL, TempCnAudRslt.RsltStatus, TempCnAudRslt.Org_RsltComment
FROM TempCnAudRslt;'
sqlQuery(ch, CDARqry, max = 0, buffsize = length(t_CnAudRslt$ResultID))


odbcClose(ch)
