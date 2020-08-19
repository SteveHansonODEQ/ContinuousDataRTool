library(stringi)
library(plyr)
library(dplyr)
library(reshape2)
library(RODBC)
library(psych)
library(ggplot2)



# This 04 script variant is used when continuous WQ data is collected at a station over an extended
# period but the data is processed and reviewed in smaller chunks.  In the DEQ system that would mean
# a contiguous deployment would be made of multiple work orders with only small breaks in between the
# work orders to service the equipment and download data.  

# This tool allows data from previous work orders to be considered when calculating summary statistics
# like daily max, min, mean, etc and moving averages.  Without considering all the work orders each day
# there was a service visit would have insufficient data and moving averages which need  7 - 30 consecutive
# days to calculate


#######################################################
#############################

###
#  #
###
#  #
#   #equirements user must enter the following information

#1# ## INPUT provide sampling organization from VolWQdb.tlu_Organization.OrgAbrv
ORG <- 'OREGONDEQ' 

#2# Enter the work order identifying number
SubID <- '2006084' # Enter the WORK ORDER ID number.  For volunteer data use the VolMon SubID

#3# needed to generate AWQMS upload files
aprj <- 'TMDL'#  AWQMS Project ID

#4# LOCATION OF DATA FILES TO BE PROCESSED (This shouldn't change)
shiny_path <- "//deqlab1/WQM/TMDL/aEastern Region/Deschutes/2020/AllFinalCnRDataFiles"

#5# LOCATION TO SAVE DATA FILES CREATED IN PROCESS
save_path <- "//deqlab1/WQM/TMDL/aEastern Region/Deschutes/2020/20200521/SondeData/RData/OutputsTest/"  # make sure this ends with an /


#######################################

# Generic data needed for chart generation 

######################################

# Continuous Data Characteristics Information file this may change depending on if you connected to deq network
ConCharInfo <- '//deqlab1/wqm/DataManagement/ContinuousDataRTool/ConCharInfo.RData'
 
# LOAD information about each charcteristic, units, methods
 load(ConCharInfo)


 
################################################################### 
###################################################################
 
 # Begin Processing Data
 
################################################################### 
###################################################################

#             
#             
### oad the Files to use in for loop

####################################

#Gather filenames from the shiny folder
in_fnames <- list.files(shiny_path, full.names = TRUE)


########
# Get a vector of the Continuous Data Files, exclude the audit info data
alldatafiles <- in_fnames[!grepl('AUDIT_INFO', in_fnames)]
alldatafiles <- alldatafiles[!grepl('SiteMasterInfo', alldatafiles)]
datafiles <- alldatafiles[grepl(SubID, alldatafiles)]

#########
# Get vector of audit data files, exclude the logged data files
allauditfiles <- in_fnames[grepl('AUDIT_INFO', in_fnames)]
auditfiles <- allauditfiles[grepl(SubID, allauditfiles)]

 ########
# LOAD the site master information dataframe for active dataset
smis <- in_fnames[grepl('SiteMasterInfo', in_fnames)]
load(smis[grepl(SubID, smis)])

# make a dataframe of the audit file components
allaudit.fileinfo<- read.table(text = basename(auditfiles) , sep = '_', colClasses = "character", as.is=TRUE)
allaudit.fileinfo<- cbind(allaudit.fileinfo,auditfiles)
names(allaudit.fileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'audit', 'info_extension', 'filepath' )
allaudit.fileinfo$filepath <- as.character(allaudit.fileinfo$filepath)

########

# Reduce datafile list to only those files with valid results
# validdatafiles <- datafiles
invaliddatafiles <- character(length = 0) # create empty vector


for (m in 1:length(datafiles)) { 
  
  fname <- datafiles[m]#"XXXX_99990_10429625_DO_20150420_Salmon R Hatchery_1_.Rdata"
  load(fname)
  
  
  # Get continuous data file information from name
  fileinfo <- read.table(text = basename(fname) , sep = '_', as.is=TRUE, colClasses = "character")
  names(fileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'depth_m','extension' )
  
  # Clean up logged result rows with poor DQL
  tmp_data$r4calc <- as.numeric(ifelse(is.na(tmp_data$rDQL) | tmp_data$rDQL == 'C'| tmp_data$rDQL == 'D',
                                       NA, tmp_data$r))
  
  # If there is not valid data for calculations, remove it from validdatafiles
  # Should have > 3 results as well
 
  if (nrow(tmp_data[which(!is.na(tmp_data$r4calc) | tmp_data$rDQL != 'C'),]) <= 3) {
    invaliddatafiles <- append(invaliddatafiles, fname)
    print(paste0('Insufficient valid data: ', fname))
  }
  
}

validdatafiles <- datafiles[!datafiles %in% invaliddatafiles]


print(paste0('Total number of datafiles = ', length(datafiles)))
print(paste0('Total number of datafiles with insufficient valid data = ', length(invaliddatafiles)))
print(paste0('Total number of valid datafiles = ', length(validdatafiles)))



#####
######Generate table with audit dates
for (n in 1:length(allauditfiles)) {
  afname <- allauditfiles[n]
  
  load(afname)
  
  dr_info$WO <- trimws(read.table(text = basename(afname) , sep = '_', as.is=TRUE, colClasses = "character")[1,1]) # add the workorder number to the dr_info dataframe
  dr_info$Logger_ID <- trimws(read.table(text = basename(afname) , sep = '_', as.is=TRUE, colClasses = "character")[1,3])# add the logger Id to the dr_info dataframe
  dr_info$lasar <- trimws(read.table(text = basename(afname) , sep = '_', as.is=TRUE, colClasses = "character")[1,2]) # add the station ID
  
  if (n == 1) { allaudits <- dr_info
  } else if (n >1) { allaudits <- rbind(allaudits, dr_info)
    } # use the allaudits$WO number and allaudits$AUDIT_DATETIME to find which datafiles have data proeeding working dataset by 30 days.
  }


################################################################################################################
################################################################################################################

#####  #
#      #
###    #
#      #  
# or   #### oop through files loading output into Volunteer WQ Database


           #
            ##
###############  START For loop through logger files here
            ##
           #  
for (i in 1:length(validdatafiles)) { 
  fname <- validdatafiles[i]# example "XXXX_99990_10429625_DO_20150420_Salmon R Hatchery_1_.Rdata"
  load(fname)
  print(paste0(i, ' of ', length(validdatafiles), ' is ' , fname))
  
  
  # Get continuous data file information from name
  fileinfo <- read.table(text = basename(fname) , sep = '_', as.is=TRUE, colClasses = "character")
  names(fileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'depth_m','extension' )
  
  
  ##
  ####
  ###### Determine what other datafiles need to be combined in order to calculate all daily stats
  ####
  ##
  
  
  deploydatetime <- min(tmp_data[which(!is.na(tmp_data$rDQL)),1]) # pick minimum date where rDQL is not NA to just get times deployed
  retrievedatetime <- max(tmp_data[which(!is.na(tmp_data$rDQL)),1]) # and same for the retrieval date...may not need this
  # calculate 30 days prior to first date of active dataset
  newstartdt <- deploydatetime-(31*86400) # subtract 30 days from deployment plus one to cover different audit times
  
  # figure out which work orders have data for this date frame using allaudits 
  # need vector of unique work orders where charid, loggerID match fileinfo and auditdatetime >= newstartdt and <= retrievaldatetime allows folder to hold all datafiles
  audrng <- filter(allaudits, (PARAMETER == fileinfo$charid & lasar == fileinfo$lasar & AUDIT_DATETIME >= newstartdt & AUDIT_DATETIME <= retrievedatetime )) ##  NOT ROBUST for different loggers at same locations
  # but is robust for changing loggers at one location
  ValidWO <- unique(audrng$WO)
  AddWO <- ValidWO[-which(ValidWO == SubID)] # these are the work orders in addition to the current dataset that need to be loaded to go back 30 days
  
  
  # need all datafiles which match basename lasar and charid
  files2load <- grep(fileinfo$lasar, alldatafiles, value = T) # only get files from the site
  files2load <- grep(paste0('_',fileinfo$charid, '_'), files2load, value = T) # then reduce again to just files with matching charid
  # now get only those files from the AddWO vector of workorders with data to add.
  files2load <- files2load[which(trimws(read.table(text = basename(files2load) , sep = '_', as.is=TRUE, colClasses = "character")[,1]) %in% AddWO)]
  # sort files to load by start of work order listed in datafile name
  # create blank data frame
  
  Files2LoadDF <- data.frame(path=character(length(files2load)),
                             StartDate=character(length(files2load)), 
                             stringsAsFactors=FALSE) 
  Files2LoadDF$path <- if (length(files2load) > 0) {files2load} 
  Files2LoadDF$StartDate <- if (length(files2load) > 0) {
    read.table(text = basename(files2load) , sep = '_', as.is=TRUE, colClasses = "character", stringsAsFactors = F)[,5]
  }
          
  if (length(files2load)>0) {Files2LoadDF <- Files2LoadDF[order(rev(Files2LoadDF$StartDate)),]} # sorts the dataframe of files to load in descending order (more recent files on top)
  
  


    #
   # # 
  ####
  #  #
  #  #dd older data
  
  # cycle through the files2load and rbind them to main dataset.
  
  # the existing tmp_data file and the data files to load must have matching columns
  # If there is no comment field in the tmp_data file then create one
  if(!'cmnt' %in% names(tmp_data)) tmp_data$cmnt<- as.character(NA) # moved this from below to get equal columns when combining data.
  
    
  if (length(Files2LoadDF$path) >0 ) { for (f in 1:length(Files2LoadDF$path))
      { # if there is at least one file to load then 
      print(paste0('Add old file ', f, ' of ', length(Files2LoadDF$path))) 
      print(Files2LoadDF$path[f])
      alldata <- tmp_data
      load(Files2LoadDF$path[f])
      if(!'cmnt' %in% names(tmp_data)) tmp_data$cmnt<- as.character(NA)
      if (f < length(Files2LoadDF$path)) { # if the added file is not the oldest file keep alldata
        #if(!'cmnt' %in% names(tmp_data)) tmp_data$cmnt<- NA
        # shorten the length of columns in added tmp_data to match alldata, should just cut off r4calc
        if(length(names(tmp_data)) > length(names(alldata))) { # print columns to delete if extra in tmp_data
        print(paste0('column(s) in added  file deleted is = ', names(tmp_data)[which(!names(tmp_data) %in% names(alldata))] ) )
           tmp_data <- tmp_data[,length(names(alldata))] } # remove hopefully only the r4calc column
        tmp_data <- rbind(tmp_data, alldata)
      } else if (f == length(Files2LoadDF$path)){ # if the added file is the oldest it is the last one to add and...
        #if(!'cmnt' %in% names(tmp_data)) tmp_data$cmnt<- NA
        alldata <- rbind(tmp_data, alldata)
        tmp_data <- alldata # ...the alldata DF should be renamed tmp_data so the rest of the script works
      }
   }  
  }
  # End of the process to append the last 30 days of data to the existing dataset.
  
  
  # Clean up logged result rows with poor DQL
  tmp_data$r4calc <- as.numeric(ifelse(is.na(tmp_data$rDQL) | tmp_data$rDQL == 'C'| tmp_data$rDQL == 'D',
                            NA, tmp_data$r))
  
  
  print(paste0(fileinfo$LoggerID, '-', fileinfo$charid, ' Calculate Daily Stats'))
  ###################
  #   #
  #   #
  #####
  #   #
  #   #ourly Values
  ##################
  
  # Get unique hour values
  tmp_data$hr <- format(tmp_data$DATETIME, "%Y-%j-%H")
  
  # If there is no comment field from running the edit script, then create one
  #if(!'cmnt' %in% names(tmp_data)) tmp_data$cmnt<- NA # moved this to get equal columns when combining data.
  
  
  # remove C level data for statistics
  td2 <- filter(tmp_data, rDQL != 'C')
  
  
  # Simplify to hourly values and Stats
  hrsumna<-ddply(td2,"hr",summarise, # transform for retaining rows
                 date = mean(date),
                 hrDTmin = min(DATETIME),
                 hrDTmax = max(DATETIME),
                 hrN = sum(!is.na(r4calc)),
                 hrMean = mean(r4calc, na.rm=TRUE),
                 hrMin = min(r4calc, na.rm=TRUE),
                 hrMax = max(r4calc, na.rm=TRUE),
                 hrdql = max(rDQL, na.rm=TRUE),
                 cmnt = toString(unique(cmnt[!is.na(cmnt)])))
  # Warnings are OK but need to run NA assignments below
  hrsumna$hrMin[which(is.infinite(hrsumna$hrMin))] <- NA
  hrsumna$hrMax[which(is.infinite(hrsumna$hrMax))] <- NA
  
  # replace blank cmnt cells with NA
  hrsumna$cmnt[which(hrsumna$cmnt == '')] <- NA
  
  
  #########################
  # #
  #  #
  #   # 
  #  #  
  ##  aily stats
  ##########################
  
  # For each date, how many hours have hrN > 0
  # remove rows with zero records in an hour.
  hrdat<- hrsumna[which(hrsumna$hrN >0),]
  
  # Summarise to daily statistics
  daydat<-ddply(hrdat,"date",summarise,
                dDTmin = min(hrDTmin),
                dDTmax = max(hrDTmax),
                hrNday = length(hrN), 
                dyN = sum(hrN),
                dyMean = mean(hrMean, na.rm=TRUE),
                dyMin = min(hrMin, na.rm=TRUE),
                dyMax = max(hrMax, na.rm=TRUE),
                dydql = max(hrdql, na.rm=TRUE),
                cmnt = paste(unique(unlist(strsplit(toString(unique(cmnt[!is.na(cmnt)])), split = ', '))), collapse = '; ') )
  
  # replace blank cmnt cells with NA
  daydat$cmnt[which(daydat$cmnt == '')] <- NA
  
  # assign dayDQL based on hours with data dydat$hrNday
  for (j in 1:length(daydat$date)) {
    daydat$dDQL[j] <-  ifelse(daydat$hrNday[j] > 22, daydat$dydql[j],
                              ifelse(daydat$hrNday[j] < 23 & daydat$hrNday[j] > 20, max(c(daydat$dydql[j],'B'))
                                     ,'C'))
  }
  
  # Add comment regarding dDQL determination
  daydat$cmnt <- ifelse(daydat$hrNday > 22, daydat$cmnt, 
                        (ifelse(is.na(daydat$cmnt),paste0(as.character(daydat$hrNday),' hrs with valid data in day'),
                                paste0(daydat$cmnt,'; ',as.character(daydat$hrNday),' hrs with valid data in day'))))
  
  
  
  # Delta T
  for (j in 1:length(daydat$date)) {
    daydat$delta[j] <- ifelse(daydat$dDQL[j] == 'C' | is.na(daydat$dDQL[j]), NA, 
                              daydat$dyMax[j] - daydat$dyMin[j] )
  }
  
  # Get the daily median values and add them to daily data
  dm <- aggregate(r4calc~date, data=tmp_data, FUN=median, na.rm = TRUE)
  names(dm) <- c("date", "dyMedian")
  daydat <- merge(x= daydat,y= dm, by="date",all.x=TRUE)
  
  
  ##########
  #   #
  ## ##
  # # #
  #   # oving Averages
  #########
  
  # create column for moving average calculations
  daydat$ma <- NA
  daydat$anaStart <- as.POSIXct(NA)# Add analysis start and end dates
  daydat$anaEnd <- as.POSIXct(NA)
  
  ##  DISSOLVED OXYGEN 
  if (fileinfo$charid  %in% c('DO','adjDO','DOs')) {
    # remove data with bad dDQL's and get daily minimum value
    daydat$r4ma <- ifelse(daydat$dDQL == 'C' | is.na(daydat$dDQL), NA, daydat$dyMin ) 
    for (j in 1:length(daydat$date)) {
      if (j < 30) {
        daydat$ma[j]<- as.numeric(NA)
      } else if (j >29 && (daydat$dDTmax[j] - daydat$dDTmin[j-29])<= 30.03) { # needs to be after the 30th day and should only span 30 days, the .03 accounts for time changes in fall .
        daydat$anaStart[j] <- daydat$dDTmin[j-29] # careful that the local time zone doesn't mess this up
        daydat$anaEnd[j] <- daydat$dDTmax[j] # careful that the timeshift doesn't mess this up
        ifelse(sum(is.na(daydat$r4ma[(j-29):j])) > 3, NA, # if more than 3 missing days than no calculation
               daydat$ma[j] <- mean(daydat$r4ma[(j-29):j], na.rm = TRUE))
      }
    }
  }
  
  ##  TEMPERATURE
  if (fileinfo$charid %in% c('TEMP','adjTEMP')) {
    # remove data with bad dDQL's and get daily minimum value
    daydat$r4ma <- ifelse(daydat$dDQL == 'C' | is.na(daydat$dDQL), NA, daydat$dyMax ) 
    for (j in 1:length(daydat$date)) {
      if (j < 7) {
        daydat$ma[j]<- as.numeric(NA)
      } else if (j > 6 && (daydat$dDTmax[j] - daydat$dDTmin[j-6]) <= 7.03) { # needs to be after the 6th day and should only span 7 days, the .03 accounts for time changes in fall .
        daydat$anaStart[j] <- daydat$dDTmin[j-6] # careful that the default time zone doesn't mess this up
        daydat$anaEnd[j] <- daydat$dDTmax[j] # careful that the the default time zone doesn't mess this up
        ifelse(sum(is.na(daydat$r4ma[(j-6):j])) > 1, NA, # if more than on missing day then no calculation
               daydat$ma[j] <- mean(daydat$r4ma[(j-6):j], na.rm = TRUE))
      }
    }
  }
  
  ###################################################
  #########################
  
  ###
  #
  ###
    #
  ### ummary Stat Tables (daily and per deployment) for export to csv
  
  # Add deployment metadata
  daydat$LASAR <- fileinfo[,"lasar"]
  daydat$charid <- fileinfo[,"charid"]
  daydat$Depth_m <- fileinfo[,"depth_m"]
  daydat$LoggerID <- fileinfo[,"LoggerID"]
  daydat$SiteDesc <-  fileinfo[,"desc"]
  
  ds <- daydat[,c("LASAR", "SiteDesc", "LoggerID", "Depth_m", "charid", "date", "dDTmin", "dDTmax","dyN", "hrNday", 
                  "dydql", "dDQL", "dyMean", "dyMin", "dyMax", "delta", "dyMedian", "anaStart", "anaEnd", "ma", "cmnt")]
  
  ###
  #  #
  ##
  # #
  #  #educe the days just those starting on the deploy date
  
  # reduce daily stats DF to just dates starting on the deploy date
  ds <- ds[which(ds$date >= as.Date(deploydatetime)),]
  
  
  #####################################
  # Add comments for all summary stats that consider data from outside this data collection
  # The first day always considers previous data
  # dsir comment for first day with moving average and daily stats, masi is when only moving average uses old data
  dsir <- "Daily stats include results from previous data collection"
  masi <- "Moving average stat includes results from previous dataset"
  
  ifelse (is.na(ds$cmnt[which(ds$date == as.Date(deploydatetime))]), 
          ds$cmnt[which(ds$date == as.Date(deploydatetime))] <- dsir, 
          ds$cmnt[which(ds$date == as.Date(deploydatetime))] <- paste0(ds$cmnt[which(ds$date == as.Date(deploydatetime))], ' ', dsir)) 
  #####
    #
    #
    # emperature moving average comments
  
  # For temperature the first 6 days "7 day moving average includes results from previous data collection"
  # ds$charid == "TEMP"
  # ds$date > as.Date(deploydatetime) && ds$date < as.Date(deploydatetime)+7
  # tricky part is that each row where the conditions above are met need to apply the ifelse statement to make either make the comment "" or append the comment to existing text

  # create a vector of comments for moving averages
  vmasi <- case_when(ds$charid == "TEMP" &
                     ds$date > as.Date(deploydatetime) &
                     ds$date < as.Date(deploydatetime)+7 ~ 
                       (ifelse (is.na(ds$cmnt), masi, paste0(ds$cmnt, ' ', masi))) )
  
  # combine the 
  ds$cmnt <- ifelse(is.na(ds$cmnt), (ifelse(is.na(vmasi), NA, masi)), (ifelse(is.na(vmasi), ds$cmnt, paste0(ds$cmnt, ' ', masi))) )
  
  
  
  ##
  # #
  # #
  ## issolved oxygen moving average comments
  
  # For dissolved oxygen the first 29 days "30 day moving average includes results from previous data collection"
  # create a vector of comments for moving averages
  vmasi <- case_when(ds$charid == "DO" &
                       ds$date > as.Date(deploydatetime) &
                       ds$date < as.Date(deploydatetime)+29 ~ 
                       (ifelse (is.na(ds$cmnt), masi, paste0(ds$cmnt, ' ', masi))) )
  
  # combine the 
  ds$cmnt <- ifelse(is.na(ds$cmnt), (ifelse(is.na(vmasi), NA, masi)), (ifelse(is.na(vmasi), ds$cmnt, paste0(ds$cmnt, ' ', masi))) )
  
  
  # get rid of comments not needed anymore
  rm(dsir, masi, vmasi)
  
  
  ########################################
  # Stack all the daily summaries together. and then save as a csv.
  if (i == 1) {
    tmpDyStat <- ds
  } else if (i > 1 && i < length(validdatafiles)) {
   tmpDyStat <- rbind(tmpDyStat, ds)
  } else if ( i == length(validdatafiles)) {
    tmpDyStat <- rbind(tmpDyStat, ds)
    write.csv(tmpDyStat, file = paste0(save_path,'/',fileinfo[,"subid"],'_','DailyStats.csv'))
    print('Daily Stat Summary CSV saved')
  }
  
  # When only one datafile
  if (length(validdatafiles) == 1) {
    write.csv(tmpDyStat, file = paste0(save_path,'/',fileinfo[,"subid"],'_','DailyStats.csv'))
    print('One Daily Stat Summary CSV saved')
  }
  
  ######################################################
  ####################################
  
  ##
  # #
  # #
  ## eployment Statistics
  
  # cut the logger data file down to deployment period after creating a temporary copy of long version
  TmpDataLong <- tmp_data
  tmp_data <- tmp_data[tmp_data$DATETIME %in% deploydatetime:retrievedatetime,]
  
  
  DepStat <- fileinfo[,c("lasar", "desc", "LoggerID", "depth_m", "date", "charid")]
  DepStat$StartDate <- min(tmp_data$DATETIME[which(!is.na(tmp_data$r4calc))])
  DepStat$EndDate <- max(tmp_data$DATETIME[which(!is.na(tmp_data$r4calc))])
  DepStat$Resultcount <- length(!is.na(tmp_data$r4calc))
  DepStat$DayCount <- length(unique(tmp_data$date[which(!is.na(tmp_data$r4calc))]))
  DepStat$Mean <- mean(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$GeoMean <- geometric.mean(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$Min <- min(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$Fifth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.05)
  DepStat$Tenth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.10)
  DepStat$Twentieth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.20)
  DepStat$TwentyFifth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.25)
  DepStat$Median <- median(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$SeventyFifth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.75)
  DepStat$Eightieth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.80)
  DepStat$Ninetieth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.90)
  DepStat$NinetyFifthth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.95)
  DepStat$Max <- max(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$PerA <- 100 * ( length(which(tmp_data$rDQL == 'A'))/length(which(!is.na(tmp_data$rDQL))) ) #sjh added 12/15 per NH request 
  DepStat$PerB <- 100 * ( length(which(tmp_data$rDQL == 'B'))/length(which(!is.na(tmp_data$rDQL))) ) #sjh added 12/15 per NH request
  DepStat$PerE <- 100 * ( length(which(tmp_data$rDQL == 'E'))/length(which(!is.na(tmp_data$rDQL))) ) #sjh added 12/15 per NH request
  
  if (i == 1) {
    tmpDepStat <- DepStat
  } else if (i > 1 && i < length(validdatafiles)) {
    tmpDepStat <- rbind(tmpDepStat, DepStat)
  } else if ( i == length(validdatafiles)) {
    tmpDepStat <- rbind(tmpDepStat, DepStat)
    write.csv(tmpDepStat, file = paste0(save_path,'/',fileinfo[,"subid"],'_','DeployStats.csv'))
    print('Deployment Stat Summary CSV saved')
  }
  
  # When only one datafile
  if (length(validdatafiles) == 1) {
    write.csv(tmpDepStat, file = paste0(save_path,'/',fileinfo[,"subid"],'_','DeployStats.csv'))
    print('One Deployment Stat Summary CSV saved')
  }
  
  #############################################################
  ###############################
  
  ####
  #  #
  ###
  #
  # lots
  

  # Build data frame with plotting data
  # reload the 
  PltDat <- tmp_data[,c("DATETIME", "date", "charid", "r", "r4calc", "field_audit_grade", "cvDQL", "rDQL", "anomaly") ]
  PltDat <- merge(x = PltDat, y = daydat, by = "date")
  # get audit data
  auditfilepath <- allaudit.fileinfo$filepath[Reduce(intersect, list(which(allaudit.fileinfo$lasar == fileinfo$lasar), 
                                                                     which(allaudit.fileinfo$charid == fileinfo$charid), 
                                                                     which(allaudit.fileinfo$LoggerID == fileinfo$LoggerID)))]
  load(auditfilepath)
  # Get error bar limits for audits...later. B/c of the low level criteria this will be difficult.
  
  # Basic plot data  
  pd <- ggplot(data = PltDat, aes(DATETIME, r)) + geom_point(aes(color = rDQL)) + xlab("Date") + theme_bw() +
    ylim(min(PltDat$r4calc[which(!is.na(PltDat$r4calc))]),max(PltDat$r4calc[which(!is.na(PltDat$r4calc))])) +
    ylab(paste0(fileinfo[,'charid'],' (',ConCharInfo$Unit[which(ConCharInfo$charid == fileinfo[,'charid'])],')')) +
    labs(title = paste0('Continuous ',fileinfo[,'charid'], ' Data at LASAR Station ', fileinfo[,'lasar'], ' Logger ', fileinfo[,'LoggerID']) ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data = dr_info, aes(x = AUDIT_DATETIME, y = AUDIT_RESULT), color = 'black', size = 3, alpha = 0.5) 
    
  
  if (fileinfo[,'charid'] %in% c('Q', 'TURB', 'adjTURB', 'CHA', 'BGA')){
    # Log Scale
    pd <- pd + scale_y_log10() + annotation_logticks( base = 10, sides = 'l')
  } else if (fileinfo[,'charid'] %in% c('TEMP', 'adjTEMP')) {
    pd <- pd + geom_hline(aes(yintercept = 20), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 20.5, label = 'Migration', color = 'blue') + # Migration corridor
    geom_hline(aes(yintercept = 18), linetype = 6, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 18.5, label = 'Rearing', color = 'blue') +# Rearing and Migration
    geom_hline(aes(yintercept = 16), linetype = 4, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 16.5, label = 'Cold Habitat', color = 'blue') +# Core Cold Habitat
    geom_hline(aes(yintercept = 13), linetype = 3, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 13.5, label = 'Spawning', color = 'blue') + # Spawning
    geom_line(data = PltDat, aes(DATETIME, ma) , size = 1.25, color = 'blue')
  } else if (fileinfo[,'charid'] %in% c('DO', 'adjDO')) {
    pd <- pd + geom_hline(aes(yintercept = 11.0), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 11.2, label = 'Spawning', color = 'blue') +  # Spawning
      geom_hline(aes(yintercept = 8.0), linetype = 6, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 8.2, label = 'Cold-water', color = 'blue') + # Coldwater
      geom_hline(aes(yintercept = 6.5), linetype = 4, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 6.7, label = 'Cool-water', color = 'blue')  + # Coolwater
      geom_hline(aes(yintercept = 5.5), linetype = 3, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 5.7, label = 'Warm-water', color = 'blue') + # Warmwater
      geom_line(data = PltDat, aes(DATETIME, ma) , size = 1.25, color = 'blue')
      
  } else if (fileinfo[,'charid'] %in% c('PH', 'adjPH')) {
    pd <- pd + geom_hline(aes(yintercept = 8.5), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 8.7, label = 'Upper Criteria', color = 'blue') +  # Upper
      geom_hline(aes(yintercept = 6.5) , linetype = 6, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 6.7, label = 'Lower Criteria', color = 'blue') # Lower
  } else {
    pd <- pd
  }
  
  ggsave(filename = gsub('.Rdata','tsPLOT.png', basename(fname)), path = save_path, width = 11, height = 8.5, units = 'in', plot = pd)
  
  
  ##########################################
  
  #     #
  #    #     Continuous Data file
  #  #
  #   olunteer WQ Database Field Generation 
  
  ###########################################
  
  
    #         #####
   # #          #
  ####          #
  #  #ctivity   #able  
  
  print(paste0(fileinfo$LoggerID, '-', fileinfo$charid, ' Continuous Data Database Fields'))
  # Activity ID SubID_SiteID_Date_LoggerID_Characteristic---ONE ACTIVITY ID/day for each logger/site/char combo
  daydat$actid <- paste0(fileinfo$subid, '_', 
                         fileinfo$lasar,'_', 
                         as.character(format(daydat$date,'%Y%m%d')),'_',  
                         fileinfo$LoggerID,'_',
                         fileinfo$charid,'_', #########  REMOVE this component
                         'FMC')
  
  daydat$actgrp <- paste0(fileinfo$subid, '_', 
                          fileinfo$lasar,'_', 
                          fileinfo$date,'_',  
                          fileinfo$LoggerID)
  
  #  Sampling Organization
  daydat$ActivityOrg <- ORG
  
  # ActivityType
  daydat$ActivityType <- 'FMC'
  
  # Submission ID
  daydat$SubID <- as.numeric(fileinfo$subid) # Must have valid value in VolWQdb.t_Submissions
  
  # Site ID
  daydat$SiteID <- as.character(fileinfo$lasar)
  
  daydat$SiteID_Context <- 'ODEQ'
  
  # Site Description
  daydat$SiteDescription <- fileinfo$desc
  
  # Media
  daydat$Media <- 'Water'
  # Sample Collection Equipment
  daydat$SmplColEquip <- 'Probe/Sensor'
  # Sample 
  daydat$SmplColMthd <- 'ContinuousPrb'
  # Sample Depth
  daydat$SmplDepth <- fileinfo$depth_m
  # Sample Depth Unit
  daydat$SmplDepthUnit <- ifelse(!is.na(as.numeric(fileinfo$depth_m)),'m',NA)
  # Deployment Comment
  daydat$Org_Comment <- smi$COMMENTS[which(intersect(smi$Logger_ID == fileinfo$LoggerID, smi$LASAR_ID == fileinfo$lasar))]
  
  # Activity Table fields dataframe
  t_ConDataActivity <- daydat[,c("actid","ActivityType","SubID","SiteID","SiteID_Context",
                                 "SiteDescription","dDTmin","dDTmax","Media","ActivityOrg",
                                 "SmplColMthd","SmplColEquip","SmplDepth","SmplDepthUnit",
                                 "Org_Comment")]
  
  names(t_ConDataActivity)[c(1,7,8)] <- c("ActivityID","StartDateTime","EndDateTime")
  
  # Adjust variable types and add all fields from VolWQdb.t_Activity
  t_ConDataActivity$SmplDepth <- as.character(t_ConDataActivity$SmplDepth)
  t_ConDataActivity$SmplDepthUnit <- as.character(t_ConDataActivity$SmplDepthUnit)
  t_ConDataActivity$Org_Comment <- as.character(t_ConDataActivity$Org_Comment)
  t_ConDataActivity$SmplColEquipComment <- as.character(NA)
  t_ConDataActivity$DEQ_Comment <- as.character(NA)
  t_ConDataActivity$Samplers <- as.character(NA)
  t_ConDataActivity$SmplEquipID <- as.character(NA)
  
  # Pull activity group information
  t_actgrp2act <- daydat[,c('actid','actgrp')]
  t_actgrp2act$ActGrpType <- 'Field Set'
  names(t_actgrp2act) <- c('ActivityID','ActGrpID','ActGrpType')
  t_actgrp <- t_actgrp2act[,c('ActGrpID','ActGrpType')]
  t_actgrp <- unique(t_actgrp)
  t_actgrp2act <- t_actgrp2act[,c('ActGrpID','ActivityID')]
  
  #
  #
  ##oad Data into VolWQdb sqlDrop(ch, "TmpConActivity")
  print('Prepare activity data')
  # rbind all the activity data together to a single dataframe, 
  #then load into ODBC database to temporary file and 
  #run SQL query to append to Activity table
  
  
  if (i == 1) {
    t_ConDatAct <- t_ConDataActivity # if it is the first one create a df
    t_ActGrp <- t_actgrp
    t_ActGrp2Act <- t_actgrp2act
  } else if (i > 1 && i < length(validdatafiles)){
    t_ConDatAct <- rbind(t_ConDatAct,t_ConDataActivity) # add subsequent files to df above
    t_ActGrp <- rbind(t_ActGrp, t_actgrp)
    t_ActGrp2Act <- rbind(t_ActGrp2Act, t_actgrp2act)
  } else if (i == length(validdatafiles)) {
    t_ConDatAct <- rbind(t_ConDatAct,t_ConDataActivity)
    t_ActGrp <- rbind(t_ActGrp, t_actgrp)
    t_ActGrp <- unique(t_ActGrp)
    t_ActGrp2Act <- rbind(t_ActGrp2Act, t_actgrp2act)
    t_ActGrp2Act <- unique(t_ActGrp2Act)
    save(t_ConDatAct, file = paste0(save_path, fileinfo$subid, 'Activity','.RData')) # this should have all the activities listed
    
    # Activity Group
    save(t_ActGrp, file = paste0(save_path, fileinfo$subid, 'ActivityGrps','.RData')) # this should have all the activity groups  == number of deployments
    
    # Activity Group to Activity Junction Table
    save(t_ActGrp2Act, file = paste0(save_path, fileinfo$subid, 'ActGrp2Act','.RData')) # this should have all the activities listed with their activity groups
  }
  
  # When only one datafile
  if (length(validdatafiles) == 1) {
    save(t_ConDatAct, file = paste0(save_path, fileinfo$subid, 'Activity','.RData')) # this should have all the activities listed
    # Activity Group One Datafile
    t_ActGrp <- unique(t_ActGrp)
    save(t_ActGrp, file = paste0(save_path, fileinfo$subid, 'ActivityGrps','.RData')) # this should have all the activity groups  == number of deployments
    # Activity Group to Activity Junction Table One Datafile
    t_ActGrp2Act <- unique(t_ActGrp2Act)
    save(t_ActGrp2Act, file = paste0(save_path, fileinfo$subid, 'ActGrp2Act','.RData')) # this should have all the activities listed with their activity groups
  }
  
  
  print('prepare results stats for upload')
  ##########################################################
  
  #      #
  # #  # #
  #  #   #
  #      #elt various daily stats into long format for Volunteer Database
  
  ############################################################
  # remove unwanted fields
  dd2melt <-daydat[,-which(names(daydat) %in% c('hrNday','dyN','dydql','r4ma',
                                                'ActivityType', 'SubID','SiteID',
                                                'SiteID_Context', 'SiteDescription',
                                                'r4ma','ActivityOrg','Media','SmplColEquip',
                                                'SmplColMthd', 'Depth_m', 'SmplDepth','SmplDepthUnit','actgrp'))]
  
  # Units look up from the Continuous Characteristic Info file
  dd2melt$Unit <- ConCharInfo$Unit[which(ConCharInfo$charid == fileinfo$charid)]
  
  # Characteristic
  dd2melt$CharID <- fileinfo$charid
  
  # Melt the dataframe from flat to Tidy long format daily data
  t_ConResult <- melt(dd2melt, id= c('date', 'CharID','Unit', 'dDTmin', 'dDTmax','cmnt','dDQL',
                                     'anaStart','anaEnd','actid', 'LASAR','charid','LoggerID',
                                     'SiteDesc','Org_Comment'), na.rm =TRUE)
  
  # Correct Analytical Start and End time for non-moving average fields.
  for (j in 1:length(t_ConResult$variable)) {
    if (!t_ConResult$variable[j] == 'ma') {
      t_ConResult$anaStart[j] <- t_ConResult$dDTmin[j]
      t_ConResult$anaEnd[j] <- t_ConResult$dDTmax[j]
    }
  }
  
  
  #subset the columns to match only those needed in the result table
  t_ConResult <- t_ConResult[,which(names(t_ConResult) %in% c('date', 'CharID', 'Unit', 'dDTmin', 'dDTmax', 'cmnt', 'dDQL', 
                                                'anaStart', 'anaEnd', 'actid', 'variable', 'value'))]
  
  
  # Rename the columns of the dataframe to be consistent with VolWQdb
  names(t_ConResult) <- c('Date', 'CharID','Unit', 'dDTmin', 'dDTmax','DEQ_RsltComment','ORDEQ_DQL',
                          'AnalyticalStartTime','AnalyticalEndTime', 'ActivityID', 'StatisticalBasis','Result')
  
  # Result ID... ActivityID_calculatedfield after melt into t_ConResult
  for (l in 1:length(t_ConResult$Date)) {
    t_ConResult$ResultID[l] <- paste0(fileinfo$subid, '_', 
                                      fileinfo$lasar,'_', 
                                      strftime(t_ConResult$Date[l],format= '%Y%m%d'),'_',
                                      fileinfo$LoggerID,'_',
                                      fileinfo$charid, '_',
                                      t_ConResult$StatisticalBasis[l])
  }
  
  # Convert methods to valid values
  t_ConResult$StatisticalBasis <- gsub('dyMean', 'Daily Mean', x=t_ConResult$StatisticalBasis)
  t_ConResult$StatisticalBasis <- gsub('dyMin', 'Daily Minimum', x=t_ConResult$StatisticalBasis)
  t_ConResult$StatisticalBasis <- gsub('dyMax', 'Daily Maximum', x=t_ConResult$StatisticalBasis)
  t_ConResult$StatisticalBasis <- gsub('delta', 'Delta', x=t_ConResult$StatisticalBasis)
  
  if (fileinfo$charid %in% c('DO','adjDO','DOs')) {
    t_ConResult$StatisticalBasis <- gsub('ma', '30DMADMin', x=t_ConResult$StatisticalBasis)
  } else if (fileinfo$charid %in% c('TEMP','adjTEMP')) {
    t_ConResult$StatisticalBasis <- gsub('ma', '7DMADMax', x=t_ConResult$StatisticalBasis)
  }
  
  # Method needs to be determined from input info
  t_ConResult$Method <- as.character(ConCharInfo$Method[which(ConCharInfo$charid == fileinfo$charid)]) 
  
  
  # Specify Result Type...Actual or Calculated
  t_ConResult$RsltType <- ifelse(t_ConResult$StatisticalBasis %in% c('Daily Minimum','Daily Maximum'), 
                                 'Actual', 'Calculated')
  
  # Result Time Basis
  t_ConResult$RsltTimeBasis <- '1 Day' # Default
  # Correct moving average time basis
  for (j in 1:length(t_ConResult$StatisticalBasis)){
    if(t_ConResult$StatisticalBasis[j] == '30DMADMin') {
      t_ConResult$RsltTimeBasis[j] <- '30 Day'
    } else if (t_ConResult$StatisticalBasis[j] == '7DMADMax'){
      t_ConResult$RsltTimeBasis[j] <- '7 Day'
    }
  }
  
  # Result Status
  t_ConResult$RsltStatus <- 'Final'
  
  # Reorder fields to match those in t_Result
  t_ConResult <- t_ConResult[,c('ResultID','ActivityID','CharID','Result','Unit','Method','RsltType',
                                'AnalyticalStartTime','AnalyticalEndTime', 'ORDEQ_DQL', 'StatisticalBasis', 
                                'RsltTimeBasis', 'RsltStatus','DEQ_RsltComment')]
  
  #
  #
  ### oad data to database after rbinding all result dataframes for each file together
  print('load results stats to db')
  
  if (i == 1) {
    t_CnRslt <- t_ConResult # if it is the first one create a df
  } else if (i > 1 && i < length(validdatafiles)){
    t_CnRslt <- rbind(t_CnRslt,t_ConResult) # add subsequent files to df above
  } else if (i == length(validdatafiles)) {
    t_CnRslt <- rbind(t_CnRslt,t_ConResult)
    save(t_CnRslt, file = paste0(save_path, fileinfo$subid, 'Results','.RData')) # this should have all the activities listed
    
  }
  
  # When only one datafile
  if (length(validdatafiles) == 1) {
    save(t_CnRslt, file = paste0(save_path, fileinfo$subid, 'Results','.RData')) # this should have all the activities from the one file
  }
  
  #########################################################################################
  
    #       #####
   # #      #
  ####      ###
  #  #      # 
  #  #udit  # ile Processing
  
  ##########################################################################################
  
  print(paste0(fileinfo$LoggerID, '-', fileinfo$charid, ' Audit File Info for Database'))
  # Load the audit data file based on fileinfo LASAR ID, characteristic and logger id
  auditfilepath <- allaudit.fileinfo$filepath[Reduce(intersect, list(which(allaudit.fileinfo$lasar == fileinfo$lasar), 
                                                                     which(allaudit.fileinfo$charid == fileinfo$charid), 
                                                                     which(allaudit.fileinfo$LoggerID == fileinfo$LoggerID)))]
  load(auditfilepath)
  
    #
   # #
  ####
  #  #ctivity Table fields for AUDIT FILES
  
  # Parse out the audit file name information
  auditfileinfo <- read.table(text = basename(auditfilepath) , sep = '_', as.is=TRUE, colClasses = "character")
  names(auditfileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'audit', 'InfoExtension' )
  
  # ADD required fields for the activity table
  dr_info$ActivityID <- paste0(auditfileinfo$subid,'-' ,
                               auditfileinfo$lasar,'-' ,
                               auditfileinfo$date,'-',
                               auditfileinfo$LoggerID,'-',
                               auditfileinfo$charid,'-',
                               strftime(dr_info$AUDIT_DATETIME, format = '%Y%m%d%H%M'),'-' ,
                               'FQMDL')
  
  # Activity Group 
  dr_info$actgrp <- paste0(auditfileinfo$subid, '_', 
                           auditfileinfo$lasar,'_', 
                           auditfileinfo$date,'_',  
                           auditfileinfo$LoggerID)
  
  
  dr_info$ActivityType <- 'FQMDL'
  dr_info$SubID <- as.character(auditfileinfo$subid)
  dr_info$SiteID <- as.character(auditfileinfo$lasar)
  dr_info$SiteID_Context <- 'ODEQ'
  dr_info$SiteDescription <- auditfileinfo$desc
  dr_info$StartDateTime <- dr_info$AUDIT_DATETIME # remove min(dr_info$AUDIT_DATETIME) which may have been an artifact of Cn data
  dr_info$Media <- 'Water'
  dr_info$ActivityOrg <- ORG
  dr_info$SmplColEquip <- 'Probe/Sensor'
  
  # Not including, sample collection equipment id, sample collection equipment comment, sample depth,
  # DEQ_Comment or Samplers.
  
  #  CREATE dataframe for upload to Volunteer WQ database
  t_CnAuditAct <- dr_info[,c('ActivityID', 'ActivityType', 'SubID', 'SiteID', 'SiteID_Context',
                             'SiteDescription', 'StartDateTime', 'Media', 'ActivityOrg')]
  
  # Pull activity group information for audit data
  t_audactgrp2act <- dr_info[,c('ActivityID', 'actgrp')]
  t_audactgrp2act$ActGrpType <- 'Field Set'
  names(t_audactgrp2act) <- c('ActivityID','ActGrpID','ActGrpType')
  t_audactgrp <- t_audactgrp2act[,c('ActGrpID','ActGrpType')]
  t_audactgrp <- unique(t_audactgrp)
  t_audactgrp2act <- t_audactgrp2act[,c('ActGrpID','ActivityID')]
  
  
  #
  #
  ## oad Data into database after rbinding all the audit activity dataframes together
  
  if (i == 1) {
    t_CnAudAct <- t_CnAuditAct # if it is the first one create a df
    t_AudActGrp <- t_audactgrp
    t_AudActGrp2Act <- t_audactgrp2act
  } else if (i > 1 && i < length(validdatafiles)){
    t_CnAudAct <- rbind(t_CnAudAct,t_CnAuditAct) # add subsequent files to df above
    t_AudActGrp <- rbind(t_AudActGrp, t_audactgrp)
    t_AudActGrp2Act <- rbind(t_AudActGrp2Act, t_audactgrp2act)
  } else if (i == length(validdatafiles)){
    t_CnAudAct <- rbind(t_CnAudAct,t_CnAuditAct)
    t_AudActGrp <- rbind(t_AudActGrp, t_audactgrp)
    t_AudActGrp <- unique(t_AudActGrp)
    t_AudActGrp2Act <- rbind(t_AudActGrp2Act, t_audactgrp2act)
    t_AudActGrp2Act <- unique(t_AudActGrp2Act)
    save(t_CnAudAct, file = paste0(save_path, fileinfo$subid, 'AuditActivity','.RData')) # this should have all the activities listed
    
    # Audit Activity Group
    save(t_AudActGrp, file = paste0(save_path, fileinfo$subid, 'AuditActivityGrps','.RData')) # this should have all the activity groups  == number of deployments
    
    # Audit Activity Group to Activity Junction Table
    save(t_AudActGrp2Act, file = paste0(save_path, fileinfo$subid, 'AuditActGrp2Act','.RData')) # this should have all the activities listed with their activity groups
    
  }
  
  # When only one audit file
  if (length(validdatafiles) == 1){
    save(t_CnAudAct, file = paste0(save_path, fileinfo$subid, 'AuditActivity','.RData')) # this should have all the activities listed
    # Audit Activity Group one audit file
    save(t_AudActGrp, file = paste0(save_path, fileinfo$subid, 'AuditActivityGrps','.RData')) # this should have all the activity groups  == number of deployments
    # Audit Activity Group to Activity Junction Table one audit file
    save(t_AudActGrp2Act, file = paste0(save_path, fileinfo$subid, 'AuditActGrp2Act','.RData')) # this should have all the activities listed with their activity groups
  }
  
#  }  deleted this not sure it is not needed
  
  ##############################################
  
  ###
  #  #
  # #
  ##
  # # 
  #  #ESULT tabe fields for AUDIT FILES
  
  ################################################
  
  dr_info$ResultID <- paste0(auditfileinfo[1,'subid'],'-' ,
                             auditfileinfo[1,'lasar'],'-' ,
                             auditfileinfo[1,'LoggerID'],'-',
                             strftime(dr_info$AUDIT_DATETIME, format = '%Y%m%d%H%M'),'-' ,
                             auditfileinfo[1,'charid'],'-' ,
                             'FQMDL')
  
  # Rename Columns to database nomenclature
  colnames(dr_info)[c(2,3,5,8)] <- c('CharID','Result','Org_RsltComment','ORDEQ_DQL')
  
  dr_info$Unit <- ConCharInfo$Unit[which(ConCharInfo$charid == auditfileinfo$charid)]
  dr_info$RsltType <- 'Actual'
  dr_info$RsltStatus <- 'Final'
  
  
  #####
  ##     NOTE- Method assigned same as continuous logger is an assumption.  May need to add this submission info
  dr_info$Method <- as.character(ConCharInfo$Method[which(ConCharInfo$charid == auditfileinfo$charid)])
  
  
  
  t_CnAuditResult <- dr_info[,c("ResultID","ActivityID","CharID","Result","Unit","Method","RsltType",
                                "ORDEQ_DQL","RsltStatus","Org_RsltComment")]
  
  #
  #
  ##oad Data into VolWQdb
  # Upload the audit activity information to a tempoary table in the database
  
  if (i == 1) {
    t_CnAudRslt <- t_CnAuditResult # if it is the first one create a df
  } else if (i > 1 && i < length(validdatafiles)){
    t_CnAudRslt <- rbind(t_CnAudRslt,t_CnAuditResult) # add subsequent files to df above
  } else if (i == length(validdatafiles)) {
    t_CnAudRslt <- rbind(t_CnAudRslt,t_CnAuditResult)
    t_CnAudRslt <- t_CnAudRslt[!is.na(t_CnAudRslt$Result),] # Removed blank result
    save(t_CnAudRslt, file = paste0(save_path, fileinfo$subid, 'AuditResults','.RData')) # this should have all the activities listed
    
  }
  
  # If only one valid datafile
  if (length(validdatafiles) == 1) {
    t_CnAudRslt <- t_CnAudRslt[!is.na(t_CnAudRslt$Result),] # Removed blank result
    save(t_CnAudRslt, file = paste0(save_path, fileinfo$subid, 'AuditResults','.RData')) # this should have all the activities listed
  }
  
  print(paste0(fileinfo$LoggerID, '-', fileinfo$charid, ' Done '))
} # end of big for loop


################################################################################################################
################################################################################################################

####         ####  #
#            #     #
##           ##    #
#            #     #
#### nd of   # or  #### oop

################################################################################################################
################################################################################################################








#  Plot invalid data files
for (k in seq(invaliddatafiles)) {

  fname <- invaliddatafiles[k]
  load(fname)
  
  print(paste0('Plot ', k, ' of ', length(invaliddatafiles), ' is ' , fname))
  
  
  # Get continuous data file information from name
  fileinfo <- read.table(text = basename(fname) , sep = '_', as.is=TRUE, colClasses = "character")
  names(fileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'depth_m','extension' )
  
  # Build data frame with plotting data
  PltDat <- tmp_data[,c("DATETIME", "date", "charid", "r", "field_audit_grade", "cvDQL", "rDQL", "anomaly") ]
 
  # get audit data
  auditfilepath <- allaudit.fileinfo$filepath[Reduce(intersect, list(which(allaudit.fileinfo$lasar == fileinfo$lasar), 
                                                                     which(allaudit.fileinfo$charid == fileinfo$charid), 
                                                                     which(allaudit.fileinfo$LoggerID == fileinfo$LoggerID)))]
  load(auditfilepath)
  # Get error bar limits for audits...later. B/c of the low level criteria this will be difficult.
  
  # Basic plot data  
  pd <- ggplot(data = PltDat, aes(DATETIME, r)) + geom_point(aes(color = rDQL)) + xlab("Date") + theme_bw() +
    ylim(min(PltDat$r[which(!is.na(PltDat$r))]),max(PltDat$r[which(!is.na(PltDat$r))])) +
    ylab(paste0(fileinfo[,'charid'],' (',ConCharInfo$Unit[which(ConCharInfo$charid == fileinfo[,'charid'])],')')) +
    labs(title = paste0('Continuous ',fileinfo[,'charid'], ' Data at LASAR Station ', fileinfo[,'lasar'], ' Logger ', fileinfo[,'LoggerID']) ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data = dr_info, aes(x = AUDIT_DATETIME, y = AUDIT_RESULT), color = 'black', size = 3, alpha = 0.5) 
  
  
  if (fileinfo[,'charid'] %in% c('Q', 'TURB', 'adjTURB', 'CHA', 'BGA')){
    # Log Scale
    pd <- pd + scale_y_log10() + annotation_logticks( base = 10, sides = 'l')
  } else if (fileinfo[,'charid'] %in% c('TEMP', 'adjTEMP')) {
    pd <- pd + geom_hline(aes(yintercept = 20), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 20.5, label = 'Migration', color = 'blue') + # Migration corridor
      geom_hline(aes(yintercept = 18), linetype = 6, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 18.5, label = 'Rearing', color = 'blue') +# Rearing and Migration
      geom_hline(aes(yintercept = 16), linetype = 4, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 16.5, label = 'Cold Habitat', color = 'blue') +# Core Cold Habitat
      geom_hline(aes(yintercept = 13), linetype = 3, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 13.5, label = 'Spawning', color = 'blue') # Spawning
  } else if (fileinfo[,'charid'] %in% c('DO', 'adjDO')) {
    pd <- pd + geom_hline(aes(yintercept = 11.0), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 11.2, label = 'Spawning', color = 'blue') +  # Spawning
      geom_hline(aes(yintercept = 8.0), linetype = 6, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 8.2, label = 'Cold-water', color = 'blue') + # Coldwater
      geom_hline(aes(yintercept = 6.5), linetype = 4, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 6.7, label = 'Cool-water', color = 'blue')  + # Coolwater
      geom_hline(aes(yintercept = 5.5), linetype = 3, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 5.7, label = 'Warm-water', color = 'blue') # Warmwater
      
    
  } else if (fileinfo[,'charid'] %in% c('PH', 'adjPH')) {
    pd <- pd + geom_hline(aes(yintercept = 8.5), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 8.7, label = 'Upper Criteria', color = 'blue') +  # Upper
      geom_hline(aes(yintercept = 6.5) , linetype = 6, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 6.7, label = 'Lower Criteria', color = 'blue') # Lower
  }
  
  ggsave(filename = paste0('NoValidData_', gsub('.Rdata','tsPLOT.png', basename(fname))), path = save_path, width = 11, height = 8.5, units = 'in', plot = pd)
}



# Save a csv of the daily summary stats
write.csv(t_CnRslt, paste0(save_path,'/','TempCnRslt.csv'))


# Save a copy of additional input data with the submitted data
load('//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/ConQC.RData')
save(ConQC, file = paste0(save_path,'ConQC.RData'))
save(ConQC, file = paste0(save_path,'ConCharInfo.RData'))

####################################################
##################################################

#####
  #
  #
  #
  #RY STOPPING HERE AND RUN SCRIPT BELOW SEPARATELY as VOLWQdb Script

