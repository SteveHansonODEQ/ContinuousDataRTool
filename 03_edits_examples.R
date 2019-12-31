
#This script is intended to be copied and saved for each logger
#Filename for saving this script should be equivalent to fname_edits.R
#Choose file to work on

# May need to run this on the first file, takes two rounds to open filepath to the correct folder
setwd('//deqlab1/WQM/DataManagement/ContinuousDataRTool/Check_shinyapp/data')



#clean out exisiting environment
#helps to avoid overwriting
rm(list = ls())

filepath <- file.choose()


print(filepath)
load(filepath)

###################################
###################################






#############################

 #        #  #   #      ###      ##          ## #                       
# # # # ###     ###      #  ##   #  ###     #   ###  ## ##  ### ###  ## 
### # # # #  #   #       #  # # ### # #     #   # # # # # # # # ##   #  
# # ### ###  ##  ##      #  # #  #  ###     #   # # ### # #  ## ### ##  
# #                     ###     ##           ##             ###         

####################################################

# choose auidt file
auditpath <- file.choose()
print(auditpath)

load(auditpath)

# provide comment with rationale for changes
dr_info$AUDIT_DATETIME[2] <- as.POSIXct("2018-06-20 13:50:00", tz = "America/Los_Angeles")
dr_info$IND[2] <- 13132
dr_info$LOGGED_RESULT [2] <- tmp_data$r[dr_info$IND[2]]
dr_info$OBS_DATETIME [2] <- tmp_data$DATETIME [dr_info$IND[2]]
dr_info$AUDIT_GRADE[2] <- "A"
# Remove bad VALUES from file
dr_info$COMMENTS[which(dr_info$AUDIT_RESULT > 20)] <- 'DO sat value reported in error'
dr_info$AUDIT_RESULT[which(dr_info$AUDIT_RESULT > 20)] <- NA

# Remove reported audits from prior deployment, only include the redeploy audits 
dr_info <- dr_info[-which(dr_info$AUDIT_DATETIME < as.POSIXct(strptime('2019-08-19 17:35:00', format = "%Y-%m-%d %H:%M:%S"))),]


save(dr_info, file = auditpath)

######################################
######################################


##       #          ###  #   #           ## #                           
# #  ## ###  ##     #        #  ###     #   ###  ## ##  ### ###  ##     
# # # #  #  # #     ##   #   #  ##      #   # # # # # # # # ##   #      
# # ###  ## ###     #    ##  ## ###     #   # # ### # #  ## ### ##      
##                  #                    ##             ###  

#When making adjustments for more than one range copy the code between 
# HERE and TO HERE and past it below in order to maintain a record of the
# changes you've made to the DQL. It would also be handy if you added a 
# little explanantion of why you are editing the DQL for that range

#########################################################################
# 1
#HERE
#To adjust DQL for a specific date range
#First set the ranges you want to modify and the new DQL to assign
start_date_time_char <- "2018-10-15 12:30:00"
end_date_time_char <- "2018-10-18 13:15:00"
new_DQL <- 'A'
# provide a brief justification if necessary.  Comment must be in ''  
cmnt <- 'Corrected field audits verfied with post run check agains NIST meter'

#Run these to actually update the file
start <- as.POSIXct(strptime(start_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
end <- as.POSIXct(strptime(end_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'rDQL'] <- new_DQL
#tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'cmnt'] <- cmnt
#TO HERE


###############################################################################
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
###############################################################################

#When you have made all the edits run this line to save it back to the shiny app data folder
save(tmp_data, file = filepath)


#Prints out filename for saving
writeClipboard(noquote(paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filepath)),"EDITS.R")))
print(noquote(paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filepath)),"EDITS.R")))



##### #   # ##
#     ##  # # #
##    # # # #  #
#     #  ## #  #
##### #   # ##

#######################################################################################
#/\/\/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
#######################################################################################

####
#  #
#  #
#### THER OPTIONS


#####
###
##
#Change DQL and comments based on result value
# 1
#HERE
#To adjust DQL for a specific date range
#First set the ranges you want to modify and the new DQL to assign
cutoff <- 5
new_DQL <- 'C'
# provide a brief justification if necessary.  Comment must be in ''  
cmnt <- 'Probable measurement error.'

#Run these to actually update the file
tmp_data[tmp_data$r > cutoff, 'rDQL'] <- new_DQL
tmp_data[tmp_data$r > cutoff, 'cmnt'] <- cmnt
#TO HERE

#####
###
##
#Change DQL and comments removing extreme values within a time window
# Remove low values over a period of time below lowest 'valid' value determined from chart
#To adjust DQL for a specific date range
#First set the ranges you want to modify and the new DQL to assign
start_date_time_char <- "2016-07-09 00:00:00"
end_date_time_char <- "2016-07-26 18:00:00"
cutoff <- 7
new_DQL <- 'C'
# provide a brief justification if necessary.  Comment must be in ''  
cmnt <- 'Probable Measurement Error'

#Run these to actually update the file
start <- as.POSIXct(strptime(start_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
end <- as.POSIXct(strptime(end_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end & tmp_data$r < cutoff, 'rDQL'] <- new_DQL
tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end & tmp_data$r < cutoff, 'cmnt'] <- cmnt
#TO HERE


            #                                   
### ### ### ### ###     ### ### ### ### ###  ## 
# # #   # # # # ##      ##  #   #   # # #    #  
### #   ### ### ###     ### #   #   ### #   ##  
#          

# create a vector of text date times of individual points that need to be edited
pts2edit <- c('2019-07-31 20:30:00', '2019-07-31 21:30:00', '2019-07-31 22:45:00',
              '2019-08-04 22:45:00', '2019-08-08 00:00:00', '2019-08-10 04:30:00',
              '2019-08-10 05:30:00', '2019-08-12 18:30:00', '2019-08-16 18:30:00',
              '2019-08-20 22:00:00', '2019-08-20 22:15:00', '2019-08-21 03:30:00',
              '2019-08-22 17:00:00', '2019-08-22 21:15:00', '2019-08-24 02:45:00',
              '2019-08-24 19:00:00', '2019-08-25 11:30:00', '2019-08-28 00:00:00',
              '2019-08-28 01:30:00', '2019-08-28 13:00:00', '2019-08-28 14:00:00',
              '2019-08-29 12:15:00', '2019-09-01 15:30:00', '2019-09-03 16:45:00',
              '2019-09-03 23:00:00', '2019-09-05 02:45:00', '2019-09-06 18:30:00',
              '2019-09-06 17:00:00', '2019-09-06 19:00:00', '2019-09-06 20:15:00',
              '2019-09-08 02:15:00', '2019-09-08 18:30:00', '2019-09-16 02:30:00',
              '2019-09-16 02:45:00', '2019-09-20 09:15:00', '2019-10-01 11:15:00',
              '2019-10-01 11:00:00', '2019-09-04 22:00:00')
new_DQL <- 'C'
# provide a brief justification if necessary.  Comment must be in ''  
cmnt <- 'Probable Measurement Error'

# Select each row of data with matching date time and edit rDQL and cmnt
pts <- as.POSIXct(strptime(pts2edit, format = "%Y-%m-%d %H:%M:%S"))
tmp_data[tmp_data$DATETIME %in% pts, 'rDQL'] <- new_DQL
tmp_data[tmp_data$DATETIME %in% pts, 'cmnt'] <- cmnt




# Save this script with the original data ..

