
#This script is intended to be copied and saved for each logger
#Filename for saving this script should be equivalent to fname_edits.R
#Choose file to work on

filepath <- file.choose()

load(filepath)




#When making adjustments for more than one range copy the code between 
# HERE and TO HERE and past it below in order to maintain a record of the
# changes you've made to the DQL. It would also be handy if you added a 
# little explanantion of why you are editing the DQL for that range

# 1
#HERE
#To adjust DQL for a specific date range
#First set the ranges you want to modify and the new DQL to assign
start_date_time_char <- "2011-06-20 00:00:00"
end_date_time_char <- "2011-09-26 14:00:00"
new_DQL <- '-'
# provide a brief justification if necessary.  Comment must be in ''  
cmnt <- 'No changes'

#Run these to actually update the file
start <- as.POSIXct(strptime(start_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
end <- as.POSIXct(strptime(end_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
#tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'rDQL'] <- new_DQL
#tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'cmnt'] <- cmnt
#TO HERE


#When you have made all the edits run this line to save it back to the shiny app data folder
save(tmp_data, file = filepath)


#Prints out filename for saving
print(noquote(paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filepath)),"EDITS.R")))


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





# Save this script with the original data ..

