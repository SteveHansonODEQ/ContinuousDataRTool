###   ####   ###   ##   #####
#  #  #  #  #     #  #    #
#  #  #  #  ###   ####    #
#  #  #  #     #  #  #    #
###   ####  ###   #  #    #  EDITS  
# Data quality level for logged dissolved oxygen satuation data is assigned from the DQL for DO data

####
#
# ##
#  #
### RADE THE DO DATA FIRST!!

################################################################
################################################################

#clean out exisiting environment
#helps to avoid overwriting
rm(list = ls())

###   ###
#  # #   #
#  # #   #
###   ###  Data Selectoin

# Retrieve the graded DO data
# filename for the graded DO data
dofile <- file.choose()


###   ####   ###   ##   #####
#  #  #  #  #     #  #    #
#  #  #  #  ###   ####    #
#  #  #  #     #  #  #    #
###   ####  ###   #  #    #  data selection

#Filename for saving this script should be equivalent to fname_edits.R
# Set the filename - This should be what you see in the select station box in the shiny app
fname <- file.choose()

#This won't need to change
path <- "//deqlab1/WQM/TMDL/RDataManagement/ContinuousDataRTool/Check_shinyapp/data/"

# This is the directory of the original Excel files and is where this script is saved at the end
#EditPath <- '//deqlab1/wqm/TMDL/aWestern Region/MidCoast/Salmon River DO TMDL/2017SalmonData/201707CnData/Rfiles/'




# Load DO data
load(dofile)
#load(paste0(path, dofile))
# Rename
dodata <- tmp_data

# Load the DOsat data
load(fname)
#load(paste0(path, fname))

# Assign DQL's 
tmp_data$rDQL <- dodata$rDQL

# Assign Comments
tmp_data$cmnt <- dodata$cmnt

# If needed use standard edits scripts to adjust DO sat if necessary before saving


#When you have made all the edits run this line to save it back to the shiny app data folder
rm(dodata)
save(tmp_data, file = fname)



#Prints out filename for saving
writeClipboard(noquote(paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(fname)),"EDITS.R")))
print(noquote(paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(fname)),"EDITS.R")))






####
#  #
#  #
####THER OPTIONS BELOW

#Assign DO rDQL to DOsat rDQL when DOsat data is truncated shorter than DO
# dataframes are not the same length
length(dodata$r) == length(tmp_data$r)
tmp_data$DATETIME[1] == dodata$DATETIME[1]
tmp_data$DATETIME[length(tmp_data$DATETIME)] ==  dodata$DATETIME[length(tmp_data$DATETIME)]

# Assign DQL's 
tmp_data$rDQL[1:length(tmp_data$rDQL)] <- dodata$rDQL[1:length(tmp_data$rDQL)]

# Assign Comments
tmp_data$cmnt[1:length(tmp_data$rDQL)] <- dodata$cmnt[1:length(tmp_data$rDQL)]



