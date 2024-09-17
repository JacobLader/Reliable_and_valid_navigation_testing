#### drag & drop map processing script ####

## extract coordinates of each image
## save in long format
## to be used in bidimensional regression macro

# save this script in the task folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyr)
library(plyr)
library(dplyr)
library(stringr)

# Step 1: read in data (A1, A2, B1, or B2) ####
filelist <- list.files(path="A2_input", pattern="*.csv", full.names=T)
data <- lapply(filelist, function(x)read.table(x, header=TRUE, sep=","))

# Step 2: organize data & extract XY coordinates ####
# save just participant ID, date, xy coordinates, and image ID
data_xy <- lapply(data, function(x)x[c("participant", "date", "image1","image2",
                                       "image3","image4","image5","image6","image7",
                                       "image8","image9","image10","image11","image12",
                                       "image13","image14","image15","image16")])
data_xy <- lapply(data_xy, function(x)filter(x, image1 != ""))
#check if anything changes after running this line^^^

data_xy_df <- do.call(rbind, data_xy)

# convert from wide to long format
# this might throw a warning but it works correctly:
data_long <- gather(data_xy_df, image, xy, image1:image16, factor_key=TRUE)
data_long$xy <- gsub("\\[|\\]", "", data_long$xy)
data_long$xy <- trimws(data_long$xy)
#now split the x and y:
data_long[c('x','y')] <- str_split_fixed(data_long$xy, ' ', 2)
data_long$x <- trimws(data_long$x)
data_long$y <- trimws(data_long$y)
data_long <- data_long[,-4]

# sort by participant so the csv can be copied into the macro
data_long_sorted <- data_long[order(data_long$participant, data_long$date),]

# save data to csv (A1, A2, B1, or B2)
write.csv(data_long_sorted, "A2_Processed_TTMB_6-6-24.csv")


