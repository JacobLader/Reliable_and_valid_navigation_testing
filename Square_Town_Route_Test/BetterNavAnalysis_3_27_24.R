### Analyze path efficiency in fMRI study ###

#### setup ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(fs)

#### Create a directory for modified files if it does not already exist ####
dir.create("nav_logs/merged_all", showWarnings = FALSE)
dir.create("nav_logs/merged_2_1", showWarnings = FALSE)
dir.create("nav_logs/merged_2_2", showWarnings = FALSE)
dir.create("nav_logs/modified", showWarnings = FALSE)
dir.create("nav_logs/2_2withdate", showWarnings = FALSE)
dir.create("nav_logs/2_1withdate", showWarnings = FALSE)

#### Create functions ####
# Function to add a new column with the date portion
addDateColumn <- function(file_path) {
  # Read the CSV file
  nav_data <- read.csv(file_path)
  
  # Extract the date from the filename
  date <- sub(".*___(\\d{4}-\\d{2}-\\d{2}).*", "\\1", basename(file_path))
  
  # Add the date as the second column
  nav_data <- cbind(nav_data[1], Date = date, nav_data[-1])
  
  return(nav_data)
}

#### create directories for 1 and 2 but with dates added ####

# create list of all trial 2 nav log csvs
nav_data_list_2_2 <- list.files(path="nav_logs/2_2", pattern="*csv", full.names=T)

#add the date and modify the block index so that it is distinct
for (file_path in nav_data_list_2_2) {
  modified_data_2_2 <- addDateColumn(file_path)
  #note that block index and trial index are different so that this is allowed
  modified_data_2_2$BlockIndex <- 7
  
  # Save the modified data to a new file
  new_file_path_2_2 <- file.path("nav_logs/2_2withdate", paste0(basename(file_path), "_Date.csv"))
  write.csv(modified_data_2_2, new_file_path_2_2, row.names = FALSE)
}

nav_data_list_2_1 <- list.files(path="nav_logs/2_1", pattern="*csv", full.names=T)

#add the date and modify the block index so that it is distinct
for (file_path in nav_data_list_2_1) {
  modified_data_2_1 <- addDateColumn(file_path)
  #note that block index and trial index are different so that this is allowed
  modified_data_2_1$BlockIndex <- 8
  
  # Save the modified data to a new file
  new_file_path_2_1 <- file.path("nav_logs/2_1withdate", paste0(basename(file_path), "_Date.csv"))
  write.csv(modified_data_2_1, new_file_path_2_1, row.names = FALSE)
}

#### creating a single day 2 file (2_1 + 2_2 with distinct blocks and dates added) ####
# Set the directory paths for the two folders
folder_path_2_2 <- "./nav_logs/2_2withdate"
folder_path_2_1 <- "./nav_logs/2_1withdate"
modified_folder <- "./nav_logs/modified"

# Get a list of CSV files in each folder
# hint: this is also a good check that everything was actually transfered over
files_2_2 <- list.files(folder_path_2_2, pattern = ".csv", full.names = TRUE)
files_2_1 <- list.files(folder_path_2_1, pattern = ".csv", full.names = TRUE)

# Iterate through the CSV files in the first folder :,(
for (file_2_1 in files_2_1) {
  # Read the first CSV file
  data_2_1 <- read.csv(file_2_1)
  # Get the values from the second row
  value_to_match <- data_2_1[2, 1:2]
  
  # Check for a match in the second folder
  match_found <- FALSE
  for (file_2_2 in files_2_2) {
    # Read the second CSV file
    data_2_2 <- read.csv(file_2_2)
    if (identical(value_to_match, data_2_2[2, 1:2])) {
      # Match found, append the data and save to the "modified" folder
      merged_data <- rbind(data_2_1, data_2_2)
      write.csv(merged_data, file.path(modified_folder, basename(file_2_1)), row.names = FALSE)
      match_found <- TRUE
      break
    }
  }
  
  # If no match was found, print "no match found" along with the values
  if (!match_found) {
    print("No match found")
  }
}

#### merge the wayfinding key with the files in each of the above directories ####
merged_folder_all <- "./nav_logs/merged_all"
merged_folder_2_1 <- "./nav_logs/merged_2_1"
merged_folder_2_2 <- "./nav_logs/merged_2_2"

# Read the CSV file you want to append to others
file_to_append <- read.csv("wayfindingkey____2023-09-21-17.53.37.csv")  # Replace with the actual CSV file name
file_to_append_with_just_a_single_set_of_trials <- read.csv("wayfindingkey_singleset.csv")

file_to_append$Date <- NA
file_to_append_with_just_a_single_set_of_trials$Date <- NA

file_to_append <- file_to_append[,c(1,29,2:28)]
file_to_append_with_just_a_single_set_of_trials <- file_to_append_with_just_a_single_set_of_trials[,c(1,29,2:28)]

# List all CSV files in the input folders
csv_files_all <- list.files(modified_folder, pattern = ".csv", full.names = TRUE)
csv_files_2_1 <- list.files(folder_path_2_1, pattern = ".csv", full.names = TRUE)
csv_files_2_2 <- list.files(folder_path_2_2, pattern = ".csv", full.names = TRUE)

# Loop through each CSV file and append the data
for (csv_file in csv_files_all) {
  data_to_append <- read.csv(csv_file)
  
  # Update the values in the first column and first row of the second column
  file_to_append[, 1] <- data_to_append[1, 1]
  file_to_append[, 2] <- data_to_append[1, 2]
  
  # Append the data to the file_to_append
  merged_data <- bind_rows(file_to_append, data_to_append)
  
  # Create a new file name for the merged CSV in the output folder
  new_file_name <- file.path(merged_folder_all, basename(csv_file))
  
  # Save the merged data to the output folder
  write.csv(merged_data, new_file_name, row.names = FALSE)
}

# Loop through each CSV file and append the data
for (csv_file in csv_files_2_1) {
  data_to_append <- read.csv(csv_file)
  
  # Update the values in the first column and first row of the second column
  file_to_append_with_just_a_single_set_of_trials[, 1] <- data_to_append[1, 1]
  file_to_append_with_just_a_single_set_of_trials[, 2] <- data_to_append[1, 2]
  
  # Append the data to the file_to_append
  merged_data <- bind_rows(file_to_append_with_just_a_single_set_of_trials, data_to_append)
  
  # Create a new file name for the merged CSV in the output folder
  new_file_name <- file.path(merged_folder_2_1, basename(csv_file))
  
  # Save the merged data to the output folder
  write.csv(merged_data, new_file_name, row.names = FALSE)
}

# Loop through each CSV file and append the data
for (csv_file in csv_files_2_2) {
  data_to_append <- read.csv(csv_file)
  
  # Update the values in the first column and first row of the second column
  file_to_append_with_just_a_single_set_of_trials[, 1] <- data_to_append[1, 1]
  file_to_append_with_just_a_single_set_of_trials[, 2] <- data_to_append[1, 2]
  
  # Append the data to the file_to_append
  merged_data <- bind_rows(file_to_append_with_just_a_single_set_of_trials, data_to_append)
  
  # Create a new file name for the merged CSV in the output folder
  new_file_name <- file.path(merged_folder_2_2, basename(csv_file))
  
  # Save the merged data to the output folder
  write.csv(merged_data, new_file_name, row.names = FALSE)
}

#### Get the subject dataframe for route test use ####

#this is too heavy to rewrite the whole thing 3 times, 
#so just change out the directories if you want
#to test other trails or blocks

#create a list of the data with dates added:
nav_data_list <- list.files(path="nav_logs/merged_2_1", pattern="*csv", full.names=T)

#initialize empty subject-level lists for test trials only (this is as opposed 
#to reading in data frames with both training and test as was done in the past)
sub_dist_list <- list()

for (sub in 1:length(nav_data_list)){
  
  #read in the csv from the folder and print the name of the csv you are on:
  nav_data <- read.table(nav_data_list[sub], header=T, sep=",")
  print(nav_data_list[sub])

  # Extract subject ID in first row for current csv:
  subject_number <- as.numeric(nav_data$StartField1[1])
  # Extract date in first row for current csv:
  date_number <- nav_data$Date[1]
  # Extract run number in first row for current csv:
  run_number <- as.numeric(nav_data$StartField3[1])
  
  # Assign the subject and run numbers to the respective columns
  nav_data$sub <- subject_number
  nav_data$Date <- date_number
  nav_data$run_num <- run_number
  
  # save x & z locations as numeric
  nav_data$PositionX <- as.numeric(as.character(nav_data$PositionX))
  nav_data$PositionZ <- as.numeric(as.character(nav_data$PositionZ))
  
  # remove loading screens & clean up
  nav_data <- filter(nav_data, TrialIndex != 1 & TrialIndex != 2 & LoadingTime == 0 & PositionX < 500)
  nav_data$TrialIndex <- as.factor(nav_data$TrialIndex)
  
  # create location index by combining the x & z coordinates
  nav_data$Loc_Index <- paste0(nav_data$PositionX, "_", nav_data$PositionZ)
  
  # get first location on each trial 
  nav_data <- nav_data %>%
    group_by(TrialIndex, BlockIndex) %>%
    dplyr::mutate(first_loc = dplyr::first(Loc_Index))
  
  # now, calculate the path distance per trial
  # first, split by block
  block_list <- split(nav_data, f = nav_data$BlockIndex, drop = T)
  
  # initialize block counter & summary list
  block_count <- 1
  dist_list <- list()
  
  # loop over blocks
  for (block in block_list){
    block$distance <- NA
    
    # calculate distance from step to step
    for (i in 1:(nrow(block)-1)){
      coord_cur <- c(block[i,]$PositionX, block[i,]$PositionZ)
      coord_next <- c(block[i+1,]$PositionX, block[i+1,]$PositionZ)
      
      if (block$TrialIndex[i] == block$TrialIndex[i+1]){
        euc.dist <- sqrt(sum((coord_cur - coord_next) ^ 2))
      } else {
        euc.dist <- NA
      }
      block$distance[i] <- euc.dist
    }
    
    # overwrite the previous dataframe with the new one w/ euclidean dist added
    block_list[[block_count]] <- block
    dist_sum <- block %>%
      group_by(TrialIndex) %>%
      mutate(total_dist = sum(distance,  na.rm = TRUE), total_time = n()) %>%
      summarise_all(last) %>%
      mutate(BlockIndex = block_count)
    
    dist_list[[block_count]] <- dist_sum
    
    # increment block count
    block_count = block_count+1
  }
  
  ## summarise train & test trials ####
  dist_all <- do.call(rbind, dist_list)
  
  dist_select <- dist_all %>% 
    select(sub, Date, BlockIndex, first_loc, total_dist)
  
  sub_dist_list[[sub]] <- dist_select
  
  
}



sub_df <- do.call(rbind, sub_dist_list)
sub_df$block_type <- ifelse(sub_df$BlockIndex < 2, 'train', 'test') #must change this each time (the block index listed on the sub_df NOT the raw data)

#create distinct subject IDs:
sub_df$subject_distinct <- paste(sub_df$sub, sub_df$Date, sep = "_")

#check count:
unique_count <- sub_df %>% 
  distinct(subject_distinct) %>% 
  nrow()

# Print the number of unique values
cat("Number of unique values in 'subject_distinct' column:", unique_count, "\n")

## plot
ggplot(data = sub_df, aes(x = BlockIndex, y = total_dist, group = subject_distinct, color = subject_distinct)) +
  facet_wrap(~block_type) +
  geom_point() + 
  geom_smooth(aes(group = subject_distinct, color = subject_distinct), method = 'lm', se = F)
# not the best plot but we can keep working on it

# remove *really* long trials from training - these are times participants got confused
# they should be following the paths
sub_df <- sub_df %>%
  filter(block_type == 'test' | (block_type == 'train' & total_dist < 500))

## now change long to wide, calculate difference between corresponding train & test
## = excess path
# summarize train & test sessions
sub_df_sum <- sub_df %>%
  group_by(subject_distinct, block_type, first_loc) %>%
  summarise(mean_dist = mean(total_dist))


#on sub_df_sum you get the distances for each trial averaged between blocks

# keep only last trial of test block
#sub_df_last_test <- filter(sub_df, BlockIndex != 4 & BlockIndex != 5)
#sub_df_sum <- sub_df_last_test %>%
#  group_by(sub, block_type, first_loc) %>%
#  summarise(mean_dist = mean(total_dist))

# there must be some sort of pivot call but we'll just do it manually
sub_df_train <- filter(sub_df_sum, block_type == 'train')
sub_df_test <- filter(sub_df_sum, block_type == 'test')

# get the distance traveled for each subject for train and test
sub_distance_sum_train <- sub_df_train %>%
  group_by(subject_distinct) %>%
  summarise(distance_sum = sum(mean_dist))

sub_distance_sum_test <- sub_df_test %>%
  group_by(subject_distinct) %>%
  summarise(distance_sum = sum(mean_dist))

# merge
sub_df_wide <- merge(sub_distance_sum_train, sub_distance_sum_test, 
                     by = c('subject_distinct'))

# get efficency_score
sub_df_wide$efficency_score <- sub_df_wide$distance_sum.y / sub_df_wide$distance_sum.x

write.csv(sub_df_wide, file = "6_6_24_efficency_scores_for_2_1_over_1_train.csv")


  