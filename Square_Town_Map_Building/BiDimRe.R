setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries:
library(tidyverse)
library(psych)
# see: https://github.com/alexander-pastukhov/bidim-regression 
#install.packages("BiDimRegression")
library(BiDimRegression)

#First, we need to know where each image was for each participant
#images are labeled "image1" through "image16"
#each image has a different set of independent X and Z coordinates for each participant
#so first we need the coordinates:
CogMapLocationKey <- read.csv(file = "CogMapLocationKey.csv", header = TRUE)

#remember to check that all participants only use IDs that correspond with those on the key!

#let's also remove what we don't need and rename for clarity and later analysis:
CogMapLocationKey <- CogMapLocationKey[,-c(1,3,5:8,11)]
#must be named indepV1 and indepV2 to work later on:
colnames(CogMapLocationKey)[3] ="indepV1"
colnames(CogMapLocationKey)[4] ="indepV2"

#next we need to label the images:
Image2Object <- read.csv(file = "image_object_match.csv", header = TRUE)
#Now we combine these two data frames so that each object is paired with the correct
#image label:
IndependentVariables <- merge(CogMapLocationKey, Image2Object, by = "object")
#but now it's all out of order! So let's reorder it by subject
#and while we're at it column order too:
IndependentVariables <- IndependentVariables[,c(3,4,2,5,1)]
IndependentVariables <- IndependentVariables %>% arrange(sub)

#Second, we need the coordinates of where each participant placed each image
DependentVariables <- read.csv(file = "drag_drop_processed_data.csv", header = TRUE)
DependentVariables <- DependentVariables[,-1]
colnames(DependentVariables)[1] ="sub"
colnames(DependentVariables)[4] ="depV1"
colnames(DependentVariables)[5] ="depV2"
DependentVariables <- DependentVariables[,c(4,5,1,2,3)]
DependentVariables <- DependentVariables[order(DependentVariables$sub, DependentVariables$date),]

#Now bring them together to get the analysis input df: 
BiDimReR_Input <- DependentVariables %>% inner_join( IndependentVariables, 
                              by=c('image'='image', 'sub'='sub'))
BiDimReR_Input <- BiDimReR_Input[,c(1,2,6,7,3,4,5,8)]
BiDimReR_Input$depV1 <- as.numeric(as.character(BiDimReR_Input$depV1))
BiDimReR_Input$depV2 <- as.numeric(as.character(BiDimReR_Input$depV2))
BiDimReR_Input$indepV1 <- as.numeric(as.character(BiDimReR_Input$indepV1))
BiDimReR_Input$indepV2 <- as.numeric(as.character(BiDimReR_Input$indepV2))

#Split off into single participants
Input_List <- split(BiDimReR_Input, BiDimReR_Input$date)

#we need to loop through the following for each participant:
BiDimReR_Results <- data.frame(sub = numeric(0), date = numeric(0), euclidean_rsqr = numeric(0))

# Loop through each data frame in the list
for (i in seq_along(Input_List)) {
  df <- Input_List[[i]]
  #run the regression:
  #see help function for more info
  BiDimResult <- BiDimRegression(df)
  #I'm also printing just to check this works, recommend leaving this here for sanity:
  cat("Data frame", i, ":\n")
  print(BiDimResult$euclidean.rsqr)
  print(df)
  #add the result with subject ID and date to a data frame
  BiDimReR_Results <- rbind(BiDimReR_Results, data.frame(sub = df[1,5], 
                                                         date = df[1,6],
                                                         euclidean_rsqr = BiDimResult$euclidean.rsqr))
}

#create a seperate df to get omega
#selcted columns
BiDimReR_omega_df <- BiDimReR_Input[,c(1:6)]

#we need a distinct subject label:
BiDimReR_omega_df$subject_distinct <- paste(BiDimReR_omega_df$sub, BiDimReR_omega_df$date, sep = "_")
BiDimReR_omega_df <- BiDimReR_omega_df[,-c(5,6)]

#next we want to get a score for relationship between the two coords (i.e., slope)
BiDimReR_omega_df$dist <- sqrt(((BiDimReR_omega_df$depV1 - BiDimReR_omega_df$indepV1)^2) + ((BiDimReR_omega_df$depV2 - BiDimReR_omega_df$indepV2)^2))
BiDimReR_omega_df <- BiDimReR_omega_df[,-c(1:4)]

#we need a distinct question label:
BiDimReR_omega_df <- BiDimReR_omega_df %>%
  group_by(subject_distinct) %>%
  mutate(question = row_number()) %>%
  ungroup()

BiDimReR_omega_df$dist <- abs(BiDimReR_omega_df$dist)

#now reshape for omega function:
# we need each subject as an observation and each question as a variable
BiDimReR_omega_df_reshaped <- BiDimReR_omega_df %>%
  pivot_wider(
    names_from = question,
    values_from = dist
  )

BiDimReR_omega_df_reshaped <- BiDimReR_omega_df_reshaped[,-1]

#now try calculating omega:
omega(BiDimReR_omega_df_reshaped)

write.csv(BiDimReR_Results, file = "CM_BiDimReR_Results_8_19_24.csv", row.names=FALSE)

#Double checking this works, which it does.
#Book1 <- read.csv(file = "Book1.csv", header = TRUE)
#colnames(Book1)[1] ="indepV1"
#colnames(Book1)[2] ="indepV2"
#colnames(Book1)[3] ="depV1"
#colnames(Book1)[4] ="depV2"
#Book1_result <- BiDimRegression(Book1)

