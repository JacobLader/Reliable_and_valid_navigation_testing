setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries:
library(tidyverse)
# see: https://github.com/alexander-pastukhov/bidim-regression 
#install.packages("BiDimRegression")
library(BiDimRegression)

#First, we need to know where each image should be by reading in a key
key <- read.csv(file = "modelBuildingTemplate.csv", header = TRUE)

#let's rename for clarity and later analysis:
#must be named indepV1 and indepV2 to work later on:
colnames(key)[1] ="indepV1"
colnames(key)[2] ="indepV2"

#Second, we need the coordinates of where each participant placed each image

#remember to change filename!
DependentVariables <- read.csv(file = "VSMB_6_6_24.csv", header = TRUE)

DependentVariables <- DependentVariables[,-c(3,7)]
colnames(DependentVariables)[1] ="sub"
colnames(DependentVariables)[2] ="date"
colnames(DependentVariables)[4] ="depV1"
colnames(DependentVariables)[5] ="depV2"
DependentVariables <- DependentVariables[,c(4,5,1,2,3)]
#DependentVariables <- DependentVariables %>% arrange(sub)

#Now bring them together to get the analysis input df: 
BiDimReR_Input <- DependentVariables %>% inner_join(key, 
                                                    by=c('building'='building'))
BiDimReR_Input <- BiDimReR_Input[,c(1,2,6,7,3,4,5)]
BiDimReR_Input$depV1 <- as.numeric(as.character(BiDimReR_Input$depV1))
BiDimReR_Input$depV2 <- as.numeric(as.character(BiDimReR_Input$depV2))
BiDimReR_Input$indepV1 <- as.numeric(as.character(BiDimReR_Input$indepV1))
BiDimReR_Input$indepV2 <- as.numeric(as.character(BiDimReR_Input$indepV2))

#Split off into single participants (should work by date in case there are multiple participants with the same ID)
Input_List <- split(BiDimReR_Input, BiDimReR_Input$date)

#first create the empty data frame...
#we need to loop through the following for each date:
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

write.csv(BiDimReR_Results, file = "SILCton_BiDimReR_Results.csv", row.names=FALSE)

#Double checking this works, which it does.
#Book1 <- read.csv(file = "Book1.csv", header = TRUE)
#colnames(Book1)[1] ="indepV1"
#colnames(Book1)[2] ="indepV2"
#colnames(Book1)[3] ="depV1"
#colnames(Book1)[4] ="depV2"
#Book1_result <- BiDimRegression(Book1)

