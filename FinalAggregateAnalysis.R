#### setup (RUN ONCE PER SESSION) ####

#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load libraries:
library(tidyverse)
library(ppcor) #for performing correlations, partial correlations, etc.
library(psych) #for description
library(lavaan) #for CFA
library(lsr) #for effect size of the predictor on outcome variable in lm
library(effectsize) #for effect size of the predictor on outcome variable in lm
library(gvlma) #checking lm assumptions
#load libraries for special plotting:
library(corrplot) #for plotting correlation plots
library(car) #added av plots
library(jtools) #for linear regression visualizations
library(factoextra) #for special pca plots
library(semPlot) #for CFA plotting

#important setup step:

#run:
trace(corrplot, edit=TRUE)

#Then replace on line 442:

#place_points = function(sig.locs, point) {
#text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
#labels = point, col = pch.col, cex = pch.cex, 
#lwd = 2)

#with:

# adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    

#place_points = function(sig.locs, point) {
#text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#labels = point, col = pch.col, cex = pch.cex, 
#lwd = 2)

#and then hit the "Save" button.

#### loading in data frames ####

#read in csv and create data frames
MoNav_df <- read.csv(file = 'MoNav_Aggregate_6_6_24.csv', header = TRUE)

#### cleaning and creating main data frames ####

#we need to add individual and less-identifiable participant IDs to the main df

#determine number of rows in MoNav_df
n <- nrow(MoNav_df)

# Add a new column called "subject" with increment values
MoNav_df$subject <- 1:n

#this next part is subject to change based on data frame formatting:

#remove columns we should not include in analysis. In this case:
MoNav_df <- MoNav_df[, !colnames(MoNav_df) %in% c("X", "X.1", "Notes", "Initials", "day_2", "day_1", "VS_ID")]

#calculate difference between first and second test trial for cogmaps wayfinding

MoNav_df$CM_efficiency_score_change <- MoNav_df$CM_efficiency_score_2_2 - MoNav_df$CM_efficiency_score_2_1

#now we want to ensure variables are ordered how we want, namely that each paradigm's data is grouped together:
#hint: use summary function to get the names and then copy paste in the preferred order:
MoNav_df <- MoNav_df[, c("subject", "ID", "Round", "TT_group",
                         "CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", "CM_efficiency_score_2_1", 
                         "CM_efficiency_score_2_2", "CM_MB_Euclidian_Rsqr", "CM_efficiency_score_change",
                         "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr", "TT_MB_Outline_Euclidian_Rsqr",
                         "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_JRD_Group", "VS_MB_Rsqr",
                         "Restricted_VS_Diff_JRD_Avrg_Angular_Error", "Restricted_VS_Same_JRD_Avrg_Angular_Error", "Restricted_VS_JRD_Group",
                         "SDT_Style_MRT", "SBSOD_Total", "SBSOD_Avrg", "PTT_A", "NSQ",
                         "Age", "Birthdate", "Gender", "Race", "Spanish_Hispanic_Latinx_ethnicity", "primary_language",
                         "other_languages", "English_first_language", "occupation", "spouse_partner_occupation",
                         "level_of_education", "spouse_partners_highest_level_of_education", "family_annual_gross_income",
                         "Distance_lived_from_major_highway_or_roadway", "zip_code", "takes_prescribed_medication",
                         "KBIT_Verbal", "KBIT_Nonverbal", "KBIT_IQ")]

#### Reverse Score necessary quant columns ####
MoNav_df$CMJRD_mean_error <- (MoNav_df$CMJRD_mean_error) * (-1)
MoNav_df$CM_efficiency_score_all <- (MoNav_df$CM_efficiency_score_all) * (-1)
MoNav_df$CM_efficiency_score_2_1 <- (MoNav_df$CM_efficiency_score_2_1) * (-1)
MoNav_df$CM_efficiency_score_2_2 <- (MoNav_df$CM_efficiency_score_2_2) * (-1)
MoNav_df$TT_JRD_Avrg_Angular_Error <- (MoNav_df$TT_JRD_Avrg_Angular_Error) * (-1)
MoNav_df$TT_Route_Efficiency <- (MoNav_df$TT_Route_Efficiency) * (-1)
MoNav_df$VS_Diff_JRD_Avrg_Angular_Error <- (MoNav_df$VS_Diff_JRD_Avrg_Angular_Error) * (-1)
MoNav_df$VS_Same_JRD_Avrg_Angular_Error <- (MoNav_df$VS_Same_JRD_Avrg_Angular_Error) * (-1)
MoNav_df$Restricted_VS_Diff_JRD_Avrg_Angular_Error <- (MoNav_df$Restricted_VS_Diff_JRD_Avrg_Angular_Error) * (-1)
MoNav_df$Restricted_VS_Same_JRD_Avrg_Angular_Error <- (MoNav_df$Restricted_VS_Same_JRD_Avrg_Angular_Error) * (-1)

#### Create other data frames here: ####

#Numeric Data frame
Quant_MoNav_df <- MoNav_df[, c("Round",
                               "CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", "CM_efficiency_score_2_1", 
                               "CM_efficiency_score_2_2", "CM_MB_Euclidian_Rsqr", "CM_efficiency_score_change",
                               "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr",
                               "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_JRD_Group", "VS_MB_Rsqr",
                               "Restricted_VS_Diff_JRD_Avrg_Angular_Error", "Restricted_VS_Same_JRD_Avrg_Angular_Error", "Restricted_VS_JRD_Group",
                               "SDT_Style_MRT", "SBSOD_Total", "SBSOD_Avrg", "PTT_A", "NSQ",
                               "Age", "KBIT_IQ")]

#create separate data frames for round 2 subjects (used in everything else) - I would just narrow down the existing data frame:
MoNav_df <- MoNav_df[MoNav_df$Round == 2,]
MoNav_df <- MoNav_df[, c("subject", "ID", "Round", "TT_group",
                         "CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", "CM_efficiency_score_2_1", 
                         "CM_efficiency_score_2_2", "CM_MB_Euclidian_Rsqr", "CM_efficiency_score_change",
                         "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr", "TT_MB_Outline_Euclidian_Rsqr",
                         "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_JRD_Group", "VS_MB_Rsqr",
                         "Restricted_VS_Diff_JRD_Avrg_Angular_Error", "Restricted_VS_Same_JRD_Avrg_Angular_Error", "Restricted_VS_JRD_Group",
                         "SDT_Style_MRT", "SBSOD_Total", "SBSOD_Avrg", "PTT_A", "NSQ",
                         "Age", "Birthdate", "Gender", "Race", "Spanish_Hispanic_Latinx_ethnicity", "primary_language",
                         "other_languages", "English_first_language", "occupation", "spouse_partner_occupation",
                         "level_of_education", "spouse_partners_highest_level_of_education", "family_annual_gross_income",
                         "Distance_lived_from_major_highway_or_roadway", "zip_code", "takes_prescribed_medication",
                         "KBIT_Verbal", "KBIT_Nonverbal", "KBIT_IQ")]

Quant_MoNav_df <- Quant_MoNav_df[Quant_MoNav_df$Round == 2,]
#create a pca df with JRD group now too:
pca_Quant_MoNav_df <- Quant_MoNav_df[, c("CMJRD_mean_error", "CM_efficiency_score_all",
                                         "CM_MB_Euclidian_Rsqr",
                                         "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr",
                                         "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_MB_Rsqr", "VS_JRD_Group")]
#regular quant frame:
Quant_MoNav_df <- Quant_MoNav_df[, c("CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", "CM_efficiency_score_2_1", 
                               "CM_efficiency_score_2_2", "CM_MB_Euclidian_Rsqr", "CM_efficiency_score_change",
                               "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr",
                               "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_MB_Rsqr",
                               "SDT_Style_MRT", "SBSOD_Total", "SBSOD_Avrg", "PTT_A", "NSQ",
                               "Age", "KBIT_IQ")]

#### preliminary t.tests for cleaning data ####

#we're performing this test to determine whether we should only use the MB blank from TT
t.test(MoNav_df$TT_MB_Blank_Euclidian_Rsqr, MoNav_df$TT_MB_Outline_Euclidian_Rsqr, var.equal = TRUE)

#visualize:
ggplot(data = MoNav_df, aes(x = TT_MB_Blank_Euclidian_Rsqr, y = TT_MB_Outline_Euclidian_Rsqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  #Add y = x line
  labs(title = "Temple Tour Map Building", x = "Blank Score", y = "Outline Score")

#we're performing this test to determine whether we should only use the Route Test 2 from CM
t.test(MoNav_df$CM_efficiency_score_2_1, MoNav_df$CM_efficiency_score_2_2, var.equal = TRUE)

#visualize:
ggplot(data = MoNav_df, aes(x = CM_efficiency_score_2_1, y = CM_efficiency_score_2_2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  #Add y = x line
  labs(title = "Cog Map Route Efficiency", x = "Test 1", y = "Test 2")

#### Look at normality of data ####

#cogmaps
hist(Quant_MoNav_df$CMJRD_mean_error)
hist(Quant_MoNav_df$CM_efficiency_score_all)
hist(Quant_MoNav_df$CM_MB_Euclidian_Rsqr)

#Temple Tour
hist(Quant_MoNav_df$TT_JRD_Avrg_Angular_Error)
hist(Quant_MoNav_df$TT_MB_Blank_Euclidian_Rsqr)
hist(Quant_MoNav_df$TT_Route_Efficiency)

#Virtual SILCton
hist(Quant_MoNav_df$VS_Diff_JRD_Avrg_Angular_Error)
hist(Quant_MoNav_df$VS_Same_JRD_Avrg_Angular_Error)
hist(Quant_MoNav_df$VS_MB_Rsqr)

#Small-scale
hist(Quant_MoNav_df$SDT_Style_MRT)
hist(Quant_MoNav_df$PTT_A)

#self-report
hist(Quant_MoNav_df$SBSOD_Avrg)
hist(Quant_MoNav_df$NSQ)

#other
hist(Quant_MoNav_df$KBIT_IQ)
hist(Quant_MoNav_df$Age)

#### making z-score composite data frames ####

#Temple Tour

MB_TT_1 <- (Quant_MoNav_df$TT_MB_Blank_Euclidian_Rsqr)
#MB_TT_2 <- (Quant_MoNav_df$TT_MB_Outline_Euclidian_Rsqr)
JRD_TT <- (Quant_MoNav_df$TT_JRD_Avrg_Angular_Error)
RT_TT <- (Quant_MoNav_df$TT_Route_Efficiency)

MB_1_sd_TT <- sd(MB_TT_1)
MB_1_mean_TT <- mean(MB_TT_1)

#MB_2_sd_TT <- sd(MB_TT_2)
#MB_2_mean_TT <- mean(MB_TT_2)

JRD_sd_TT <- sd(JRD_TT)
JRD_mean_TT <- mean(JRD_TT)

RT_sd_TT <- sd(RT_TT)
RT_mean_TT <- mean(RT_TT)

MB_1_z_TT <- (MB_TT_1 -MB_1_mean_TT)/MB_1_sd_TT
#MB_2_z_TT <- (MB_TT_2 -MB_2_mean_TT)/MB_2_sd_TT
JRD_z_TT <- (JRD_TT - JRD_mean_TT)/JRD_sd_TT
RT_z_TT <- (RT_TT - RT_mean_TT)/RT_sd_TT

TT_comp <- (MB_1_z_TT+JRD_z_TT+RT_z_TT)

TT_comp_df <- as.data.frame(TT_comp)

#Cog Maps

MB_CM <- (Quant_MoNav_df$CM_MB_Euclidian_Rsqr)
JRD_CM <- (Quant_MoNav_df$CMJRD_mean_error)
RT_1_CM <- (Quant_MoNav_df$CM_efficiency_score_2_1)
RT_2_CM <- (Quant_MoNav_df$CM_efficiency_score_2_2)

MB_sd_CM <- sd(MB_CM)
MB_mean_CM <- mean(MB_CM)

JRD_sd_CM <- sd(JRD_CM)
JRD_mean_CM <- mean(JRD_CM)

RT_1_sd_CM <- sd(RT_1_CM)
RT_1_mean_CM <- mean(RT_1_CM)

RT_2_sd_CM <- sd(RT_2_CM)
RT_2_mean_CM <- mean(RT_2_CM)

MB_z_CM <- (MB_CM -MB_mean_CM)/MB_sd_CM
JRD_z_CM <- (JRD_CM - JRD_mean_CM)/JRD_sd_CM
RT_1_z_CM <- (RT_1_CM - RT_1_mean_CM)/RT_1_sd_CM
RT_2_z_CM <- (RT_2_CM - RT_2_mean_CM)/RT_2_sd_CM

CM_comp <- (MB_z_CM+JRD_z_CM+RT_1_z_CM+RT_2_z_CM)

CM_comp_df <- as.data.frame(CM_comp)

#Virtual SILCton

MB_VS <- (Quant_MoNav_df$VS_MB_Rsqr)
JRD_VS_1 <- (Quant_MoNav_df$VS_Diff_JRD_Avrg_Angular_Error)
JRD_VS_2 <- (Quant_MoNav_df$VS_Same_JRD_Avrg_Angular_Error)

MB_sd_VS <- sd(MB_VS)
MB_mean_VS <- mean(MB_VS)

JRD_1_sd_VS <- sd(JRD_VS_1)
JRD_1_mean_VS <- mean(JRD_VS_1)

JRD_2_sd_VS <- sd(JRD_VS_2)
JRD_2_mean_VS <- mean(JRD_VS_2)

MB_z_VS <- (MB_VS - MB_mean_VS)/MB_sd_VS
JRD_1_z_VS <- (JRD_VS_1 - JRD_1_mean_VS)/JRD_1_sd_VS
JRD_2_z_VS <- (JRD_VS_2 - JRD_2_mean_VS)/JRD_2_sd_VS

VS_comp <- (MB_z_VS+JRD_1_z_VS+JRD_2_z_VS)

VS_comp_df <- as.data.frame(VS_comp)

#IQ
IQ <- (Quant_MoNav_df$KBIT_IQ)

IQ_sd <- sd(IQ)
IQ_mean <- mean(IQ)

IQ_z <- (IQ - IQ_mean)/IQ_sd

IQ_comp <- (IQ_z)
IQ_comp_df <- as.data.frame(IQ_comp)

#combine distinct composite score data frames into a single data frame:
comp_bind <- cbind(TT_comp_df,CM_comp_df)
comp_bind <- cbind(comp_bind,VS_comp_df)
#comp_bind <- cbind(comp_bind,VS_comp_df_R)
comp_bind <- cbind(comp_bind,IQ_comp_df)

#### Description ####

#a helpful note on the describe function:
#If the check option is TRUE, variables that are categorical or logical are 
#converted to numeric and then described. These variables are marked with an * in the row name. 
#This is somewhat slower. Note that in the case of categories or factors, the numerical ordering 
#is not necessarily the one expected. For instance, if education is coded "high school", "some college" , 
#"finished college", then the default coding will lead to these as values of 2, 3, 1. Thus, statistics for 
#those variables marked with * should be interpreted cautiously (if at all).

descriptives_df <- describe(MoNav_df)

#Getting counts for gender
count(MoNav_df, Gender)

count(MoNav_df, Race)

#count(MoNav_df$Gender, Race)

MoNav_df %>%
  filter(Gender == "Female") %>%
  group_by(Spanish_Hispanic_Latinx_ethnicity) %>%
  summarise(count = n())

MoNav_df %>%
  filter(Gender == "Male") %>%
  group_by(Spanish_Hispanic_Latinx_ethnicity) %>%
  summarise(count = n())

#Getting counts for group
count(MoNav_df, TT_group)
count(MoNav_df, VS_JRD_Group)
count(MoNav_df, Restricted_VS_JRD_Group)

count(MoNav_df, Age)
mean(MoNav_df$Age)

ggplot(MoNav_df, aes(x = Age, fill = Gender)) +
  geom_bar() +
  labs(title = "",
       x = "Age (years)",
       y = "") +
  scale_fill_manual(values = c("Male" = "#333", "Female" = "#999")) +
  theme_classic()

#get quant descriptives:
Quant_descriptives_df <- describe(Quant_MoNav_df)

omega(Quant_MoNav_df[,c(2,3,6,8:13)])

#### Create partial correlation matrices ####

#create a partial correlation matrix while controlling for age KBIT IQ
#KBIT-IQ must be last with this method,though you can also do this by specifying variables
#I'm pretty sure that the way I'm doing this now requires the pcor.test variables to be specified by numeric index
#Initialize the matrix to store partial correlation coefficients
pcor_matrix_IQ <- matrix(NA, ncol = ncol(Quant_MoNav_df)-2, nrow = ncol(Quant_MoNav_df)-2)
#Loop through each pair of variables
for (i in 1:(ncol(Quant_MoNav_df)-2)) {
  for (j in 1:(ncol(Quant_MoNav_df)-2)) {
    if (i == j) {
      #Set diagonal elements to 1
      pcor_matrix_IQ[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function, unlike previously where we used pcor.test
      partial_cor <- pcor(Quant_MoNav_df[, c(i, j, (ncol(Quant_MoNav_df)-1), ncol(Quant_MoNav_df))])
      #Assign the estimate to the corresponding cells in the matrix
      pcor_matrix_IQ[i, j] <- partial_cor$estimate[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      pcor_matrix_IQ[j, i] <- pcor_matrix_IQ[i, j]
    }
  }
}
# Set row names for partial correlation matrix 
rownames(pcor_matrix_IQ) <- colnames(Quant_MoNav_df)[1:nrow(pcor_matrix_IQ)]
# Set column names for partial correlation matrix 
colnames(pcor_matrix_IQ) <- colnames(Quant_MoNav_df)[1:nrow(pcor_matrix_IQ)]

#calculate significance values and create a matrix
#Initialize the matrix to store p-values
testRes <- matrix(NA, ncol = ncol(Quant_MoNav_df)-2, nrow = ncol(Quant_MoNav_df)-2)
#Loop through each pair of variables
for (i in 1:(ncol(Quant_MoNav_df)-2)) {
  for (j in 1:(ncol(Quant_MoNav_df)-2)) {
    if (i == j) {
      #Set diagonal elements to 1
      testRes[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function
      partial_cor_test_res <- pcor(Quant_MoNav_df[, c(i, j, (ncol(Quant_MoNav_df)-1), ncol(Quant_MoNav_df))])
      #Assign the p-value to the corresponding cells in the matrix
      testRes[i, j] <- partial_cor_test_res$p.value[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      testRes[j, i] <- testRes[i, j]
    }
  }
}
# Set row names for p value matrix 
rownames(testRes) <- colnames(Quant_MoNav_df)[1:nrow(testRes)]
# Set column names for  p value matrix 
colnames(testRes) <- colnames(Quant_MoNav_df)[1:nrow(testRes)]

#### Visualize correlations ####
#Create a broad correlation plot
corrplot(pcor_matrix_IQ,
         method = "number",
         p.mat = testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 0.5,
         pch.col = "#fca50a",
         type = "lower",
         tl.srt = 45,
         tl.cex = 0.5,
         tl.col = 1,
         number.cex = 0.4,
         cl.cex = 0.6,
         title = "Partial correlation matrix controlling for age and KBIT IQ",
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#420a68",
         diag = FALSE)

#create a correlation plots for within-paradigm plots

#Temple Tour
TT_pcor_IQ <- pcor_matrix_IQ[8:10,8:10]
TT_testRes <- testRes[8:10,8:10]

colnames(TT_pcor_IQ) <- c("Pointing", "Route Efficiency", "Map Building")

rownames(TT_pcor_IQ) <- c("Pointing", "Route Efficiency", "Map Building")

colnames(TT_testRes) <- c("Pointing", "Route Efficiency", "Map Building")

rownames(TT_testRes) <- c("Pointing", "Route Efficiency", "Map Building")

#coef_colors <- ifelse(TT_pcor_IQ > 0.4 | TT_pcor_IQ < -0.4, "white", "black")
corrplot(TT_pcor_IQ,
         method = "number",
         p.mat = TT_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 3,
         pch.col = "white",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.8,
         tl.col = 1,
         number.cex = 3,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#440154",
         diag = FALSE)

#Cog Maps
CM_pcor_IQ <- pcor_matrix_IQ[c(2,3,6),c(2,3,6)]
CM_testRes <- testRes[c(2,3,6),c(2,3,6)]

colnames(CM_pcor_IQ) <- c("Pointing", "Route Efficiency", "Map Building")

rownames(CM_pcor_IQ) <- c("Pointing", "Route Efficiency", "Map Building")

colnames(CM_testRes) <- c("Pointing", "Route Efficiency", "Map Building")

rownames(CM_testRes) <- c("Pointing", "Route Efficiency", "Map Building")

#coef_colors <- ifelse(CM_pcor_IQ > 0.4 | CM_pcor_IQ < -0.4, "white", "black")
corrplot(CM_pcor_IQ,
         method = "number",
         p.mat = CM_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 3,
         pch.col = "white",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.8,
         tl.col = 1,
         number.cex = 3,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#3b528b",
         diag = FALSE)

#Virtual SILCton
VS_pcor_IQ <- pcor_matrix_IQ[11:13,11:13]
VS_testRes <- testRes[11:13,11:13]

colnames(VS_pcor_IQ) <- c("Between Route Pointing", "Within Route Pointing", "Map Building")

rownames(VS_pcor_IQ) <- c("Between Route Pointing", "Within Route Pointing", "Map Building")

colnames(VS_testRes) <- c("Between Route Pointing", "Within Route Pointing", "Map Building")

rownames(VS_testRes) <- c("Between Route Pointing", "Within Route Pointing", "Map Building")

#coef_colors <- ifelse(VS_pcor_IQ > 0.4 | VS_pcor_IQ < -0.4, "white", "black")
corrplot(VS_pcor_IQ,
         method = "number",
         p.mat = VS_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 3,
         pch.col = "white",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.5,
         tl.col = 1,
         number.cex = 3,
         cl.cex = 1.2,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#21918c",
         diag = FALSE)

#between MB Tasks
AllMB_pcor_IQ <- pcor_matrix_IQ[c(6,10,13),c(6,10,13)]
AllMB_testRes <- testRes[c(6,10,13),c(6,10,13)]

colnames(AllMB_pcor_IQ) <- c("Square Town Map Building", "Temple Tour Map Building Blank", "SILCton Map Building")

rownames(AllMB_pcor_IQ) <- c("Square Town Map Building", "Temple Tour Map Building Blank", "SILCton Map Building")

colnames(AllMB_testRes) <- c("Square Town Map Building", "Temple Tour Map Building Blank", "SILCton Map Building")

rownames(AllMB_testRes) <- c("Square Town Map Building", "Temple Tour Map Building Blank", "SILCton Map Building")

corrplot(AllMB_pcor_IQ,
         method = "number",
         p.mat = AllMB_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 4,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.2,
         tl.col = 1,
         number.cex = 4,
         cl.cex = 0.5,
         cex.main = 1.6,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "azure4",
         diag = FALSE)

#between JRD Tasks
AllJRD_pcor_IQ <- pcor_matrix_IQ[c(2,8,11,12),c(2,8,11,12)]
AllJRD_testRes <- testRes[c(2,8,11,12),c(2,8,11,12)]

colnames(AllJRD_pcor_IQ) <- c("Square Town Pointing", "Temple Tour Pointing", "SILCton Between Route Pointing", "SILCton Within Route Pointing")

rownames(AllJRD_pcor_IQ) <- c("Square Town Pointing", "Temple Tour Pointing", "SILCton Between Route Pointing", "SILCton Within Route Pointing")

colnames(AllJRD_testRes) <- c("Square Town Pointing", "Temple Tour Pointing", "SILCton Between Route Pointing", "SILCton Within Route Pointing")

rownames(AllJRD_testRes) <- c("Square Town Pointing", "Temple Tour Pointing", "SILCton Between Route Pointing", "SILCton Within Route Pointing")

corrplot(AllJRD_pcor_IQ,
         method = "number",
         p.mat = AllJRD_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 1.7,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.2,
         tl.col = 1,
         number.cex = 1.5,
         cl.cex = 0.5,
         cex.main = 1.6,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "azure4",
         diag = FALSE)

#between RT Tasks
AllRT_pcor_IQ <- pcor_matrix_IQ[c(4,5,9),c(4,5,9)]
AllRT_testRes <- testRes[c(4,5,9),c(4,5,9)]

colnames(AllRT_pcor_IQ) <- c("Square Town Efficiency Test 1", "Square Town Efficiency Test 2", "Temple Tour Efficiency")

rownames(AllRT_pcor_IQ) <- c("Square Town Efficiency Test 1", "Square Town Efficiency Test 2", "Temple Tour Efficiency")

colnames(AllRT_testRes) <- c("Square Town Efficiency Test 1", "Square Town Efficiency Test 2", "Temple Tour Efficiency")

rownames(AllRT_testRes) <- c("Square Town Efficiency Test 1", "Square Town Efficiency Test 2", "Temple Tour Efficiency")

corrplot(AllRT_pcor_IQ,
         method = "number",
         p.mat = AllRT_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 2,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.2,
         tl.col = 1,
         number.cex = 1.5,
         cl.cex = 0.5,
         title = "Correlations between Route Efficiency Tasks controlling for KBIT IQ",
         cex.main = 1.6,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "azure4",
         diag = FALSE)

#CHECK THAT INDICES ARE RIGHT!!
#small scale task correlations
Small_scale_pcor_IQ <- pcor_matrix_IQ[c(17,14,
                                        8,9,10,
                                        2,3,6,
                                        11,12,13),c(17,14)]
Small_scale_testRes <- testRes[c(17,14,
                                 8,9,10,
                                 2,3,6,
                                 11,12,13),c(17,14)]

rownames(Small_scale_pcor_IQ) <- c("PTT-A", "MRT",
                                   "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                                   "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                   "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(Small_scale_pcor_IQ) <- c("PTT-A", "MRT")

rownames(Small_scale_testRes) <- c("PTT-A", "MRT",
                                   "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                                   "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                   "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(Small_scale_testRes) <- c("PTT-A", "MRT")

corrplot(Small_scale_pcor_IQ,
         method = "number",
         p.mat = Small_scale_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = .8,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1,
         tl.col = 1,
         number.cex = .8,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#2f4f4f",
         diag = FALSE)

#Self Report
Self_scale_pcor_IQ <- pcor_matrix_IQ[c(16,18,8,9,10,
                                       2,3,6,
                                       11,12,13),c(16,18)]
Self_scale_testRes <- testRes[c(16,18,8,9,10,
                                2,3,6,
                                11,12,13),c(16,18)]

colnames(Self_scale_pcor_IQ) <- c("SBSOD", "NSQ")

rownames(Self_scale_pcor_IQ) <- c("SBSOD", "NSQ",
                                  "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                                  "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                  "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(Self_scale_testRes) <- c("SBSOD", "NSQ")

rownames(Self_scale_testRes) <- c("SBSOD", "NSQ",
                                  "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                                  "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                  "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")


#coef_colors <- ifelse(TT_pcor_IQ > 0.4 | TT_pcor_IQ < -0.4, "white", "black")
corrplot(Self_scale_pcor_IQ,
         method = "number",
         p.mat = Self_scale_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = .8,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1,
         tl.col = 1,
         number.cex = .8,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#2f4f4f",
         diag = FALSE)

#### Create partial correlation matrix for PTT-A controlling for MRT ####
#need to rearrange the data frame so that MRT is right before age and KBIT:
Quant_MoNav_df <- Quant_MoNav_df[, c("CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", "CM_efficiency_score_2_1", 
                                     "CM_efficiency_score_2_2", "CM_MB_Euclidian_Rsqr", "CM_efficiency_score_change",
                                     "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr",
                                     "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_MB_Rsqr",
                                     "SBSOD_Total", "SBSOD_Avrg", "PTT_A", "NSQ",
                                    "SDT_Style_MRT", "Age", "KBIT_IQ")]

PTTA_matrix_PCorr_MRT <- matrix(NA, ncol = ncol(Quant_MoNav_df)-3, nrow = ncol(Quant_MoNav_df)-3)
#Loop through each pair of variables
for (i in 1:(ncol(Quant_MoNav_df)-3)) {
  for (j in 1:(ncol(Quant_MoNav_df)-3)) {
    if (i == j) {
      #Set diagonal elements to 1
      PTTA_matrix_PCorr_MRT[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function, unlike previously where we used pcor.test
      partial_cor <- pcor(Quant_MoNav_df[, c(i, j, (ncol(Quant_MoNav_df)-2), (ncol(Quant_MoNav_df)-1), ncol(Quant_MoNav_df))])
      #Assign the estimate to the corresponding cells in the matrix
      PTTA_matrix_PCorr_MRT[i, j] <- partial_cor$estimate[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      PTTA_matrix_PCorr_MRT[j, i] <- PTTA_matrix_PCorr_MRT[i, j]
    }
  }
}
# Set row names for partial correlation matrix 
rownames(PTTA_matrix_PCorr_MRT) <- colnames(Quant_MoNav_df)[1:nrow(PTTA_matrix_PCorr_MRT)]
# Set column names for partial correlation matrix 
colnames(PTTA_matrix_PCorr_MRT) <- colnames(Quant_MoNav_df)[1:nrow(PTTA_matrix_PCorr_MRT)]

#calculate significance values and create a matrix
#Initialize the matrix to store p-values
PTT_testRes <- matrix(NA, ncol = ncol(Quant_MoNav_df)-3, nrow = ncol(Quant_MoNav_df)-3)
#Loop through each pair of variables
for (i in 1:(ncol(Quant_MoNav_df)-3)) {
  for (j in 1:(ncol(Quant_MoNav_df)-3)) {
    if (i == j) {
      #Set diagonal elements to 1
      PTT_testRes[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function
      partial_cor_test_res <- pcor(Quant_MoNav_df[, c(i, j, (ncol(Quant_MoNav_df)-2), (ncol(Quant_MoNav_df)-1), ncol(Quant_MoNav_df))])
      #Assign the p-value to the corresponding cells in the matrix
      PTT_testRes[i, j] <- partial_cor_test_res$p.value[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      PTT_testRes[j, i] <- PTT_testRes[i, j]
    }
  }
}
# Set row names for p value matrix 
rownames(PTT_testRes) <- colnames(Quant_MoNav_df)[1:nrow(PTT_testRes)]
# Set column names for  p value matrix 
colnames(PTT_testRes) <- colnames(Quant_MoNav_df)[1:nrow(PTT_testRes)]

PTTA_pcor_IQ <- PTTA_matrix_PCorr_MRT[c(16,8,9,10,2,3,6,11,12,13),c(16,8,9,10,2,3,6,11,12,13)]

PTTA_testRes <- PTT_testRes[c(16,8,9,10,2,3,6,11,12,13),c(16,8,9,10,2,3,6,11,12,13)]

rownames(PTTA_pcor_IQ) <- c("PTT-A",
                                   "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                                   "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                   "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(PTTA_pcor_IQ) <- c("PTT-A",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

rownames(PTTA_testRes) <- c("PTT-A",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(PTTA_testRes) <- c("PTT-A",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

corrplot(PTTA_pcor_IQ,
         method = "number",
         p.mat = PTTA_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = .8,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1,
         tl.col = 1,
         number.cex = .8,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#2f4f4f",
         diag = FALSE)

#### Create partial correlation matrix for MRT controlling for PTT-A ####
#need to rearrange the data frame so that PTT=A is right before age and KBIT:
Quant_MoNav_df <- Quant_MoNav_df[, c("CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", "CM_efficiency_score_2_1", 
                                     "CM_efficiency_score_2_2", "CM_MB_Euclidian_Rsqr", "CM_efficiency_score_change",
                                     "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr",
                                     "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_MB_Rsqr",
                                     "SBSOD_Total", "SBSOD_Avrg", "SDT_Style_MRT", "NSQ",
                                     "PTT_A", "Age", "KBIT_IQ")]

MRT_matrix_PCorr_PTTA <- matrix(NA, ncol = ncol(Quant_MoNav_df)-3, nrow = ncol(Quant_MoNav_df)-3)
#Loop through each pair of variables
for (i in 1:(ncol(Quant_MoNav_df)-3)) {
  for (j in 1:(ncol(Quant_MoNav_df)-3)) {
    if (i == j) {
      #Set diagonal elements to 1
      MRT_matrix_PCorr_PTTA[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function, unlike previously where we used pcor.test
      partial_cor <- pcor(Quant_MoNav_df[, c(i, j, (ncol(Quant_MoNav_df)-2), (ncol(Quant_MoNav_df)-1), ncol(Quant_MoNav_df))])
      #Assign the estimate to the corresponding cells in the matrix
      MRT_matrix_PCorr_PTTA[i, j] <- partial_cor$estimate[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      MRT_matrix_PCorr_PTTA[j, i] <- MRT_matrix_PCorr_PTTA[i, j]
    }
  }
}
# Set row names for partial correlation matrix 
rownames(MRT_matrix_PCorr_PTTA) <- colnames(Quant_MoNav_df)[1:nrow(MRT_matrix_PCorr_PTTA)]
# Set column names for partial correlation matrix 
colnames(MRT_matrix_PCorr_PTTA) <- colnames(Quant_MoNav_df)[1:nrow(MRT_matrix_PCorr_PTTA)]

#calculate significance values and create a matrix
#Initialize the matrix to store p-values
MRT_testRes <- matrix(NA, ncol = ncol(Quant_MoNav_df)-3, nrow = ncol(Quant_MoNav_df)-3)
#Loop through each pair of variables
for (i in 1:(ncol(Quant_MoNav_df)-3)) {
  for (j in 1:(ncol(Quant_MoNav_df)-3)) {
    if (i == j) {
      #Set diagonal elements to 1
      MRT_testRes[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function
      partial_cor_test_res <- pcor(Quant_MoNav_df[, c(i, j, (ncol(Quant_MoNav_df)-2), (ncol(Quant_MoNav_df)-1), ncol(Quant_MoNav_df))])
      #Assign the p-value to the corresponding cells in the matrix
      MRT_testRes[i, j] <- partial_cor_test_res$p.value[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      MRT_testRes[j, i] <- MRT_testRes[i, j]
    }
  }
}
# Set row names for p value matrix 
rownames(MRT_testRes) <- colnames(Quant_MoNav_df)[1:nrow(MRT_testRes)]
# Set column names for  p value matrix 
colnames(MRT_testRes) <- colnames(Quant_MoNav_df)[1:nrow(MRT_testRes)]

MRTpcor_IQ <- MRT_matrix_PCorr_PTTA[c(16,8,9,10,2,3,6,11,12,13),c(16,8,9,10,2,3,6,11,12,13)]

MRT_testRes <- MRT_testRes[c(16,8,9,10,2,3,6,11,12,13),c(16,8,9,10,2,3,6,11,12,13)]

rownames(MRTpcor_IQ) <- c("MRT",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(MRTpcor_IQ) <- c("MRT",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

rownames(MRT_testRes) <- c("MRT",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

colnames(MRT_testRes) <- c("MRT",
                            "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building Blank",
                            "Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                            "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

corrplot(MRTpcor_IQ,
         method = "number",
         p.mat = MRT_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = .8,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1,
         tl.col = 1,
         number.cex = .8,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#2f4f4f",
         diag = FALSE)

#### check work for small scale partial correlations ####
pcor(Quant_MoNav_df[,c(18, 8, 16, 19, 20)])
pcor.test(Quant_MoNav_df$PTT_A, Quant_MoNav_df$TT_JRD_Avrg_Angular_Error, Quant_MoNav_df[,c("SDT_Style_MRT", "Age", "KBIT_IQ")])

pcor.test(Quant_MoNav_df$SDT_Style_MRT, Quant_MoNav_df$TT_MB_Blank_Euclidian_Rsqr, Quant_MoNav_df[,c("PTT_A", "Age", "KBIT_IQ")])

#### create partial correlation matrices for Z-standardized composite scores ####

#Initialize the matrix to store partial correlation coefficients
comp_pcor_matrix_IQ <- matrix(NA, ncol = ncol(comp_bind)-1, nrow = ncol(comp_bind)-1)
#Loop through each pair of variables
for (i in 1:(ncol(comp_bind)-1)) {
  for (j in 1:(ncol(comp_bind)-1)) {
    if (i == j) {
      #Set diagonal elements to 1
      comp_pcor_matrix_IQ[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function, unlike previously where we used pcor.test
      comp_partial_cor <- pcor(comp_bind[, c(i, j, ncol(comp_bind))])
      #Assign the estimate to the corresponding cells in the matrix
      comp_pcor_matrix_IQ[i, j] <- comp_partial_cor$estimate[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      comp_pcor_matrix_IQ[j, i] <- comp_pcor_matrix_IQ[i, j]
    }
  }
}
# Set row names for partial correlation matrix 
rownames(comp_pcor_matrix_IQ) <- colnames(comp_bind)[1:nrow(comp_pcor_matrix_IQ)]
# Set column names for partial correlation matrix 
colnames(comp_pcor_matrix_IQ) <- colnames(comp_bind)[1:nrow(comp_pcor_matrix_IQ)]

#calculate significance values and create a matrix
#Initialize the matrix to store p-values
comp_testRes <- matrix(NA, ncol = ncol(comp_bind)-1, nrow = ncol(comp_bind)-1)
#Loop through each pair of variables
for (i in 1:(ncol(comp_bind)-1)) {
  for (j in 1:(ncol(comp_bind)-1)) {
    if (i == j) {
      #Set diagonal elements to 1
      comp_testRes[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function
      comp_partial_cor_test_res <- pcor(comp_bind[, c(i, j, ncol(comp_bind))])
      #Assign the p-value to the corresponding cells in the matrix
      comp_testRes[i, j] <- comp_partial_cor_test_res$p.value[1,2]
      #Since the matrix is symmetric, assign the same value to the symmetric cell
      comp_testRes[j, i] <- comp_testRes[i, j]
    }
  }
}
# Set row names for p value matrix 
rownames(comp_testRes) <- colnames(comp_bind)[1:nrow(comp_testRes)]
# Set column names for  p value matrix 
colnames(comp_testRes) <- colnames(comp_bind)[1:nrow(comp_testRes)]

#### visualize between Z-scores correlations ####

colnames(comp_pcor_matrix_IQ) <- c("Temple Tour", "Square Town", "SILCton")

rownames(comp_pcor_matrix_IQ) <- c("Temple Tour", "Square Town", "SILCton")

colnames(comp_testRes) <- c("Temple Tour", "Square Town", "SILCton")

rownames(comp_testRes) <- c("Temple Tour", "Square Town", "SILCton")

corrplot(comp_pcor_matrix_IQ,
         method = "number",
         p.mat = comp_testRes,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 3,
         pch.col = "#fde725",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.8,
         tl.col = 1,
         number.cex = 3,
         cl.cex = 0.5,
         cex.main = 1,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "azure4",
         diag = FALSE)

#### Cog Maps  Rate of Change ####

CM_Route_Efficiency_df <- MoNav_df[,c(1,8:9)]

#Convert the data from wide to long format
CM_Route_Efficiency_df <- pivot_longer(CM_Route_Efficiency_df, cols = c(CM_efficiency_score_2_1, CM_efficiency_score_2_2), 
                        names_to = "Round", 
                        values_to = "Score")

# Create line plot for each particiapant
ggplot(CM_Route_Efficiency_df, aes(x = Round, y = Score, group = subject, color = subject)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(label = Score), vjust = -0.5, hjust = 1) +
  labs(x = "Trial", y = "Eficiency Score", color = " ") +
  theme_minimal() +
  scale_x_discrete(labels = c("CM_efficiency_score_2_1" = "Test 1", "CM_efficiency_score_2_2" = "Test 2")) +
  ggtitle("Test 1 v Test 2 on Eficiency Score")

#### get data frame for factor analysis ####

Quant_MoNav_df <- Quant_MoNav_df[,c(2,3,6,8:14)]
Quant_MoNav_df <- Quant_MoNav_df[,c(1:9)]

#get partial correlation matrix for factor analysis (may not be needed):
fa_pcor_matrix_IQ<- pcor_matrix_IQ[c(2,3,6,8:13),c(2,3,6,8:13)]
colnames(fa_pcor_matrix_IQ) <- c("Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                 "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building",
                                 "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")
rownames(fa_pcor_matrix_IQ) <- c("Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                 "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building",
                                 "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")

#get correlation matrix for factor analysis (may not be needed either): <- this is the same as if you did not created a matrix.
fa_cor_matrix <- cor(Quant_MoNav_df, method = "pearson")
colnames(fa_cor_matrix) <- c("Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                 "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building",
                                 "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")
rownames(fa_cor_matrix) <- c("Square Town Pointing", "Square Town Efficiency", "Square Town Map Building",
                                 "Temple Tour Pointing", "Temple Tour Efficiency", "Temple Tour Map Building",
                                 "SILCton Between Route Pointing", "SILCton Within Route Pointing", "SILCton Map Building")


#### EFA ####

#Are the data appropriate for factor analysis?

KMO(Quant_MoNav_df) # you want MSA to be > .60
KMO(fa_pcor_matrix_IQ)
KMO(fa_cor_matrix) #illustrating that this is the same as KMO with the data

bartlett.test(Quant_MoNav_df) # you want K to be a large number p should be significant

#### parallel analysis ####

#How many factors to retain?

#looking at where it crosses simulated data:
par_analysis <- fa.parallel(Quant_MoNav_df, fm = "pa", n.iter = 100)

#looking at factors above 1:
ev<-eigen(cor(Quant_MoNav_df))

#Output of parallel analysis only shows a subset of eigenvalues
#The following creates the full list of eigenvalues

#Read names of variables in the dataframe
names(par_analysis)

#Organize the variable labels into the same order as the output
all_par_val <- data.frame(cbind(par_analysis[[1]], par_analysis[[6]], par_analysis[[5]], par_analysis[[2]], par_analysis[[4]], par_analysis[[3]]))

#Rename the columns
names(all_par_val) <- c(names(par_analysis[1]),
                        names(par_analysis[6]),
                        names(par_analysis[5]),
                        names(par_analysis[2]),
                        names(par_analysis[4]),
                        names(par_analysis[3]))

#Compute proportion of variance explained by each component individually
all_par_val$pro_var_com <- all_par_val$pc.values/3 #divide by number of factors - you can try different numbers at a judgement point 
#(say if 3 is just above simulation, you can try both 2 and 3)

#should this be fa not pc???
 
#Compute proportion of total variance explained by component solutions
all_par_val$pro_cum_var_com <- cumsum(all_par_val$pro_var_com)

all_par_val

#### Velicer's MAP analysis ####
vss_map <- vss(Quant_MoNav_df, 3, "varimax", fm = "pc") #second argument is number of components)

#names(vss_map)

#### PCA Function ####
#A custom function to estimate a PCA for a specific number of components. 
#This function writes output as new objects into the environment
#more about oblimin rotation here: https://medium.com/@baogorek/what-happens-when-you-rotate-confirmatory-factor-analysis-loadings-d597811a6870

pca_est <- function(x) {
  txt.read.in.data <- paste0("df_",formatC(x),"c_oblimin <<- principal(Quant_MoNav_df, ",formatC(x),", rotate = 'oblimin')")
  eval(parse(text=txt.read.in.data))
} 

# change df_norm if you want to change df!

#### First part of PCA ####
#note that many(but not necessarily all) PCA functions scale data for you, 
#manually normalizing the data before this point may give you weird results.
#we will normalize it later on

#Create sequence of integers to pass to the function
comp2 <- seq(1, 2, 1) #second argument is the number of factors 
comp3 <- seq(1, 3, 1) #trying out 2 and 3 factors

#Execute PCA function for 2 to 3 factor solutions
pca_sum2 <- lapply(comp2, pca_est)
pca_sum3 <- lapply(comp3, pca_est)

pca_sum2
pca_sum3

#factor scores are the subject's score on a factor
scores2 <- pca_sum2[[2]]$scores #get factor scores that you can use as observed vars in subsequent analysis
scores3 <- pca_sum3[[3]]$scores 

#### quick break to set up data frames for linear regression between factor scores and small-scale tasks ####
factor_score3_by_self_and_small_df <- as.data.frame(scores3)

self_and_small_df <- MoNav_df[,c(25,27,23,26,46)]

factor_score3_by_self_and_small_df <- cbind(factor_score3_by_self_and_small_df, self_and_small_df)

#correlations to test for linear relationships
#check that these are all good linear correlations:

#3 factor:
factor_score3_by_self_and_small_matrix <- cor(factor_score3_by_self_and_small_df)
factor_score3_by_self_and_small_testRes <- cor.mtest(factor_score3_by_self_and_small_df)

rownames(factor_score3_by_self_and_small_matrix) <- c("factor 2", "factor 3", "factor 1", "SBSOD", "NSQ", "MRT", "PTT-A", "KBIT-IQ")

colnames(factor_score3_by_self_and_small_matrix) <- c("factor 2", "factor 3", "factor 1", "SBSOD", "NSQ", "MRT", "PTT-A", "KBIT-IQ")

rownames(factor_score3_by_self_and_small_testRes$p) <- c("factor 2", "factor 3", "factor 1", "SBSOD", "NSQ", "MRT", "PTT-A", "KBIT-IQ")

colnames(factor_score3_by_self_and_small_testRes$p) <- c("factor 2", "factor 3", "factor 1", "SBSOD", "NSQ", "MRT", "PTT-A", "KBIT-IQ")

corrplot(factor_score3_by_self_and_small_matrix,
         method = "number",
         p.mat = factor_score3_by_self_and_small_testRes$p,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 2,
         pch.col = "#fac228",
         type = "lower",
         tl.srt = 45,
         tl.cex = 1,
         tl.col = 1,
         number.cex = 1,
         cl.cex = 0.5,
         mar=c(0,0,2,0),
         col = "white",
         cl.pos = "n",
         bg = "#2f4f4f",
         diag = FALSE)

#### box plots for lm####
#doesn't matter what df small scale tasks come from for this:
boxplot(factor_score3_by_self_and_small_df$SDT_Style_MRT,ylab = "MRT")
boxplot(factor_score3_by_self_and_small_df$PTT_A,ylab = "PTT-A")
boxplot(factor_score3_by_self_and_small_df$NSQ,ylab = "NSQ")
boxplot(factor_score3_by_self_and_small_df$SBSOD_Avrg,ylab = "SBSOD")

boxplot(factor_score3_by_self_and_small_df$TC2,ylab = "SILCton Factor Scores")
boxplot(factor_score3_by_self_and_small_df$TC3,ylab = "Temple Tour Factor Scores")
boxplot(factor_score3_by_self_and_small_df$TC1, ylab = "Square Town Factor Scores")

#check normality of factors:
hist(factor_score3_by_self_and_small_df$TC1)

hist(log(factor_score3_by_self_and_small_df$TC1 + 3))

hist(factor_score3_by_self_and_small_df$TC2)
hist(factor_score3_by_self_and_small_df$TC3)

desc_factor_scores <- describe(factor_score3_by_self_and_small_df)

#Linear models for factor scores and small scale scores
#3 factor:
lm_model_1 <- lm(scale(log(TC1 + 3)) ~ scale(PTT_A) + scale(SDT_Style_MRT) + scale(NSQ) + scale(SBSOD_Avrg) + scale(KBIT_IQ), data = factor_score3_by_self_and_small_df)
lm_model_2 <- lm(scale(TC2) ~ scale(PTT_A) + scale(SDT_Style_MRT) + scale(NSQ) + scale(SBSOD_Avrg) + scale(KBIT_IQ), data = factor_score3_by_self_and_small_df)
lm_model_3 <- lm(scale(TC3) ~ scale(PTT_A) + scale(SDT_Style_MRT) + scale(NSQ) + scale(SBSOD_Avrg) + scale(KBIT_IQ), data = factor_score3_by_self_and_small_df)

#get RSE and R squared:
summary(lm_model_1)
summary(lm_model_2)
summary(lm_model_3)

#cohens f squared:
cohens_f(lm_model_1)
cohens_f(lm_model_2)
cohens_f(lm_model_3)

#these should look random:
plot(lm_model_1$residuals, pch = 16, col = "red")
plot(lm_model_2$residuals, pch = 16, col = "red")
plot(lm_model_3$residuals, pch = 16, col = "red")

#visualize models:
summ(lm_model_1, scale = FALSE)
summ(lm_model_2, scale = FALSE)
summ(lm_model_3, scale = FALSE)

plot_summs(lm_model_1, lm_model_2, lm_model_3, plot.distributions = TRUE)

avPlots(lm_model_1)
avPlots(lm_model_2)
avPlots(lm_model_3)

gvlma(lm_model_1)
gvlma(lm_model_2)
gvlma(lm_model_3)

#### Back to PCA! ####

#compute the PCA:
df.pca <- princomp(Quant_MoNav_df)
#show the result:
summary(df.pca)

#compute the PCA based on partial correlations (don't do this way):
df.pca_2 <- princomp(fa_pcor_matrix_IQ)
#show the result based on partial correlations (don't do this way):
summary(df.pca_2)

#compute the PCA using prcomp with group excluded but in the df:
df.pca_3 <- princomp(pca_Quant_MoNav_df[, -10],  scale = TRUE)
#show the result:
summary(df.pca_3)

# #factor loadings are the correlation of the original variable with a factor
# pc_load <- pca_sum2[[2]]$loadings #get factor loadings for the factor congruence analysis
# 
# ##Factor congruence across age groups
# #Given two sets of factor loadings, report their degree of #congruence (vector cosine). Although first reported by Burt #(1948), this is frequently known as the Tucker index of factor congruence.
# #https://personality-project.org/r/html/factor.congruence.html
# 
# #Running PCA
# #run PCA for each age group df
# pca_all <- lapply(comp2, pca_est)
# all_load <- pca_all[[2]]$loadings
# all_load <- data.frame(cbind(all_load))
# 
# #Factor 1 and 2
# factor.congruence(all_load$TC1, all_load$TC2,digits=3,use="complete",structure=FALSE)

#explore how the first two components relate to each column 
#using the loadings of each principal component: 
df.pca$loadings[,1:3]

#Scree Plot with percentage of explained variance (compare to the other
fviz_eig(df.pca, barfill = "#999", addlabels = TRUE)

#explore aspects used to graph these here:
var1 <- get_pca_var(df.pca)
var1

corrplot(var1$cos2, is.corr=FALSE)

# Graph of the variables
fviz_pca_var(df.pca, axes = c(1, 2), col.var = "cos2",
             gradient.cols = c("black", "darkorchid3", "darkorange"),
             repel = TRUE)
#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.
#The closer a variable is to the circle of correlations, the better its representation on the factor map (and the more important it is to interpret these components)
#Variables that are closed to the center of the plot are less important for the first components.

#graph of the individuals
fviz_pca_ind(df.pca_3, axes = c(1, 3), label="none", habillage=pca_Quant_MoNav_df$VS_JRD_Group,
             addEllipses=TRUE, ellipse.type = "convex", ellipse.level=0.95)
#individuals that are similar are grouped together on the plot.

#1 and 3 are best represented by dim 1
#1 and 2 are best represented by dim 2 
#2 and 3 are best represented by dim 3 

#### normalize Quant data frames ####
Quant_MoNav_df <- scale(Quant_MoNav_df)
Quant_MoNav_df <- as.data.frame(Quant_MoNav_df)

#### CFA for EFA/PCA factors ####

#build models with latent factors

#1 factor model

#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df) #only pass in continuous vars
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

#Define model to be estimated
m0 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
        Factor1 ~~ 1*Factor1'

#Estimate specified model

m0_fit <- cfa(m0, Quant_MoNav_df, mimic = 'Mplus')

#Summary of fit information

fitMeasures(m0_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

#Return Coefficients

m0_fit_c <- parameterEstimates(m0_fit)

m0_fit_sc <-parameterEstimates(m0_fit, standardized = T)

#Visualize model result
#Raw coefficients
#semPaths(m1_fit, whatLabels = "par", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
#         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#Standardized coefficients
semPaths(m0_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#model for factor 1

#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df) #only pass in continuous vars
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

#Define model to be estimated
m1 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
        Factor1 ~~ 1*Factor1'

#+ TT_JRD_Avrg_Angular_Error +
#VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr + 
#  TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr + TT_MB_Outline_Euclidian_Rsqr

#Estimate specified model

m1_fit <- cfa(m1, Quant_MoNav_df, mimic = 'Mplus')

#Summary of fit information

fitMeasures(m1_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

#Return Coefficients

m1_fit_c <- parameterEstimates(m1_fit)

m1_fit_sc <-parameterEstimates(m1_fit, standardized = T)

#Visualize model result
#Raw coefficients
#semPaths(m1_fit, whatLabels = "par", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
#         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#Standardized coefficients
semPaths(m1_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#model for factor 2

#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

#Define model to be estimated
m2 <- 'Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
        Factor2 ~~ 1*Factor2'

#Estimate specified model

m2_fit <- cfa(m2, Quant_MoNav_df, mimic = 'Mplus')

#Summary of fit information

fitMeasures(m2_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

#Return Coefficients

m2_fit_c <- parameterEstimates(m2_fit) 

m2_fit_sc <-parameterEstimates(m2_fit, standardized = T)

#Visualize model result
#Raw coefficients
#semPaths(m2_fit, whatLabels = "par", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
#         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#Standardized coefficients
semPaths(m2_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#put the two models together

m3 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor1 ~~ 1*Factor1
            Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor2 ~~ 1*Factor2'

m3_fit <- cfa(m3, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m3_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m3_fit_c <- parameterEstimates(m3_fit) 

m3_fit_sc <-parameterEstimates(m3_fit, standardized = T)

semPaths(m3_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)


#Standardized coefficients
semPaths(m3_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#put the three models together

cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal


m4 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr
            Factor1 ~~ 1*Factor1
            Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor2 ~~ 1*Factor2
            Factor3 =~ NA*TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor3 ~~ 1*Factor3'

m4_fit <- cfa(m4, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m4_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m4_fit_c <- parameterEstimates(m4_fit) 

m4_fit_sc <-parameterEstimates(m4_fit, standardized = T)

semPaths(m4_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=.75,edge.color="black",
         sizeMan=9,sizeLat=5,fade=FALSE,esize=2,asize=2)

#Standardized coefficients
semPaths(m4_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black",
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### Apriori EFA ####

#2 factors: Navigation Environment: (tasks reflect similar performance, differing by environment type)
#virtual environments vs real world (Virtual SILCton and Square Town vs Temple Tour)

cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

m5 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor1 ~~ 1*Factor1
            Factor2 =~ NA*TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor2 ~~ 1*Factor2'

m5_fit <- cfa(m5, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m5_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m5_fit_c <- parameterEstimates(m5_fit) 

m5_fit_sc <-parameterEstimates(m5_fit, standardized = T)

semPaths(m5_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=.75,edge.color="black",
         sizeMan=9,sizeLat=5,fade=FALSE,esize=2,asize=2)

#3 factor: Navigation Task (different environments reflect similar performance, but differ by task type)

cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

m6 <- 'Factor1 =~ NA*CM_MB_Euclidian_Rsqr + VS_MB_Rsqr + TT_MB_Blank_Euclidian_Rsqr
            Factor1 ~~ 1*Factor1
            Factor2 =~ NA*TT_JRD_Avrg_Angular_Error + CMJRD_mean_error + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error
            Factor2 ~~ 1*Factor2
            Factor3 =~ NA*TT_Route_Efficiency + CM_efficiency_score_all
            Factor3 ~~ 1*Factor3'

m6_fit <- cfa(m6, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m6_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m6_fit_c <- parameterEstimates(m6_fit) 

m6_fit_sc <-parameterEstimates(m6_fit, standardized = T)

semPaths(m6_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=.75,edge.color="black",
         sizeMan=9,sizeLat=5,fade=FALSE,esize=2,asize=2)


