#### setup (RUN ONCE PER SESSION) ####

#NOTE: Cogmaps (cognitive maps) was the name of the study in which the Square Town paradigm was used. Here these names are used interchangeably

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
library(stargazer) #stargzer tables with confidence intervals
#load libraries for special plotting:
library(corrplot) #for plotting correlation plots
library(car) #added av plots
library(jtools) #for linear regression visualizations
library(factoextra) #for special pca plots (not used here)
library(semPlot) #for CFA plotting
library(performance) #checking multicolinearity of models (VIF)

#important setup step:
#this makes the corr plots look nice!

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

#read in csv and create main data frame
MoNav_df <- read.csv(file = 'RELIABLE_AND_VALID_NAVIGATION_TESTING_Aggregate_Data.csv', header = TRUE)

#### cleaning and creating main data frames ####

#determine number of rows in MoNav_df
n <- nrow(MoNav_df)

#Add a new column called "subject" with increment values
MoNav_df$subject <- 1:n

#this next part is subject to change based on data frame formatting:

#remove columns we should not include in analysis. In this case:
MoNav_df <- MoNav_df[, !colnames(MoNav_df) %in% c("day_2", "day_1")]

#now we want to ensure variables are ordered how we want, namely that each paradigm's data is grouped together:
#hint: use summary function to get the names and then copy paste in the preferred order:
MoNav_df <- MoNav_df[, c("subject", "ID", "Round", "TT_group",
                         "CMJRD_mean_acc", "CMJRD_mean_error", "CM_efficiency_score_all", 
                         "CM_MB_Euclidian_Rsqr",
                         "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr", "TT_MB_Outline_Euclidian_Rsqr",
                         "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error", "VS_MB_Rsqr",
                         "SDT_Style_MRT", "SBSOD_Avrg", "PTT_A", "NSQ",
                         "Age", "Gender", "Race", "Spanish_Hispanic_Latinx_ethnicity", "KBIT_IQ")]

#### Reverse Score necessary quant columns ####
MoNav_df$CMJRD_mean_error <- (MoNav_df$CMJRD_mean_error) * (-1)
MoNav_df$CM_efficiency_score_all <- (MoNav_df$CM_efficiency_score_all) * (-1)
MoNav_df$TT_JRD_Avrg_Angular_Error <- (MoNav_df$TT_JRD_Avrg_Angular_Error) * (-1)
MoNav_df$TT_Route_Efficiency <- (MoNav_df$TT_Route_Efficiency) * (-1)
MoNav_df$VS_Diff_JRD_Avrg_Angular_Error <- (MoNav_df$VS_Diff_JRD_Avrg_Angular_Error) * (-1)
MoNav_df$VS_Same_JRD_Avrg_Angular_Error <- (MoNav_df$VS_Same_JRD_Avrg_Angular_Error) * (-1)

#### Create other data frames here: ####

#Numeric Data frame
Quant_MoNav_df <- MoNav_df[, c("CMJRD_mean_error", "CM_efficiency_score_all",
                               "CM_MB_Euclidian_Rsqr",
                               "TT_JRD_Avrg_Angular_Error", "TT_Route_Efficiency", "TT_MB_Blank_Euclidian_Rsqr",
                               "VS_Diff_JRD_Avrg_Angular_Error", "VS_Same_JRD_Avrg_Angular_Error","VS_MB_Rsqr",
                               "SDT_Style_MRT","SBSOD_Avrg", "PTT_A", "NSQ", "Age", "KBIT_IQ")]

#### preliminary t.tests for cleaning data ####

#we're performing this test to determine whether we should only use the MB blank from TT
t.test(MoNav_df$TT_MB_Blank_Euclidian_Rsqr, MoNav_df$TT_MB_Outline_Euclidian_Rsqr, var.equal = TRUE)

#visualize:
ggplot(data = MoNav_df, aes(x = TT_MB_Blank_Euclidian_Rsqr, y = TT_MB_Outline_Euclidian_Rsqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  #Add y = x line
  labs(title = "Temple Tour Map Building", x = "Blank Score", y = "Outline Score")

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
hist(Quant_MoNav_df$Age) #we visualize this better later so it is non-essential here

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

#### Create partial correlation matrices ####

#create a partial correlation matrix while controlling for age KBIT IQ
#KBIT-IQ must be last with this method,though you can also do this by specifying variables
#The way I'm doing this now requires the pcor.test variables to be specified by numeric index
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
#add non-standardized age to dataframe
comp_bind <- cbind(comp_bind, Age = Quant_MoNav_df$Age)

#Initialize the matrix to store partial correlation coefficients
comp_pcor_matrix_IQ <- matrix(NA, ncol = ncol(comp_bind)-2, nrow = ncol(comp_bind)-2)
#Loop through each pair of variables
for (i in 1:(ncol(comp_bind)-2)) {
  for (j in 1:(ncol(comp_bind)-2)) {
    if (i == j) {
      #Set diagonal elements to 1
      comp_pcor_matrix_IQ[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function, unlike previously where we used pcor.test
      comp_partial_cor <- pcor(comp_bind[, c(i, j, (ncol(comp_bind)-1), ncol(comp_bind))])
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
comp_testRes <- matrix(NA, ncol = ncol(comp_bind)-2, nrow = ncol(comp_bind)-2)
#Loop through each pair of variables
for (i in 1:(ncol(comp_bind)-2)) {
  for (j in 1:(ncol(comp_bind)-2)) {
    if (i == j) {
      #Set diagonal elements to 1
      comp_testRes[i, j] <- 1
    } else if (i < j) {
      #Compute partial correlation using pcor function
      comp_partial_cor_test_res <- pcor(comp_bind[, c(i, j, (ncol(comp_bind)-1), ncol(comp_bind))])
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

#### get data frame for factor analysis ####

#first we just want navigation tasks:
Quant_MoNav_df <- Quant_MoNav_df[,c(2,3,6,8:13)]

#### normalize Quant data frames ####

#normalize:
Quant_MoNav_df <- scale(Quant_MoNav_df)

#### CFA 1 factor model ####
#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

#Define model to be estimated
m1 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor1 ~~ 1*Factor1'

#Estimate specified model
m1_fit <- cfa(m1, Quant_MoNav_df, mimic = "Mplus")

#Summary of fit information
fitMeasures(m1_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", 
                      "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

#Return Coefficients
m1_fit_c <- parameterEstimates(m1_fit) 

m1_fit_sc <-parameterEstimates(m1_fit, standardized = T)

#Standardized coefficients
semPaths(m1_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black", 
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### CFA 2 factor model ####
#Navigation Environment: (tasks reflect similar performance, differing by environment type)
#virtual environments vs real world (Virtual SILCton and Square Town vs Temple Tour)
#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

#Define model to be estimated
m2 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor2 =~ NA*TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor1 ~~ 1*Factor1
            Factor2 ~~ 1*Factor2'

#Estimate specified model
m2_fit <- cfa(m2, Quant_MoNav_df, mimic = "Mplus")

#Summary of fit information
fitMeasures(m2_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", 
                      "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

#Return Coefficients
m2_fit_c <- parameterEstimates(m2_fit) 

m2_fit_sc <-parameterEstimates(m2_fit, standardized = T)

#Standardized coefficients
semPaths(m2_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black", 
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### CFA 2 factor model ####
#(gridded vs non-gridded)
#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

#Define model to be estimated
m3 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_efficiency_score_all + CM_MB_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor1 ~~ 1*Factor1
            Factor2 ~~ 1*Factor2'

#Estimate specified model
m3_fit <- cfa(m3, Quant_MoNav_df, mimic = "Mplus")

#Summary of fit information
fitMeasures(m3_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", 
                      "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

#Return Coefficients
m3_fit_c <- parameterEstimates(m3_fit) 

m3_fit_sc <-parameterEstimates(m3_fit, standardized = T)

#Standardized coefficients
semPaths(m3_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black", 
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### CFA 3 factor model ####
#Navigation Task (different environments reflect similar performance, but differ by task type)
#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

m4 <- 'Factor1 =~ NA*CM_MB_Euclidian_Rsqr + VS_MB_Rsqr + TT_MB_Blank_Euclidian_Rsqr
            Factor2 =~ NA*TT_JRD_Avrg_Angular_Error + CMJRD_mean_error + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error
            Factor3 =~ NA*TT_Route_Efficiency + CM_efficiency_score_all
            Factor1 ~~ 1*Factor1
            Factor2 ~~ 1*Factor2
            Factor3 ~~ 1*Factor3'

m4_fit <- cfa(m4, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m4_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", 
                      "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m4_fit_c <- parameterEstimates(m4_fit) 

m4_fit_sc <-parameterEstimates(m4_fit, standardized = T)

semPaths(m4_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black", 
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### CFA 3 factor (optimal) model ####
#(paradigm based)
#get covariance matrices
cov_mat1 <- cov(Quant_MoNav_df)
cov_mat1[upper.tri(cov_mat1)] <- NA #Means to assign NA to the elements above the diagonal

m5 <- 'Factor1 =~ NA*CM_MB_Euclidian_Rsqr + CMJRD_mean_error + CM_efficiency_score_all
            Factor2 =~ NA*VS_MB_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error
            Factor3 =~ NA*TT_MB_Blank_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency
            Factor1 ~~ 1*Factor1
            Factor2 ~~ 1*Factor2
            Factor3 ~~ 1*Factor3'

m5_fit <- cfa(m5, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m5_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", 
                      "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m5_fit_c <- parameterEstimates(m5_fit) 

m5_fit_sc <-parameterEstimates(m5_fit, standardized = T)

semPaths(m5_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=1.25,edge.color="black", 
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### add in other factors to test with the optimal model ####
#we want to add any of the predictors to the factor score df:
Age_Gender_IQ <- MoNav_df[,c(28,30,46)]
#make sure gender can be used for CFA:
Age_Gender_IQ$Gender <- factor(Age_Gender_IQ$Gender, levels = c("Male","Female"))
Age_Gender_IQ$KBIT_IQ <- scale(Age_Gender_IQ$KBIT_IQ)
#add in gender, age, IQ:
Quant_MoNav_df <- cbind(Quant_MoNav_df, Age_Gender_IQ)

#### CFA 3 factor (optimal) model with other factors ####
#(paradigm based)

m6 <- 'Factor1 =~ NA*CM_MB_Euclidian_Rsqr + CMJRD_mean_error + CM_efficiency_score_all
            Factor2 =~ NA*VS_MB_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error
            Factor3 =~ NA*TT_MB_Blank_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency
            Factor1 ~~ 1*Factor1
            Factor2 ~~ 1*Factor2
            Factor3 ~~ 1*Factor3

            Factor1 ~ a*Age + b*KBIT_IQ + c*Gender
            Factor2 ~ d*Age + e*KBIT_IQ + f*Gender
            Factor3 ~ g*Age + h*KBIT_IQ + i*Gender'

m6_fit <- cfa(m6, Quant_MoNav_df, mimic = "Mplus")

fitMeasures(m6_fit, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", 
                      "rmsea.ci.upper", "rmsea.pvalue", "BIC", "AIC")) 

m6_fit_c <- parameterEstimates(m6_fit) 

m6_fit_sc <-parameterEstimates(m6_fit, standardized = T)

semPaths(m6_fit, whatLabels = "std", nCharNodes = 0, rotation = 2, edge.label.cex=.25,edge.color="black", 
         sizeMan=10,sizeLat=10,fade=FALSE,esize=2,asize=2)

#### Power analyses adding CFA model 6 ####

#model for two factors, df = 8
#minimum comparison that we can do is between two factors to get degrees of freedom for power analysis, N = 85
m_min <- 'Factor1 =~ NA*CMJRD_mean_error + CM_MB_Euclidian_Rsqr + CM_efficiency_score_all
           Factor1 ~~ 1*Factor1
           Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error +VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
           Factor2 ~~ 1*Factor2'

#3-factor model we care about, df = 24
m_3f <- 'Factor1 =~ NA*CMJRD_mean_error + CM_MB_Euclidian_Rsqr + CM_efficiency_score_all
            Factor1 ~~ 1*Factor1
            Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error +VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor2 ~~ 1*Factor2
            Factor3 =~ NA*TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor3 ~~ 1*Factor3'

#2-factor model ve vs real, df = 25
m1_2f <- 'Factor1 =~ NA*CMJRD_mean_error + CM_MB_Euclidian_Rsqr + CM_efficiency_score_all+VS_Diff_JRD_Avrg_Angular_Error +VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
Factor1 ~~ 1*Factor1
            Factor2 =~ NA*TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
            Factor2 ~~ 1*Factor2'

#2-factor model grid vs not grid, df = 25
m2_2f <- 'Factor1 =~ NA*CMJRD_mean_error + CM_MB_Euclidian_Rsqr + CM_efficiency_score_all+TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr
Factor1 ~~ 1*Factor1
            Factor2 =~ NA*VS_Diff_JRD_Avrg_Angular_Error +VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
            Factor2 ~~ 1*Factor2'

#single factor, df = 25
m1 <- 'Factor1 =~ NA*CMJRD_mean_error + CM_MB_Euclidian_Rsqr + CM_efficiency_score_all+TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency + TT_MB_Blank_Euclidian_Rsqr+VS_Diff_JRD_Avrg_Angular_Error +VS_Same_JRD_Avrg_Angular_Error + VS_MB_Rsqr
Factor1 ~~ 1*Factor1'

#three factor with age, gender, kbit, df = 42
m6 <- 'Factor1 =~ NA*CM_MB_Euclidian_Rsqr + CMJRD_mean_error + CM_efficiency_score_all
            Factor2 =~ NA*VS_MB_Rsqr + VS_Diff_JRD_Avrg_Angular_Error + VS_Same_JRD_Avrg_Angular_Error
            Factor3 =~ NA*TT_MB_Blank_Euclidian_Rsqr + TT_JRD_Avrg_Angular_Error + TT_Route_Efficiency
            Factor1 ~~ 1*Factor1
            Factor2 ~~ 1*Factor2
            Factor3 ~~ 1*Factor3

            Factor1 ~ a*Age + b*KBIT_IQ + c*Gender
            Factor2 ~ d*Age + e*KBIT_IQ + f*Gender
            Factor3 ~ g*Age + h*KBIT_IQ + i*Gender'


semPower.getDf(m6) #get degrees of freedom for each model

power <- semPower.aPriori(effect = .15, effect.measure =
                            'RMSEA', alpha = .05, beta = .20, df = 42) #replace df for degrees of freedom for each type of model
print(power$requiredN)

#### set up data frames for linear regression between factor scores and small-scale tasks ####

#Factor scores from CFA model: https://rdrr.io/cran/lavaan/man/lavPredict.html

#what are the factor scores we use?
#These come from the optimal model CFA
#They are the estimated values for the latent variables (Factors) in the model
#The regression approach shows individuals placement on each factor and comes from:
#Thurstone, L. L. (1935). The vectors of mind. University of Chicago Press. (pp. 226-231).
m5_factor_scores <- lavPredict(m5_fit, method = "regression", type = "lv")

m5_factor_scores_df <- as.data.frame(m5_factor_scores)

#we want to add any of the predictors to the factor score df:
self_and_small_df <- MoNav_df[,c(25,27,23,26,28,30,46)]
#make sure gender can be used for regression:
self_and_small_df$Gender  <- factor(self_and_small_df$Gender, levels = c("Male", "Female"))

m5_factor_scores_df <- cbind(m5_factor_scores_df, self_and_small_df)

#check for missing data:
levels(m5_factor_scores_df$Gender)

#### box plots for lm####
#doesn't matter what df small scale tasks come from for this:
boxplot(m5_factor_scores_df$SDT_Style_MRT ,ylab = "MRT")
boxplot(m5_factor_scores_df$PTT_A,ylab = "PTT-A")
boxplot(m5_factor_scores_df$NSQ,ylab = "NSQ")
boxplot(m5_factor_scores_df$SBSOD_Avrg,ylab = "SBSOD")
boxplot(m5_factor_scores_df$Age,ylab = "Age")
boxplot(m5_factor_scores_df$KBIT_IQ,ylab = "IQ")

boxplot(m5_factor_scores_df$Factor2,ylab = "SILCton Factor Scores")
boxplot(m5_factor_scores_df$Factor3,ylab = "Temple Tour Factor Scores")
boxplot(m5_factor_scores_df$Factor1, ylab = "Square Town Factor Scores")

#check normality of factors:
hist(m5_factor_scores_df$Factor1)
hist(m5_factor_scores_df$Factor2)
hist(m5_factor_scores_df$Factor3)

desc_factor_scores <- describe(m5_factor_scores_df)

##### Linear models for factor scores and small scale scores ####
#3 factor:
lm_model_1 <- lm(scale(Factor1) ~ scale(PTT_A) + scale(SDT_Style_MRT) + scale(NSQ) + scale(SBSOD_Avrg) + scale(Age) + Gender + scale(KBIT_IQ), data = m5_factor_scores_df)
lm_model_2 <- lm(scale(Factor2) ~ scale(PTT_A) + scale(SDT_Style_MRT) + scale(NSQ) + scale(SBSOD_Avrg) + scale(Age) + Gender + scale(KBIT_IQ), data = m5_factor_scores_df)
lm_model_3 <- lm(scale(Factor3) ~ scale(PTT_A) + scale(SDT_Style_MRT) + scale(NSQ) + scale(SBSOD_Avrg) + scale(Age) + Gender + scale(KBIT_IQ), data = m5_factor_scores_df)

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

plot(lm_model_1)
plot(lm_model_2)
plot(lm_model_3)

#### Check multicollinearity for linear models ####
#https://rdrr.io/cran/performance/man/check_collinearity.html

check_collinearity(lm_model_1)
check_collinearity(lm_model_2)
check_collinearity(lm_model_3)

#### Cook's distance ####
#https://rpubs.com/DragonflyStats/Cooks-Distance

cooks.distance(lm_model_1)
cooks.distance(lm_model_2)
cooks.distance(lm_model_3)


#### Stargazer tables with confidence intervals ####
#visualize up to 5 models at a time
#the html output will give you a .html file in your directory that you can open in a browser
stargazer(lm_model_1,lm_model_2,lm_model_3,type="html",out="linear.models.html",
          report=('vcsp*'), #reports SE, 95% CI, p values and stars)
          star.cutoffs=c(.05, .01, .001), 
          ci = TRUE, ci.level = .95,
          single.row = TRUE,
          notes = "95% CI in parentheses.")

#there's option to specify covariate and outcome variable names



