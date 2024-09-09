### Calculate error in path-based JRD ###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(plyr)
library(tidyverse)
library(ggplot2)
library(igraph)
library(reshape2)
library(psych)

## generate graph plot

nodes <- read.csv("nodes_oneway.csv",header=T,as.is=T)
links <- read.csv("edges_oneway.csv",header=T,as.is=T)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net # inspect

plot(net)

# get steps between each pair of locations
path_dist <- distances(net, mode="out")
# transpose to match existing file
path_dist <- t(path_dist)
path_dist <- as.data.frame(path_dist)
# convert to long format to match existing file
path_long <- tidyr::gather(path_dist, loc, steps, 1:16, factor_key=TRUE)

path_long <- filter(path_long, steps != 0)


## get distances
dist <- distances(net, v = V(net), to = V(net), mode = c("out"), 
          weights = NULL, algorithm = c("automatic"))

# transpose so each origin is a column instead of row
# & convert to long form
m2 <- melt(dist)[melt(upper.tri(dist))$value,]
names(m2) <- c("from", "to", "distance")

## analysis of path-based JRD ####
# read in direction for every location: coded manually
loc_dir_all <- read.table("oneway_graph_all_connections.csv", header=T, sep=",")

# read in data files one at a time and calculate
filelist <- list.files(path="data", pattern="*.csv*", full.names=T)

jrd_out_list <- list()

for (file in 1:length(filelist)){
  
  data <- read.csv(filelist[file],  fileEncoding = 'UTF-8-BOM')
  
  # filter so you have just path based JRD trials & make degrees absolute
  jrd_path <- filter(data, facing_angle != "NA")
  
  jrd_path <- jrd_path %>%
    select('sub','date','facing_angle','probe_loc_code','target_loc_code',
           'ego_direction','deg_diff','dist_diff','deg_tertile','dist_tertile',
           'probe_object','target_object','facing_dir','angle_degrees')
  
  #create distinct subject IDs:
  jrd_path$subject_distinct <- paste(jrd_path$sub, jrd_path$date, sep = "_")
  
  jrd_path$angle_degrees <- abs(jrd_path$angle_degrees)
  
  # recode approximate angle estimates with precise ones
  # they are approximate because people could click anywhere within the stars
  ##### this will be edited in the future!!! and only precise angles will be recorded
  jrd_path$angle_degrees <- with(jrd_path, ifelse(
    angle_degrees >= 0 & angle_degrees <= 10, 0, ifelse(
      angle_degrees >= 80 & angle_degrees <= 100, 90, ifelse(
        angle_degrees >= 170 & angle_degrees <= 190, 180, 'whoops'))))
  
  jrd_path$angle_degrees <- as.numeric(jrd_path$angle_degrees)

  # now, append the correct egocentric angle
  jrd_path <- merge(jrd_path, loc_dir_all, by=c('probe_loc_code','target_loc_code','facing_angle','ego_direction'))
  
  jrd_path$dir_error <- abs(jrd_path$angle_degrees - jrd_path$ego_direction)
  jrd_path$corr[jrd_path$dir_error > 0] <- 0
  jrd_path$corr[jrd_path$dir_error == 0] <- 1
  
  jrd_out_list[[file]] <- jrd_path

  }

jrd_out_df <- bind_rows(jrd_out_list)

#create a seperate df to get omega
#selcted columns
jrd_omega_df <- jrd_out_df[,c(15,18)]
#we need a distinct question label:
jrd_omega_df <- jrd_omega_df %>%
  group_by(subject_distinct) %>%
  mutate(question = row_number()) %>%
  ungroup()

#now reshape for omega function:
# we need each subject as an observation and each question as a variable
jrd_omega_df_reshaped <- jrd_omega_df %>%
  pivot_wider(
    names_from = question,
    values_from = dir_error
  )

jrd_omega_df_reshaped <- jrd_omega_df_reshaped[,-1]

#now try calculating omega:
omega(jrd_omega_df_reshaped)

# summarize per subject
jrd_per_sub <- jrd_out_df %>%
  group_by(subject_distinct) %>%
  summarise(mean_acc = mean(corr), mean_error = mean(dir_error))

write.csv(jrd_per_sub, '8_19_24_CM_path_based_per_sub.csv', row.names=F)
# read in once created so you don't have to re-do the above steps every time
#jrd_out_df <- read.table("pilot_data_path.csv", header=T, sep=",")

## plot
jrd_out_df$subject_distinct <- as.factor(jrd_out_df$subject_distinct)

ggplot(jrd_out_df, aes(x = ego_direction, y = dir_error, color=subject_distinct, group=subject_distinct)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se = F) +
  theme_minimal(base_size=20)

jrd_out_df$error <- as.numeric(as.character(jrd_out_df$dir_error))
jrd_out_df$ego_direction <- as.numeric(as.character(jrd_out_df$ego_direction))
jrd_out_df$subject_distinct <- as.factor(jrd_out_df$subject_distinct)

jrd_sum <- jrd_out_df %>%
  group_by(subject_distinct, facing_dir) %>%
  summarise(mean_error = mean(error))

ggplot(jrd_sum, aes(x = facing_dir, y = mean_error, group=facing_dir)) +
  geom_boxplot(size=2) +
  geom_jitter(width = .25, size=2) +
  theme_minimal(base_size=20)

# summarise per subject
data_per_sub_path <- jrd_out_df %>%
  group_by(subject_distinct) %>%
  summarise(mean_error = mean(error))
data_per_sub_path$grouping_var <- 1


ggplot(data_per_sub_path, aes(x = grouping_var, y = mean_error, group=grouping_var)) +
  geom_boxplot(size=.5, width = .25) +
  geom_jitter(width = .1, size=2) +
  theme_minimal(base_size=20) +
  xlab("")

#this does not work right now because ego_direction is not in the dataframe:
ggplot(data_per_sub_path, aes(x = ego_direction, y = mean_error, color=subject_distinct, group=ego_direction)) +
  geom_boxplot(size=2) +
  geom_point(size=2) +
  theme_minimal(base_size=20)


# summarise per subject: accuracy between 0 and 1
data_per_sub_path <- jrd_out_df %>%
  group_by(subject_distinct) %>%
  summarise(mean_error = mean(error), mean_acc = mean(corr))

cor.test(data_per_sub_path$mean_error, data_per_sub_path$mean_acc)
