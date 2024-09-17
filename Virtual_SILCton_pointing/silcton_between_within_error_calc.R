### Calculate between & within error for new Silcton participants ###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidyr)

raw_data <- read.csv('VSJRD_6_6_24.csv')

# calculate between & within error, group by subject and date
error_data <- raw_data %>%
  group_by(participant, date, same.or.different.route) %>%
  summarise(mean_error = mean(abs.error))

#get a wide version:
error_data_long <- pivot_wider(error_data,
                               id_cols = c(participant, date),
                               names_from = same.or.different.route,
                               values_from = mean_error)
#get group:
error_data_long$group <- ifelse(error_data_long$same <= 27.5 & error_data_long$different <= 40, 1,
                        ifelse(error_data_long$same > 27.5 & error_data_long$different <= 40, 4,
                               ifelse(error_data_long$same > 27.5 & error_data_long$different > 40, 3, 2)))

group_counts <- table(error_data_long$group)

print(group_counts)

write.csv(error_data_long, "SILCton_PointingErrors_6_6_24.csv")
