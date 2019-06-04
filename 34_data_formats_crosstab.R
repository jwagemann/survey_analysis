source('31_data_use_prepare.R')
df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

# Load data formats
df_formats <- df_new[,'X3.4']
df_formats <- data.frame(df_formats)
df_formats$df_formats <- as.character(df_formats$df_formats)

#Load work sector data and create a dummy table
df_work_sector <- data.frame(df_new[,'X2.1'])
df_work_sector$df_new....X2.1.. <- as.character(df_work_sector$df_new....X2.1..)
work_sector_cols <- unique(unlist(strsplit(df_work_sector$df_new....X2.1.., ";", fixed = TRUE)))
dummies_work_sector <- sapply(work_sector_cols, function(co)grepl(co,df_work_sector$df_new....X2.1..))



colNames_vector <- c('Climate reanalysis','Daily meteorological data', 'Seasonal forecast data', 'Environmental forecast data', 'Earth Observations','Other Geospatial data','Value-added prodcuts')

dataUse_freq <- dataUse_freq[,c('A1', 'B1','C1','D1','E1','F1','G1')]
colnames(dataUse_freq) <- colNames_vector

# Get all data formats
test_formats_cols <- unique(unlist(strsplit(df_formats$df_formats, ";", fixed = TRUE)))

# Split data formats and make dummy table for each entry
dummies_formats <- sapply(test_formats_cols, function(co)grepl(co,df_formats$df_formats))

# Summaries columns
formats_freq_all <- colSums(dummies_formats[,-12])
formats_freq_all$perc <- formats_freq_all / no_of_respondents

# Crosstab users of specific data types and data formats 
df_crosstab <- data.frame(dataUse_freq, dummies_formats[,-12])
df_subset_A <- subset(df_crosstab[,c(1,8:18)], (!is.na(df_crosstab[,1])))
df_subset_B <- subset(df_crosstab[,c(2,8:18)], (!is.na(df_crosstab[,2])))
df_subset_C <- subset(df_crosstab[,c(3,8:18)], (!is.na(df_crosstab[,3])))
df_subset_D <- subset(df_crosstab[,c(4,8:18)], (!is.na(df_crosstab[,4])))
df_subset_E <- subset(df_crosstab[,c(5,8:18)], (!is.na(df_crosstab[,5])))
df_subset_F <- subset(df_crosstab[,c(6,8:18)], (!is.na(df_crosstab[,6])))
df_subset_G <- subset(df_crosstab[,c(7,8:18)], (!is.na(df_crosstab[,7])))

crosstab_final <- rbind(formats_freq_all, colSums(df_subset_A[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_B[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_C[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_D[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_E[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_F[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_G[,2:12]))

rownames(crosstab_final) <- c('all',colNames_vector)

# Crosstab between worksectors and specific data formats
df_crosstab_df_worksector <- data.frame(dummies_work_sector, dummies_formats[,-12])
df_subset_A <- subset(df_crosstab_df_worksector[,c(1,8:18)], (df_crosstab_df_worksector[,1]==TRUE))
df_subset_B <- subset(df_crosstab_df_worksector[,c(2,8:18)], (df_crosstab_df_worksector[,2]==TRUE))
df_subset_C <- subset(df_crosstab_df_worksector[,c(3,8:18)], (df_crosstab_df_worksector[,3]==TRUE))
df_subset_D <- subset(df_crosstab_df_worksector[,c(4,8:18)], (df_crosstab_df_worksector[,4]==TRUE))
df_subset_E <- subset(df_crosstab_df_worksector[,c(5,8:18)], (df_crosstab_df_worksector[,5]==TRUE))
df_subset_F <- subset(df_crosstab_df_worksector[,c(6,8:18)], (df_crosstab_df_worksector[,6]==TRUE))
df_subset_G <- subset(df_crosstab_df_worksector[,c(7,8:18)], (df_crosstab_df_worksector[,7]==TRUE))


work_sector_vec <- c('all','Non.profit', 'University', 'Intergovernmental.organisation', 'Government', 'Established company', 'Start up','Other')

crosstab_final <- rbind(colSums(df_crosstab_df_worksector[8:18]), colSums(df_subset_A[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_B[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_C[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_D[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_E[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_F[,2:12]))
crosstab_final <- rbind(crosstab_final, colSums(df_subset_G[,2:12]))

rownames(crosstab_final) <- work_sector_vec
