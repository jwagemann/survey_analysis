source('data_survey_functions.R') # Load dataUse_freq

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

#Load country of residence

df_11 <- as.data.frame(df_new['X1.1'])
colnames(df_11) <- c('cor')
df_11_freq <- as.data.frame(count(df_11))
df_11_freq <- df_11_freq[df_11_freq$freq >=10,]

is.na(df_11) <- df_11 != c('Austria') & df_11 !=c('Germany') & df_11 !=c('Italy') & df_11 !=c('Spain') & df_11 !=c('United Kingdom') & df_11 !=c('United States of America') & df_11 !=c('Canada')

df_11$region <- ifelse(df_11$cor == "United States of America" | df_11$cor == 'Canada','United States of America & Canada', 'Europe')

# Load work sector
df_21 <- as.data.frame(df_new[,'X2.1'])
colnames(df_21) <- c('work_sector')

df_21_split <- separate_rows(df_21,work_sector, sep=';')
df_21_freq <- count(df_21_split)
df_21_freq$perc <- df_21_freq$freq / no_of_respondents * 100

# Load programming languages
df_41 <- as.data.frame(df_new[,'X4.1'])
colnames(df_41) <- c('programming.language')
df_41$programming.language <- as.character(df_41$programming.language)

df_41_split <- separate_rows(df_41,programming.language, sep=';')
df_41_freq <- count(df_41_split)
df_41_freq$perc <- df_41_freq$freq / no_of_respondents * 100

#Load desktop software

df_421 <- as.data.frame(df_new[,'X4.2.1'])
colnames(df_421) <- 'software'
df_421_split <- separate_rows(df_421,software, sep=';')
df_421_freq <- count(df_21_split)
df_421_freq$perc <- df_21_freq$freq / no_of_respondents * 100



# Load data user
df_221 <- as.data.frame(df_new[,'X2.2.1'])
colnames(df_221) <- c('data_user_type')
df_221$data_user_type <- as.character(df_221$data_user_type)

# Load age groups
df_14 <- as.data.frame(df_new[,'X1.4'])
colnames(df_14) <- c('age_group')
df_14$age_group <- as.character(df_14$age_group)


dummies_du <- getDummies(df_221,1)
dummies_du <- dummies_du[,c(-5,-6)]

# Split data formats and make dummy table for each entry
dummies_pl <- getDummies(df_41,1)
dummies_pl <- dummies_pl[,c(-5,-6,-14)]

dummies_ws <- getDummies(df_21,1)
dummies_ws <- dummies_ws[,-7]

dummies_ag <- getDummies(df_14,1)
dummies_ag <- dummies_ag[,-6]

colNames_vector_ws <- c('Non profit','University','Intergovernmental organisation','Government', 'Established company', 'Start-up')
colNames_vector_du <- c('Climate reanalysis','Daily meteorological data', 'Seasonal forecast data', 'Environmental forecast data', 'Earth Observations','Other Geospatial data','Value-added products')
colNames_vector_dut <- c('Software Developer', 'Researcher', 'Project manager', 'Data Analyst', 'Team leader', 'Data Scientist', 'Student')
colNames_vector_ag <- c('30-40 years', '20-30 years', '50-60 years', '40-50 years', '>60 years', '<20 years')
colNames_vector_cor <- as.vector((count(df_11$cor)[-8,])$x)
colNames_vector_region <- c('Europe', 'United States of America & Canada')

