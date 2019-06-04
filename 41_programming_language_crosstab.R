source('31_data_use_prepare.R') # Load dataUse_freq
source('data_survey_functions.R') # Load dataUse_freq
library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

dataUse_freq <- dataUse_freq[,c('A1', 'B1','C1','D1','E1','F1','G1')]

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
colNames_vector_du <- c('Software Developer', 'Researcher', 'Project manager', 'Data Analyst', 'Team leader', 'Data Scientist', 'Student')
colNames_vector_ag <- c('30-40 years', '20-30 years', '50-60 years', '40-50 years', '>60 years', '<20 years')

crosstab_ws_pl <- getCrossTabMelt(dummies_ws,dummies_pl, colNames_vector_ws)
crosstab_du_pl <- getCrossTabMelt(dataUse_freq, dummies_pl,colNames_vector_du)
crosstab_dut_pl <- getCrossTabMelt(dummies_du, dummies_pl, colNames_vector_du)
crosstab_ag_pl <- getCrossTabMelt(dummies_ag, dummies_pl, colNames_vector_ag)

facet_plot <- ggplot(data=crosstab_ag_pl, aes(x=variable, y=value)) +
  geom_bar(stat='identity', aes(fill=variable), width=0.7) +
  scale_fill_brewer(palette='Spectral') +
  facet_wrap(~ cat, ncol=2, labeller=labeller(data=label_wrap_gen(45))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))
