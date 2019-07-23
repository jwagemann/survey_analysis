
source('data_survey_functions.R') # Load dataUse_freq
source('data_load.R')
source('31_data_use_prepare.R')


dummies_du <- getDummies(df_221,1)
dummies_du <- dummies_du[,c(-5,-6)]

dummies_df <- getDummies(df_34,1)
dummies_df <- dummies_df[,c(-10,-12)]

# Programming language dummy table for each entry
dummies_pl <- getDummies(df_41,1)
dummies_pl <- dummies_pl[,c(-5,-6,-14)]

# Work sector dummy table for each entry
dummies_ws <- getDummies(df_21,1)
dummies_ws <- dummies_ws[,-7]

# Age group dummy table for each entry
dummies_ag <- getDummies(df_14,1)
dummies_ag <- dummies_ag[,-6]

# Desktop-based software dummy table for each entry
dummies_ds <- getDummies(df_42,1)
dummies_ds <- dummies_ds[,-3]

dummies_ds_type <- getDummies(df_421,1)
dummies_ds_type <- dummies_ds_type[,c(-1,-5)]

# Country-of-residence dummy table for each entry
dummies_cor <- getDummies(df_11,1)
dummies_cor <- dummies_cor[,-28]

# Application dummy table for each entry
dummies_app <- getDummies(df_33,1)
dummies_app <- dummies_app[,c(-3,-6)]

# Country-of-residence filter dummy table for each entry
dummies_cor_filter <- getDummies(df_11_filter_all,2)
dummies_cor_filter <- dummies_cor_filter[,-2]

colNames_vector_ws <- c('Non profit','University','Intergovernmental organisation','Government', 'Established company', 'Start-up')
colNames_vector_du <- c('Climate reanalysis','Daily meteorological data', 'Seasonal forecast data', 'Environmental forecast data', 'Earth Observations','Other Geospatial data','Value-added products')
colNames_vector_dut <- c('Software Developer', 'Researcher', 'Project manager', 'Data Analyst', 'Team leader', 'Data Scientist', 'Student')
colNames_vector_ag <- c('30-40 years', '20-30 years', '50-60 years', '40-50 years', '>60 years', '<20 years')
colNames_vector_cor <- plyr::count(as.data.frame(df_11$cor))[-38,]
colNames_vector_cor <- as.vector(colNames_vector_cor$df_11.cor)
colNames_vector_region <- c('Europe', 'United States of America & Canada')
colNames_vector_app <- c('Enviornmental monitoring', 'Research', 'Business', 'Humanitarian application')
colNames_vector_apps <- c('Machine-Learning','Interactive web applications','Data visualisations', 'Glocal/regional data analysis','Analysis over long time spans','Time-series analysis')
