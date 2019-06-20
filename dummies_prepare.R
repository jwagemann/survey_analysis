source('data_survey_functions.R') # Load dataUse_freq


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
