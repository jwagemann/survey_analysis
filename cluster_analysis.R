setwd('/Users/julia_wagemann/Documents/Notebooks/phd/survey_analysis/')

df_new <- read.csv('20190131_final_results_header_modified.csv', header=TRUE, na.string="")


df_sel <- df_new[,c('X1.1',
                    'X1.4',
                    'X2.1',
                    'X2.2',
                    'X3.1..climate.data',
                    'X3.1.daily.meteorological.forecasts',
                    'X3.1.seasonal.forecasts',
                    'X3.1.environmental.forecasts',
                    'X3.1.eo.data',
                    'X3.1.other.geospatial.data',
                    'X3.1.value.added.products',
                    'X3.1.other',
                    'X4.5.cloud.code.editor',
                    'X4.5.code.routines.access.cloud.services',
                    'X4.5.code.routines.python.r',
                    'X4.5.geospatial.software',
                    'X4.5.Other'
                    )]

test <- df_sel[complete.cases(df_sel[,13:17]),]


                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    'X4.5')]



