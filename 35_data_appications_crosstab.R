source('data_survey_functions.R')
source('dummies_prepare.R')
source('31_data_use_prepare.R')

getDummies(df_likert_35,6)
is.na(df_likert_35$X3.5.machine.learning) <- df_likert_35$X3.5.machine.learning != 'Interested' & df_likert_35$X3.5.machine.learning != 'Very much interested'
is.na(df_likert_35$X3.5.web.applications) <- df_likert_35$X3.5.web.applications != 'Interested' & df_likert_35$X3.5.web.applications != 'Very much interested'
is.na(df_likert_35$X3.5.data.visualizations) <- df_likert_35$X3.5.data.visualizations != 'Interested' & df_likert_35$X3.5.data.visualizations != 'Very much interested'
is.na(df_likert_35$X3.5.global.regional.analyses) <- df_likert_35$X3.5.global.regional.analyses != 'Interested' & df_likert_35$X3.5.global.regional.analyses != 'Very much interested'
is.na(df_likert_35$X3.5.Analysis.over.long.time.spans) <- df_likert_35$X3.5.Analysis.over.long.time.spans != 'Interested' & df_likert_35$X3.5.Analysis.over.long.time.spans != 'Very much interested'
is.na(df_likert_35$X3.5.time.series.analysis) <- df_likert_35$X3.5.time.series.analysis != 'Interested' & df_likert_35$X3.5.time.series.analysis != 'Very much interested'


test <-  createCrossTab(df_likert_35,dummies_ws,c('all',colNames_vector_apps))
test$cat <- rownames(test)

test_melt <- melt(createCrossTab)