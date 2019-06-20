source('./dummies_prepare.R')
library(likert)
library(plyr)
require(ggplot2)
library(reshape2)
library(RColorBrewer)


df_new_countries <- df_new[df_new$X1.1 == 'United States of America' | df_new$X1.1 == 'United Kingdom' |
                          df_new$X1.1 == 'Germany' |
                          df_new$X1.1 == 'Italy' |
                          df_new$X1.1 == 'Canada' |
                          df_new$X1.1 == 'Spain' |
                          df_new$X1.1 == 'Austria',]

df_2 <- na.omit(df_new[c('X2.1',
                 'X3.5.machine.learning',
                 'X3.5.web.applications',
                 'X3.5.data.visualizations',
                 'X3.5.global.regional.analyses',
                 'X3.5.Analysis.over.long.time.spans',
                 'X3.5.time.series.analysis')])



df_separated <- separate_rows(df_2,X2.1, sep=';')

df_filter <- na.omit(df_new_countries[c('X1.1',
                               'X3.5.machine.learning',
                               'X3.5.web.applications',
                               'X3.5.data.visualizations',
                               'X3.5.global.regional.analyses',
                               'X3.5.Analysis.over.long.time.spans',
                               'X3.5.time.series.analysis')])




data_applications <- plot(likertObj_35, centered=FALSE, include.center=TRUE, text.size=4, panel.arrange='h', digits=1, wrap=15, include.histogram=FALSE)
data_applications + theme(legend.text=element_text(size=14),legend.position='none',
                          axis.text = element_text(size=14))
