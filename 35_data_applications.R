library(likert)
library(plyr)
require(ggplot2)
library(reshape2)
library(RColorBrewer)
data(pisaitems)

items24 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']
l24g <- likert(items24, grouping=pisaitems$CNT)



setwd('/Users/julia_wagemann/Documents/github//survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")
df_new_other <- df_new[,'X3.5.1']

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


# What applications are you interested in doing with Big Earth Data?

df_likert_35 <- df_new[, c('X3.5.machine.learning',
                           'X3.5.web.applications',
                           'X3.5.data.visualizations',
                           'X3.5.global.regional.analyses',
                           'X3.5.Analysis.over.long.time.spans',
                           'X3.5.time.series.analysis')]

levels_35 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very much interested')

df_likert_35_ord <- lapply(df_likert_35, function(x) ordered(x, levels = levels_35))
df_likert_35_res <- do.call(data.frame, df_likert_35_ord)

names(df_likert_35_res) <- c(
  X3.5.machine.learning="Machine-learning / Deep learning",
  X3.5.web.applications="Interactive web applications",
  X3.5.data.visualizations="Data visualizations",
  X3.5.global.regional.analyses="Global / regional data analysis",
  X3.5.Analysis.over.long.time="Analyses over long time spans",
  X3.5.time.series.analysis="Time-series analysis")

likertObj_35 <- likert(df_likert_35_res)

data_applications <- plot(likertObj_35, centered=FALSE, include.center=TRUE, text.size=4, panel.arrange='h', digits=1, wrap=15, include.histogram=TRUE)
data_applications + theme(legend.text=element_text(size=12),
                          axis.text = element_text(size=14))
