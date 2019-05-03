setwd('/Users/julia_wagemann/Documents/github//survey_analysis/')

library(ggplot2)
library(dplyr)
library(scales)
df_new <- read.csv('20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- 213

df_2 <- df_new[,'X2.1']

df_2_stacked <- stack(setNames(strsplit(as.character(df_2),';'), df_2))
df_2_summary <- as.data.frame(table(df_2_stacked$values))
df_2_summary$per <- df_2_summary$Freq / no_of_respondents * 100


df_2_summary <- df_2_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(Var1 = factor(Var1, unique(Var1)))

  
  
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

bp <- ggplot(df_2_summary, aes(x="", y=per, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  guides(fill=guide_legend(reverse=TRUE, title="", label.position='left')) 

pie <- bp + coord_polar('y', start=0) + scale_fill_brewer(palette='Spectral') + 
  blank_theme + theme(axis.text.x=element_blank(), legend.position="left",legend.text=element_text(size=14), legend.text.align = 1)
pie


df_35 <- df_new[, c('X3.5.machine.learning',
                           'X3.5.web.applications',
                           'X3.5.data.visualizations',
                           'X3.5.global.regional.analyses',
                           'X3.5.Analysis.over.long.time.spans',
                           'X3.5.time.series.analysis',
                           'X3.5.other')]

df_43 <- df_new[, c('X4.3.Download.service',
                           'X4.3.cloud.computing.infrastructure',
                           'X4.3.ogc.service',
                           'X4.3.custom.api.opendap',
                           'X4.3.virtual.research.infrastructure',
                           'X4.3.data.cube.technology',
                           'X4.3.spatial.array.database',
                           'X4.3.other')]

df_44 <- df_new[, c('X4.4.download.service',
                           'X4.4.cloud.computing.infrastructure',
                           'X4.4.ogc.service',
                           'X4.4.custom.api.opendap',
                           'X4.4.virtual.research.infrastructure',
                           'X4.4.data.cube.technology',
                           'X4.4.spatial.array.database',
                           'X4.4.other')]
