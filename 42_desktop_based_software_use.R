library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

df_42 <- as.data.frame(df_new[,'X4.2'])

df_42_freq <- count(df_42)
df_42_freq$perc <- df_42_freq$freq / no_of_respondents * 100
df_42_freq <- df_42_freq[-3,]

colourCount = length(unique(data_use_sums$perc))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

ggplot(df_42_freq, aes(y=perc, x=df_new....X4.2..,fill=df_new....X4.2.., ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="", y="Percent") +
  scale_fill_brewer(palette='Blues') +
  ylim(0,80) +
  theme_light()+ 
  theme(legend.position="none", aspect.ratio = 3/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
