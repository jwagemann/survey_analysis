source('data_survey_functions.R')
library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

df_42 <- as.data.frame(df_new[,'X4.2'])
df_421 <- as.data.frame(df_new[,'X4.2.1'])
colnames(df_421) <- 'software'
df_422 <- as.data.frame(df_new[,'X4.2.2'])

df_421_split <- separate_rows(df_421,software, sep=';')
df_421_freq <- count(df_421_split)
df_421_freq$perc <- df_421_freq$freq / 134 * 100

df_421_freq <- df_421_freq[c(-9),]

df_421_order <- df_421_freq %>%
  arrange(perc) %>% 
  mutate(software = factor(software, unique(software)))

ggplot(df_421_order, aes(y=perc, x=software,fill=software, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Desktop-based software", y="Percent") +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  ylim(0,80) +
  coord_flip()+
  theme_light() + 
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +

  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
