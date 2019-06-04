library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

df_41 <- as.data.frame(df_new[,'X4.1'])
df_411 <- as.data.frame(df_new[,'X4.1.1'])

colnames(df_41) <- c('programming.language')

df_41_split <- separate_rows(df_41,programming.language, sep=';')
df_41_freq <- count(df_41_split)
df_41_freq$perc <- df_41_freq$freq / no_of_respondents * 100

df_41_freq <- df_41_freq[c(-8,-9,-14),]

df_41_order <- df_41_freq %>%
  arrange(perc) %>% 
  mutate(programming.language = factor(programming.language, unique(programming.language)))


colourCount <- length(unique(df_41_freq$perc))
getPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

myPalette <- brewer.pal(9,'Blues')
test = colorRampPalette(myPalette)(11)

ggplot(df_41_order, aes(y=perc, x=programming.language,fill=programming.language, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Programming language", y="Percent") +
  scale_fill_manual(values=test) + 
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
