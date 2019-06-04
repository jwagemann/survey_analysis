library('stringr')
library('grid')
library('ggsci')

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

df_36 <- df_new[,'X3.6']
df_36 <- data.frame(df_36)

df_36_other <- df_new[,'X3.6.1']

df_36_count <- count(df_36)
df_36_count$perc <- df_36_count$freq / no_of_respondents * 100

data_use_sums <- data.frame(df_36_count$df_36, df_36_count$perc)

data_use_sums_sort <- data_use_sums[order(-data_use_sums$df_36_count.perc),]
data_use_sums_sort$df_36_count.df_36 <-factor(data_use_sums_sort$df_36_count.df_36, levels(data_use_sums_sort$df_36_count.df_36)[c(data_use_sums_sort$df_36_count.df_36)])

data_use_sums_sort

ggplot(data_use_sums_sort, aes(y=df_36_count.perc, x=df_36_count.df_36,fill=df_36_count.df_36, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Motivation to invest time", y="Percent") +
  scale_fill_aaas() +
  theme_light() +
  ylim(0,80) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
