source('./dummies_prepare.R')
library('stringr')
library('grid')
library('ggsci')


ggplot(df_36_freq, aes(y=perc, x=x,fill=x, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Motivation to invest time", y="Percent") +
  scale_fill_brewer(palette='Blues',direction = -1) +
  theme_light() +
  ylim(0,80) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
