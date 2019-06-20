source('./dummies_prepare.R')


ggplot(df_42_freq, aes(y=perc, x=yes.no,fill=yes.no, ymin=0)) + 
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
