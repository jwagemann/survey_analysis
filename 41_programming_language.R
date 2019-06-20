source('./dummies_prepare.R')


ggplot(df_41_order, aes(y=perc, x=programming.language,fill=programming.language, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Programming language", y="Percent") +
  scale_fill_brewer(palette='Spectral') + 
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
