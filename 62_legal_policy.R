source('dummies_prepare.R')

barplot_62 <- ggplot(df_62_freq_ord, aes(fill=policy,y=freq, x=policy)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x="", y="n") +
  theme_light() + 
  theme(legend.position='none',
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_62 <- ggplot(df_62_perc_ord, aes(fill=policy,y=freq, x=policy)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x="", y="Percent") +
  theme_light() + 
  theme(legend.position='none',
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
