source('dummies_prepare.R')

barplot_64_freq <- ggplot(df_64_freq_ord, aes(fill=Statement,y=freq, x=Statement)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=1) +
  coord_flip() +
  labs(x="Statement", y="n") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  ylim(0,170) +
  theme_light() +
  theme(legend.title=element_blank(),
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_64_perc <- ggplot(df_64_freq_ord, aes(fill=Statement,y=perc, x=Statement)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=1) +
  coord_flip() +
  labs(x="Statement", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  ylim(0,100) +
  theme_light() +
  theme(legend.title=element_blank(),
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
