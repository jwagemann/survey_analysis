source('dummies_prepare.R')

barplot_63_freq <- ggplot(df_63_freq, aes(fill=Use,y=freq, x=Use)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=-1) +
  labs(x="Cloud service", y="n") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,170) +
  theme_light() +
  theme(legend.title=element_blank(), aspect.ratio=2/1,
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_63_perc <- ggplot(df_63_freq, aes(fill=Use,y=perc, x=Use)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=-1) +
  labs(x="Cloud service", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,100) +
  theme_light() +
  theme(legend.title=element_blank(), aspect.ratio=2/1,
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_631_freq <- ggplot(df_631_freq_ord, aes(fill=Response,y=perc, x=Response)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=-1) +
  labs(x="Would you need processing tools available on the cloud?", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,100) +
  theme_light() +
  theme(legend.title=element_blank(), aspect.ratio=2/1,
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
