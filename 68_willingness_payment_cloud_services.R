source('dummies_prepare.R')

# Grouped
barplot_68 <- ggplot(df_68_freq_ord, aes(fill=x,y=freq, x=x)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=-1) +
  labs(x="", y="n") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(0,110) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=2/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_681 <- ggplot(df_681_freq_ord, aes(fill=cloud.service,y=freq, x=cloud.service)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=1) +
  coord_flip()+
  labs(x="", y="n") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(0,130) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=2/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_682 <- ggplot(df_682_freq_ord, aes(fill=amount,y=freq, x=amount)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=-1) +
  coord_flip()+
  labs(x="", y="n") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  ylim(0,130) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=2/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
