

# Grouped - Country of residence
barplot_1 <- ggplot(df_11_freq_ord, aes(fill=cor,y=freq, x=cor)) + 
  geom_bar(stat="identity",width=0.7) +
  labs(x="", y="n") +
  coord_flip() +
  scale_x_discrete(labels=wrap_format(30))+
  ylim(0,55) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=2/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

# Grouped - Age groups
barplot_14 <- ggplot(df_14_freq, aes(fill=age.group,y=freq, x=age.group)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Spectral",direction=1) +
  labs(x="", y="%") +

  scale_x_discrete(labels=wrap_format(30))+
  ylim(0,105) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/2,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
