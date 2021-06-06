# Grouped bar chart - Question 6.8.1 What service would you be willing to pay for?
barplot_681 <- ggplot(df_681_freq_ord, aes(fill=cloud.service,y=freq, x=reorder(cloud.service, -freq))) + 
  geom_bar(stat="identity",width=0.5) +
  scale_fill_uchicago(palette='light', alpha=0.7) +
  labs(x="", y="n") +
  scale_x_discrete(labels = wrap_format(10)) +
  ylim(-18,130) +
  theme_light() +
  geom_text(aes(y=freq, label=freq), size=5, vjust=1.5, color='black') +
  geom_label(y=-10, aes(label=round(perc,1)), size=6) +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/3,
        axis.text=element_text(size=16),
        legend.text = element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16))

# Bar chart - Question 6.8.2 How much would you be willing to pay?
barplot_682 <- ggplot(df_682_freq_ord, aes(fill=amount,y=freq, x=amount)) + 
  geom_bar(stat="identity",width=0.8) +
  scale_fill_manual(values=brewer.pal(8, 'Spectral')) +
  labs(x="", y="n") +
  coord_flip()+
  scale_x_discrete(limits=levels(df_682_freq_ord$amount),labels = wrap_format(40)) +
  ylim(0,100) +
  geom_text(aes(y=freq, label=freq), size=5, hjust=-0.3, color='black') +
  geom_label(y=97, aes(label=round(perc,1)), size=5) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/3,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

