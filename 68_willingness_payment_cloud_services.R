
# Grouped bar chart - Question 6.8 Would you be willing to pay for processing services in the cloud?
barplot_68 <- ggplot(df_68_freq_ord, aes(fill=x,y=perc, x=x)) + 
  geom_bar(stat="identity",width=0.5) +
  scale_fill_manual(values=rev(brewer.pal(3, 'Spectral'))) +
  labs(x="\nWould you be willing to pay for processing services in the cloud?", y="Percent\n") +
  scale_x_discrete(labels = wrap_format(20)) +
  geom_label(aes(y=perc, label=round(perc,1)), size=4, vjust=-0.5)+
  ylim(0,60) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/3,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))


# Grouped bar chart - Question 6.8.1 What service would you be willing to pay for?
barplot_681 <- ggplot(df_681_freq_ord, aes(fill=cloud.service,y=freq, x=reorder(cloud.service, -freq))) + 
  geom_bar(stat="identity",width=0.5) +
  scale_fill_uchicago(palette='light', alpha=0.7) +
  labs(x="", y="n") +
  scale_x_discrete(labels = wrap_format(10)) +
  ylim(-18,130) +
  theme_light() +
  geom_text(aes(y=freq, label=freq), size=5, vjust=1.5, color='black') +
  geom_label(y=-10, aes(label=round(perc*100,1)), size=5) +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/3,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

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
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/2,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

