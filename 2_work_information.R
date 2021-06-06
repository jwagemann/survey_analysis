
# Bar plot frequencies of work sectors
Freq = df_21_summary$Freq
barplot_21 <- ggplot(df_21_summary[-2,], aes(fill=Var1,y=Freq, x=reorder(Var1, -Freq))) + 
  geom_bar(stat="identity",width=0.7) +
  ylim(0, 130) +
  scale_fill_brewer(palette="Spectral") +
  labs(x="Work sector", y="n") +
  scale_x_discrete(labels=wrap_format(5)) +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=1.4, color='black', size=5) +
  geom_label(y=125, aes(label=round(per,1)), size=5, show.legend=FALSE) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=14),
        legend.text = element_text(size=14),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0,l=0)),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.right = element_text(colour = "black"),
        aspect.ratio=1/4,
        plot.margin = unit(c(1, 1, 0, 3), "lines"))


# Frequencies - Data user and data provider
# Make barplot for data user freuencies
barplot_22 <- ggplot(df_22_summary[1:2,], aes(fill=Var1,y=per, x=Var1)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_manual(values=c('#006D2C', '#6E016B'))+
  labs(x="", y="%") +
  coord_flip() +
  scale_x_discrete(labels=wrap_format(5)) +
  ylim(0,100) +
  geom_text(aes(label=round(per,1)), color='white', size=5, hjust=1.5)+
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=14),
        legend.text = element_text(size=14),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        plot.margin = unit(c(1, 1, 0, 3), "lines"))

# Make plot of data user / data provider frequencies
barplot_221 <- ggplot(df_221_summary[-1,], aes(fill=df_221,y=Freq, x=reorder(df_221, desc(df_221)))) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="BuPu",direction=1) +
  labs(x="", y="n") +
  scale_x_discrete(labels=wrap_format(10)) +
  ylim(0,100) +
  geom_text(aes(y=0, label=Freq), color='black', size=5, vjust=-.8)+
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=14),
        legend.text = element_text(size=14),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        plot.margin = unit(c(1, 1, 0, 3), "lines"))

# Make bar plot for data providers
barplot_222 <- ggplot(df_222_summary[-2,], aes(fill=df_222,y=Freq, x=reorder(df_222, desc(df_222)))) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="BuGn",direction=1) +
  labs(x="", y="n") +
  scale_x_discrete(labels=wrap_format(10)) +
  ylim(0,20) +
  geom_text(aes(y=0, label=Freq), color='black', size=5, vjust=-.8)+
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=16),
        legend.text = element_text(size=14),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        plot.margin = unit(c(1, 1, 0, 3), "lines"))

# Bring all three plots together onto a 1-column grid
grid.draw(rbind(ggplotGrob(barplot_221), ggplotGrob(barplot_22), ggplotGrob(barplot_222), size='first'))

