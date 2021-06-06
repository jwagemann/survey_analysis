
# Bar plot of frequencies of programming languages in a decreasing order

ggplot(df_41_order[-9,], aes(y=freq, x=reorder(programming.language, -perc),fill=programming.language, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Programming language", y="n") +
  scale_fill_brewer(palette='Spectral') + 
  scale_y_continuous(
    name='n\n',
    lim=c(-2,220)) +
  theme_light() + 
  theme(legend.position="none",
        axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0,l=0)),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(5)) + 
  geom_text(aes(label=freq), size=6, position=position_dodge(width=0.9), vjust=-0.6, color='black') +
  geom_label(aes(y=215, label=round(perc,1)), size=6, color='black')
