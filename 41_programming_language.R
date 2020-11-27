
# Bar plot of frequencies of programming languages in a decreasing order
coeff <- 2
ggplot(df_41_order, aes(y=perc, x=reorder(programming.language, -perc),fill=programming.language, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Programming language", y="n") +
  scale_fill_brewer(palette='Spectral') + 
  scale_y_continuous(
    name='rel. Frequency',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ .*coeff, name="n"),
    lim=c(-2,95)) +
  geom_point(aes(y=freq/coeff), color='black')+
  theme_light() +
  theme(legend.position="none",
        axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0,l=0)),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(5)) + 
  geom_text(y=0, aes(label=round(perc,1)), size=5, position=position_dodge(width=0.9), vjust=1.4, color='black') +
  geom_text(aes(y=freq/coeff, label=freq), size=5, vjust=-1, color='black')
