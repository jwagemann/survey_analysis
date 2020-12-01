# Barplot for Question on 'What do you want to use the cloud for?'
coeff <- 2.7
barplot_63_freq <- ggplot(df_63_freq[-5,], aes(fill=Use,y=freq, x=Use)) + 
  geom_bar(stat="identity",width=0.5) +
  geom_point(aes(y=perc*coeff), color='black', group=1)+
  geom_line(aes(y=perc*coeff), linetype='twodash', color='black', group=1)+
  scale_fill_uchicago(palette='light', alpha=0.7) +
  labs(x="Cloud service use", y="n") +
  scale_y_continuous(
    name='n',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./coeff, name="rel. Frequency"),
    lim=c(-30,250)) +
  scale_x_discrete(labels = wrap_format(10)) +
  geom_label(aes(y=0, label=freq), size=4, vjust=1.3, color='black') +
  geom_text(aes(y=perc*coeff, label=round(perc,1)), size=4, vjust=-1.5, color='black')+
  theme_light() +
  theme(legend.title=element_blank(), aspect.ratio=1/4,
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

# Barplot for Question on 'Would you need processing tools available on the cloud?'
barplot_631_freq <- ggplot(df_631_freq_ord, aes(fill=Response,y=perc, x=Response)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_manual(values=rev(brewer.pal(3, 'Spectral'))) +
  labs(x="Would you need processing tools available on the cloud?", y="Percent") +
  scale_x_discrete(labels =wrap_format(10)) +
  ylim(0,70) +
  geom_label(aes(y=perc, label=round(perc,1)), size=4, vjust=-0.5)+
  theme_light() +
  theme(legend.title=element_blank(), aspect.ratio=3/1,
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
