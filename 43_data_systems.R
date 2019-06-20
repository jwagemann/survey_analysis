source('43_data_access_systems_prepare.R')

dataAccess_melt <- melt(dataAccess_order)

# Grouped
plot_current <- ggplot(dataAccess_order, aes(fill=dataSystems, y=ds_current, x=reorder(dataSystems,-ds_current))) +
  geom_bar(stat='identity', width=0.6) +
  #  coord_flip() +
  labs(x="", y="Number of users", Colour="Data system") +
  #  scale_fill_brewer(palette='BrBG') +
  scale_fill_uchicago(palette='dark') +
  scale_y_reverse()+
  scale_x_discrete(labels=wrap_format(15)) + 
  #  ggtitle('Current') + 
  theme_light()+
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        #        axis.text.y=element_text(angle=90, vjust=0.5, hjust=0.5),
        axis.text.y=element_text(size=12))
#        axis.title.y = element_blank())

positions <- dataAccess_order$dataSystems
plot_future <- ggplot(dataAccess_order, aes(fill=dataSystems, y=ds_no_interest, x=dataSystems)) +
  geom_bar(stat='identity', width=0.6) +
  #  coord_flip() +
  labs(x="", y="Number of users", Colour="Data system") +
  scale_x_discrete(labels=wrap_format(15), limits=positions, position='top') +
  #ggtitle('Future')+
  ylim(0,150)+
  scale_fill_uchicago(palette='light') +
  theme_light() + 
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))
#        axis.title.y = element_blank())


grid.draw(rbind(ggplotGrob(plot_future), ggplotGrob(plot_current), size='first'))


