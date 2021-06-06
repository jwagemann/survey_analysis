source('431_data_access_systems_prepare_work_sectors.R')

levels_ws <- rev(dataSystems)
levels(dataSystems_work_sectors$dataSystems) <- rev(dataSystems)
levels_ws_ws <- c('University', 'Government', 'Private sector', 'Nonprofit / Intergov. Org.')

dataSystems_work_sectors$dataSystems <- factor(dataSystems_work_sectors$dataSystems, levels=levels_ws)
dataSystems_work_sectors$variable <- factor(dataSystems_work_sectors$variable, levels=levels_ws_ws)

dataAccess_current <- ggplot(dataSystems_work_sectors, aes(y=current, x=reorder(dataSystems,-current))) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=variable)) +
  labs(x="", y="n\n", title='') +
  scale_fill_uchicago(palette='light', alpha=0.8) +
  theme_light() + 
  ylim(-1,80)+
  theme(
    legend.direction='horizontal',
        legend.title = element_blank(),
    legend.position='bottom',
        axis.text=element_text(size=16),
        legend.text = element_text(size=16),
        strip.text.x=element_text(size=16),
    title=element_text(size=14),
        axis.title = element_text(size=16),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(15), position='bottom') +
  geom_text(aes(y=current, label=current, fill=variable), position=position_dodge(width=0.7), vjust=-0.5,size=5, color='black')



dataAccess_future <- ggplot(dataSystems_work_sectors, aes(y=future, x=dataSystems)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=variable)) +
  coord_flip()+
  labs(x="", y="n\n", title='Future use') +
  scale_fill_uchicago(palette='light') +
  theme_light() + 
  ylim(-1,80)+
  theme(legend.position='none',
        axis.text=element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16),
        title=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        aspect.ratio=2/1) +
  geom_text(aes(y=future, label=future, fill=variable), position=position_dodge(width=0.7), hjust=-0.5,size=5, color='black')


dataAccess_noInterest <- ggplot(dataSystems_work_sectors, aes(y=noInterest, x=dataSystems)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=variable)) +
  coord_flip()+
  labs(x="", y="n\n", title='No interest') +
  scale_fill_uchicago(palette='light') +
  theme_light() + 
  ylim(-1,80)+
  theme(legend.position='none',
        axis.text=element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16),
        title=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        aspect.ratio=2/1) +
  geom_text(aes(y=noInterest, label=noInterest, fill=variable), position=position_dodge(width=0.7), hjust=-0.5,size=5, color='black')

dataAccess_current


