source('431_data_access_systems_region_prepare.R')

levels_ds <- c('Download service', 'Cloud-computing infrastructure', 'OGC web service, e.g. WMS / WCS', 'Custom API / OpeNDAP', 
                              'Spatial / Array database', 'Data Cube technology', 'Virtual Research Infrastructure')

levels_region <- c('Europe_abs','usa_ca_abs')

dataAccess_region_future_final$dataSystems <- factor(dataAccess_region_future_final$dataSystems, levels=levels_ds)
dataAccess_region_future_final$variable <- factor(dataAccess_region_future_final$variable, levels=levels_region)

dataAccess_region_current_final$dataSystems <- factor(dataAccess_region_current_final$dataSystems, levels=levels_ds)
dataAccess_region_current_final$variable <- factor(dataAccess_region_current_final$variable, levels=levels_region)

europe_current <- dataAccess_region_current_final[1:7,]
europe_current$timing <- 'Currently used'
colnames(europe_current) <- c('dataSystems', 'variable','abs', 'per','timing')
europe_future <- dataAccess_region_future_final[1:7,]
europe_future$timing <- 'Interest in future use'
colnames(europe_future) <- c('dataSystems', 'variable','abs', 'per','timing')
europe_noInterest <- dataAccess_region_noInterest_final[1:7,]
europe_noInterest$timing <- 'No interest'
colnames(europe_noInterest) <- c('dataSystems', 'variable','abs', 'per','timing')

europe_final <- rbind(europe_current, europe_future, europe_noInterest)

usa_ca_current <- dataAccess_region_current_final[8:14,]
usa_ca_current$timing <- 'Currently used'
colnames(usa_ca_current) <- c('dataSystems', 'variable','abs', 'per','timing')
usa_ca_future <- dataAccess_region_future_final[8:14,]
usa_ca_future$timing <- 'Interest in future use'
colnames(usa_ca_future) <- c('dataSystems', 'variable','abs', 'per','timing')
usa_ca_noInterest <- dataAccess_region_noInterest_final[8:14,]
usa_ca_noInterest$timing <- 'No interest'
colnames(usa_ca_noInterest) <- c('dataSystems', 'variable','abs', 'per','timing')


usa_ca_final <- rbind(usa_ca_current, usa_ca_future, usa_ca_noInterest)

col <- brewer.pal(n=5,'BrBG')

dataAccess_europe <- ggplot(europe_final, aes(y=per*100, x=dataSystems)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.8, aes(fill=timing)) +
  labs(x="", y="rel. Frequency\n", title='') +
  scale_fill_manual(values=c(col[5], col[4], 'darkgrey')) +
  theme_light() + 
  ylim(-15,85)+
  theme(
    legend.title = element_blank(),
    legend.position='none',
    axis.text=element_text(size=14),
    legend.text = element_text(size=16),
    strip.text.x=element_text(size=16),
    title=element_text(size=14),
    axis.title = element_text(size=16),
    aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(15), position='top') +
  annotate('text', label='Europe', size=6, fontface='bold',  x=0.9, y=84) +
  geom_label(aes(y=-8, label=round(per*100,0), fill=timing), position=position_dodge(width=0.8),size=5, color='black', show.legend=FALSE)

dataAccess_usa_ca <- ggplot(usa_ca_final, aes(y=per*100, x=dataSystems)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.8, aes(fill=timing)) +
  labs(x="", y="rel. Frequency\n", title='') +
  scale_fill_manual(values=c(col[5], col[4], 'darkgrey')) +
  theme_light() + 
  ylim(-15,85)+
  theme(
    legend.direction='horizontal',
    legend.title = element_blank(),
    legend.position='bottom',
    axis.text=element_text(size=14),
    legend.text = element_text(size=16),
    strip.text.x=element_text(size=16),
    title=element_text(size=14),
    axis.title = element_text(size=16),
    aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(15), position='bottom') +
  annotate('text', label='USA & Canada', size=6, fontface='bold',  x=1.2, y=84) +
  geom_label(aes(y=-8, label=round(per*100,0), fill=timing), position=position_dodge(width=0.8), size=5, color='black', show.legend=FALSE)

grid.draw(rbind(ggplotGrob(dataAccess_europe), ggplotGrob(dataAccess_usa_ca)))
