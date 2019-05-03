dataService <- c("Data download", 
                 "Data processing", 
                 "Data storage",
                 "Data upload",
                 "Service support")

perc <- c(21.1, 55.9,48.4,12.7,28.2)

data_use_sums <- data.frame(as.factor(dataService), perc)

data_use_sums$as.factor.dataService. <-factor(data_use_sums$as.factor.dataService., levels(data_use_sums$as.factor.dataService.)[order(data_use_sums$perc)]) 

# Grouped
ggplot(data_use_sums, aes(fill=as.factor.dataService.,y=perc, x=as.factor.dataService.)) + 
  geom_bar(stat="identity",width=0.7) +
  coord_flip()+
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Data service", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,60) +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=1/2,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

