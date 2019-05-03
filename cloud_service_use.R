dataService <- c("Data Access", 
                 "Data Processing", 
                 "Data Storage")

sums <- c(77.5,78.9,71.8)

data_use_sums <- data.frame(as.factor(dataService), sums)

data_use_sums$as.factor.dataService. <-factor(data_use_sums$as.factor.dataService., levels(data_use_sums$as.factor.dataService.)[c(4,5,1,2,3)]) 

# Grouped
ggplot(data_use_sums, aes(fill=dataService,y=sums, x=dataService)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Cloud service", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,100) +
  theme(legend.title=element_blank(), aspect.ratio=2/1,
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
