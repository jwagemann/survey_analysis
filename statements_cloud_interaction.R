dataService <- c("Want to work in a collaborative way on the cloud", 
                 "Want to work privately on the cloud", 
                 "Opportunity to upload data and combine them with other data on the cloud",
                 "Need to export results out of the cloud",
                 "Geographic location of the cloud is important")

sums <- c(70.4,56.3,77.5,73.2,37.1)

data_use_sums <- data.frame(as.factor(dataService), sums)

data_use_sums$as.factor.dataService. <-factor(data_use_sums$as.factor.dataService., levels(data_use_sums$as.factor.dataService.)[c(4,5,1,2,3)]) 

# Grouped
ggplot(data_use_sums, aes(fill=dataService,y=sums, x=dataService)) + 
  geom_bar(stat="identity",width=0.7) +
  coord_flip()+
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Statement", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  ylim(0,100) +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
