dataCosts <- c("I prefer a costing model", 
                 "Up to 100 Euro", 
                 "Up to 500 Euro",
                 "Up to 1,000 Euro",
                 "Up to 10,000 Euro",
                 "Up to 50,000 Euro",
                 "Up to 100,000 Euro",
                 "More than 100,000 Euro")

perc <- c(34.7,10.3,7.0,8.9,6.6,0.9,0.9,0.5)

data_use_sums <- data.frame(as.factor(dataCosts), perc)

data_use_sums$as.factor.dataService. <-factor(data_use_sums$as.factor.dataCosts., levels(data_use_sums$as.factor.dataCosts.)[c(1,5,8,3,4,7,6,2)]) 

# Grouped
ggplot(data_use_sums, aes(fill=as.factor.dataService.,y=perc, x=as.factor.dataService.)) + 
  geom_bar(stat="identity",width=0.7) +
  coord_flip()+
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Amount to be willing to pay", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  ylim(0,50) +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
