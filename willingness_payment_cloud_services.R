dataService <- c("Yes",
                 "No",
                 "Depends on the cost")

sums <- c(21.6, 27.7,48.4)

data_use_sums <- data.frame(as.factor(dataService), sums)

data_use_sums$as.factor.dataService. <-factor(data_use_sums$as.factor.dataService., levels(data_use_sums$as.factor.dataService.)[c(3,2,1)]) 

# Grouped
ggplot(data_use_sums, aes(fill=as.factor.dataService.,y=sums, x=as.factor.dataService.)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Response", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(0,60) +
  theme(legend.title=element_blank(), legend.position = "none", aspect.ratio=2/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
