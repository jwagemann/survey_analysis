library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')


dataService <- c("Publicly-funded cloud, e.g. EOSC", 
                 'Publicly-funded specialised cloud, e.g.WekEO', 
                 "Commercial cloud vendor, such as AWS or GCP",
                 "I do not mind",
                 "None of the above")

sums <- c(31.0,22.1,12.2,25.8,6.1)

data_use_sums <- data.frame(as.factor(dataService), sums)

data_use_sums$as.factor.dataService. <-factor(data_use_sums$as.factor.dataService., levels(data_use_sums$as.factor.dataService.)[c(4,5,1,2,3)]) 

# Grouped
ggplot(data_use_sums, aes(fill=as.factor.dataService.,y=sums, x=as.factor.dataService.)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Type of cloud policy", y="Percent") +
  scale_x_discrete(labels=element_blank()) +
  theme(legend.title=element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
