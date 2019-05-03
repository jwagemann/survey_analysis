library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')


dataService <- c(rep("Code-editor in the cloud", 3), 
                 rep('Code-based processing routines accessing the cloud',3), 
                 rep("Code-based processing routines on a local machine",3), 
                 rep("Geospatial software on a local machine",3))

use <- rep(c("Always", "Sometimes","Never"),4)
sums <- c(7.0,36.0,46.9,9.4,32.4,49.8,54.5,31.9,7.0,24.4,47.9,19.2)

data_use_sums <- data.frame(as.factor(dataService), as.factor(use), sums)
data_use_sums$as.factor.dataService. <- factor(data_use_sums$as.factor.dataService., levels=data_use_sums$as.factor.dataService.[c(4,2,1,3)])


# Grouped
ggplot(data_use_sums, aes(fill=as.factor.use., y=sums, x=as.factor.use.)) + 
  geom_bar(stat="identity",width=0.7) +
  facet_wrap(as.factor.dataService.~., ncol=1) +
  coord_flip() +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Frequency of use", y="Percent") +
  theme(legend.title=element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
