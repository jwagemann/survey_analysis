library('stringr')

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

time_investment <- c("Yes",
             "No",
             "Depends",
             "Tried, but gave up",
             "Don't know"
)

perc <- c(69.0, 4.7,20.2,3.2,2.8)
data_use_sums <- data.frame(as.factor(time_investment), perc)

data_use_sums$as.factor.time_investment. <-factor(data_use_sums$as.factor.time_investment., levels(data_use_sums$as.factor.time_investment.)[c(5,3,1,4,2)])

ggplot(data_use_sums, aes(y=perc, x=as.factor.time_investment.,fill=as.factor.time_investment., ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Motivation to invest time", y="Percent") +
  scale_fill_brewer(palette='Spectral') +
  ylim(0,80) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
