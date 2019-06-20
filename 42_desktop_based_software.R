source('data_survey_functions.R')
source('./dummies_prepare.R')

ggplot(df_421_order, aes(y=perc, x=software,fill=software, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Desktop-based software", y="Percent") +
  scale_fill_brewer(palette="Blues",direction=1) +
  ylim(0,80) +
  coord_flip()+
  theme_light() + 
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +

  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
