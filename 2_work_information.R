source('data_load.R')


barplot_22 <- ggplot(df_22_summary[1:2,], aes(fill=Var1,y=Freq, x=Var1)) + 
  geom_bar(stat="identity",width=0.7) +
#  scale_fill_brewer(palette="Blues",direction=-1) +
  labs(x="", y="n") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  ylim(0,180) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_221 <- ggplot(df_221_summary[-1,], aes(fill=df_221,y=per, x=df_221)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Blues",direction=-1) +
  labs(x="", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,60) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

barplot_222 <- ggplot(df_222_summary[-2,], aes(fill=df_222,y=per, x=df_222)) + 
  geom_bar(stat="identity",width=0.7) +
  scale_fill_brewer(palette="Reds",direction=-1) +
  labs(x="", y="Percent") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0,60) +
  theme_light() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

grid.draw(rbind(ggplotGrob(barplot_221), ggplotGrob(barplot_22), ggplotGrob(barplot_222), size='first'))
