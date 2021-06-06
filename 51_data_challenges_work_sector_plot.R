# Load .rdata files
file_names = as.list(dir(pattern='*_final.Rda'))
lapply(file_names, load, environment())

# Horizontal stacked bar plot for all challenge categories
likert_1 <- ggplot(data=a_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Growing data volume') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(33), position='top') +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x=element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0.2,-0.5,-0.5,0.5), "cm"))

likert_2 <- ggplot(data=b_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Limited processing capacity') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(33), position='top') +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x=element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,-0.5,-0.5,0.5), "cm"))

likert_3 <- ggplot(data=c_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Data are disseminated in a non-standardised way') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(33), position='top') +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x=element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,-0.5,-0.5,0.5), "cm"))

likert_4 <- ggplot(data=d_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Too many data platforms and portals') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(33),position='top') +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x=element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,-0.5,-0.5,0.5), "cm"))

likert_5 <- ggplot(data=e_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Data discovery') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(33),position='top') +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x=element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,-0.5,-0.5,0.5), "cm"))


likert_6 <- ggplot(data=f_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y="%") +
  ggtitle('Data services are too restricted') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(33), position='top') +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position=c(0.72,-1),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.key.size=unit(0.5,'cm'),
        legend.direction='horizontal',
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y=element_text(size=10),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,-0.5,1.5,0.5), "cm"))


likert_7 <- ggplot(data=g_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Complex data formats') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(35)) +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0.2,0.5,-0.5,-0.2), "cm"))

likert_8 <- ggplot(data=h_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Lacking easy-to-use tools') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(35)) +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,0.5,-0.5,-0.2), "cm"))

likert_9 <- ggplot(data=i_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Data access systems') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(35)) +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,0.5,-0.5,-0.2), "cm"))

likert_10 <- ggplot(data=k_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Cost of data services') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(35)) +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,0.5,-0.5,-0.2), "cm"))

likert_11 <- ggplot(data=l_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y=" ") +
  ggtitle('Combining different kind of geospatial data') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(35)) +
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='none',
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,0.5,-0.5,-0.2), "cm"))

likert_12 <- ggplot(data=m_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x=" ", y="%") +
  ggtitle('Data complexity') +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(30)) +
  guides(fill=guide_legend(reverse=TRUE, labels=wrap_format(10)))+
  theme(legend.position='none',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.5,'cm'),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x=element_text(size=10),
        plot.title = element_text(size=10, face='italic', vjust=-1),
        strip.text.x=element_text(size=10),
        aspect.ratio=1/6,
        plot.margin=unit(c(0,0.5,1.5,-0.2), "cm"))

# Bring left column plots together
g1 <- rbind(ggplotGrob(likert_1), ggplotGrob(likert_2), ggplotGrob(likert_3), 
            ggplotGrob(likert_4), ggplotGrob(likert_5), ggplotGrob(likert_6), size='first')
# Bring right column plots together
g2 <- rbind(ggplotGrob(likert_7), ggplotGrob(likert_8), ggplotGrob(likert_9), 
            ggplotGrob(likert_10), ggplotGrob(likert_11), ggplotGrob(likert_12), size='first')
# Combine everything onto a grid table
grid.newpage()
grid.draw(cbind(g1,g2))

