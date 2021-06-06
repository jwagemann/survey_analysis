
####################################################
# Technical specifications of cloud services - CPU, Storage, RAM, number of instances
####################################################

col <- brewer.pal(4, 'Dark2')

memory <- as.data.frame(c(7, 4, 6, 9, 5))
levels_memory <- c('<10 GB', '16 GB', '32 GB', '64 GB', '128 GB')
rownames(memory) <- c('<10 GB', '16 GB', '32 GB', '64 GB', '128 GB')
colnames(memory) <- 'n'

memory$cats <- rownames(memory)
memory$cats <- factor(memory$cats, levels=levels_memory)

memory_plot <- ggplot(memory, aes(y=n, x=cats)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, fill=col[1]) +
  labs(x="", y="n\n", title='Memory') +
  ylim(0,13)+
  theme_light() + 
  theme(
    legend.direction='horizontal',
    legend.title = element_blank(),
    legend.position='bottom',
    axis.text=element_text(size=14),
    legend.text = element_text(size=16),
    strip.text.x=element_text(size=16),
    plot.title=element_text(size=16),
    axis.title = element_text(size=16),
    aspect.ratio=1/4) +
  scale_x_discrete(labels = wrap_format(15)) +
  geom_label(aes(y=12, label=n), position=position_dodge(width=0.8), size=5, color='black', show.legend=FALSE)

instances <- as.data.frame(c(10, 8, 4, 2, 7))
levels_instances <- c('<10', '<50', '<100', '>100', 'scalable with user load')
rownames(instances) <- levels_instances
colnames(instances) <- 'n'

instances$cats <- rownames(instances)
instances$cats <- factor(instances$cats, levels=levels_instances)

instances_plot <- ggplot(instances, aes(y=n, x=cats)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, fill=col[2]) +
  labs(x="", y="n\n", title='Number of instances') +
  ylim(0,15)+
  theme_light() + 
  theme(
    legend.direction='horizontal',
    legend.title = element_blank(),
    legend.position='bottom',
    axis.text=element_text(size=14),
    legend.text = element_text(size=16),
    strip.text.x=element_text(size=16),
    plot.title=element_text(size=16),
    axis.title = element_text(size=16),
    aspect.ratio=1/4) +
  scale_x_discrete(labels = wrap_format(15)) +
  geom_label(aes(y=14, label=n), position=position_dodge(width=0.8), size=5, color='black', show.legend=FALSE)

volumes <- c(7,24,8,5,1)
volumes <- as.data.frame(c(7,24,8,5,1))
levels_volumes <- c('< 1 TB', '1 to 10 TB', '11 to 100 TB', '> 100 TB', 'application dependent')
rownames(volumes) <- levels_volumes
colnames(volumes) <- 'n'

volumes$cats <- rownames(volumes)
volumes$cats <- factor(volumes$cats, levels=levels_volumes)

volumes_plot <- ggplot(volumes, aes(y=n, x=cats)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, fill=col[3]) +
  labs(x="", y="n\n", title='Data volume') +
  ylim(0,31)+
  theme_light() + 
  theme(
    legend.direction='horizontal',
    legend.title = element_blank(),
    legend.position='bottom',
    axis.text=element_text(size=14),
    legend.text = element_text(size=16),
    strip.text.x=element_text(size=16),
    plot.title=element_text(size=16),
    axis.title = element_text(size=16),
    aspect.ratio=1/4) +
  scale_x_discrete(labels = wrap_format(15)) +
  geom_label(aes(y=29, label=n), position=position_dodge(width=0.8), size=5, color='black', show.legend=FALSE)

cpu <- as.data.frame(c(15, 8, 1,7))
levels_cpu <- c('<10', '<50', '<100','>100')
rownames(cpu) <- levels_cpu
colnames(cpu) <- 'n'

cpu$cats <- rownames(cpu)
cpu$cats <- factor(cpu$cats, levels=levels_cpu)

cpu_plot <- ggplot(cpu, aes(y=n, x=cats)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, fill=col[4]) +
  labs(x="", y="n\n", title='Number of CPUs') +
  ylim(-1,20)+
  theme_light() + 
  theme(
    legend.direction='horizontal',
    legend.title = element_blank(),
    legend.position='bottom',
    axis.text=element_text(size=14),
    legend.text = element_text(size=16),
    strip.text.x=element_text(size=16),
    plot.title=element_text(size=16),
    axis.title = element_text(size=16),
    aspect.ratio=1/4) +
  scale_x_discrete(labels = wrap_format(15)) +
  geom_label(aes(y=19, label=n), position=position_dodge(width=0.8), size=5, color='black', show.legend=FALSE)


grid.draw(rbind(ggplotGrob(volumes_plot), ggplotGrob(instances_plot),
                ggplotGrob(memory_plot), ggplotGrob(cpu_plot)))
