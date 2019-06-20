source('./65_cloud_security_aspects_prepare.R')

col <- c("#F5F5F5","#80CDC1","#DFC27D","#A6611A")

df_65_perc_melt$risk <- factor(df_65_perc_melt$risk, levels=rev(levels(df_65_perc_melt$risk)))

likert_perc_65 <- ggplot(data=df_65_perc_melt, aes(x=variable, y=value, fill=risk)) +
  geom_bar(stat='identity',width=0.7) +
  scale_fill_manual(values=col) +
  coord_flip() +
  theme_light() 

likert_abs_65 <- ggplot(data=df_65_freq_melt, aes(x=variable, y=reorder(-value), fill=risk)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  coord_flip() +
  theme_light() 
