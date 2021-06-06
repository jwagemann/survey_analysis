source('./65_cloud_security_aspects_prepare.R')

# Define risk levels
levels_65 <- c('No risk at all', 'Risk', 'Major risk')
# Order risk levels
df_65_freq_ord <- df_65_freq[order(factor(df_65_freq$risk, levels=levels_65)),]
df_65_perc_ord <- df_65_perc[order(factor(df_65_perc$risk,levels=levels_65)),]

# Melt data frame
df_65_perc_melt <- reshape2::melt(df_65_perc_ord[c(1,2,3),])
df_65_freq_melt <- reshape2::melt(df_65_freq_ord[c(1,2,3),])

# Bring freqency and percentage values into one data frame
df_65_freq_melt$perc <- df_65_perc_melt$value

# Melt frequencies for not important risk factor responses
df_65_freq_melt_not_important <- reshape2::melt(df_65_freq_ord[4,])

# Set factor to reorder items
df_65_freq_melt$variable <- factor(df_65_freq_melt$variable, levels=df_65_main_ord$items)
df_65_freq_melt_not_important$variable <- factor(df_65_freq_melt_not_important$variable, levels=df_65_main_ord$items)

# Reorder the risk factors based on the risk levels
#df_65_freq_melt$risk <- factor(df_65_freq_melt$risk, levels=levels_65)


# Rename the variable names for plotting
df_65_freq_melt$variable <- revalue(factor(df_65_freq_melt$variable),c("data.breaches"='Data breaches',
                                                                       'data.integrity'='Data integrity',
                                                                       'data.security'='Data security',
                                                                       'data.loss'='Data loss',
                                                                       'service.unavailability'='Service unavailability'))

# Rename the variable names for plotting
df_65_freq_melt_not_important$variable <- revalue(factor(df_65_freq_melt_not_important$variable),c("data.breaches"='Data breaches',
                                                                                     'data.integrity'='Data integrity',
                                                                                     'data.security'='Data security',
                                                                                     'data.loss'='Data loss',
                                                                                    'service.unavailability'='Service unavailability'))

# Retrieve percentage values for not important risk factor for plot labels
not_important_perc <- as.numeric(df_65_perc_ord[4,-1])

# Color scale for likert scale data
col <- brewer.pal(n=5,'BrBG')

# grouped bar plot indicating major risk, risk and no risk on the right side
likert_perc_65_right <- ggplot(data=df_65_freq_melt, aes(x=variable, y=value, fill=risk)) +
  geom_bar(stat='identity', width=0.7, position=position_dodge()) +
  scale_fill_manual(values=c('darkgrey', col[4], col[5])) +
  labs(x="",y="n")+
  theme_light()+
  coord_flip()+
  ylim(0,120)+
  theme(legend.position="top",
        axis.text=element_text(size=16),
        aspect.ratio=2/1,
        legend.text = element_text(size=16),
        legend.title=element_blank(),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16)) +
  geom_text(aes(y=6, label=value), position=position_dodge(width=0.7), size=6, color='white') +  
  geom_label(aes(y=115,label=round(perc,1)), position=position_dodge(width=0.7), size=6, show.legend=FALSE) +  
  scale_x_discrete(labels = wrap_format(10),position='top') +
  guides(fill=guide_legend(reverse=TRUE))

# bar plot indicating not important risk factor on the left side
likert_perc_65_left <- ggplot(data=df_65_freq_melt_not_important, aes(x=variable, y=value, fill=risk)) +
  geom_bar(stat='identity',width=0.5) +
  scale_fill_manual(values='#FFD77B') +
  labs(x="",y="n")+
  theme_light()+
  coord_flip()+
  theme(legend.position="top",
        axis.text=element_text(size=16),
        aspect.ratio=2/1,
        legend.text = element_text(size=16),
        legend.title=element_blank(),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16)) +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_reverse(lim=c(120,0))+
  geom_label(aes(y=115,label=round(not_important_perc,1)), size=6, show.legend=FALSE) +
  geom_text(aes(y=6, label=value), position=position_dodge(width=0.7), size=6, color='white') +  
  guides(fill=guide_legend(reverse=TRUE))

# Bring both plots together
grid.draw(cbind(ggplotGrob(likert_perc_65_left), ggplotGrob(likert_perc_65_right)))

          