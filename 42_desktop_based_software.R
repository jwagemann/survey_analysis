
####################################################
# Barplot to yes / no responses to use of desktop-based software
####################################################
ggplot(df_42_freq, aes(y=freq, x=yes.no,fill=yes.no, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="", y="n") +
  scale_fill_brewer(palette='Blues') +
  ylim(0,180) +
  theme_light()+ 
  theme(legend.position="none", aspect.ratio = 3/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14)) +
  scale_x_discrete(labels = wrap_format(10))


####################################################
# Create regional subsets for US / Canada and Europe
####################################################

df_42 <- as.data.frame(df_new[,'X4.2'])
colnames(df_42) <- c('yes.no')
df_421 <- as.data.frame(df_new[,'X4.2.1'])
colnames(df_421) <- 'software'

df_combined <- cbind(df_11_filter_all, df_42, df_421)
df_filter_eur <- as.data.frame(subset(df_combined, df_combined[,3]=='Yes' & df_combined[,2]=='Europe'))
df_filter_us <- as.data.frame(subset(df_combined, df_combined[,3]=='Yes' & df_combined[,2]=='United States of America & Canada'))

df_freq_eur <- splitInRows(df_filter_eur[,-1],3,nrow(df_filter_eur))
df_freq_us <- splitInRows(df_filter_us[,-1],3,nrow(df_filter_us))


####################################################
# Create data frame with total and regions for US / Canada and Europe
####################################################
df_total = df_421_order[c(-4,-1),]
df_total$region = 'Total'
df_eur = df_freq_eur[c(-7,-5),-2]
df_us = df_freq_us[c(-7,-5),-2]

df_final <- rbind(df_total, df_eur, df_us)

# Set levels to the data frame
levels_421 <- c('Total', 'Europe', 'United States of America & Canada')
df_final$region <- factor(df_final$region,levels=levels_421) 

# Barplot of absolute numbers for three regions: total, Europe and US / Canada
ggplot(df_final, aes(y=freq, x=reorder(software, -freq))) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=region)) +
  labs(x="\nDesktop-based software", y="n\n") +
  scale_fill_uchicago(palette='light') +
  ylim(0,110) +
  theme_light() + 
  theme(legend.direction='horizontal',
        legend.title = element_blank(),
        legend.position=c(0.68,0.9),
        axis.text=element_text(size=16),
        legend.text = element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(10)) +
  geom_text(aes(y=freq, label=freq, fill=region), position=position_dodge(width=0.7), vjust=-1, size=6, color='black')

