
####################################################
# Create regional subsets for US / Canada and Europe
####################################################

df_62 <- as.data.frame(df_new[,'X6.2'])

df_combined <- cbind(df_11_filter_all, df_62)
df_filter_eur <- as.data.frame(subset(df_combined, df_combined[,2]=='Europe'))
df_filter_us <- as.data.frame(subset(df_combined, df_combined[,2]=='United States of America & Canada'))

# Caculate frequencies per subset
df_62_eur <- plyr::count(df_filter_eur[,-1])
df_62_eur$per <- df_62_eur$freq / nrow(df_filter_eur)
df_62_us <- plyr::count(df_filter_us[,-1])
df_62_us$per <- df_62_us$freq / nrow(df_filter_us)
df_62_total <- plyr::count(df_62)
df_62_total$per <- df_62_total$freq / nrow(df_combined)
df_62_total$region <- 'Total'

# Bring everything together into one data frame
df_final <- rbind(df_62_total[-6,], df_62_eur[-6,], df_62_us[-6,])

# Add shortened policy lables as new column
df_final$policy <- levels_62
# Add labels for different region as column
levels_621 <- c('Total', 'Europe', 'United States of America & Canada')
df_final$region <- factor(df_final$region,levels=levels_621)

# Order the data based on levels order
df_final_ord <- df_final %>%
  arrange(freq) %>% 
  mutate(policy = factor(policy, levels=levels_62))

col <- brewer.pal(n=5,'BrBG')

# Grouped bar plot
policy_regional <- ggplot(df_final_ord, aes(y=per*100, x=policy)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=region)) +
  labs(x="", y="Percent\n") +
  scale_fill_manual(values=c('grey', col[5], '#FFD77B')) +
  ylim(-10,60) +
  theme_light() + 
  theme(legend.direction='horizontal',
        legend.title = element_blank(),
        legend.position=c(0.73,0.9),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=16),
        legend.text = element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(20)) +
  geom_label(aes(y=-5, label=round(per*100,0), fill=region), position=position_dodge(width=0.7), size=5, color='black',
             show.legend=FALSE)

####################################################
# Create subsets for work sectors
####################################################
# Combine data frame for legal policy and work sectors
df_combined_ws <- cbind(df_21, df_62)
# Seperate multiple mentions into different rows
df_combined_ws_split <- separate_rows(df_combined_ws, 1, sep=';')

# Filter our data frames for four work sector groups
df_filter_np_ig <- as.data.frame(subset(df_combined_ws_split, (df_combined_ws_split[,1]=='Non-profit' | df_combined_ws_split[,1]=='Intergovernmental organisation')))
df_filter_gov <- as.data.frame(subset(df_combined_ws_split, df_combined_ws_split[,1]=='Government'))
df_filter_uni <- as.data.frame(subset(df_combined_ws_split, df_combined_ws_split[,1]=='University'))
df_filter_com <- as.data.frame(subset(df_combined_ws_split, (df_combined_ws_split[,1]=='Start-up' | df_combined_ws_split[,1]=='Established company')))

# Count frequencies and calculate rel. Frequencies for each work sector group
df_np_ig_freq <- plyr::count(df_filter_np_ig[,-1])
df_np_ig_freq$sector <- 'Intergov. Org. / Non-profit'
df_np_ig_freq$perc <- df_np_ig_freq$freq / sum(df_np_ig_freq$freq) *100
df_gov_freq <- plyr::count(df_filter_gov[,-1])
df_gov_freq$sector <- 'Government'
df_gov_freq$perc <- df_gov_freq$freq / sum(df_gov_freq$freq) * 100
df_uni_freq <- plyr::count(df_filter_uni[,-1])
df_uni_freq$sector <- 'University'
df_uni_freq$perc <- df_uni_freq$freq / sum(df_uni_freq$freq) * 100
df_com_freq <- plyr::count(df_filter_com[,-1])
df_com_freq$sector <- 'Start-up / Established company'
df_com_freq$perc <- df_com_freq$freq / sum(df_com_freq$freq) * 100

# Bring the data frames together
df_final_ws <- rbind(df_np_ig_freq[-6,], df_gov_freq[-6,], df_uni_freq[-6,], df_com_freq[-6,])
df_final_ws$policy <- levels_62

# Order the data based on levels order
df_final_ws_ord <- df_final_ws %>%
  arrange(freq) %>% 
  mutate(policy = factor(policy, levels=levels_62))

# Grouped bar plot
policy_ws <- ggplot(df_final_ws_ord, aes(y=perc, x=policy)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=sector)) +
  labs(x="\nPreference of cloud service policy", y="Percent\n") +
  scale_fill_uchicago(palette='light', alpha=0.8) +
  theme_light() + 
  ylim(-10,70)+
  theme(legend.direction='horizontal',
        legend.title = element_blank(),
        legend.position=c(0.60,0.9),
        axis.text=element_text(size=16),
        legend.text = element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(20)) +
  geom_label(aes(y=-5, label=round(perc,0), fill=sector), position=position_dodge(width=0.7), size=5, color='black', show.legend=FALSE)

grid.draw(rbind(ggplotGrob(policy_regional), ggplotGrob(policy_ws), size='first'))
