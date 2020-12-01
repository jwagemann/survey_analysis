
barplot_62 <- ggplot(df_62_freq_ord, aes(fill=policy,y=perc, x=policy)) + 
  geom_bar(stat="identity",width=0.7) +
  geom_point(aes(y=freq/2), color='black', group=1) +
  scale_fill_uchicago(palette='light') +
  scale_x_discrete(labels = wrap_format(20)) +
  labs(x="", y="n") +
  scale_y_continuous(
    name='rel. Frequency',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ .*2, name="n"),
    lim=c(0,45)) +
  geom_text(aes(y=0,label=round(perc,1)), color='white', vjust=-0.5, size=5)+
  geom_text(aes(y=freq/2, label=freq), color='black', vjust=-1, size=5)+
  theme_light() + 
  theme(legend.position='none',
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=1/3)

####################################################
# Create regional subsets for US / Canada and Europe
####################################################

df_62 <- as.data.frame(df_new[,'X6.2'])

df_combined <- cbind(df_11_filter_all, df_62)
df_filter_eur <- as.data.frame(subset(df_combined, df_combined[,2]=='Europe'))
df_filter_us <- as.data.frame(subset(df_combined, df_combined[,2]=='United States of America & Canada'))

# Caculate frequencies per subset
df_62_eur <- plyr::count(df_filter_eur[,-1])
df_62_us <- plyr::count(df_filter_us[,-1])
df_62_total <- plyr::count(df_62)
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

# Grouped bar plot
ggplot(df_final_ord, aes(y=freq, x=policy)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=region)) +
  labs(x="Preference of cloud service policy", y="n") +
  scale_fill_uchicago(palette='light') +
  ylim(0,110) +
  theme_light() + 
  theme(legend.direction='horizontal',
        legend.title = element_blank(),
        legend.position=c(0.68,0.9),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=1/3) +
  scale_x_discrete(labels = wrap_format(20)) +
  geom_text(aes(y=freq, label=freq, fill=region), position=position_dodge(width=0.7), vjust=-1, size=4, color='darkgrey')

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
ggplot(df_final_ws_ord, aes(y=perc, x=policy)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.7, aes(fill=sector)) +
  labs(x="Preference of cloud service policy", y="Percent") +
  scale_fill_uchicago(palette='light') +
  theme_light() + 
  ylim(-1,50)+
  theme(legend.direction='horizontal',
        legend.title = element_blank(),
        legend.position='top',
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=1/4) +
  scale_x_discrete(labels = wrap_format(25)) +
  geom_text(aes(y=perc, label=round(perc,1), fill=sector), position=position_dodge(width=0.7), vjust=-1, size=4, color='darkgrey')
