
# Please rate how important the following tasks are for you?
df_46 <- df_new[, c('X4.6.server.cloud.processing',
                           'X4.6.parallel.computing',
                           'X4.6.time.series.retrieval',
                           'X4.6.Download.large.data.volumes',
                           'X4.6.standard.data.access',
                           'X4.6.web.applications.visualization',
                           'X4.6.data.discovery',
                           'X4.6.interoperability')]

# Filter out rows where all entries are NAs
df_46 <- df_46[!(rowSums(is.na(df_46))==8),]
n_row <- nrow(df_46)
# Set remaining NAs to 'neutral' response
df_46[is.na(df_46)] <- 'Neither not important nor important'

# Count frequencies for each statement
a_46 <- plyr::count(df_46$X4.6.server.cloud.processing)
b_46 <- plyr::count(df_46$X4.6.parallel.computing)
c_46 <- plyr::count(df_46$X4.6.time.series.retrieval)
d_46 <- plyr::count(df_46$X4.6.Download.large.data.volumes)
e_46 <- plyr::count(df_46$X4.6.standard.data.access)
f_46 <- plyr::count(df_46$X4.6.web.applications.visualization)
g_46 <- plyr::count(df_46$X4.6.data.discovery)
h_46 <- plyr::count(df_46$X4.6.interoperability)

# Bring all frequencies together
df_46_freq <- a_46 %>% left_join(b_46, by='x') %>% left_join(c_46, by='x') %>% left_join(d_46, by='x') %>% 
  left_join(e_46, by='x') %>% left_join(f_46, by='x') %>% left_join(g_46, by='x') %>% left_join(h_46, by='x')

# Add column names
colnames(df_46_freq) <- c('Importance', 'cloud.processing', 'parallel.computing', 'time.series.retrieval', 'download.large.volumes','standard.data.access',
                          'web.applications.visualization','data.discovery','interoperability')

# Define levels for liker scale
levels_46 <- c('Not at all important', 'Not important', 'Neither not important nor important', 'Important', 'Very important')

# Set levels to data frame
df_46_freq$Importance <- factor(df_46_freq$Importance, levels=levels_46)
#df_46_freq[is.na(df_46_freq)] <- 0

# Calculate percent
df_46_perc <- df_46_freq[,2:9] / n_row * 100
# Combine levels with percent values in a data frame
df_46_perc <- cbind(df_46_freq[1], df_46_perc)
#df_46_perc[is.na(df_46_perc)] <- 0

# Order levels based on the correct likert scale order
df_46_freq_ord <- df_46_freq[order(factor(df_46_freq$Importance, levels=levels_46)),]
df_46_perc_ord <- df_46_perc[order(factor(df_46_perc$Importance,levels=levels_46)),]

# Reorder horizontal items based on frequencies of two most important mentions
df_46_main <-df_46_freq_ord[c(4,5),]
df_46_main_sum <- as.data.frame(colSums(df_46_main[,-1]))
df_46_main_sum$items <- rownames(df_46_main_sum)
df_46_main_ord <- df_46_main_sum[order(df_46_main_sum$`colSums(df_46_main[, -1])`),]

# Melt data frame
df_46_perc_melt <- reshape2::melt(df_46_perc_ord)
df_46_freq_melt <- reshape2::melt(df_46_freq_ord)

# Set factor to reorder items
df_46_perc_melt$variable <- factor(df_46_perc_melt$variable, levels=df_46_main_ord$items)
df_46_freq_melt$variable <- factor(df_46_freq_melt$variable, levels=df_46_main_ord$items)

