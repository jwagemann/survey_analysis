
# What are currently the greates obstacles accessing and working with Big Earth Data?
df_51 <- df_new[,c('X5.1.growing.data.volume',
                   'X5.1.limited.processing.capacity',
                   'X5.1.complex.data.formats',
                   'X5.1.non.standard.data',
                   'X5.1.data.discovery',
                   'X5.1.data.access.systems',
                   'X5.1.data.complexity',
                   'X5.1.data.combination',
                   'X5.1.too.many.data.platforms',
                   'X5.1.restricted.data.services',
                   'X5.1.lack.of.tools',
                   'X5.1.data.services.cost')]

df_52 <- df_new[,'X5.1.1']

# Remove all NA entries
df_51 <- df_51[!(rowSums(is.na(df_51))==12),]

# Translate the number 1 - 5 to obtacle classes
df_51 <- data.frame(list(ifelse(df_51==1,'No obstacle at all', 
                                ifelse(df_51==2, 'No obstacle', 
                                       ifelse(df_51==3, 'Neither no obstacle nor an obstacle', 
                                              ifelse(df_51==4, 'An obstacle', 
                                                     ifelse(df_51==5, 'A great obstacle', 'test')))))))

# Define the obstacle classes
levels_51 <- c('No obstacle at all','No obstacle','Neither no obstacle nor an obstacle','An obstacle','A great obstacle')

# Convert NA answers to 'Neither no obstacle nor an obstacle'
df_51[is.na(df_51)] <- "Neither no obstacle nor an obstacle"
# Get number of rows
nrow_51 <- nrow(df_51)

# For each challenge, count the number of rows
a_51 <- plyr::count(df_51$X5.1.growing.data.volume)
b_51 <- plyr::count(df_51$X5.1.limited.processing.capacity)
c_51 <- plyr::count(df_51$X5.1.complex.data.formats)
d_51 <- plyr::count(df_51$X5.1.non.standard.data)
e_51 <- plyr::count(df_51$X5.1.data.discovery)
f_51 <- plyr::count(df_51$X5.1.data.access.systems)
g_51 <- plyr::count(df_51$X5.1.data.complexity)
h_51 <- plyr::count(df_51$X5.1.data.combination)
i_51 <- plyr::count(df_51$X5.1.too.many.data.platforms)
k_51 <- plyr::count(df_51$X5.1.restricted.data.services)
l_51 <- plyr::count(df_51$X5.1.lack.of.tools)
m_51 <- plyr::count(df_51$X5.1.data.services.cost)

# Bring frequencies together into one dataframe
df_51_freq <- a_51 %>% left_join(b_51, by='x') %>% left_join(c_51, by='x') %>% left_join(d_51, by='x') %>% 
  left_join(e_51, by='x') %>% left_join(f_51, by='x') %>% left_join(g_51, by='x') %>% left_join(h_51, by='x') %>% 
  left_join(i_51, by='x') %>% left_join(k_51, by='x') %>% left_join(l_51, by='x') %>% left_join(m_51, by='x')

# Define colnames
colnames(df_51_freq) <- c('Scale',
                          'Growing data volume',
                          'Limited processing capacity',
                          'Complex data formats',
                          'Data are dissemintated in a non-standardised way',
                          'Data discovery',
                          'Data access systems',
                          'Data complexity',
                          'Combining different kind of geospatial data',
                          'Too many data platforms and portals',
                          'Data services are too restricted',
                          'Lacking easy-to-use tools ',
                          'Cost of data services'
)

# Add levels to data frame
df_51_freq$Scale <- factor(df_51_freq$Scale, levels=levels_51)

# Data frame with percentages
df_51_perc <- df_51_freq[,-1] / nrow_51 * 100
df_51_perc <- cbind(df_51_freq[1], df_51_perc)

# Order likert scale items based on level order
df_51_freq <- df_51_freq[order(factor(df_51_freq$Scale, levels=levels_51)),]
df_51_perc <- df_51_perc[order(factor(df_51_perc$Scale,levels=levels_51)),]

# Order items in an increasing order based on 'an obstacle' and 'a great obstacle' responses
df_51_main <- as.data.frame(colSums(df_51_freq[c(4,5),-1]))
df_51_main$items <- rownames(df_51_main)
df_51_main_ord <- df_51_main[order(df_51_main$`colSums(df_51_freq[c(4, 5), -1])`),]

# Melt the two data frames
df_51_freq_melt <- reshape2::melt(df_51_freq)
df_51_perc_melt <- reshape2::melt(df_51_perc)

# Order the levels of the melted dataframe based on the 'obstacle' responses
df_51_freq_melt$variable <- factor(df_51_freq_melt$variable, levels=df_51_main_ord$items)
df_51_perc_melt$variable <- factor(df_51_perc_melt$variable, levels=df_51_main_ord$items)


