
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

df_51 <- df_51[!(rowSums(is.na(df_51))==12),]

df_51 <- data.frame(list(ifelse(df_51==1,'No obstacle at all', 
                                ifelse(df_51==2, 'No obstacle', 
                                       ifelse(df_51==3, 'Neither no obstacle nor an obstacle', 
                                              ifelse(df_51==4, 'An obstacle', 
                                                     ifelse(df_51==5, 'A great obstacle', 'test')))))))

levels_51 <- c('No obstacle at all','No obstacle','Neither no obstacle nor an obstacle','An obstacle','A great obstacle')
df_51[is.na(df_51)] <- "Neither no obstacle nor an obstacle"
nrow_51 <- nrow(df_51)

a_51 <- count(df_51$X5.1.growing.data.volume)
b_51 <- count(df_51$X5.1.limited.processing.capacity)
c_51 <- count(df_51$X5.1.complex.data.formats)
d_51 <- count(df_51$X5.1.non.standard.data)
e_51 <- count(df_51$X5.1.data.discovery)
f_51 <- count(df_51$X5.1.data.access.systems)
g_51 <- count(df_51$X5.1.data.complexity)
h_51 <- count(df_51$X5.1.data.combination)
i_51 <- count(df_51$X5.1.too.many.data.platforms)
k_51 <- count(df_51$X5.1.restricted.data.services)
l_51 <- count(df_51$X5.1.lack.of.tools)
m_51 <- count(df_51$X5.1.data.services.cost)


df_51_freq <- a_51 %>% left_join(b_51, by='x') %>% left_join(c_51, by='x') %>% left_join(d_51, by='x') %>% 
  left_join(e_51, by='x') %>% left_join(f_51, by='x') %>% left_join(g_51, by='x') %>% left_join(h_51, by='x') %>% 
  left_join(i_51, by='x') %>% left_join(k_51, by='x') %>% left_join(l_51, by='x') %>% left_join(m_51, by='x')

colnames(df_51_freq) <- c('Scale',
                          'Growing data volume',
                          'Limited processing capacity',
                          'Complex data formats',
                          'Data are dissemintated in a non-standardised way',
                          'Data discovery',
                          'Data access systems',
                          'Data complexity',
                          'Combining different kind of geospatial datas',
                          'Too many data platforms and portals',
                          'Data services are too restricted',
                          'Lacking easy-to-use tools ',
                          'Cost of data services'
)

df_51_freq$Scale <- factor(df_51_freq$Scale, levels=levels_51)

df_51_perc <- df_51_freq[,-1] / nrow_51 * 100
df_51_perc <- cbind(df_51_freq[1], df_51_perc)

df_51_freq <- df_51_freq[order(factor(df_51_freq$Scale, levels=levels_51)),]
df_51_perc <- df_51_perc[order(factor(df_51_perc$Scale,levels=levels_51)),]

df_51_main <- df_51_freq[c(4,5),]
df_51_main_sum <- as.data.frame(colSums(df_51_main[,-1]))
df_51_main_sum$items <- rownames(df_51_main_sum)
df_51_main_ord <- df_51_main_sum[order(df_51_main_sum$`colSums(df_51_main[, -1])`),]

df_51_freq_melt <- melt(df_51_freq)
df_51_perc_melt <- melt(df_51_perc)

df_51_freq_melt$variable <- factor(df_51_freq_melt$variable, levels=df_51_main_ord$items)
df_51_perc_melt$variable <- factor(df_51_perc_melt$variable, levels=df_51_main_ord$items)


