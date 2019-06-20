source('./dummies_prepare.R')

# How strong do you consider the following security aspects as a risk of cloud services?
df_65 <- df_new[, c('X6.5.data.integrity',
                    'X6.5.data.breaches',
                    'X6.5.data.loss',
                    'X6.5.service.unavailability',
                    'X6.5.data.security')]

levels_65 <- c('Major risk', 'Risk','No risk at all','Might be a risk, but not important for me')


df_65 <- df_65[!(rowSums(is.na(df_65))==5),]
n_row <- nrow(df_65)


a_65 <- count(df_65$X6.5.data.integrity)
b_65 <- count(df_65$X6.5.data.breaches)
c_65 <- count(df_65$X6.5.data.loss)
d_65 <- count(df_65$X6.5.service.unavailability)
e_65 <- count(df_65$X6.5.data.security)


df_65_freq <- a_65 %>% left_join(b_65, by='x') %>% left_join(c_65, by='x') %>% left_join(d_65, by='x') %>% 
  left_join(e_65, by='x')

colnames(df_65_freq) <- c('risk','data.integrity','data.breaches','data.loss','service.unavailability','data.security')


df_65_freq$risk <- factor(df_65_freq$risk, levels=levels_65)
df_65_freq <- df_65_freq[-5,]

df_65_perc <- df_65_freq[,2:6] / n_row * 100
df_65_perc <- cbind(df_65_freq[1], df_65_perc)
df_65_freq_ord <- df_65_freq[order(factor(df_65_freq$risk, levels=levels_65)),]
df_65_perc_ord <- df_65_perc[order(factor(df_65_perc$risk,levels=levels_65)),]

# Reorder horizontal items based on frequencies of two most important mentions
df_65_main <-df_65_freq_ord[c(1,2),]
df_65_main_sum <- as.data.frame(colSums(df_65_main[,-1]))
df_65_main_sum$items <- rownames(df_65_main_sum)
df_65_main_ord <- df_65_main_sum[order(df_65_main_sum$`colSums(df_65_main[, -1])`),]

# Melt data frame
df_65_perc_melt <- melt(df_65_perc_ord)
df_65_freq_melt <- melt(df_65_freq_ord)

# Set factor to reorder items
df_65_perc_melt$variable <- factor(df_65_perc_melt$variable, levels=df_65_main_ord$items)
df_65_freq_melt$variable <- factor(df_65_freq_melt$variable, levels=df_65_main_ord$items)
