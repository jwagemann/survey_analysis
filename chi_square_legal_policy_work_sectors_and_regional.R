legal_policy <- cbind(df_62_eur[-6,3], df_62_us[-6,3])

##################################
# Chi-square - Legal policy difference between Europe and NA
################################
rownames(legal_policy) <- levels_62
colnames(legal_policy) <- c('Europe', 'North America')

chi_lp <- chisq.test(legal_policy)
fisher.test(legal_policy)

residual_lp <- round(chi_lp$residuals, 3)
contrib <- 100*chi_lp$residuals^2/chi_lp$statistic
round(contrib, 3)

##################################
# Chi-square - Legal policy difference between work sectors
################################
lp_ws <- cbind(df_np_ig_freq[-6,2], df_gov_freq[-6,2], df_uni_freq[-6,2], df_com_freq[-6,2])
rownames(lp_ws) <- levels_62
colnames(lp_ws) <- c('non-profit/ig', 'gov', 'acamdia', 'commercial')
chi_lp_ws <- chisq.test(lp_ws)
fisher.test(lp_ws)

residual_lp_ws <- round(chi_lp_ws$residuals, 3)
contrib_ws <- 100*chi_lp_ws$residuals^2/chi_lp_ws$statistic
round(contrib_ws, 3)
