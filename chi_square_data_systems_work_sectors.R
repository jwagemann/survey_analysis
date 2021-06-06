##############################
# Download system
#############################
download_np_ig <- dataAccess_np_ig[1,2:4]
colnames(download_np_ig) <- c('current', 'future', 'no_interest')
download_gov <- dataAccess_gov[1,2:4]
colnames(download_gov) <- c('current', 'future', 'no_interest')
download_com <- dataAccess_com[1,2:4]
colnames(download_com) <- c('current', 'future', 'no_interest')
download_uni <- dataAccess_uni[1,2:4]
colnames(download_uni) <- c('current', 'future', 'no_interest')

download_ws <- rbind(download_np_ig, download_gov, download_com, download_uni)
rownames(download_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_download_ws <- chisq.test(download_ws)
residual_download_ws <- round(chi_download_ws$residuals, 3)
contrib <- 100*chi_download_ws$residuals^2/chi_download_ws$statistic
round(contrib, 3)

##############################
# Cloud system
#############################
cloud_np_ig <- dataAccess_np_ig[2,2:4]
colnames(cloud_np_ig) <- c('current', 'future', 'no_interest')
cloud_gov <- dataAccess_gov[2,2:4]
colnames(cloud_gov) <- c('current', 'future', 'no_interest')
cloud_com <- dataAccess_com[2,2:4]
colnames(cloud_com) <- c('current', 'future', 'no_interest')
cloud_uni <- dataAccess_uni[2,2:4]
colnames(cloud_uni) <- c('current', 'future', 'no_interest')

cloud_ws <- rbind(cloud_np_ig, cloud_gov, cloud_com, cloud_uni)
rownames(cloud_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_cloud_ws <- chisq.test(cloud_ws)

##############################
# OGC Web Service
#############################
ogc_np_ig <- dataAccess_np_ig[3,2:4]
colnames(ogc_np_ig) <- c('current', 'future', 'no_interest')
ogc_gov <- dataAccess_gov[3,2:4]
colnames(ogc_gov) <- c('current', 'future', 'no_interest')
ogc_com <- dataAccess_com[3,2:4]
colnames(ogc_com) <- c('current', 'future', 'no_interest')
ogc_uni <- dataAccess_uni[3,2:4]
colnames(ogc_uni) <- c('current', 'future', 'no_interest')

ogc_ws <- rbind(ogc_np_ig, ogc_gov, ogc_com, ogc_uni)
rownames(ogc_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_ogc_ws <- chisq.test(ogc_ws)
residual_ogc_ws <- round(chi_ogc_ws$residuals, 3)
contrib <- 100*chi_ogc_ws$residuals^2/chi_ogc_ws$statistic
round(contrib, 3)

##############################
# Custom API
#############################
api_np_ig <- dataAccess_np_ig[4,2:4]
colnames(api_np_ig) <- c('current', 'future', 'no_interest')
api_gov <- dataAccess_gov[4,2:4]
colnames(api_gov) <- c('current', 'future', 'no_interest')
api_com <- dataAccess_com[4,2:4]
colnames(api_com) <- c('current', 'future', 'no_interest')
api_uni <- dataAccess_uni[4,2:4]
colnames(api_uni) <- c('current', 'future', 'no_interest')

api_ws <- rbind(api_np_ig, api_gov, api_com, api_uni)
rownames(api_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_api_ws <- chisq.test(api_ws)

##############################
# VRI
#############################
vri_np_ig <- dataAccess_np_ig[5,2:4]
colnames(vri_np_ig) <- c('current', 'future', 'no_interest')
vri_gov <- dataAccess_gov[5,2:4]
colnames(vri_gov) <- c('current', 'future', 'no_interest')
vri_com <- dataAccess_com[5,2:4]
colnames(vri_com) <- c('current', 'future', 'no_interest')
vri_uni <- dataAccess_uni[5,2:4]
colnames(vri_uni) <- c('current', 'future', 'no_interest')

vri_ws <- rbind(vri_np_ig, vri_gov, vri_com, vri_uni)
rownames(vri_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_vri_ws <- chisq.test(vri_ws)
fisher.test(vri_ws)
residual_vri_ws <- round(chi_vri_ws$residuals, 3)
contrib <- 100*chi_vri_ws$residuals^2/chi_vri_ws$statistic
round(contrib, 3)

##############################
# Data cube technology
#############################
dc_np_ig <- dataAccess_np_ig[6,2:4]
colnames(dc_np_ig) <- c('current', 'future', 'no_interest')
dc_gov <- dataAccess_gov[6,2:4]
colnames(dc_gov) <- c('current', 'future', 'no_interest')
dc_com <- dataAccess_com[6,2:4]
colnames(dc_com) <- c('current', 'future', 'no_interest')
dc_uni <- dataAccess_uni[6,2:4]
colnames(dc_uni) <- c('current', 'future', 'no_interest')

dc_ws <- rbind(dc_np_ig, dc_gov, dc_com, dc_uni)
rownames(dc_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_dc_ws <- chisq.test(dc_ws)

##############################
# Spatial Array Database
#############################
ad_np_ig <- dataAccess_np_ig[7,2:4]
colnames(ad_np_ig) <- c('current', 'future', 'no_interest')
ad_gov <- dataAccess_gov[7,2:4]
colnames(ad_gov) <- c('current', 'future', 'no_interest')
ad_com <- dataAccess_com[7,2:4]
colnames(ad_com) <- c('current', 'future', 'no_interest')
ad_uni <- dataAccess_uni[7,2:4]
colnames(ad_uni) <- c('current', 'future', 'no_interest')

ad_ws <- rbind(ad_np_ig, ad_gov, ad_com, ad_uni)
rownames(ad_ws) <- c('np_ig', 'gov', 'com', 'uni')

chi_ad_ws <- chisq.test(ad_ws)
