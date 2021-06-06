########################
# Download service
#######################
download_eur <- dataAccess_eur[1,2:4]
colnames(download_eur) <- c('current', 'future', 'no_interest')

download_us <- dataAccess_us_ca[1,2:4]
colnames(download_us) <- c('current', 'future', 'no_interest') 

table(download_eur, download_us)

download <- rbind(download_eur,download_us)
rownames(download) <- c('Europe', 'North America')
download
chisq.test(download)

#####################
# Cloud service
#####################
cloud_eur <- dataAccess_eur[2,2:4]
colnames(cloud_eur) <- c('current', 'future', 'no_interest')

cloud_us <- dataAccess_us_ca[2,2:4]
colnames(cloud_us) <- c('current', 'future', 'no_interest')

cloud <- rbind(cloud_eur,cloud_us)
rownames(cloud) <- c('Europe', 'North America')
cloud

# Chi-Square test statistic
chi_cloud <- chisq.test(cloud)
residual_cloud <- round(chi_cloud$residuals, 3)
contrib <- 100*chi_cloud$residuals^2/chi_cloud$statistic
round(contrib, 3)

##########################
# OGC web service
##########################
ogc_eur <- dataAccess_eur[3,2:4]
colnames(ogc_eur) <- c('current', 'future', 'no_interest')

ogc_us <- dataAccess_us_ca[3,2:4]
colnames(ogc_us) <- c('current', 'future', 'no_interest')

ogc <- rbind(ogc_eur,ogc_us)
rownames(ogc) <- c('Europe', 'North America')
ogc

chi_ogc <- chisq.test(ogc)

##########################
# API / OpenDAP
##########################

api_eur <- dataAccess_eur[4,2:4]
colnames(api_eur) <- c('current', 'future', 'no_interest')

api_us <- dataAccess_us_ca[4,2:4]
colnames(api_us) <- c('current', 'future', 'no_interest')

api <- rbind(api_eur,api_us)
rownames(api) <- c('Europe', 'North America')
api

api_ogc <- chisq.test(api)

##########################
# VRI
##########################

vri_eur <- dataAccess_eur[5,2:4]
colnames(vri_eur) <- c('current', 'future', 'no_interest')

vri_us <- dataAccess_us_ca[5,2:4]
colnames(vri_us) <- c('current', 'future', 'no_interest')

vri <- rbind(vri_eur,vri_us)
rownames(vri) <- c('Europe', 'North America')
vri

vri_ogc <- chisq.test(vri)
vri_ogc

##########################
# Data cube technology
##########################

dc_eur <- dataAccess_eur[6,2:4]
colnames(dc_eur) <- c('current', 'future', 'no_interest')

dc_us <- dataAccess_us_ca[6,2:4]
colnames(dc_us) <- c('current', 'future', 'no_interest')

dc <- rbind(dc_eur,dc_us)
rownames(dc) <- c('Europe', 'North America')
dc

dc_chi <- chisq.test(dc)
dc_chi


##########################
# Array database
##########################

ad_eur <- dataAccess_eur[7,2:4]
colnames(ad_eur) <- c('current', 'future', 'no_interest')

ad_us <- dataAccess_us_ca[7,2:4]
colnames(ad_us) <- c('current', 'future', 'no_interest')

ad <- rbind(ad_eur,ad_us)
rownames(ad) <- c('Europe', 'North America')
ad

ad_chi <- chisq.test(ad)
ad_chi




###############################################################


current_eur <- as.data.frame(dataAccess_eur$ds_current_eur)
colnames(current_eur) <- 'europe'
rownames(current_eur) <- c('download_c', 'cloud_c', 'ogc_c', 'custom_api_c', 'vri_c', 'data_cube_c', 'array_db_c')

future_eur <- as.data.frame(dataAccess_eur$ds_futureUse_eur)
colnames(future_eur) <- 'europe'
rownames(future_eur) <- c('download_f', 'cloud_f', 'ogc_f', 'custom_api_f', 'vri_f', 'data_cube_f', 'array_db_f')

noInterest_eur <- as.data.frame(dataAccess_eur$ds_no_interest_eur)
colnames(noInterest_eur) <- 'europe'
rownames(noInterest_eur) <- c('download_n', 'cloud_n', 'ogc_n', 'custom_api_n', 'vri_n', 'data_cube_n', 'array_db_n')

current_us <- as.data.frame(dataAccess_us_ca$ds_current_us_ca)
colnames(current_us) <- 'North America'
rownames(current_us) <- c('download_c', 'cloud_c', 'ogc_c', 'custom_api_c', 'vri_c', 'data_cube_c', 'array_db_c')

future_us <- as.data.frame(dataAccess_us_ca$ds_futureUse_us_ca)
colnames(future_us) <- 'North America'
rownames(future_us) <- c('download_f', 'cloud_f', 'ogc_f', 'custom_api_f', 'vri_f', 'data_cube_f', 'array_db_f')

noInterest_us <- as.data.frame(dataAccess_us_ca$ds_no_interest_us_ca)
colnames(noInterest_us) <- 'North America'
rownames(noInterest_us) <- c('download_n', 'cloud_n', 'ogc_n', 'custom_api_n', 'vri_n', 'data_cube_n', 'array_db_n')

data_systems_eur <- rbind(current_eur, future_eur, noInterest_eur)
data_systems_us <- rbind(current_us, future_us, noInterest_us)
data_systems <- cbind(data_systems_eur, data_systems_us)

