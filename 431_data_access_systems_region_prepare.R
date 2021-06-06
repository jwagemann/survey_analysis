# Load responses to question which data access systems users are currently using
df_43 <- df_new[,c('X4.3.Download.service', 'X4.3.cloud.computing.infrastructure','X4.3.ogc.service','X4.3.custom.api.opendap', 'X4.3.virtual.research.infrastructure','X4.3.data.cube.technology','X4.3.spatial.array.database')]
df_43 <- df_43[-67,] # Remove entry with strange responses

wr <- df_11_filter_all$region
wr <- wr[-67]


colnames(df_43) <- c('Download.service', 'Cloud.computing.infrastructure','OGC.web.service', 'Custom.API', 'Virtual.Research.Infrastructure', 'Data.Cube.Technology', 'Spatial.Array.Database')


# Split columns and seperate columns with multiple responses
A = tidyr::separate(as.data.frame(df_43[,1]),col=1,sep="[;]", into=c("A1","A2","A3"), remove=TRUE)
B = tidyr::separate(as.data.frame(df_43[,2]),col=1,sep="[;]", into=c("B1","B2","B3"), remove=TRUE)
C = tidyr::separate(as.data.frame(df_43[,3]),col=1,sep="[;]", into=c("C1","C2","C3"), remove=TRUE)
D = tidyr::separate(as.data.frame(df_43[,4]),col=1,sep="[;]", into=c("D1","D2","D3"), remove=TRUE)
E = tidyr::separate(as.data.frame(df_43[,5]),col=1,sep="[;]", into=c("E1","E2","E3"), remove=TRUE)
F = tidyr::separate(as.data.frame(df_43[,6]),col=1,sep="[;]", into=c("F1","F2","F3"), remove=TRUE)
G = tidyr::separate(as.data.frame(df_43[,7]),col=1,sep="[;]", into=c("G1","G2","G3"), remove=TRUE)

A[is.na(A$A1),1] <- "I am not interested in this kind of service" 
B[is.na(B$B1),1] <- "I am not interested in this kind of service" 
C[is.na(C$C1),1] <- "I am not interested in this kind of service" 
D[is.na(D$D1),1] <- "I am not interested in this kind of service" 
E[is.na(E$E1),1] <- "I am not interested in this kind of service" 
F[is.na(F$F1),1] <- "I am not interested in this kind of service" 
G[is.na(G$G1),1] <- "I am not interested in this kind of service" 



A$A2 <- ifelse(A$A1=="I would like to use or continue to use this kind of service in the future", A$A1, A$A2)
A$A3 <- ifelse(A$A1=="I am not interested in this kind of service", A$A1, A$A3)
A$A1<- ifelse(A$A1=="I would like to use or continue to use this kind of service in the future", NA, A$A1)
A$A1 <- ifelse(A$A1=="I am not interested in this kind of service", NA, A$A1)
A$A2 <- ifelse(!is.na(A$A1) & !is.na(A$A2), NA, A$A2)

B$B2 <- ifelse(B$B1=="I would like to use or continue to use this kind of service in the future", B$B1, B$B2)
B$B3 <- ifelse(B$B1=="I am not interested in this kind of service", B$B1, B$B3)
B$B1 <- ifelse(B$B1=="I would like to use or continue to use this kind of service in the future", NA, B$B1)
B$B1 <- ifelse(B$B1=="I am not interested in this kind of service", NA, B$B1)
B$B2 <- ifelse(!is.na(B$B1) & !is.na(B$B2), NA, B$B2)

C$C2 <- ifelse(C$C1=="I would like to use or continue to use this kind of service in the future", C$C1, C$C2)
C$C3 <- ifelse(C$C1=="I am not interested in this kind of service", C$C1, C$C3)
C$C1 <- ifelse(C$C1=="I would like to use or continue to use this kind of service in the future", NA, C$C1)
C$C1 <- ifelse(C$C1=="I am not interested in this kind of service", NA, C$C1)
C$C2 <- ifelse(!is.na(C$C1) & !is.na(C$C2), NA, C$C2)

D$D2 <- ifelse(D$D1=="I would like to use or continue to use this kind of service in the future", D$D1, D$D2)
D$D3 <- ifelse(D$D1=="I am not interested in this kind of service", D$D1, D$D3)
D$D1 <- ifelse(D$D1=="I would like to use or continue to use this kind of service in the future", NA, D$D1)
D$D1 <- ifelse(D$D1=="I am not interested in this kind of service", NA, D$D1)
D$D2 <- ifelse(!is.na(D$D1) & !is.na(D$D2), NA, D$D2)

E$E2 <- ifelse(E$E1=="I would like to use or continue to use this kind of service in the future", E$E1, E$E2)
E$E3 <- ifelse(E$E1=="I am not interested in this kind of service", E$E1, E$E3)
E$E1 <- ifelse(E$E1=="I would like to use or continue to use this kind of service in the future", NA, E$E1)
E$E1 <- ifelse(E$E1=="I am not interested in this kind of service", NA, E$E1)
E$E2 <- ifelse(!is.na(E$E1) & !is.na(E$E2), NA, E$E2)

F$F2 <- ifelse(F$F1=="I would like to use or continue to use this kind of service in the future", F$F1, F$F2)
F$F3 <- ifelse(F$F1=="I am not interested in this kind of service", F$F1, F$F3)
F$F1 <- ifelse(F$F1=="I would like to use or continue to use this kind of service in the future", NA, F$F1)
F$F1 <- ifelse(F$F1=="I am not interested in this kind of service", NA, F$F1)
F$F2 <- ifelse(!is.na(F$F1) & !is.na(F$F2), NA, F$F2)

G$G2 <- ifelse(G$G1=="I would like to use or continue to use this kind of service in the future", G$G1, G$G2)
G$G3 <- ifelse(G$G1=="I am not interested in this kind of service", G$G1, G$G3)
G$G1 <- ifelse(G$G1=="I would like to use or continue to use this kind of service in the future", NA, G$G1)
G$G1 <- ifelse(G$G1=="I am not interested in this kind of service", NA, G$G1)
G$G2 <- ifelse(!is.na(G$G1) & !is.na(G$G2), NA, G$G2)

# Bring all split columns together into one data frame
dataSystems_freq <- cbind(wr,A,B,C,D,E,F,G)

dataSystems_eur <- as.data.frame(subset(dataSystems_freq, dataSystems_freq[,1]=='Europe'))
dataSystems_us_ca <- as.data.frame(subset(dataSystems_freq, dataSystems_freq[,1]=='United States of America & Canada'))

dataSystems <- c('Download service', 'Cloud-computing infrastructure', 'OGC web service, e.g. WMS / WCS', 'Custom API / OpeNDAP', 
                 'Virtual Research Infrastructure', 'Data Cube technology', 'Spatial / Array database')

# Europe
freqs_eur <- lapply(dataSystems_eur, function(x) as.data.frame(table(x)))
sums <- c(freqs_eur$A1[,2],freqs_eur$A2[,2],freqs_eur$A3[,2],freqs_eur$B1[,2],freqs_eur$B2[,2],freqs_eur$B3[,2],freqs_eur$C1[,2],freqs_eur$C2[,2],freqs_eur$C3[,2],freqs_eur$D1[,2],freqs_eur$D2[,2],freqs_eur$D3[,2],freqs_eur$E1[,2],freqs_eur$E2[,2],freqs_eur$E3[,2],
          freqs_eur$F1[,2],freqs_eur$F2[,2],freqs_eur$F3[,2],freqs_eur$G1[,2],freqs_eur$G2[,2],freqs_eur$G3[,2])

ds_current_eur <- c(freqs_eur$A1[,2],freqs_eur$B1[,2],freqs_eur$C1[,2],freqs_eur$D1[,2],freqs_eur$E1[,2],
                freqs_eur$F1[,2],freqs_eur$G1[,2])


ds_futureUse_eur <- c(freqs_eur$A2[,2],freqs_eur$B2[,2],freqs_eur$C2[,2],freqs_eur$D2[,2],freqs_eur$E2[,2],
                  freqs_eur$F2[,2],freqs_eur$G2[,2])


ds_no_interest_eur <- c(freqs_eur$A3[,2],freqs_eur$B3[,2],freqs_eur$C3[,2],freqs_eur$D3[,2],freqs_eur$E3[,2],
                    freqs_eur$F3[,2],freqs_eur$G3[,2])

nrow_eur <- nrow(dataSystems_eur)
dataAccess_eur <- data.frame(dataSystems, ds_current_eur, ds_futureUse_eur, ds_no_interest_eur)
dataAccess_eur$current_per <- dataAccess_eur$ds_current_eur/nrow_eur
dataAccess_eur$future_per <- dataAccess_eur$ds_futureUse_eur/nrow_eur
dataAccess_eur$noInterest_per <- dataAccess_eur$ds_no_interest_eur/nrow_eur


# USA & Canada
freqs <- lapply(dataSystems_us_ca, function(x) as.data.frame(table(x)))
sums <- c(freqs$A1[,2],freqs$A2[,2],freqs$A3[,2],freqs$B1[,2],freqs$B2[,2],freqs$B3[,2],freqs$C1[,2],freqs$C2[,2],freqs$C3[,2],freqs$D1[,2],freqs$D2[,2],freqs$D3[,2],freqs$E1[,2],freqs$E2[,2],freqs$E3[,2],
          freqs$F1[,2],freqs$F2[,2],freqs$F3[,2],freqs$G1[,2],freqs$G2[,2],freqs$G3[,2])

ds_current_us_ca <- c(freqs$A1[,2],freqs$B1[,2],freqs$C1[,2],freqs$D1[,2],freqs$E1[,2],
                    freqs$F1[,2],freqs$G1[,2])


ds_futureUse_us_ca <- c(freqs$A2[,2],freqs$B2[,2],freqs$C2[,2],freqs$D2[,2],freqs$E2[,2],
                      freqs$F2[,2],freqs$G2[,2])


ds_no_interest_us_ca <- c(freqs$A3[,2],freqs$B3[,2],freqs$C3[,2],freqs$D3[,2],freqs$E3[,2],
                        freqs$F3[,2],freqs$G3[,2])

nrow_us_ca <- nrow(dataSystems_us_ca)
dataAccess_us_ca <- data.frame(dataSystems, ds_current_us_ca, ds_futureUse_us_ca, ds_no_interest_us_ca)
dataAccess_us_ca$current_per <- dataAccess_us_ca$ds_current_us_ca/nrow_us_ca
dataAccess_us_ca$future_per <- dataAccess_us_ca$ds_futureUse_us_ca/nrow_us_ca
dataAccess_us_ca$noInterest_per <- dataAccess_us_ca$ds_no_interest_us_ca/nrow_us_ca

# Current
dataAccess_region_cur <- data.frame(dataSystems, dataAccess_eur[,2], dataAccess_us_ca[,2])
colnames(dataAccess_region_cur) <- c('dataSystems', 'Europe_abs', 'usa_ca_abs')
dataAccess_region_cur_melt <- reshape2::melt(dataAccess_region_cur)

dataAccess_region_cur_per <- data.frame(dataSystems, dataAccess_eur[,5], dataAccess_us_ca[,5])
colnames(dataAccess_region_cur_per) <- c('dataSystems', 'Europe_per', 'usa_ca_per')
dataAccess_region_cur_per_melt <- reshape2::melt(dataAccess_region_cur_per)

dataAccess_region_current_final <- data.frame(dataAccess_region_cur_melt, dataAccess_region_cur_per_melt$value)
colnames(dataAccess_region_current_final) <- c('dataSystems', 'variable', 'current_abs', 'current_per')

# Future
dataAccess_region_fut <- data.frame(dataSystems, dataAccess_eur[,3], dataAccess_us_ca[,3])
colnames(dataAccess_region_fut) <- c('dataSystems', 'Europe_abs', 'usa_ca_abs')
dataAccess_region_fut_melt <- reshape2::melt(dataAccess_region_fut)

dataAccess_region_fut_per <- data.frame(dataSystems, dataAccess_eur[,6], dataAccess_us_ca[,6])
colnames(dataAccess_region_fut_per) <- c('dataSystems', 'Europe_per', 'usa_ca_per')
dataAccess_region_fut_per_melt <- reshape2::melt(dataAccess_region_fut_per)

dataAccess_region_future_final <- data.frame(dataAccess_region_fut_melt, dataAccess_region_fut_per_melt$value)
colnames(dataAccess_region_future_final) <- c('dataSystems', 'variable', 'future_abs', 'future_per')

# No interest
dataAccess_region_noInterest <- data.frame(dataSystems, dataAccess_eur[,4], dataAccess_us_ca[,4])
colnames(dataAccess_region_noInterest) <- c('dataSystems', 'Europe_abs', 'usa_ca_abs')
dataAccess_region_noInterest_melt <- reshape2::melt(dataAccess_region_noInterest)

dataAccess_region_noInterest_perc <- data.frame(dataSystems, dataAccess_eur[,7], dataAccess_us_ca[,7])
colnames(dataAccess_region_noInterest_perc) <- c('dataSystems', 'Europe_per', 'usa_ca_per')
dataAccess_region_noInterest_per_melt <- reshape2::melt(dataAccess_region_noInterest_perc)

dataAccess_region_noInterest_final <- data.frame(dataAccess_region_noInterest_melt, dataAccess_region_noInterest_per_melt$value)
colnames(dataAccess_region_noInterest_final) <- c('dataSystems', 'variable', 'noInterest_abs', 'noInterest_per')

dataAccess_order <- dataAccess %>%
  arrange(-ds_current) %>% 
  mutate(dataSystems = factor(dataSystems, unique(dataSystems)))

dataAccess_rel_order <- dataAccess_rel %>%
  arrange(-ds_current_rel) %>% 
  mutate(dataSystems = factor(dataSystems, unique(dataSystems)))

