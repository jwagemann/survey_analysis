# Load responses to question which data access systems users are currently using

df_431 <- df_new[,c('X4.3.Download.service', 'X4.3.cloud.computing.infrastructure','X4.3.ogc.service','X4.3.custom.api.opendap', 'X4.3.virtual.research.infrastructure','X4.3.data.cube.technology','X4.3.spatial.array.database')]
df_431 <- df_431[-67,] # Remove entry with strange responses
work.sector <- df_new[,'X2.1']
work.sector <- work.sector[-67]
colnames(df_431) <- c('work.sector','Download.service', 'Cloud.computing.infrastructure','OGC.web.service', 'Custom.API', 'Virtual.Research.Infrastructure', 'Data.Cube.Technology', 'Spatial.Array.Database')


# Split columns and seperate columns with multiple responses
A = tidyr::separate(as.data.frame(df_431[,1]),col=1,sep="[;]", into=c("A1","A2","A3"), remove=TRUE)
B = tidyr::separate(as.data.frame(df_431[,2]),col=1,sep="[;]", into=c("B1","B2","B3"), remove=TRUE)
C = tidyr::separate(as.data.frame(df_431[,3]),col=1,sep="[;]", into=c("C1","C2","C3"), remove=TRUE)
D = tidyr::separate(as.data.frame(df_431[,4]),col=1,sep="[;]", into=c("D1","D2","D3"), remove=TRUE)
E = tidyr::separate(as.data.frame(df_431[,5]),col=1,sep="[;]", into=c("E1","E2","E3"), remove=TRUE)
F = tidyr::separate(as.data.frame(df_431[,6]),col=1,sep="[;]", into=c("F1","F2","F3"), remove=TRUE)
G = tidyr::separate(as.data.frame(df_431[,7]),col=1,sep="[;]", into=c("G1","G2","G3"), remove=TRUE)

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
dataSystems_freq <- cbind(work.sector,A,B,C,D,E,F,G)

# Filter for work sectors
dataSystems_np_ig <- as.data.frame(subset(dataSystems_freq, (dataSystems_freq[,1]=='Non-profit' | dataSystems_freq[,1]=='Intergovernmental organisation')))
dataSystems_gov <- as.data.frame(subset(dataSystems_freq, dataSystems_freq[,1]=='Government'))
dataSystems_uni <- as.data.frame(subset(dataSystems_freq, dataSystems_freq[,1]=='University'))
dataSystems_com <- as.data.frame(subset(dataSystems_freq, (dataSystems_freq[,1]=='Start-up' | dataSystems_freq[,1]=='Established company')))

dataSystems <- c('Download service', 'Cloud-computing infrastructure', 'OGC web service, e.g. WMS / WCS', 'Custom API / OpeNDAP', 
                 'Virtual Research Infrastructure', 'Data Cube technology', 'Spatial / Array database')

# Non-Profit / Intergovernmental organisation
freqs_np_ig <- lapply(dataSystems_np_ig, function(x) as.data.frame(table(x)))
sums_np_ig <- c(freqs_np_ig$A1[,2],freqs_np_ig$A2[,2],freqs_np_ig$A3[,2],freqs_np_ig$B1[,2],freqs_np_ig$B2[,2],freqs_np_ig$B3[,2],freqs_np_ig$C1[,2],freqs_np_ig$C2[,2],freqs_np_ig$C3[,2],freqs_np_ig$D1[,2],
          freqs_np_ig$D2[,2],freqs_np_ig$D3[,2],freqs_np_ig$E1[1],freqs_np_ig$E2[,2],freqs_np_ig$E3[,2],
          freqs_np_ig$F1[,2],freqs_np_ig$F2[,2],freqs_np_ig$F3[,2],freqs_np_ig$G1[,2],freqs_np_ig$G2[,2],freqs_np_ig$G3[,2])

ds_current_np_ig <- c(freqs_np_ig$A1[,2],freqs_np_ig$B1[,2],freqs_np_ig$C1[,2],freqs_np_ig$D1[,2],0,
                freqs_np_ig$F1[,2],freqs_np_ig$G1[,2])

ds_futureUse_np_ig <- c(freqs_np_ig$A2[,2],freqs_np_ig$B2[,2],freqs_np_ig$C2[,2],freqs_np_ig$D2[,2],freqs_np_ig$E2[,2],
                  freqs_np_ig$F2[,2],freqs_np_ig$G2[,2])

ds_no_interest_np_ig <- c(freqs_np_ig$A3[,2],freqs_np_ig$B3[,2],freqs_np_ig$C3[,2],freqs_np_ig$D3[,2],freqs_np_ig$E3[,2],
                    freqs_np_ig$F3[,2],freqs_np_ig$G3[,2])

dataAccess_np_ig <- data.frame(dataSystems, ds_current_np_ig, ds_futureUse_np_ig, ds_no_interest_np_ig)

# Governmentt
freqs_gov <- lapply(dataSystems_gov, function(x) as.data.frame(table(x)))
freqs_gov$E1 <- 0
sums_gov <- c(freqs_gov$A1[,2],freqs_gov$A2[,2],freqs_gov$A3[,2],freqs_gov$B1[,2],freqs_gov$B2[,2],freqs_gov$B3[,2],freqs_gov$C1[,2],freqs_gov$C2[,2],freqs_gov$C3[,2],freqs_gov$D1[,2],
                freqs_gov$D2[,2],freqs_gov$D3[,2],freqs_gov$E1[1],freqs_gov$E2[,2],freqs_gov$E3[,2],
                freqs_gov$F1[,2],freqs_gov$F2[,2],freqs_gov$F3[,2],freqs_gov$G1[,2],freqs_gov$G2[,2],freqs_gov$G3[,2])

ds_current_gov <- c(freqs_gov$A1[,2],freqs_gov$B1[,2],freqs_gov$C1[,2],freqs_gov$D1[,2],freqs_gov$E1[1],
                      freqs_gov$F1[,2],freqs_gov$G1[,2])

ds_futureUse_gov <- c(freqs_gov$A2[,2],freqs_gov$B2[,2],freqs_gov$C2[,2],freqs_gov$D2[,2],freqs_gov$E2[,2],
                        freqs_gov$F2[,2],freqs_gov$G2[,2])

ds_no_interest_gov <- c(freqs_gov$A3[,2],freqs_gov$B3[,2],freqs_gov$C3[,2],freqs_gov$D3[,2],freqs_gov$E3[,2],
                          freqs_gov$F3[,2],freqs_gov$G3[,2])

dataAccess_gov <- data.frame(dataSystems, ds_current_gov, ds_futureUse_gov, ds_no_interest_gov)

# Academia
freqs_uni <- lapply(dataSystems_uni, function(x) as.data.frame(table(x)))
sums_uni <- c(freqs_uni$A1[,2],freqs_uni$A2[,2],freqs_uni$A3[,2],freqs_uni$B1[,2],freqs_uni$B2[,2],freqs_uni$B3[,2],freqs_uni$C1[,2],freqs_uni$C2[,2],freqs_uni$C3[,2],freqs_uni$D1[,2],
              freqs_uni$D2[,2],freqs_uni$D3[,2],freqs_uni$E1[,2],freqs_uni$E2[,2],freqs_uni$E3[,2],
              freqs_uni$F1[,2],freqs_uni$F2[,2],freqs_uni$F3[,2],freqs_uni$G1[,2],freqs_uni$G2[,2],freqs_uni$G3[,2])

ds_current_uni <- c(freqs_uni$A1[,2],freqs_uni$B1[,2],freqs_uni$C1[,2],freqs_uni$D1[,2],freqs_uni$E1[,2],
                    freqs_uni$F1[,2],freqs_uni$G1[,2])

ds_futureUse_uni <- c(freqs_uni$A2[,2],freqs_uni$B2[,2],freqs_uni$C2[,2],freqs_uni$D2[,2],freqs_uni$E2[,2],
                      freqs_uni$F2[,2],freqs_uni$G2[,2])

ds_no_interest_uni <- c(freqs_uni$A3[,2],freqs_uni$B3[,2],freqs_uni$C3[,2],freqs_uni$D3[,2],freqs_uni$E3[,2],
                        freqs_uni$F3[,2],freqs_uni$G3[,2])

dataAccess_uni <- data.frame(dataSystems, ds_current_uni, ds_futureUse_uni, ds_no_interest_uni)

# Private sector
freqs_com <- lapply(dataSystems_com, function(x) as.data.frame(table(x)))

sums_com <- c(freqs_com$A1[,2],freqs_com$A2[,2],freqs_com$A3[,2],freqs_com$B1[,2],freqs_com$B2[,2],freqs_com$B3[,2],freqs_com$C1[,2],freqs_com$C2[,2],freqs_com$C3[,2],freqs_com$D1[,2],
              freqs_com$D2[,2],freqs_com$D3[,2],freqs_com$E1[,2],freqs_com$E2[,2],freqs_com$E3[,2],
              freqs_com$F1[,2],freqs_com$F2[,2],freqs_com$F3[,2],freqs_com$G1[,2],freqs_com$G2[,2],freqs_com$G3[,2])

ds_current_com <- c(freqs_com$A1[,2],freqs_com$B1[,2],freqs_com$C1[,2],freqs_com$D1[,2],freqs_com$E1[,2],
                    freqs_com$F1[,2],freqs_com$G1[,2])

ds_futureUse_com <- c(freqs_com$A2[,2],freqs_com$B2[,2],freqs_com$C2[,2],freqs_com$D2[,2],freqs_com$E2[,2],
                      freqs_com$F2[,2],freqs_com$G2[,2])

ds_no_interest_com <- c(freqs_com$A3[,2],freqs_com$B3[,2],freqs_com$C3[,2],freqs_com$D3[,2],freqs_com$E3[,2],
                        freqs_com$F3[,2],freqs_com$G3[,2])

dataAccess_com <- data.frame(dataSystems, ds_current_com, ds_futureUse_com, ds_no_interest_com)


dataAccess_work_sectors_cur <- data.frame(dataSystems, dataAccess_np_ig[,2], dataAccess_gov[,2], dataAccess_uni[,2], dataAccess_com[,2])
colnames(dataAccess_work_sectors_cur) <- c('dataSystems', 'Nonprofit / Intergov. Org.', 'Government', 'University', 'Private sector')
dataAccess_work_sector_cur_melt <- reshape2::melt(dataAccess_work_sectors_cur)

dataAccess_work_sectors_future <- data.frame(dataSystems, dataAccess_np_ig[,3], dataAccess_gov[,3], dataAccess_uni[,3], dataAccess_com[,3])
colnames(dataAccess_work_sectors_future) <- c('dataSystems', 'Nonprofit / Intergov. Org.', 'Government', 'University', 'Private sector')
dataAccess_work_sector_future_melt <- reshape2::melt(dataAccess_work_sectors_future)

dataAccess_work_sectors_noInterest <- data.frame(dataSystems, dataAccess_np_ig[,4], dataAccess_gov[,4], dataAccess_uni[,4], dataAccess_com[,4])
colnames(dataAccess_work_sectors_noInterest) <- c('dataSystems', 'Nonprofit / Intergov. Org.', 'Government', 'University', 'Private sector')
dataAccess_work_sector_noInterest_melt <- reshape2::melt(dataAccess_work_sectors_noInterest)

dataSystems_work_sectors <- data.frame(dataAccess_work_sector_cur_melt, dataAccess_work_sector_future_melt[,3], dataAccess_work_sector_noInterest_melt[,3])
colnames(dataSystems_work_sectors) <- c('dataSystems', 'variable', 'current', 'future', 'noInterest')

