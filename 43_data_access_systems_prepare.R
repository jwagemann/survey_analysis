library(tidyr)
wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

surveyData <- read.csv('./data/20190131_final_results.csv', header = TRUE, na.strings="")
no_of_respondents <- nrow(surveyData)

df_43 <- df_new[,c('X4.3.Download.service', 'X4.3.cloud.computing.infrastructure','X4.3.ogc.service','X4.3.custom.api.opendap', 'X4.3.virtual.research.infrastructure','X4.3.data.cube.technology','X4.3.spatial.array.database')]
df_43 <- df_43[-67,] # Remove entry with strange responses

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
dataSystems_freq <- cbind(A,B,C,D,E,F,G)


freqs <- lapply(dataSystems_freq, function(x) as.data.frame(table(x)))
sums <- c(freqs$A1[,2],freqs$A2[,2],freqs$A3[,2],freqs$B1[,2],freqs$B2[,2],freqs$B3[,2],freqs$C1[,2],freqs$C2[,2],freqs$C3[,2],freqs$D1[,2],freqs$D2[,2],freqs$D3[,2],freqs$E1[,2],freqs$E2[,2],freqs$E3[,2],
          freqs$F1[,2],freqs$F2[,2],freqs$F3[,2],freqs$G1[,2],freqs$G2[,2],freqs$G3[,2])

ds_current <- c(freqs$A1[,2],freqs$B1[,2],freqs$C1[,2],freqs$D1[,2],freqs$E1[,2],
                                freqs$F1[,2],freqs$G1[,2])

ds_current_rel <- ds_current / no_of_respondents * 100

ds_futureUse <- c(freqs$A2[,2],freqs$B2[,2],freqs$C2[,2],freqs$D2[,2],freqs$E2[,2],
               freqs$F2[,2],freqs$G2[,2])

ds_futureUse_rel <- ds_futureUse / no_of_respondents * 100

ds_no_interest <- c(freqs$A3[,2],freqs$B3[,2],freqs$C3[,2],freqs$D3[,2],freqs$E3[,2],
                  freqs$F3[,2],freqs$G3[,2])

ds_no_interest_rel <- ds_no_interest / no_of_respondents * 100

dataSystems <- c('Download service', 'Cloud-computing infrastructure', 'OGC web service, e.g. WMS / WCS', 'Custom API / OpeNDAP', 
                 'Virtual Research Infrastructure', 'Data Cube technology', 'Spatial / Array database')

dataAccess <- data.frame(dataSystems, ds_current, ds_futureUse, ds_no_interest)
dataAccess_rel <- data.frame(dataSystems, ds_current_rel, ds_futureUse_rel, ds_no_interest_rel)

dataAccess_order <- dataAccess %>%
  arrange(-ds_current) %>% 
  mutate(dataSystems = factor(dataSystems, unique(dataSystems)))

dataAccess_rel_order <- dataAccess_rel %>%
  arrange(-ds_current_rel) %>% 
  mutate(dataSystems = factor(dataSystems, unique(dataSystems)))
