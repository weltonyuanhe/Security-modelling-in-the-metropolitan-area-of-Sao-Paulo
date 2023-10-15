install.packages("tidyverse")
install.packages("MASS")
install.packages("matlib")
install.packages("stringr")
install.packages("graphics")
install.packages("zoo")
install.packages("Metrics")
install.packages("qtl2")

library(tidyverse)  # Modern data science workflow
library(spdep)
library(spatialreg)
library(rgdal)
library(dplyr)
library(stats)
library(glmnet)
library(matlib)
library(MASS)
library(stringr)
library(graphics)
library(zoo)
library(Metrics)
library(qtl2)
options(prompt="R> ", digits=4, scipen=7)

agregado_tudo <- read_csv("pesquisa 2/agregado_tudo.csv")
#View(agregado_tudo)
summary(agregado_tudo)

agregado_tudo$População <- str_replace(agregado_tudo$População, '\\.', '')

agregado_tudo$População <- sapply(agregado_tudo$População, as.numeric)

target <- c('São Paulo', 'Guarulhos','Caieiras', 'Cajamar', 'Francisco Morato', 'Franco da Rocha',
           'Mairiporã','Arujá', 'Ferraz de Vasconcelos', 'Guararema', 
            'Itaquaquecetuba','Guarulhos', 'Mogi das Cruzes', 'Poá', 'Salesópolis', 
            'Santa Isabel', 'Suzano','Diadema', 'Mauá', 'Santo André', 'São Bernardo do Campo'
            ,'São Caetano do Sul', 'Ribeirão Pires', 'Rio Grande da Serra','Cotia'
            , 'Embu-Guaçu', 'Itapecerica da Serra', 'Juquitiba', 
            'São Lourenço da Serra', 'Taboão da Serra', 'Vargem Grande Paulista'
            ,'Barueri', 'Carapicuíba', 'Itapevi', 'Jandira', 'Osasco'
            ,'Pirapora do Bom Jesus', 'Santana de Parnaíba')

#filtrar as cidades que estao proximo de sp 
agregado_tudo <- agregado_tudo %>% 
  filter(name %in% target)
nrow(agregado_tudo)
ncol(agregado_tudo)
agregado_tudo$População[which(agregado_tudo$name == "Guarulhos")] <- 1404694
agregado_tudo$População[which(agregado_tudo$name == "São Paulo")] <- 12396372 
#agregado_tudo$Total_de_crimes12m[agregado_tudo$Total_de_crimes12m == 0] <- NA
#agregado_tudo_1  <- na.locf(na.locf(agregado_tudo))
#agregado_tudo_1 <- agregado_tudo_1[agregado_tudo_1$name != "Arujá", ]  
nrow(agregado_tudo_1)


################################coletar os dados de latitude e longitude pelo formato de geojson

vc <- "https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-35-mun.json"
downloader::download(url = vc, destfile = "/tmp/gas.GeoJSON")
municipio <- readOGR(dsn = "/tmp/gas.GeoJSON")
summary(municipio)

municipio_1 <- subset(municipio, name %in% agregado_tudo$name)
summary(municipio_1)
head(municipio_1)
nrow(municipio_1)

########################################################## criar a matriz w 

nmunicipio_1 <- municipio_1[rep(seq_len(nrow(municipio_1)), each = 24), ]  # Base R

summary(nmunicipio_1)
head(nmunicipio_1)
nrow(nmunicipio_1)

queens_1 <- poly2nb(municipio_1, queen = TRUE)
queens_1[[1]]
str(queens_1)

alistw_1 <- nb2listw(queens_1,style = "W" ,zero.policy = TRUE) 

#W <- nb2mat(queens_1, glist=NULL, style="B", zero.policy=TRUE)



nqueens_1 <- poly2nb(nmunicipio_1, queen = TRUE)
nqueens_1[[1]]
nqueens_1[[2]]
str(nqueens_1)

a2listw_1<- nb2listw(nqueens_1,style = "W" ,zero.policy = TRUE) 
a2listw_1
names(a2listw_1)

W <- nb2mat(nqueens_1, glist=NULL, style="W", zero.policy=TRUE)
W


###################### Análise exploratória de dados -  Cibele 

attach(agregado_tudo)
matriz = cbind(Total_de_crimes, Total_de_crimes1m, Total_de_crimes2m, População, sessenta_e_mais, matriculas_creche_municipal, matriculas_ESAI_estadual, matriculas_ESAF_municipal, matriculas_EM_municipal)
pairs(matriz)

matriz2 = cbind(log(Total_de_crimes), log(Total_de_crimes1m), log(Total_de_crimes2m), log(População), log(sessenta_e_mais), log(matriculas_creche_municipal), log(matriculas_ESAI_estadual), log(matriculas_ESAF_municipal), log(matriculas_EM_municipal))
colnames(matriz2) <- c('log(Total_de_crimes)','log(Total_de_crimes1m)', 'log(Total_de_crimes2m)', 'log(População)', 'log(sessenta_e_mais)', 'log(matriculas_creche_municipal)', 'log(matriculas_ESAI_estadual)', 'log(matriculas_ESAF_municipal)', 'log(matriculas_EM_municipal)')
pairs(matriz2)



########################################################## regressao sar 
agregado_tudo_3 <- agregado_tudo[, c('crime_per_capita','Total_de_crimes1m', 'Total_de_crimes2m','População',
                         'sessenta_e_mais','matriculas_creche_municipal','matriculas_ESAI_estadual'
                         ,'matriculas_ESAF_municipal','matriculas_EM_municipal')]

#agregado_tudo_3 <- log(agregado_tudo_3 + 0.01)

agregado_tudo_3$Total_de_crimes1m[is.na(agregado_tudo_3$Total_de_crimes1m)] <- 0
agregado_tudo_3$Total_de_crimes1m[is.na(agregado_tudo_3$Total_de_crimes1m)] <- 0
agregado_tudo_3$Total_de_crimes2m[is.na(agregado_tudo_3$Total_de_crimes2m)] <- 0
agregado_tudo_3$População[is.na(agregado_tudo_3$População)] <- 0
agregado_tudo_3$sessenta_e_mais[is.na(agregado_tudo_3$sessenta_e_mais)] <- 0
agregado_tudo_3$matriculas_creche_municipal[is.na(agregado_tudo_3$matriculas_creche_municipal)] <-0
agregado_tudo_3$matriculas_ESAI_estadual[is.na(agregado_tudo_3$matriculas_ESAI_estadual)] <- 0
agregado_tudo_3$matriculas_ESAF_municipal[is.na(agregado_tudo_3$matriculas_ESAF_municipal)] <- 0
agregado_tudo_3$matriculas_EM_municipal[is.na(agregado_tudo_3$matriculas_EM_municipal)] <- 0
agregado_tudo_3

reg3 <- lagsarlm(formula =
                   Total_de_crimes ~
                   Total_de_crimes1m + Total_de_crimes2m +
                   #+ Total_de_crimes3m + Total_de_crimes4m + Total_de_crimes5m +Total_de_crimes6m
                   #+ Total_de_crimes7m + Total_de_crimes8m + Total_de_crimes9m + Total_de_crimes10m
                   # Total_de_crimes12m + #+X 
                   População 
                 #   area + Grau + habdom 
                 #+ trinta_a_cinquentanove + 
                 + sessenta_e_mais 
                 #+ Valor_Agropecuaria + Valor_Serviços
                 #+ Valor_Adi + Valor_PIB_per_capita + VTI_bebidas + VTI_metal +
                 # +  VTI_outros_equi 
                 #+ VTI_texteis
                 #+ VTI_min_n_met
                 #tx_aprovacao_fai_publica   + tx_reprovacao_fai_publica  
                 # + tx_aprovacao_fai_privada + tx_reprovacao_fai_privada 
                 #  + tx_aprovacao_medio_publica + tx_reprovacao_medio_publica 
                 # + tx_aprovacao_medio_privada  + tx_reprovacao_medio_privada
                 #  + tx_abandono_medio_privada  
                 + matriculas_creche_municipal 
                 #  + matriculas_creche_particular + matriculas_pre_escola_estadual
                 #  + matriculas_pre_escola_municipal + matriculas_pre_escola_particular
                 + matriculas_ESAI_estadual #+ matriculas_ESAI_municipal 
                 #  + matriculas_ESAI_particular + matriculas_ESAF_estadual
                 +matriculas_ESAF_municipal#+ matriculas_ESAF_particular 
                 #+  matriculas_EM_estadual 
                 +  matriculas_EM_municipal
                 #  + matriculas_EM_particular, 
                 ,data = agregado_tudo_3, listw = a2listw_1,type="lag",method="MC",zero.policy = TRUE)
summary(reg3,Nagelkerke=T)



############ Ajuste com log da resposta e log das covariáveis - sem Total_de_crimes11m


reg6 <- lagsarlm(formula =
                   log(Total_de_crimes+.01) ~
                   log(Total_de_crimes1m+.01) + log(Total_de_crimes2m+.01) +
                   log(População+.01) 
                 + log(sessenta_e_mais+.01) 
                 + log(matriculas_creche_municipal+.01) 
                  + log(matriculas_ESAI_estadual+.01) #+ matriculas_ESAI_municipal 
                 +log(matriculas_ESAF_municipal+.01)#+ matriculas_ESAF_particular 
                 +  log(matriculas_EM_municipal+.01)
                  , data = agregado_tudo_3, listw = a2listw_1, type="lag",method="MC",zero.policy = TRUE)

summary(reg6,Nagelkerke=T)


beta_teorico = reg6$coefficients
beta_teorico
# Colocar os resultados desse ajuste acima na monografia


###############################################################loop 




i <- 1
#todas as vezes que roda o while, muda o nome do arquivo(por exemplo, test_22 para test_23
#senão no arquivo em vez de registrar 200 vezes no novo arquivo vai registrar 400 vezes
#os parametros no arquivo velho) o lugar que modifica o nome do arquivo é no if-else 
while(i <= 1000){
  
I <- diag(888)

rho <- 0.7
epsilon <- rnorm(n =888, mean = 0, sd = 0.1)
epsilon
   
beta = reg6$coefficients
beta
beta <- data.matrix(beta)
nrow(beta)

Xis <- agregado_tudo_3[, c('Total_de_crimes1m', 'Total_de_crimes2m','População',
                                     'sessenta_e_mais','matriculas_creche_municipal','matriculas_ESAI_estadual'
                                     ,'matriculas_ESAF_municipal','matriculas_EM_municipal')]


new1 <- c(rnorm(n = 888, mean = 1, sd = 0))

Xis <- cbind(new1,Xis)
#transformar na para log(0.01) 
#Xis$new1[is.na(Xis$new1)] <- log(0.01)
#Xis$Total_de_crimes1m[is.na(Xis$Total_de_crimes1m)] <- log(0.01)
#Xis$Total_de_crimes2m[is.na(Xis$Total_de_crimes2m)] <- log(0.01)
#Xis$População[is.na(Xis$População)] <- log(0.01)
#Xis$sessenta_e_mais[is.na(Xis$sessenta_e_mais)] <- log(0.01)
#Xis$matriculas_creche_municipal[is.na(Xis$matriculas_creche_municipal)] <-log(0.01)
#Xis$matriculas_ESAI_estadual[is.na(Xis$matriculas_ESAI_estadual)] <- log(0.01)
#Xis$matriculas_ESAF_municipal[is.na(Xis$matriculas_ESAF_municipal)] <- log(0.01)
#Xis$matriculas_EM_municipal[is.na(Xis$matriculas_EM_municipal)] <- log(0.01)


X <-data.matrix(Xis)
X
nrow(X)
ncol(X)
E  = (I - (rho*W))
ES <- solve(E)
ES
ESD  <- X %*% beta + epsilon 
ESD
W
nrow(ESD)
ncol(ESD)
y  <- ES %*% ESD #simulacao dos y
y
agregado_tudo_n <- Xis
agregado_tudo_n$Total_de_crimes1m  <- Xis$Total_de_crimes1m
agregado_tudo_n$Total_de_crimes2m  <- Xis$Total_de_crimes2m
agregado_tudo_n$População           <- Xis$População
agregado_tudo_n$sessenta_e_mais  <- Xis$sessenta_e_mais
agregado_tudo_n$matriculas_creche_municipal  <- Xis$matriculas_creche_municipal
agregado_tudo_n$matriculas_ESAI_estadual  <- Xis$matriculas_ESAI_estadual
agregado_tudo_n$matriculas_ESAF_municipal  <- Xis$matriculas_ESAF_municipal
agregado_tudo_n$matriculas_EM_municipal  <- Xis$matriculas_EM_municipal
agregado_tudo_n$ipslon <- y
summary(agregado_tudo_n)
ncol(agregado_tudo_n)
ncol(Xis)



#estimacao dos parametros 
reg4 <- lagsarlm(formula =
                  ipslon ~
                   Total_de_crimes1m + Total_de_crimes2m +
                    (População) 
                  +(sessenta_e_mais) 
                  + (matriculas_creche_municipal) 
                  + (matriculas_ESAI_estadual) #+ matriculas_ESAI_municipal 
                  + (matriculas_ESAF_municipal)#+ matriculas_ESAF_particular 
                  +  (matriculas_EM_municipal)
                 ,data = agregado_tudo_n, listw = a2listw_1,type="lag",method="MC",zero.policy = TRUE)
summary(reg4,Nagelkerke=T)

matrix_coef <- summary(reg4,Nagelkerke=T)$coefficients  # Extract coefficients in matrix
matrix_rho <- summary(reg4,Nagelkerke=T)$rho  # Extract coefficients in matrix
as.numeric(matrix_coef)# Return matrix of coefficients
as.numeric(matrix_rho)

c <- bias(beta,matrix_coef)
d <- rmse(beta,matrix_coef )


#coef <- as.vector(append(matrix_coef,matrix_rho))
#coef <- round(c(matrix_coef, matrix_rho), 3)
# Matrix manipulation to extract estimates
coef <- as.vector(round(c(matrix_coef, matrix_rho,c,d), 7))
coef <- t(coef)
coef


if(i == 1){
  
write.table(coef, file = "test_44.csv" , #modifica aqui o nome do arquivo
            append = TRUE,col.names = TRUE, row.names = FALSE)
}
else{
write.table(coef, file = "test_44.csv", #modifica aqui o nome do arquivo)
            append = TRUE,col.names = FALSE, row.names = FALSE)
}
print(i)
i = i + 1
#my_data <- read.csv("test_7.csv")
#my_data
#close(file)
}

test_10 <-read_csv("test_44.csv")


View(test_10)
test_10[c('V1', 'V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12')] <- str_split_fixed(
  test_10$'V1" "V2" "V3" "V4" "V5" "V6" "V7" "V8" "V9" "V10" "V11" "V12', ' ', 12)

test_10$V1
test_10$V1 <- as.numeric(unlist(test_10$V1))
mean1 <- mean(test_10$V1, na.rm=TRUE)
mean1
bias(mean1,test_10$V1)
rmse(-5.5046977,test_10$V1)
boxplot(test_10$V1)

test_10$V2 <- as.numeric(unlist(test_10$V2))
mean2 <- mean(test_10$V2, na.rm=TRUE)
mean2
bias(mean2,test_10$V2)
rmse(mean2,test_10$V2)
boxplot(test_10$V2)

test_10$V3 <- as.numeric(unlist(test_10$V3))
mean3 <- mean(test_10$V3, na.rm=TRUE)
mean3
bias(mean3,test_10$V3)
rmse(mean3,test_10$V3)
boxplot(test_10$V3)

test_10$V4 <- as.numeric(unlist(test_10$V4))
mean4 <- mean(test_10$V4, na.rm=TRUE)
mean4
bias(mean4,test_10$V4)
rmse(mean4,test_10$V4)

test_10$V5 <- as.numeric(unlist(test_10$V5))
mean5 <- mean(test_10$V5, na.rm=TRUE)
mean5
bias(mean5,test_10$V5)
rmse(mean5,test_10$V5)

test_10$V6 <- as.numeric(unlist(test_10$V6))
mean6 <- mean(test_10$V6, na.rm=TRUE)
mean6
bias(mean6,test_10$V6)
rmse(mean6,test_10$V6)


test_10$V7 <- as.numeric(unlist(test_10$V7))
mean7 <- mean(test_10$V7, na.rm=TRUE)
mean7
bias(mean7,test_10$V7)
rmse(mean7,test_10$V7)


test_10$V8 <- as.numeric(unlist(test_10$V8))
mean8 <- mean(test_10$V8, na.rm=TRUE)
mean8
bias(mean8,test_10$V8)
rmse(mean8,test_10$V8)


test_10$V9 <- as.numeric(unlist(test_10$V9))
mean9 <- mean(test_10$V9, na.rm=TRUE)
mean9
bias(mean9,test_10$V9)
rmse(mean9,test_10$V9)



test_10$V10 
test_10$V10 <- as.numeric(unlist(test_10$V10))
mean10 <- mean(test_10$V10, na.rm=TRUE)
mean10
bias(mean10,test_10$V10)
rmse(mean10,test_10$V10)
boxplot(test_10$V10)




