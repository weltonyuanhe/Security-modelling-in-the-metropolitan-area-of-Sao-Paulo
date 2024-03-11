file.edit("~/.Renviron")
#install.packages("tidyverse")
#install.packages("MASS")
#install.packages("matlib")
#install.packages("stringr")
#install.packages("graphics")
#install.packages("zoo")
#install.packages("Metrics")
#install.packages("qtl2")
#install.packages("janitor")
install.packages("spdep")
install.packages("classInt")

installed.packages()
library(sf)
library(tidyverse)  # Modern data science workflow
library(spdep)
library(spatialreg)
library(rgdal)
library(dplyr)
library(stats)
library(glmnet)
#library(matlib)
library(MASS)
library(stringr)
library(graphics)
library(zoo)
library(Metrics)
library(qtl2)
library(janitor)
options(prompt="R> ", digits=4, scipen=7)


atn1 <- read_csv("pesquisa 2/atn1.csv")
atn2 <- read_csv("pesquisa 2/atn2.csv")


#View(atn1)
agregado_tudo_novo_1 <- atn1
agregado_tudo_novo_2 <- atn2

#agregado_tudo_novo_8 <- read_csv("Desktop/pesquisa 
#2/agregado_tudo_novo_8.csv")
#agregado_tudo_novo_9csv <- read_csv("Desktop/pesquisa 
#2/agregado_tudo_novo_9csv.csv")

#View(agregado_tudo_novo_1)
#View(agregado_tudo_novo_2)

agregado_tudo_novo_1$População <- 
str_replace(agregado_tudo_novo_1$População, '\\.', '')

agregado_tudo_novo_1$População <- 
sapply(agregado_tudo_novo_1$População, as.numeric)

target <- c('São Paulo', 'Guarulhos','Caieiras', 'Cajamar', 
            'Francisco Morato', 'Franco da Rocha',
            'Mairiporã','Arujá', 'Ferraz de Vasconcelos'
            ,'Guararema', 
            'Itaquaquecetuba', 'Mogi das 
            Cruzes', 'Poá', 'Salesópolis', 
            'Santa Isabel', 'Suzano','Diadema', 'Mauá', 
            'Santo André', 'São Bernardo do Campo'
            ,'São Caetano do Sul', 'Ribeirão Pires', 'Rio 
            Grande da Serra','Cotia'
            ,'Embu','Biritiba-Mirim', 'Embu-Guaçu', 'Itapecerica da Serra', 
            'Juquitiba', 
            'São Lourenço da Serra', 'Taboão da Serra', 
            'Vargem Grande Paulista'
            ,'Barueri', 'Carapicuíba', 'Itapevi', 
            'Jandira', 
            'Osasco'
            ,'Pirapora do Bom Jesus', 'Santana de Parnaíba')

#filtrar as cidades que estao proximo de sp 
agregado_tudo_novo_1 <- agregado_tudo_novo_1 %>% 
  filter(name %in% target)
nrow(agregado_tudo_novo_1)
ncol(agregado_tudo_novo_1)
agregado_tudo_novo_1$População[which(agregado_tudo_novo_1$name == "Guarulhos")] <- 1404694
agregado_tudo_novo_1$População[which(agregado_tudo_novo_1$name == "São Paulo")] <- 12396372 
#agregado_tudo$Total_de_crimes12m[agregado_tudo$Total_de_crimes12m == 0] <- NA
#agregado_tudo_1  <- na.locf(na.locf(agregado_tudo))
#agregado_tudo_1 <- agregado_tudo_1[agregado_tudo_1$name != "Arujá", ]  
nrow(agregado_tudo_novo_1)


vc <- "https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-35-mun.json"
dir.create("C:/temp")
downloader::download(url = vc, destfile = "C:/temp/gas.GeoJSON")
municipio <- st_read("C:/temp/gas.GeoJSON")
summary(municipio)

municipio_1 <- subset(municipio, name %in% agregado_tudo_novo_1$name)
summary(municipio_1)
head(municipio_1)
nrow(municipio_1)



#############################################################

agregado_tudo_novo_1$Total_de_crimes <- 
as.numeric(agregado_tudo_novo_1$Total_de_crimes)
agregado_tudo_novo_1$População <- 
as.numeric(agregado_tudo_novo_1$População)

agregado_tudo_novo_1$crime_per_capita <- 
(agregado_tudo_novo_1$Total_de_crimes / 
agregado_tudo_novo_1$População) * 100000 

agregado_tudo_novo_1$crime_per_capita

df_1 <- agregado_tudo_novo_1[c("name", "Total_de_crimes", 
"tempo", "População", "crime_per_capita")]
 

write.csv(df_1, "agregado_tudo_novo_6.csv", row.names = 
FALSE)
View(agregado_tudo_novo_6)
########################################################## 
#criar a matriz w 

nmunicipio_1 <- municipio_1[rep(seq_len(nrow(municipio_1)), 
each = 192), ]  # Base R

summary(nmunicipio_1)
head(nmunicipio_1)
nrow(nmunicipio_1)

queens_1 <- poly2nb(municipio_1, queen = TRUE)
queens_1[[1]]
str(queens_1)

alistw_1 <- nb2listw(queens_1,style = "W" ,zero.policy = TRUE) 

#W <- nb2mat(queens_1, glist=NULL, style="B", 
#zero.policy=TRUE)



nqueens_1 <- poly2nb(nmunicipio_1, queen = TRUE)
nqueens_1[[1]]
nqueens_1[[2]]
str(nqueens_1)

a2listw_1<- nb2listw(nqueens_1,style = "W" ,zero.policy = TRUE) 
a2listw_1
names(a2listw_1)

W <- nb2mat(nqueens_1, glist=NULL, style="W", zero.policy=TRUE)
W
dim(W)



attach(agregado_tudo_novo_1)

matriz = cbind(crime_per_capita, Total_de_crimes1m, 
Total_de_crimes2m, População,'60 e mais', 
matriculas_creche_municipal, matriculas_ESAI_estadual, 
matriculas_ESAF_municipal, matriculas_EM_municipal)
str(matriz)
matriz <- data.frame(crime_per_capita, Total_de_crimes1m, 
Total_de_crimes2m, População, '60 e mais', 
matriculas_creche_municipal, matriculas_ESAI_estadual, 
matriculas_ESAF_municipal, matriculas_EM_municipal)

matriz$crime_per_capita <- 
as.numeric(matriz$crime_per_capita)
matriz$Total_de_crimes1m <- 
as.numeric(matriz$Total_de_crimes1m)
matriz$Total_de_crimes2m <- 
as.numeric(matriz$Total_de_crimes2m)
matriz$População <- as.numeric(matriz$População)
unique(matriz$"X60.e.mais")

matriz$"X60.e.mais" <- as.numeric(matriz$"X60.e.mais")
matriz$matriculas_creche_municipal <- 
as.numeric(matriz$matriculas_creche_municipal)
matriz$matriculas_ESAI_estadual <- 
as.numeric(matriz$matriculas_ESAI_estadual)
matriz$matriculas_ESAF_municipal <- 
as.numeric(matriz$matriculas_ESAF_municipal)
matriz$matriculas_EM_municipal <- 
as.numeric(matriz$matriculas_EM_municipal)
matriz_numeric <- matriz[sapply(matriz, is.numeric)]

pairs(matriz_numeric)

matriz2 = cbind(log(Total_de_crimes), 
log(Total_de_crimes1m), log(Total_de_crimes2m), 
log(População), log('60 e mais'), 
log(matriculas_creche_municipal), 
log(matriculas_ESAI_estadual), 
log(matriculas_ESAF_municipal), log(matriculas_EM_municipal))
colnames(matriz2) <- 
c('log(Totaldecrimes)','log(Totaldecrimes1m)', 
'log(Totaldecrimes2m)', 'log(População)', 
'log(sessenta_e_mais)', 'log(matriculas_creche_municipal)', 
'log(matriculas_ESAI_estadual)', 
'log(matriculas_ESAF_municipal)', 
'log(matriculas_EM_municipal)')

pairs(matriz2)



########################################################## regressao sar 
agregado_tudo_3 <- agregado_tudo_novo_1[, 
c('crime_per_capita','Total_de_crimes1m', 
'Total_de_crimes2m','População','60 e mais','matriculas_creche_municipal','matriculas_ESAI_estadual'
                                     ,'matriculas_ESAF_municipal','matriculas_EM_municipal')]

#agregado_tudo_3 <- log(agregado_tudo_3 + 0.01)_
names(agregado_tudo_3)[names(agregado_tudo_3) == "60 e mais"] <- "sessenta_e_mais"
nrow(agregado_tudo_3)

agregado_tudo_3$Total_de_crimes1m[agregado_tudo_3$Total_de_crimes1m == ""] <- 0
agregado_tudo_3$Total_de_crimes2m[agregado_tudo_3$Total_de_crimes2m == ""] <- 0


agregado_tudo_3$Total_de_crimes1m[is.na(agregado_tudo_3$Total_de_crimes1m)] <- 0
agregado_tudo_3$Total_de_crimes2m[is.na(agregado_tudo_3$Total_de_crimes2m)] <- 0
agregado_tudo_3$População[is.na(agregado_tudo_3$População)] <- 0
agregado_tudo_3$sessenta_e_mais[is.na(agregado_tudo_3$sessenta_e_mais)] <- 0
agregado_tudo_3$matriculas_creche_municipal[is.na(agregado_tudo_3$matriculas_creche_municipal)] <-0
agregado_tudo_3$matriculas_ESAI_estadual[is.na(agregado_tudo_3$matriculas_ESAI_estadual)] <- 0
agregado_tudo_3$matriculas_ESAF_municipal[is.na(agregado_tudo_3$matriculas_ESAF_municipal)] <- 0
agregado_tudo_3$matriculas_EM_municipal[is.na(agregado_tudo_3$matriculas_EM_municipal)] <- 0
#View(agregado_tudo_3)
nrow(agregado_tudo_3)
ncol(agregado_tudo_3)

agregado_tudo_3$crime_per_capita <- 
as.numeric(agregado_tudo_3$crime_per_capita)
agregado_tudo_3$Total_de_crimes1m <- 
as.numeric(agregado_tudo_3$Total_de_crimes1m)
agregado_tudo_3$Total_de_crimes2m <- 
as.numeric(agregado_tudo_3$Total_de_crimes2m)
agregado_tudo_3$População <- 
as.numeric(agregado_tudo_3$População)
agregado_tudo_3$sessenta_e_mais <- 
as.numeric(agregado_tudo_3$sessenta_e_mais)
agregado_tudo_3$matriculas_creche_municipal <- 
as.numeric(agregado_tudo_3$matriculas_creche_municipal)
agregado_tudo_3$matriculas_ESAI_estadual <- 
as.numeric(agregado_tudo_3$matriculas_ESAI_estadual)
agregado_tudo_3$matriculas_ESAF_municipal <- 
as.numeric(agregado_tudo_3$matriculas_ESAF_municipal)
agregado_tudo_3$matriculas_EM_municipal <- 
as.numeric(agregado_tudo_3$matriculas_EM_municipal)

#reg3 <- lagsarlm(formula =
          #         crime_per_capita ~
           #        Total_de_crimes1m + Total_de_crimes2m +
                   #+ Total_de_crimes3m + Total_de_crimes4m 
                  # + Total_de_crimes5m +Total_de_crimes6m
                   #+ Total_de_crimes7m + Total_de_crimes8m 
                  # + Total_de_crimes9m + Total_de_crimes10m
                   # Total_de_crimes12m + #+X 
            #       População 
                 #   area + Grau + habdom 
                 #+ trinta_a_cinquentanove + 
             #    + sessenta_e_mais
                 #+ Valor_Agropecuaria + Valor_Serviços
                 #+ Valor_Adi + Valor_PIB_per_capita + VT
                # I_bebidas + VTI_metal +
                 # +  VTI_outros_equi 
                 #+ VTI_texteis
                 #+ VTI_min_n_met
                 #tx_aprovacao_fai_publica   + 
                # tx_reprovacao_fai_publica  
                 # + tx_aprovacao_fai_privada + 
                # tx_reprovacao_fai_privada 
                 #  + tx_aprovacao_medio_publica + 
                # tx_reprovacao_medio_publica 
                 # + tx_aprovacao_medio_privada  + 
                # tx_reprovacao_medio_privada
                 #  + tx_abandono_medio_privada  
              #   + matriculas_creche_municipal 
                # #  + matriculas_creche_particular + 
                # matriculas_pre_escola_estadual
                 #  + matriculas_pre_escola_municipal + 
                # matriculas_pre_escola_particular
               #  + matriculas_ESAI_estadual #+ ma
              # triculas_ESAI_municipal 
                 #  + matriculas_ESAI_particular + 
               #  matriculas_ESAF_estadual
                # +matriculas_ESAF_municipal#+ m
              #  atriculas_ESAF_particular 
                 #+  matriculas_EM_estadual 
              #   +  matriculas_EM_municipal
                 #  + matriculas_EM_particular, 
               #  ,data = agregado_tudo_3, listw = 
               #a2listw_1,type="lag",method="MC",zero.policy 
               #= TRUE)
#summary(reg3,Nagelkerke=T)



############ Ajuste com log da resposta e log das covariáveis - sem Total_de_crimes11m

#algumas vezes vai ter variacao de valor do rho 
reg6 <- lagsarlm(formula =
                   log(crime_per_capita+.01) ~
                   log(Total_de_crimes1m+.01) + log(Total_de_crimes2m+.01) +
                   log(População+.01) 
                 + log(sessenta_e_mais+.01) 
                 + log(matriculas_creche_municipal+.01) 
                 + log(matriculas_ESAI_estadual+.01) #+ matriculas_ESAI_municipal 
                 +log(matriculas_ESAF_municipal+.01)#+ matriculas_ESAF_particular 
                 +  log(matriculas_EM_municipal+.01)
                 , data = agregado_tudo_3, listw = a2listw_1, type="lag",method="MC",zero.policy = TRUE)

summary(reg6,Nagelkerke=T)



#summary(reg6,Nagelkerke=T)


options(prompt="R> ", digits=4, scipen=7)

agregado_tudo_novo_2$População <- 
str_replace(agregado_tudo_novo_2$População, '\\.', '')

agregado_tudo_novo_2$População <- 
sapply(agregado_tudo_novo_2$População, as.numeric)

target <- c('São Paulo', 'Guarulhos','Caieiras', 'Cajamar', 
            'Francisco Morato', 'Franco da Rocha',
            'Mairiporã','Arujá', 'Ferraz de Vasconcelos'
            ,'Guararema', 
            'Itaquaquecetuba', 'Mogi das 
            Cruzes', 'Poá', 'Salesópolis', 
            'Santa Isabel', 'Suzano','Diadema', 'Mauá', 
            'Santo André', 'São Bernardo do Campo'
            ,'São Caetano do Sul', 'Ribeirão Pires', 'Rio 
            Grande da Serra','Cotia'
            ,'Embu','Biritiba-Mirim', 'Embu-Guaçu', 'Itapecerica da Serra', 
            'Juquitiba', 
            'São Lourenço da Serra', 'Taboão da Serra', 
            'Vargem Grande Paulista'
            ,'Barueri', 'Carapicuíba', 'Itapevi', 
            'Jandira', 
            'Osasco'
            ,'Pirapora do Bom Jesus', 'Santana de Parnaíba')

#filtrar as cidades que estao proximo de sp 
agregado_tudo_novo_2 <- agregado_tudo_novo_2 %>% 
  filter(name %in% target)
nrow(agregado_tudo_novo_2)
ncol(agregado_tudo_novo_2)
agregado_tudo_novo_2$População[which(agregado_tudo_novo_2$name == "Guarulhos")] <- 1404694
agregado_tudo_novo_2$População[which(agregado_tudo_novo_2$name == "São Paulo")] <- 12396372 
#agregado_tudo$Total_de_crimes12m[agregado_tudo$Total_de_crimes12m == 0] <- NA
#agregado_tudo_1  <- na.locf(na.locf(agregado_tudo))
#agregado_tudo_1 <- agregado_tudo_1[agregado_tudo_1$name != "Arujá", ]  
nrow(agregado_tudo_novo_2)



agregado_tudo_novo_2$Total_de_crimes <- 
as.numeric(agregado_tudo_novo_2$Total_de_crimes)
agregado_tudo_novo_2$População <- 
as.numeric(agregado_tudo_novo_2$População)

agregado_tudo_novo_2$crime_per_capita <- 
(agregado_tudo_novo_2$Total_de_crimes / 
agregado_tudo_novo_2$População) * 100000

df_2 <- agregado_tudo_novo_2[c("name", "Total_de_crimes", 
"tempo", "População", "crime_per_capita")]


write.csv(df_1, "agregado_tudo_novo_5.csv", row.names = 
FALSE)
View(agregado_tudo_novo_5)




nmunicipio_2 <- municipio_1[rep(seq_len(nrow(municipio_1)), 
each = 12), ]  # Base R

summary(nmunicipio_2)
head(nmunicipio_2)
nrow(nmunicipio_2)

queens_2 <- poly2nb(municipio_1, queen = TRUE)
queens_2[[1]]
str(queens_2)

alistw_2 <- nb2listw(queens_2,style = "W" ,zero.policy = TRUE) 
alistw_2
#W <- nb2mat(queens_1, glist=NULL, style="B", zero.policy=TRUE)



nqueens_2 <- poly2nb(nmunicipio_2, queen = TRUE)
nqueens_2[[1]]
nqueens_2[[2]]
str(nqueens_2)

a2listw_2<- nb2listw(nqueens_2,style = "W" ,zero.policy = 
TRUE) 
a2listw_2
names(a2listw_2)

W2 <- nb2mat(nqueens_2, glist=NULL, style="W", 
zero.policy=TRUE)
round(W2)
W2
#W2 <- kronecker(diag(12), W)


beta = reg6$coefficients
beta
beta <- data.matrix(beta)
nrow(beta)

attach(agregado_tudo_novo_2)

my_colnames1 <- colnames(agregado_tudo_novo_2)
my_colnames1 # Apply colnames function
name(agregado_tudo_novo_2)
Xis_3 <-  agregado_tudo_novo_2 %>% dplyr::select( 
"Total_de_crimes1m", "Total_de_crimes2m","População",
                                            "60 e mais","matriculas_creche_municipal","matriculas_ESAI_estadual","matriculas_ESAF_municipal","matriculas_EM_municipal")
Xis_5 <-  agregado_tudo_novo_2 %>% 
dplyr::select("crime_per_capita")

nrow(Xis_3)

Xis_5$crime_per_capita <- as.numeric(Xis_5$crime_per_capita)
Xis_5$crime_per_capita <- log(Xis_5$crime_per_capita + 0.01)
Xis_5$crime_per_capita[is.na(Xis_5$crime_per_capita)] <- 0.01

Xis_3$Total_de_crimes1m <- 
as.numeric(Xis_3$Total_de_crimes1m)
Xis_3$Total_de_crimes2m <- 
as.numeric(Xis_3$Total_de_crimes2m)
Xis_3$População <- as.numeric(Xis_3$População)
Xis_3$"60 e mais" <- as.numeric(Xis_3$"60 e mais")
Xis_3$matriculas_creche_municipal <- 
as.numeric(Xis_3$matriculas_creche_municipal)
Xis_3$matriculas_ESAI_estadual <- 
as.numeric(Xis_3$matriculas_ESAI_estadual)
Xis_3$matriculas_ESAF_municipal <- 
as.numeric(Xis_3$matriculas_ESAF_municipal)

Xis_3$matriculas_EM_municipal <- 
as.numeric(Xis_3$matriculas_EM_municipal)


Xis_3$Total_de_crimes1m <- log(Xis_3$Total_de_crimes1m + 
0.01)
Xis_3$Total_de_crimes2m <- log(Xis_3$Total_de_crimes2m + 
0.01)
Xis_3$População <- log(Xis_3$População + 0.01)
Xis_3$"60 e mais" <- log(Xis_3$"60 e mais" + 0.01)
Xis_3$matriculas_creche_municipal <- 
log(Xis_3$matriculas_creche_municipal + 0.01)
Xis_3$matriculas_ESAI_estadual <- 
log(Xis_3$matriculas_ESAI_estadual + 0.01)
Xis_3$matriculas_ESAF_municipal <- 
log(Xis_3$matriculas_ESAF_municipal + 0.01)
Xis_3$matriculas_EM_municipal <- 
log(Xis_3$matriculas_EM_municipal + 0.01)


#transformar na para log(0.01) 


Xis_3$Total_de_crimes1m[is.na(Xis_3$Total_de_crimes1m)] <- 
0.01
Xis_3$Total_de_crimes2m[is.na(Xis_3$Total_de_crimes2m)] <- 
0.01
Xis_3$População[is.na(Xis_3$População)] <- 0.01
Xis_3$"60 e mais"[is.na(Xis_3$"60 e mais")] <- 0.01
Xis_3$matriculas_creche_municipal[is.na(Xis_3$matriculas_creche_municipal)] <-0.01
Xis_3$matriculas_ESAI_estadual[is.na(Xis_3$matriculas_ESAI_estadual)] <- 0.01
Xis_3$matriculas_ESAF_municipal[is.na(Xis_3$matriculas_ESAF_municipal)] <- 0.01
Xis_3$matriculas_EM_municipal[is.na(Xis_3$matriculas_EM_municipal)] <- 0.01
nrow(Xis_3)


new1 <- c(rnorm(n =444 , mean = 1, sd = 0))

Xis_4 <- cbind(new1,Xis_3)
#Xis_4 <- Xis_3



I <- diag(444)

rho <- 0.5959

X <-data.matrix(Xis_4)
X
nrow(X)
ncol(X)
nrow(beta)
ncol(beta)
beta
E  = (I - (rho*W2))
nrow(E)
ncol(E)
ES <- solve(E)
#View(ES)
ESD  <- X %*% beta 
#View(ESD)
nrow(ES)
ncol(ES)
nrow(ESD)
ncol(ESD)
e  <- ES %*% ESD
#View(e)
y <- Xis_5
y_chapeu <- e


cbind(y,y_chapeu)
combined_data <- cbind(
  agregado_tudo_novo_2$crime_per_capita,
  exp(y_chapeu),
  test_data_sf$predicted
)
colnames(combined_data) <- c("crime_per_capita", "SAR_model", "ANN")
View(combined_data)

install.packages("patchwork")
library(ggplot2)
library(patchwork)


# Create the first plot for "predicted"
plot_predicted <- ggplot(test_data_sf) +
  geom_sf(aes(fill = test_data_sf$predicted, geometry = 
  geometry)) +
  scale_fill_gradient(low="white", high="blue", 
  name="predicted") +
  labs(title="Choropleth plot of ANN about crime per capita 
\n on the metropolitan area of São Paulo")

# Create the second plot for "exp(y_chapeu)"
plot_exp_y_chapeu <- ggplot(test_data_sf) +
  geom_sf(aes(fill = exp(y_chapeu), geometry = geometry)) +
  scale_fill_gradient(low="white", high="blue", 
  name="predicted") +
  labs(title="Choropleth plot of SAR model about crime per 
capita \n on the metropolitan area of São Paulo")

# Set the figure size and resolution
options(repr.plot.width=40, repr.plot.height=5, repr.plot.res=80)

# Arrange the two plots side by side
final_plot <- plot_predicted + plot_exp_y_chapeu +
  plot_layout(ncol = 2)

# Show the final plot
final_plot


esti <- y_chapeu - y
esti

y <- unlist(y)
y <- as.numeric(y)
y_chapeu <- as.numeric(y_chapeu)
y_chapeu
res <- exp(y_chapeu)
res
resy <- exp(y)
resy

# Calculating RMSE using rmse()         
result = rmse(resy, res)

# Printing the value
print(result) 

#erro absoluto médio.
mae = mae(resy, res)

