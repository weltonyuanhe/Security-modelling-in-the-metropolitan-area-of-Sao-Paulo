install.packages("spdep")
install.packages("spatialreg")
install.packages("rgdal")
install.packages("rgeos")
install.packages("jsonlite")
install.packages("sf")
install.packages("lassopv")
install.packages("covTest")



library(tidyverse)  # Modern data science workflow
library(spdep)
library(spatialreg)
library(rgdal)
library(rgeos)
library(readr)
library(jsonlite)
library(readr)
library(dplyr)    
library(glmnet)
library(lassopv)
library(covTest)



# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(prompt="R> ", digits=4, scipen=7)

agregado_tudo <- read_csv("Desktop/pesquisa 2/agregado_tudo.csv")
View(agregado_tudo)
summary(agregado_tudo)

agregado_tudo <- agregado_tudo %>%
  mutate(across(-1,  ~ as.numeric(replace(., . == '', 0))))

agregado_tudo_lasso <- agregado_tudo[ , -
                                        which(names(agregado_tudo) %in% c("Cod IBGE","name"
                                                                          ,"tempo","Unnamed: 0","Localidades","Área (Em
km2)","Densidade Demográfica (Habitantes/km2)"
                                                                          ,"Grau de Urbanização (Em %)","Hab./Domicílio","Total 
H","Total M","00 a 14","15 a 29","30 a 59","60 e mais"
                                                                          ,"Cod_Ibge_x","Localidade_x","cod 
municipio","Localidade_y","Cod_Ibge_y","cod_ibge_x","Cod_Ibg
e
","cod_ibge_y" ))]



vc <- "https://raw.githubusercontent.com/tbrugz/geodata-
br/master/geojson/geojs-35-mun.json"
downloader::download(url = vc, destfile = "/tmp/gas.GeoJSON")
municipio <- readOGR(dsn = "/tmp/gas.GeoJSON")

newdata1 <- setdiff(municipio$name, agregado_tudo$name)
newdata2 <- setdiff(agregado_tudo$name, municipio$name)

municipio <- subset(municipio, name != "Natividade da Serra")
municipio <- subset(municipio, name != "Nova Canaã Paulista")
municipio <- subset(municipio, name != "Mirassolândia")
municipio <- subset(municipio, name != "Nazaré Paulista")
municipio <- subset(municipio, name != "Nova Odessa")
municipio <- subset(municipio, name != "Nipoã")
municipio <- subset(municipio, name != "Nova Castilho")
municipio <- subset(municipio, name != "Nhandeara")
municipio <- subset(municipio, name != "Aparecida d'Oeste")
municipio <- subset(municipio, name != "Estrela d'Oeste")
municipio <- subset(municipio, name != "Guarani d'Oeste")
municipio <- subset(municipio, name != "Moji Mirim")
municipio <- subset(municipio, name != "Nova Aliança")
municipio <- subset(municipio, name != "Palmeira d'Oeste")
municipio <- subset(municipio, name != "Santa Bárbara 
d'Oeste")
municipio <- subset(municipio, name != "Santa Rita d'Oeste")
municipio <- subset(municipio, name != "São João do Pau 
d'Alho")
municipio <- subset(municipio, name != "São Luís do 
Paraitinga")

summary(municipio)
head(municipio)
nrow(municipio)


nmunicipio <- municipio[rep(seq_len(nrow(municipio)), each = 
                              24), ]  # Base R


summary(nmunicipio)
head(nmunicipio)
nrow(nmunicipio)


queens <- poly2nb(municipio, queen = TRUE)
queens[[1]]
str(queens)

alistw <- nb2listw(queens,style = "W" ,zero.policy = TRUE) 
summary(alistw)
names(alistw)
alistw$neighbours[1]


alistw$weights[1]


plot(municipio, borders = 'lightgrey') 
plot(queens, coordinates(municipio), pch = 19, cex = 0.6, 
     add = TRUE, col = "red")



nqueens <- poly2nb(nmunicipio, queen = TRUE)
nqueens[[1]]
nqueens[[2]]
str(nqueens)

a2listw <- nb2listw(nqueens,style = "W" ,zero.policy = TRUE) 
a2listw
names(a2listw)
a2listw$neighbours[1]





# VTI_texteis + VTI_vestuario + VTI_couro + VTI_celulose + 
VTI_impressao + VTI_petroleo + VTI_biocombustíveis + 
  VTI_quimica + VTI_farmaceutica + VTI_min_n_met + 
  VTI_metalurgia +VTI_prod_div +tx_abandono_fai_publica   + 
  tx_abandono_fai_privada+ tx_aprovacao_faf_publica_x + 
  tx_reprovacao_faf_publica + tx_abandono_faf_publica  + 
  tx_reprovacao_faf_privada + tx_abandono_faf_privada 
+tx_abandono_medio_publica+ matriculas_creche_estadual

reg3 <- lagsarlm(formula =
                   
                   Total_de_crimes ~
                   #  Total_de_crimes1m + Total_de_crimes2m 
                   # + Total_de_crimes3m + Total_de_crimes4m + 
                   Total_de_crimes5m +Total_de_crimes6m
                 # + Total_de_crimes7m + Total_de_crimes8m + 
                 Total_de_crimes9m + Total_de_crimes10m
                 # + Total_de_crimes11m +
                 Total_de_crimes12m #+ População + area + 
                 Grau + habdom 
                 # + trinta_a_cinquentanove + sessenta_e_mais 
                 + Valor_Agropecuaria + Valor_Serviços
                 
                 # + Valor_Adi + Valor_PIB_per_capita + 
                 VTI_bebidas + VTI_metal + VTI_outros_equi
                 #     + tx_aprovacao_fai_publica   + 
                 tx_reprovacao_fai_publica  
                 #    + tx_aprovacao_fai_privada + 
                 tx_reprovacao_fai_privada 
                 #     + tx_aprovacao_medio_publica + 
                 tx_reprovacao_medio_publica 
                 #    + tx_aprovacao_medio_privada  + 
                 tx_reprovacao_medio_privada
                 #   + tx_abandono_medio_privada  + 
                 matriculas_creche_municipal 
                 #   + matriculas_creche_particular + 
                 matriculas_pre_escola_estadual
                 #   + matriculas_pre_escola_municipal + 
                 matriculas_pre_escola_particular
                 #   + matriculas_ESAI_estadual + 
                 matriculas_ESAI_municipal 
                 #   + matriculas_ESAI_particular + 
                 matriculas_ESAF_estadual
                 #   + matriculas_ESAF_municipal + 
                 matriculas_ESAF_particular 
                 #   + matriculas_EM_estadual + 
                 matriculas_EM_municipal 
                 #   + matriculas_EM_particular, 
                 ,data = agregado_tudo, listw = 
                   a2listw,na.action = na.omit)
summary(reg3,Nagelkerke=T)


#-------------------lasso 

cols.dont.want <- "Total_de_crimes"

y  <- as.matrix(agregado_tudo_lasso$Total_de_crimes)
X <- as.matrix(data.frame(agregado_tudo_lasso[,! 
                                                names(agregado_tudo_lasso) %in% cols.dont.want, drop = F]))
X[is.na(X)] <- 0
y[is.na(y)] <- 0


lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = 
                        lambdas_to_try,
                      standardize = TRUE, nfolds = 3)

plot(lasso_cv)
lasso_cv$lambda.min

coef(lasso_cv, s = "lambda.min")

summary(lasso_cv)
W <- as.matrix(coef(lasso_cv, s = "lambda.min"))
W

keep_X <- rownames(W)[W!=0]
keep_X <- keep_X[!keep_X == "(Intercept)"]
X <- X[,keep_X]
summary(lm(y~X))
