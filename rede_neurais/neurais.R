install.packages("neuralnet")
install.packages("NeuralNetTools")
install.packages("DiagrammeR")
install.packages("keras")
install.packages("ggspatial")
install.packages("jpeg")
install.packages("nnet")
install.packages("tensorflow")
install.packages("reticulate")
install.packages("sjPlot")
library(reticulate)
#Change the path to your Python installation directory
py_install("pydot")



library(ggspatial)
library(nnet)
library(jpeg)
library(sf)
library(tidyverse)  # Modern data science workflow
library(spdep)
library(spatialreg)
#library(rgdal)
library(dplyr)
library(stats)
library(glmnet)
#library(matlib)
library(MASS)
library(stringr)
library(readr)
library(graphics)
library(zoo)
library(Metrics)
library(qtl2)
library(janitor)
library(neuralnet)
library(NeuralNetTools)
library(DiagrammeR)
library(keras)
#library(graphviz) 
#library(pydot)
library(tensorflow)
#log_dir <- "logs"  # Specify a directory to store TensorBoard logs
#tensorboard_callback <- callback_tensorboard(log_dir = log_dir, 
#                                             histogram_freq = 1, write_graph = TRUE, write_images = TRUE)

options(prompt="R> ", digits=4, scipen=7)


atn1 <- read_csv("/Users/wellingtonzhao/Desktop/atn1.csv")
atn2 <- read_csv("/Users/wellingtonzhao/Desktop/atn2.csv")
#agregado_tudo_novo_8 <- read_csv("Desktop/pesquisa 2/agregado_tudo_novo_8.csv")
#agregado_tudo_novo_9csv <- read_csv("Desktop/pesquisa 2/agregado_tudo_novo_9csv.csv")

agregado_tudo_novo_11 <- atn1
agregado_tudo_novo_2 <- atn2

agregado_tudo_novo_11$População <- 
  str_replace(agregado_tudo_novo_11$População, '\\.', '')

agregado_tudo_novo_11$População <- 
  sapply(agregado_tudo_novo_11$População, as.numeric)

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
agregado_tudo_novo_11 <- agregado_tudo_novo_11 %>% 
  filter(name %in% target)
nrow(agregado_tudo_novo_11)
ncol(agregado_tudo_novo_11)
agregado_tudo_novo_11$População[which(agregado_tudo_novo_11$name 
                                      == "Guarulhos")] <- 1404694
agregado_tudo_novo_11$População[which(agregado_tudo_novo_11$name 
                                      == "São Paulo")] <- 12396372
agregado_tudo_novo_11$População[which(agregado_tudo_novo_11$name 
                                      == "Embu")] <- 276535
agregado_tudo_novo_11$População[which(agregado_tudo_novo_11$name 
                                      == "Biritiba-Mirim")] <- 32936
#agregado_tudo$Total_de_crimes12m[agregado_tudo$Total_de_crimes12m == 0] <- NA
#agregado_tudo_1  <- na.locf(na.locf(agregado_tudo))
#agregado_tudo_1 <- agregado_tudo_1[agregado_tudo_1$name != "Arujá", ]  
nrow(agregado_tudo_novo_11)


vc <- "https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-35-mun.json"
dir.create("C:/temp")
downloader::download(url = vc, destfile = "C:/temp/gas.GeoJSON")
municipio <- st_read("C:/temp/gas.GeoJSON")
summary(municipio)

municipio_1 <- subset(municipio, name %in% 
                        agregado_tudo_novo_11$name)
summary(municipio_1)
head(municipio_1)
nrow(municipio_1)


# Convert SpatialPolygonsDataFrame to sf object
municipio_1_sf <- st_as_sf(municipio_1)

# Calculate area in km2
municipio_1_sf$area_km2 <- st_area(municipio_1_sf) / 10^6

# Calculate population density
agregado_tudo_novo_11 <- agregado_tudo_novo_11 %>%
  left_join(municipio_1_sf %>% st_drop_geometry(), by = c("name" = "name")) %>%
  mutate(pop_density = População / area_km2)


# View results
head(agregado_tudo_novo_11$pop_density)
#View(agregado_tudo_novo_1)

agregado_tudo_novo_11$pop_density <- gsub("[^0-9.]", "", 
                                          agregado_tudo_novo_11$pop_density)
agregado_tudo_novo_11$pop_density <- 
  as.numeric(agregado_tudo_novo_11$pop_density)
head(agregado_tudo_novo_11$pop_density)


agregado_tudo_novo_11$Total_de_crimes <- 
  as.numeric(agregado_tudo_novo_11$Total_de_crimes)
agregado_tudo_novo_11$População <- 
  as.numeric(agregado_tudo_novo_11$População)

agregado_tudo_novo_11$crime_per_capita <- 
  (agregado_tudo_novo_11$Total_de_crimes / 
     agregado_tudo_novo_11$População) * 100000 
agregado_tudo_novo_11$crime_per_capita




# Load data
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
gridded(meuse) <- TRUE
meuse_polygons <- as(meuse, "SpatialPolygonsDataFrame")

# Load municipio_2 and convert to sf object
municipio_1_sf <- st_as_sf(municipio_1, coords = c("long", 
                                                   "lat"), crs = 4326)

# Join municipio_2_sf with test_data_norm_2 based on "name" 
column
test_data_norm_df_3 <- as.data.frame(agregado_tudo_novo_11)

test_data_sf_2 <- left_join(test_data_norm_df_3, municipio_1_sf, 
                            by = c("name" = "name")) %>%
  dplyr::select(name, crime_per_capita, pop_density, geometry)

test_data_sf_2$crime_per_capita <- 
  as.numeric(test_data_sf_2$crime_per_capita)
test_data_sf_2$crime_per_capita

library(ggplot2)



p <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = crime_per_capita, geometry = geometry)) +
  scale_fill_gradient(low="white", high="blue", 
                      name="crime_per_capita") +
  # Set the figure size and resolution
  options(repr.plot.width=10, repr.plot.height=5, repr.plot.res=80)

# Show the plot
p

# Ensure test_data_sf_2 is an sf object with proper geometry
# If test_data_sf_2 is already correctly formatted, this step is not necessary
test_data_sf_2 <- st_as_sf(test_data_sf_2, coords = c("lon", "lat"), crs = 4326)

test_data_sf_2 <- st_as_sf(test_data_sf_2, sf_column_name = "geometry")


pdois <- ggplot(data = test_data_sf_2) +
  geom_sf(aes(fill = crime_per_capita)) +  # Visualize data with fill based on 'crime_per_capita'
  scale_fill_gradient(low = "white", high = "blue", name = "Crime per Capita") +
  geom_sf_text(aes(label = name), size = 2.2, check_overlap = FALSE, position = position_nudge(y = 0.005)) +  # Force display all city names
  theme_minimal() +  # Use a minimal theme for clarity
  labs(x = "Longitude", y = "Latitude")

# Show the plot
print(pdois)














###########################################################
agregado_tudo_3 <- agregado_tudo_novo_11[, 
                                         c('crime_per_capita','População','pop_density','60 e mais','matriculas_creche_municipal'
                                           ,'matriculas_ESAI_estadual','matriculas_ESAF_municipal','matriculas_EM_municipal')]

#agregado_tudo_3 <- log(agregado_tudo_3 + 0.01)_
names(agregado_tudo_3)[names(agregado_tudo_3) == "60 e mais"] <- 
  "sessenta_e_mais"
nrow(agregado_tudo_3)

#View(agregado_tudo_3) 

agregado_tudo_3$crime_per_capita[is.na(agregado_tudo_3$crime_per_capita)] <- 0
agregado_tudo_3$pop_density[is.na(agregado_tudo_3$pop_density)] <- 0
agregado_tudo_3$População[is.na(agregado_tudo_3$População)] <- 0
agregado_tudo_3$sessenta_e_mais[is.na(agregado_tudo_3$sessenta_e_mais)] <- 0
agregado_tudo_3$matriculas_creche_municipal[is.na(agregado_tudo_3$matriculas_creche_municipal)] <-0
agregado_tudo_3$matriculas_ESAI_estadual[is.na(agregado_tudo_3$matriculas_ESAI_estadual)] <- 0
agregado_tudo_3$matriculas_ESAF_municipal[is.na(agregado_tudo_3$matriculas_ESAF_municipal)] <- 0
agregado_tudo_3$matriculas_EM_municipal[is.na(agregado_tudo_3$matriculas_EM_municipal)] <- 0
#View(agregado_tudo_3)
nrow(agregado_tudo_3)
ncol(agregado_tudo_3)

agregado_tudo_novo_3$crime_per_capita <- 
  as.numeric(agregado_tudo_novo_3$crime_per_capita)
agregado_tudo_3$pop_density <- 
  as.numeric(agregado_tudo_3$pop_density)
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
#View(agregado_tudo_3)

# Criando uma matriz de entrada com as variáveis independentes
x <- as.matrix(agregado_tudo_3)

# Criando uma matriz de saída com a variável dependente
y <- as.matrix(agregado_tudo_3$crime_per_capita)

# Normalizando os dados
x_norm <- apply(x, 2, function(x) (x - min(x)) / (max(x) - 
                                                    min(x)))
y_norm <- apply(y, 2, function(y) (y - min(y)) / (max(y) - 
                                                    min(y)))
View(x_norm)
# Dividindo os dados em conjunto de treino e teste
set.seed(123)








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
agregado_tudo_novo_2$População[which(agregado_tudo_novo_2$name 
                                     == "Guarulhos")] <- 1404694
agregado_tudo_novo_2$População[which(agregado_tudo_novo_2$name 
                                     == "São Paulo")] <- 12396372 
#agregado_tudo$Total_de_crimes12m[agregado_tudo$Total_de_crimes12m == 0] <- NA
#agregado_tudo_1  <- na.locf(na.locf(agregado_tudo))
#agregado_tudo_1 <- agregado_tudo_1[agregado_tudo_1$name != "Arujá", ]  
nrow(agregado_tudo_novo_2)


vc <- "https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-35-mun.json"
dir.create("C:/temp")
downloader::download(url = vc, destfile = "C:/temp/gas.GeoJSON")
municipio <- st_read("C:/temp/gas.GeoJSON")
summary(municipio)

municipio_2 <- subset(municipio, name %in% 
                        agregado_tudo_novo_2$name)
summary(municipio_2)
head(municipio_2)
nrow(municipio_2)


# Convert SpatialPolygonsDataFrame to sf object
municipio_2_sf <- st_as_sf(municipio_2)

# Calculate area in km2
municipio_2_sf$area_km2 <- st_area(municipio_2_sf) / 10^6

# Calculate population density
agregado_tudo_novo_2 <- agregado_tudo_novo_2 %>%
  left_join(municipio_2_sf %>% st_drop_geometry(), by = c("name" 
                                                          = "name")) %>%
  mutate(pop_density = População / area_km2)

agregado_tudo_novo_2
nrow(agregado_tudo_novo_2)

agregado_tudo_novo_2$Total_de_crimes <- 
  as.numeric(agregado_tudo_novo_2$Total_de_crimes)
agregado_tudo_novo_2$População <- 
  as.numeric(agregado_tudo_novo_2$População)

agregado_tudo_novo_2$crime_per_capita <- 
  (agregado_tudo_novo_2$Total_de_crimes / 
     agregado_tudo_novo_2$População) * 100000 




# View results
head(agregado_tudo_novo_2$pop_density)
#View(agregado_tudo_novo_1)


# View results
head(agregado_tudo_novo_2$pop_density)
#View(agregado_tudo_novo_1)
head(agregado_tudo_novo_2)


agregado_tudo_novo_10 <- agregado_tudo_novo_2[, 
                                              c('crime_per_capita', 'População','pop_density','60 e mais',
                                                'matriculas_creche_municipal','matriculas_ESAI_estadual',
                                                'matriculas_ESAF_municipal','matriculas_EM_municipal')]

print(agregado_tudo_novo_10)
#agregado_tudo_3 <- log(agregado_tudo_3 + 0.01)_
names(agregado_tudo_novo_10)[names(agregado_tudo_novo_10) == "60 e mais"] <- "sessenta_e_mais"
nrow(agregado_tudo_novo_10)

crimeperca <- agregado_tudo_novo_10$crime_per_capita
agregado_tudo_novo_10$pop_density <- gsub("[^0-9.]", "", 
                                          agregado_tudo_novo_10$pop_density)
agregado_tudo_novo_10$pop_density <- 
  as.numeric(agregado_tudo_novo_10$pop_density)
head(agregado_tudo_novo_10$pop_density)

agregado_tudo_novo_10$crime_per_capita[is.na(agregado_tudo_novo_10$crime_per_capita)] <- 0
agregado_tudo_novo_10$pop_density[is.na(agregado_tudo_novo_10$pop_density)] <- 0
agregado_tudo_novo_10$População[is.na(agregado_tudo_novo_10$População)] <- 0
agregado_tudo_novo_10$sessenta_e_mais[is.na(agregado_tudo_novo_10$sessenta_e_mais)] <- 0
agregado_tudo_novo_10$matriculas_creche_municipal[is.na(agregado_tudo_novo_10$matriculas_creche_municipal)] <-0
agregado_tudo_novo_10$matriculas_ESAI_estadual[is.na(agregado_tudo_novo_10$matriculas_ESAI_estadual)] <- 0
agregado_tudo_novo_10$matriculas_ESAF_municipal[is.na(agregado_tudo_novo_10$matriculas_ESAF_municipal)] <- 0
agregado_tudo_novo_10$matriculas_EM_municipal[is.na(agregado_tudo_novo_10$matriculas_EM_municipal)] <- 0
#View(agregado_tudo_novo_10)
nrow(agregado_tudo_novo_10)
ncol(agregado_tudo_novo_10)

agregado_tudo_novo_10$crime_per_capita <- 
  as.numeric(agregado_tudo_novo_10$crime_per_capita)
agregado_tudo_novo_10$pop_density <- 
  as.numeric(agregado_tudo_novo_10$pop_density)
agregado_tudo_novo_10$População <- 
  as.numeric(agregado_tudo_novo_10$População)
agregado_tudo_novo_10$sessenta_e_mais <- 
  as.numeric(agregado_tudo_novo_10$sessenta_e_mais)
agregado_tudo_novo_10$matriculas_creche_municipal <- 
  as.numeric(agregado_tudo_novo_10$matriculas_creche_municipal)
agregado_tudo_novo_10$matriculas_ESAI_estadual <- 
  as.numeric(agregado_tudo_novo_10$matriculas_ESAI_estadual)
agregado_tudo_novo_10$matriculas_ESAF_municipal <- 
  as.numeric(agregado_tudo_novo_10$matriculas_ESAF_municipal)
agregado_tudo_novo_10$matriculas_EM_municipal <- 
  as.numeric(agregado_tudo_novo_10$matriculas_EM_municipal)

# Load data
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
gridded(meuse) <- TRUE
meuse_polygons <- as(meuse, "SpatialPolygonsDataFrame")

# Load municipio_2 and convert to sf object
municipio_1_sf <- st_as_sf(municipio_2, coords = c("long", 
                                                   "lat"), crs = 4326)

# Join municipio_2_sf with test_data_norm_2 based on "name" column
test_data_norm_df_3 <- as.data.frame(agregado_tudo_novo_2)

test_data_sf_2 <- left_join(test_data_norm_df_3, municipio_1_sf, 
                            by = c("name" = "name")) %>%
  dplyr::select(name, crime_per_capita, pop_density, geometry)

test_data_sf_2$crime_per_capita <- 
  as.numeric(test_data_sf_2$crime_per_capita)
test_data_sf_2$crime_per_capita

p <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = crime_per_capita, geometry = 
                geometry)) +
  scale_fill_gradient(low="white", high="blue", 
                      name="crime_per_capita") +
  labs(title="Choropleth plot of test data about 
crime per capita \n on the metropolitan area of 
São Paulo using ANN")

# Set the figure size and 
resolution
options(repr.plot.width=10, repr.plot.height=5, repr.plot.res=80)

# Show the plot
p

# Save the figure as a PNG file
ggsave("figura_pedidos_por_distrito.png", plot=p, width=10, height=5, dpi=80)





vc <- "https://raw.githubusercontent.com/tbrugz/geodata-
  br/master/geojson/geojs-35-mun.json"
downloader::download(url = vc, destfile = "/tmp/gas.GeoJSON")
municipio <- readOGR(dsn = "/tmp/gas.GeoJSON")
summary(municipio)

municipio_1 <- subset(municipio, name %in% agregado_tudo_novo_2$name)
summary(municipio_1)
head(municipio_1)
nrow(municipio_1)


nmunicipio_1 <- municipio_1[rep(seq_len(nrow(municipio_1)), each 
                                = 192), ]  # Base R

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
dim(W)





nmunicipio_2 <- municipio_1[rep(seq_len(nrow(municipio_1)), each 
                                = 12), ]  # Base R

summary(nmunicipio_2)
head(nmunicipio_2)
nrow(nmunicipio_2)

queens_2 <- poly2nb(municipio_1, queen = TRUE)
queens_2[[1]]
str(queens_2)

alistw_2 <- nb2listw(queens_2,style = "W" ,zero.policy = TRUE) 

#W <- nb2mat(queens_1, glist=NULL, style="B", zero.policy=TRUE)



nqueens_2 <- poly2nb(nmunicipio_2, queen = TRUE)
nqueens_2[[1]]
nqueens_2[[2]]
str(nqueens_2)

a2listw_2<- nb2listw(nqueens_2,style = "W" ,zero.policy = TRUE) 
a2listw_2
names(a2listw_2)

W2 <- nb2mat(nqueens_2, glist=NULL, style="W", zero.policy=TRUE)
W2
dim(W2)






train_data <- as.matrix(agregado_tudo_3)
test_data <- as.matrix(agregado_tudo_novo_10)


train_target <- train_data[, "crime_per_capita"]
test_target <- test_data[, "crime_per_capita"]

train_data_norm <- apply(train_data, 2, function(x) (x - min(x)) 
                         / (max(x) - min(x)))
test_data_norm <- apply(test_data, 2, function(x) (x - min(x)) / 
                          (max(x) - min(x)))
train_target_norm <- (train_target - min(train_target)) / 
  (max(train_target) - min(train_target))
test_target_norm <- (test_target - min(test_target)) / 
  (max(test_target) - min(test_target))



#install.packages("keras")
#library(keras)
#install_keras(tensorflow = "2.9.0")
library(tensorflow)

# Create a dedicated virtual environment
#install_tensorflow(method = "virtualenv", envname = "r-tensorflow")

# Restart R, then:
library(reticulate)
# For Intel Macs
#install_tensorflow(version = "2.15.0")

# For M1/M2 Macs, you might need:
# install_tensorflow(version = "2.15.0", extra_packages = "tensorflow-macos")
library(tensorflow)
library(keras)



create_model_with_W <- function(input_shape, W_shape) {
  input_data <- layer_input(shape = input_shape, name = "data_input")
  input_W <- layer_input(shape = W_shape, name = "W_input")
  
  merged_layer <- layer_concatenate(list(input_data, input_W))
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1)
  
  model <- keras_model(inputs = list(input_data, input_W), outputs = model(merged_layer))
  
  return(model)
}

input_shape <- dim(train_data_norm)[2] 
input_shape
W_shape <- dim(W)[2]  
W_shape

model <- create_model_with_W(input_shape, W_shape)


model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(lr = 0.001)
)

history <- model %>% fit(
  list(train_data_norm, W), train_target_norm,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2,
  #callbacks = list(tensorboard_callback)
)

n_samples_test <- dim(test_data_norm)[1]
test_data_reshaped <- array(test_data_norm, dim = c(n_samples_test, dim(test_data_norm)[2]))
W_test_reshaped <- array(W, dim = c(n_samples_test, dim(W)[2]))

predicted_norm <- predict(model, list(test_data_reshaped, W_test_reshaped))



predicted <- predicted_norm * (max(train_target) - 
                                 min(train_target)) + min(train_target)



predicted <- predicted_norm * (max(train_target) - 
                                 min(train_target)) + min(train_target)






predic <- predicted_norm * (max(train_target) - 
                              min(train_target)) + min(train_target)
predic


mae <- mean(abs(predic - test_target))
rmse <- sqrt(mean((predic - test_target)^2))


cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")






library(keras)


create_lstm_model_with_W <- function(input_shape, W_shape, lstm_units = 32) {
  
  input_data <- layer_input(shape = input_shape, name = "data_input")
  input_W <- layer_input(shape = W_shape, name = "W_input")
  
  merged_layer <- layer_concatenate(list(input_data, input_W))
  
  lstm_layer <- layer_lstm(units = lstm_units, return_sequences = FALSE)(merged_layer)
  
  output_layer <- layer_dense(units = 1, activation = "linear")(lstm_layer)
  
  model_lstm <- keras_model(inputs = list(input_data, input_W), outputs = output_layer)
  
  return(model_lstm)
}



time_steps <- 1 
input_shape <- list(time_steps, dim(train_data_norm)[2])  
W_shape <- list(time_steps, dim(W)[2]) 



train_data_reshaped <- array(train_data_norm, dim = 
                               c(dim(train_data_norm)[1], time_steps, dim(train_data_norm)[2]))
W_reshaped <- array(W, dim = c(dim(W)[1], time_steps, dim(W)[2]))




lstm_model <- create_lstm_model_with_W(input_shape, W_shape, 
                                       lstm_units = 32)


lstm_model %>% compile(
  loss = "mean_squared_error",
  optimizer_adam(learning_rate = 0.001, clipnorm = 1)
)


history <- lstm_model %>% fit(
  list(train_data_reshaped, W_reshaped), train_target_norm,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)


n_samples_test <- dim(test_data_norm)[1]
test_data_reshaped <- array(test_data_norm, dim = 
                              c(n_samples_test, time_steps, dim(test_data_norm)[2]))
W_test_reshaped <- array(W, dim = c(n_samples_test, time_steps, 
                                    dim(W)[2]))





predicted_norm_lstm <- predict(lstm_model, 
                               list(test_data_reshaped, W_test_reshaped))


predicted_lstm <- predicted_norm_lstm * (max(train_target) - 
                                           min(train_target)) + min(train_target)
predicted_lstm

mae_lstm <- mean(abs(predicted_lstm - test_target))
rmse_lstm <- sqrt(mean((predicted_lstm - test_target)^2))


cat("LSTM Model Metrics:\n")
cat("MAE:", mae_lstm, "\n")
cat("RMSE:", rmse_lstm, "\n")



combinada <- cbind(
  test_data_sf_2$crime_per_capita,
  predic,
  predicted_lstm,
  exp(y_chapeu)
)
colnames(combinada) <- c("crime_per_capita", "ANN", "LSTM")
combinada <- as.data.frame(combinada)
View(combinada)



library(ggplot2)
library(patchwork)




plot_predicted <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = crime_per_capita, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "blue", name = 
                        "crime per capita", na.value = "white") +
  labs(title = "Observation of crime per capita\n 
  on the metropolitan area of São Paulo in 2017")



plot_exp_y_chapeu <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = exp(y_chapeu), geometry = geometry)) +  # Specify 'geometry'
  scale_fill_gradient(low = "white", high = "blue", name = 
                        "predicted") +
  labs(title = "Choropleth plot of SAR model \n 
  on the metropolitan area of São Paulo")



plot_legal <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = predic, geometry = geometry)) +  # Specify 'geometry'
  scale_fill_gradient(low = "white", high = "blue", name = 
                        "predicted") +
  labs(title = "Choropleth plot of ANN model \n 
  on the metropolitan area of São Paulo")


plot_lstm <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = predicted_lstm, geometry = geometry)) +  #   Specify 'geometry'
  scale_fill_gradient(low = "white", high = "blue", name = 
                        "predicted") +
  labs(title = "Choropleth plot of LSTM model \n 
  on the metropolitan area of São Paulo")



options(repr.plot.width = 60, repr.plot.height = 5, repr.plot.res = 80)


final_plot <- plot_predicted + plot_exp_y_chapeu + plot_legal + 
  plot_lstm
plot_layout(nrow = 2, ncol = 2)


final_plot




# Calculate residues for each model
observed <- test_data_sf_2$crime_per_capita

# Assuming you have exp(y_chapeu) from your SAR model
# If you don't have it, you'll need to add it from your SAR model results
# exp_y_chapeu <- exp(y_chapeu)  # uncomment and define this

# Calculate residues
residue_ann <- observed - predic
residue_lstm <- observed - predicted_lstm
residue_sar <- observed - exp(y_chapeu) 
# residue_sar <- observed - exp_y_chapeu  # uncomment when you have SAR predictions

# Add residues to your spatial data frame
test_data_sf_2$residue_ann <- residue_ann
test_data_sf_2$residue_lstm <- residue_lstm
test_data_sf_2$residue_sar <- residue_sar  # uncomment when you have SAR predictions

# Create residue plots
plot_residue_ann <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = residue_ann, geometry = geometry)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "Residue") +
  labs(title = "Residue plot of ANN model\n on the metropolitan area of São Paulo") +
  theme_minimal()

plot_residue_lstm <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = residue_lstm, geometry = geometry)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "Residue") +
  labs(title = "Residue plot of LSTM model\n on the metropolitan area of São Paulo") +
  theme_minimal()

# Uncomment when you have SAR model predictions
 plot_residue_sar <- ggplot(test_data_sf_2) +
   geom_sf(aes(fill = residue_sar, geometry = geometry)) +
   scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                        midpoint = 0, name = "Residue") +
  labs(title = "Residue plot of SAR model\n on the metropolitan area of São Paulo") +
   theme_minimal()

# Display individual plots
print(plot_residue_ann)
print(plot_residue_lstm)
 print(plot_residue_sar)  # uncomment when available

# Optional: Create a combined plot using patchwork
library(patchwork)

# For now, combining just ANN and LSTM residues
residue_combined <- plot_residue_ann + plot_residue_lstm
print(residue_combined)

# Alternative: Create a comprehensive comparison including predictions and residues
# First, let's create all prediction plots
plot_observed <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = crime_per_capita, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "blue", name = "Crime per capita") +
  labs(title = "Observed crime per capita") +
  theme_minimal()

plot_pred_ann <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = predic, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "blue", name = "Predicted") +
  labs(title = "ANN Predictions") +
  theme_minimal()

plot_pred_lstm <- ggplot(test_data_sf_2) +
  geom_sf(aes(fill = predicted_lstm, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "blue", name = "Predicted") +
  labs(title = "LSTM Predictions") +
  theme_minimal()

# Create a comprehensive comparison plot
comprehensive_plot <- (plot_observed | plot_pred_ann | plot_pred_lstm) / 
  (plot_residue_ann | plot_residue_lstm)

print(comprehensive_plot)

# Optional: Save the plots
ggsave("residue_ann.png", plot_residue_ann, width = 10, height = 8, dpi = 300)
ggsave("residue_lstm.png", plot_residue_lstm, width = 10, height = 8, dpi = 300)
ggsave("comprehensive_comparison.png", comprehensive_plot, width = 15, height = 10, dpi = 300)

# Print summary statistics for residues
cat("ANN Model Residue Statistics:\n")
cat("Mean:", mean(residue_ann), "\n")
cat("Median:", median(residue_ann), "\n")
cat("Min:", min(residue_ann), "\n")
cat("Max:", max(residue_ann), "\n")
cat("SD:", sd(residue_ann), "\n\n")

cat("LSTM Model Residue Statistics:\n")
cat("Mean:", mean(residue_lstm), "\n")
cat("Median:", median(residue_lstm), "\n")
cat("Min:", min(residue_lstm), "\n")
cat("Max:", max(residue_lstm), "\n")
cat("SD:", sd(residue_lstm), "\n\n")

# Create a summary table
residue_summary <- data.frame(
  Model = c("ANN", "LSTM"),
  Mean_Residue = c(mean(residue_ann), mean(residue_lstm)),
  Median_Residue = c(median(residue_ann), median(residue_lstm)),
  SD_Residue = c(sd(residue_ann), sd(residue_lstm)),
  Min_Residue = c(min(residue_ann), min(residue_lstm)),
  Max_Residue = c(max(residue_ann), max(residue_lstm))
)

print(residue_summary)


