#ENTREGA 1 - LAB - R
#Alumnos: Sade, Saravia, Torres y Vignoli

#Ejercicio 2 comienza en línea 288, ejercicio 3 en línea 502

#Aclaracion: los ejercicios 1 y 3 fueron codeados en R desde Windows mientras que el 2 fue codeado en R desde Mac.
#Por alguna razón desconocida, a algunos integrantes del equipo el código hecho Mac, al correrse en Windows, generaba bugs en algunos graficos. Sospechamos que tiene que ver con algun tema de distintas versiones
#De todos modos, todo gráfico y resultado presentado en el informe tiene su origen en este mismo código. 

#--------------------------------------Ejercicio 1: Analisis de datos------------------------------------------------

#Comenzamos bajando las librerías a utilizar

library(tidyverse)
library(WDI)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)


#Definimos los países con los que vamos a trabajar y los años para los que buscamos los datos

paises <- c("AR","BR","CL","CO","MX","PE","UY","PY","BO","EC")   # Datos de Arg, Brasil, Chile, Colombia, Mexico, Peru, Uruguay, Paraguay, Bolivia y Ecuador
anios  <- c(1970, 2024)

#Queremos indicadores de tasa de inflación y desempleo, ambos anuales. Utilizamos los códigos correspondientes.
indicadores <- c(
  infl  = "FP.CPI.TOTL.ZG",    # Inflación anual CPI (%)
  unemp = "SL.UEM.TOTL.ZS"     # Desempleo total (% fuerza laboral)
)

#Descarga de datos
datos <- WDI(
  country   = paises,
  indicator = indicadores,
  start     = anios[1],
  end       = anios[2],
  extra     = TRUE
)

View(datos)

# Eliminamos las filas que contienen al menos un NA. No hay datos de inflación para todos los años para todos los países
datos_limpios <- datos %>%
  drop_na()

#-------------Cálculo de correlaciones muestrales entre inflación y desempleo bajo distintos criterios------

# 1. Calculamos la correlación muestral entre inflación y desempleo, usando todos los datos (cor = -0.01104).
datos_limpios %>%
  summarise(correlacion_total = cor(infl, unemp))

#da 0.01104227

# 2. Calculamos correlación por país
cor_por_pais <- datos_limpios %>%
  group_by(country) %>%
  summarise(correlacion = cor(infl, unemp, use = "complete.obs")) %>%
  arrange(desc(correlacion))

cor_por_pais

# 3. Calculamos correlación por nivel de ingresos: filtramos de acuerdo a la clasificación de nivel de ingreso del país hecha por el Banco Mundial
cor_por_ingreso <- datos_limpios %>%
  group_by(income) %>%
  summarise(correlacion = cor(infl, unemp, use = "complete.obs")) %>%
  arrange(desc(correlacion))

cor_por_ingreso

# 4. Calculamos correlación por año: para cada año, tenemos datos de inflación y empleo de varios países. Podemos usarlos para calcular la cor(infl, unemp) dentro de cada año
cor_por_anio <- datos_limpios %>%
  group_by(year) %>%
  summarise(correlacion = cor(infl, unemp, use = "complete.obs")) %>%
  arrange(desc(correlacion))

View(cor_por_anio)
1
#Para ciertos países, la "curva de Phillips" tiene pendiente positiva. Para ciertos años, la relación entre inflación y desempleo también es positiva.

#-------------Caso de México: Pillips con pendiente positiva. ¿Cómo luce la curva?  ----------


#Consideramos el caso de México. Queremos visualizar la inflación y el desempleo a lo largo de los años
# y luego graficar la correspondiente curva de Phillips

datos_mexico <- datos_limpios %>% select(country, year, infl, unemp, income) %>% filter(country=="Mexico")
View(datos_mexico)
datos_mexico


#Hacemos los gráficos de inflación y desempleo en el tiempo

datos_mexico %>%
  
  pivot_longer(c(infl, unemp), names_to = "variable", values_to = "value") %>%
  
  #Queremos dos gráficos juntos. Usaremos facet wrap. Precisamos pivotear el dataframe para filtrar datos por inflacion o desempleo, dado un mismo año
  
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  
  facet_wrap(
    ~ variable, ncol = 2, scales = "free_y",
    labeller = as_labeller(c(infl = "Inflación (%)", unemp = "Desempleo (%)"))
  ) +
  
  #Acá indicamos que pedimos dos gráficos, usando las dos variables (infl y unemp) de la columna "variable" del df pivoteado.
  
  labs(title = "México: Inflación y Desempleo en el tiempo", x = "Año", y = NULL) +
  theme_minimal()



#Veamos la relación entre la inflación y el desempleo en México

datos_limpios %>%
  filter(country == "Mexico") %>%
  ggplot(aes(x = unemp, y = infl)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Curva de Phillips - Mexico",
       x = "Desempleo (%)", y = "Inflación (%)") +
  theme_minimal()

#Consideremos fittear una parábola, en vez de una recta, a los datos

datos_limpios %>%
  filter(country == "Mexico") %>%
  ggplot(aes(unemp, infl)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "darkred", se=FALSE) +
  labs(title = "Curva de Phillips no lineal (cuadrática)",
       x = "Desempleo (%)", y = "Inflación (%)") +
  theme_minimal()

#Finalizamos con un breve testeo luego de regresar inflación contra desempleo. De más está decir que sobran razones por las cuales puede no ser válida esta regresión (endogeneidad, etc.)

modelo_mexico <- datos_limpios %>%
  filter(country == "Mexico") %>%
  lm(infl ~ unemp, data = .)

# Resultados
summary(modelo_mexico)

#----------PARTE II: Respeto por las unidades ------

#PIB en PPP, Per cápita, USD correintes, etc. Mostrar como puede cambiar el ranking y recordar intuiciones de macro con medir mal. Ejemplo de Argentina de dolares corrientes, se te derrumba PIB a inicios de los 2000. 

#Queremos estudiar la relevancia de las unidades de medida para comparar PIB entre países
#Bajamos datos de PIB dólares corrientes, dólares constantes y ajustados por PPP, tanto la serie orginal como la per capita
ind <- c(
  gdp_real   = "NY.GDP.MKTP.KD",      # PIB total, USD constantes de 2015
  gdp_pc     = "NY.GDP.PCAP.CD",      # PIB per cápita, USD corrientes
  gdp_pc_ppp_kd = "NY.GDP.PCAP.PP.KD",# PIB PPP per cápita, const 2021 intl$
  pop        = "SP.POP.TOTL",         # Población
  )



datos_pbi <- WDI(country = paises, indicator = ind, start = 1970, end = 2024, extra = TRUE)
View(datos_pbi)

datos_pbi_limpios <- datos_pbi %>%
  drop_na() %>%            # Sacamos filas con NAs
  mutate(
    gdp_pc_real = gdp_real / pop        # Agregamos la columna PIB real per cápita (USD constantes de 2015)
  )

View(datos_pbi_limpios)


#Queremos hacer un ranking de países de PIB per cápita usando las distintas métricas: dolares constantes de 2015, dolares PPP constantes de 2021 y dolares corrientes

datos_pbi_filtrado <- datos_pbi_limpios %>%
  select(country, year, gdp_pc_real, gdp_pc_ppp_kd, gdp_pc)

View(datos_pbi_filtrado)

#Utilizamos la función rank para rankear a los países según las distintas métricas para PIB per capita

ranking_pbi_2024 <- datos_pbi_filtrado %>%
  filter(year == 2024) %>%
  mutate(
    rank_usd_const    = rank(-gdp_pc_real),
    rank_usd_ppp_ctes = rank(-gdp_pc_ppp_kd),   #El "-" adelante para rankear de mayor a menor
    rank_usd_curr     = rank(-gdp_pc)
  ) %>%
  arrange(rank_usd_const)
ranking_pbi_2024

#Perú está 6to en USD corrientes, 8vo en USD PPP constantes y 7mo en dólares constantes de 2015. Las unidades importan!

#Grafiquemos las series de PIB per capita de PERÚ


datos_pbi_filtrado %>%
  filter(country=="Peru") %>%
  ggplot(aes(x = year)) +
  
  # USD constantes (real)
  geom_line(aes(y = gdp_pc_real, color = "USD constantes (real)")) +
  geom_point(aes(y = gdp_pc_real, color = "USD constantes (real)"), size = 1.2) +
  
  # USD corrientes
  geom_line(aes(y = gdp_pc, color = "USD corr.")) +
  geom_point(aes(y = gdp_pc, color = "USD corr."), size = 1.2) +
  
  # PPP constantes
  geom_line(aes(y = gdp_pc_ppp_kd, color = "PPP USD constantes")) +
  geom_point(aes(y = gdp_pc_ppp_kd, color = "PPP USD constantes"), size = 1.2) +
  
  
  labs(title = "Perú — PIB per cápita (tres medidas)",
       x = "Año", y = "Dólares per cápita",
       color = "Serie") +
  scale_color_manual(values = c(
    "USD constantes (real)"   = "black",
    "USD corr."          = "firebrick",
    "PPP USD constantes"  = "green"
  )) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = seq(2011, 2024, by = 3))  +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

#--------------Parte III : Ingresos y desigualdad --------
#Ya rankeamos a países de LATAM por ingreso por cápita utilizando distintas métricas. ¿Cómo es el ranking de desigualdad entre países?
# Utilizamos datos del índice de Gini y datos de ingreso para responder estas preguntas. 


ind_3 <- c(
  gdp_pc_ppp_kd = "NY.GDP.PCAP.PP.KD",
  gini        = "SI.POV.GINI"
)



datos_desig <- WDI(country = paises, indicator = ind_3,
                     start = 1970, end = 2024, extra = TRUE) %>%
  drop_na()

View(datos_desig)

#Consideremos el año 2023. ¿Cuáles son los países con menor y mayor PIB per cápita? Comparemos sus índices de Gini luego

datos_desig_23 <- filter(datos_desig, year=="2023")
datos_desig_23 

#A ojo vemos que Uruguay es el de mayor PIB per capita y Bolivia el de menor, con indices de Gini de 40.9 y 42.1 respectivamente.
#El país de mayores ingresos tiene menor índice de Gini, más ingresos y más igualitario. 
#Grafiquemos el índice de Gini y el PIB per capita de Uruguay en el tiempo. ¿Cómo se relaciona con la "Curva de Kuznets"? 

datos_desig %>%
  
  filter(country=="Uruguay") %>%
  
  pivot_longer(c(gdp_pc_ppp_kd, gini), names_to = "variable", values_to = "value") %>%
  
  
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  
  facet_wrap(
    ~ variable, ncol = 2, scales = "free_y",
    labeller = as_labeller(c(gdp_pc_ppp_kd = "GDP PC PPP cte", gini = "Gini"))
  ) +
  
  
  labs(title = "Uruguay: evolución de PIB pc e índice de Gini", x = "Año", y = NULL) +
  theme_minimal()

#Curva de Kuznets para Uruguay

datos_desig %>%
  filter(country == "Uruguay") %>%
  ggplot(aes(x = gdp_pc_ppp_kd, y = gini)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "darkred", se=FALSE) +
  labs(title = "Curva de Kuznets - Uruguay",
       x = "PIB pc PPP cte", y = "Gini") +
  theme_minimal()


#---------------------------------Ejercicio 2: ---------------------------


gapminder <- read_csv("C:/Users/Asus/Documents/LAB/TP Entrega 1/gapminder.csv")
View(gapminder) #confirmamos que el dataset se halla descargado correctamente

# Inciso 1 #

# filtramos solo a Argentina

datos_argentina <- gapminder %>% filter(country == "Argentina") %>% 
  select(year,income_per_person)
View(datos_argentina)
#obtenemos una tabla con los años y los gdp per capita de Argentina

# Graficamos la evolución del ingreso por persona en Argentina

ggplot(datos_argentina, aes(x = year, y = income_per_person)) +
  geom_line(color = "red", linewidth = 1) +
  labs(
    title = "Evolución del ingreso per cápita en Argentina",
    x = "Año",
    y = "Ingreso per capita"
  ) 

# Inciso 2 #

# Separamos los datos entre train y test

# Separamos los últimos 10 años para test
test_data <- tail(datos_argentina, 10)

# Separamos el resto para entrenamiento
train_data <- head(datos_argentina, - 10)

# nombramos las variables independiente y dependiente tanto de train como de test
x_train <- train_data$year
y_train <- train_data$income_per_person
x_test  <- test_data$year
y_test  <- test_data$income_per_person

# Pasamos a estimar los tres modelos solicitados, regresando a income_per_person sobre
# las variables explicativas, en este caso, el tiempo.

# Modelo lineal
fit_lineal <- lm(y_train ~ x_train)
summary (fit_lineal)

# Modelo polinómico grado 2
fit_poly2 <- lm(y_train ~ poly(x_train, 2, raw = TRUE))
summary(fit_poly2)

# Modelo polinómico grado 10
fit_poly10 <- lm(y_train ~ poly(x_train, 10, raw = TRUE))
summary(fit_poly10)


# obtenemos el resumen de los datos relevantes de cada regresion

# ahora creamos una grilla de años para lograr un mejor grafico
x_grid <- seq(min(datos_argentina$year), max(datos_argentina$year), length.out = 400)
grid <- data.frame(year = x_grid)

# Predecimos a partir de la grilla los distintos modelos
grid$lineal <- predict(fit_lineal, newdata = data.frame(x_train = x_grid))
grid$poly2  <- predict(fit_poly2 ,  newdata = data.frame(x_train = x_grid))
grid$poly10 <- predict(fit_poly10, newdata = data.frame(x_train = x_grid))

#finalmente graficamos los modelos en train e incluimos puntos reales (datos de train y test) 
ggplot() +
  geom_point(data = train_data, aes(x = year, y = income_per_person), color = "blue", size = 2, alpha = 0.7) +
  geom_point(data = test_data, aes(x = year, y = income_per_person), color = "red", size = 2, alpha = 0.7) +
  geom_line(data = grid, aes(x = year, y = lineal, color = "Lineal"), size = 1) +
  geom_line(data = grid, aes(x = year, y = poly2, color = "Grado 2"), size = 1) +
  geom_line(data = grid, aes(x = year, y = poly10, color = "Grado 10"), size = 1) +
  labs(
    title = "Modelos sobre income_per_person en Argentina",
    subtitle = "Azul = train, Rojo = test",
    x = "Año",
    y = "Ingreso por persona",
    color = "Modelo"
  ) +
  theme_minimal()


# para notar la presencia de overfitting, usamos la raíz del error cuadrático medio
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

rmse_train <- c(
  Lineal = rmse(y_train, fitted(fit_lineal)),
  Grado2 = rmse(y_train, fitted(fit_poly2)),
  Grado10 = rmse(y_train, fitted(fit_poly10))
)

rmse_test <- c(
  Lineal = rmse(y_test, predict(fit_lineal, newdata = data.frame(x_train = x_test))),
  Grado2 = rmse(y_test, predict(fit_poly2,  newdata = data.frame(x_train = x_test))),
  Grado10 = rmse(y_test, predict(fit_poly10, newdata = data.frame(x_train = x_test)))
)

cat("\nRMSE en TRAIN:\n"); print(rmse_train)
cat("\nRMSE en TEST:\n"); print(rmse_test)

#Notamos que el modelo polinómico de grado 10 tiene overfitting.

# Inciso 3 #

# elegimos brazil,chile,bolivia y uruguay

#Filtramos el dataset
paises <- c("Argentina", "Brazil", "Bolivia", "Chile", "Uruguay")
sudamericanos <- gapminder %>% filter(country %in% paises)


#ponemos en formato ancho
sudamericanos_ancho <- sudamericanos %>%
  select(year, country, income_per_person) %>%
  pivot_wider(names_from = country, values_from = income_per_person)
View(sudamericanos_ancho)

#armamos la matriz de correlaciones
cor_ingresos_sudamericanos <- cor(sudamericanos_ancho[ , -1])  # quitamos la columna 'year'
print(cor_ingresos_sudamericanos)

# para el inciso B, calculamos las variaciones porcentuales anuales

# Calculamos el crecimiento interanual (variación porcentual) para cada país
crecimiento_sudamericanos <- sudamericanos_ancho %>%
  arrange(year) %>%
  mutate(across(-year, ~ (.-lag(.)) / lag(.))) #realiza el lag a todos los ingresos per capita salvo a la columna year

# Eliminamos los NA
crecimiento_sudamericanos <- na.omit(crecimiento_sudamericanos)

# Calculamos la matriz de correlación del crecimiento
cor_crecimiento <- cor(crecimiento_sudamericanos[ , -1])
print(cor_crecimiento)

## PARTE 2 ##

#Inciso 5
# Elegimos el año 2000
#filtramos el dataset por año y life_expectancy, life_expectancy_female

gapminder_2000 <- gapminder %>%
  filter(year == 2000) %>%  
  na.omit() %>%   
  mutate(
    life_expectancy = as.numeric(life_expectancy), 
    life_expectancy_female = as.numeric(life_expectancy_female),
    income_per_person = as.numeric(income_per_person) 
  ) 

View(gapminder_2000)

#omitimos los NAs y nos aseguramos de que las variables 
#esten en formato numerico


#Graficamos 

ggplot(gapminder_2000, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre esperanza de vida general y femenina (2000)",
       x = "Esperanza de vida - Mujeres",
       y = "Esperanza de vida - Total") +
  theme_minimal()

#Inciso 6
#corremos la regresion lineal simple
modelo_simple <-  lm(life_expectancy ~ life_expectancy_female, data = gapminder_2000)
summary(modelo_simple)

#Inciso 7
#Agregamos una nueva variable que sea la diferencia entre life_expectancy_female y life_expectancy

gapminder_2000 <- gapminder_2000 %>% mutate(diferencia = life_expectancy_female - life_expectancy
)

# Realizamos un Test t de una muestra sobre la diferencia
t_test <- t.test(gapminder_2000$diferencia, mu = 0, alternative = "greater")

print(t_test)

# rechazamos H0, por lo tanto life expectancy female es mayor

#Inciso 8
# Estimamos la regresion multiple
modelo_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = gapminder_2000)
summary(modelo_multiple)

#Comparamos ambos modelos
modelo_simple <-  lm(life_expectancy ~ life_expectancy_female, data = gapminder_2000)
summary(modelo_simple)
modelo_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = gapminder_2000)
summary(modelo_multiple)

#Inciso 9

#elegimos child_mortality, children_per_woman y life_expectancy_male
gapminder_2000 <- gapminder_2000 %>%
  filter(year == 2000) %>%
  mutate(
    child_mortality = as.numeric(child_mortality),
    children_per_woman = as.numeric(children_per_woman),
    life_expectancy_male = as.numeric(life_expectancy_male),
  ) %>% na.omit()

modelo_triple <- lm(life_expectancy ~ life_expectancy_male + child_mortality + children_per_woman, data = gapminder_2000)
summary(modelo_triple)



# ------------------------------------ Ejercicio 3: simulación de demandas con preferencias Cobb Douglas ----------------



# Función simular.ingreso

simular_ingreso <- function(n, k) {
  
  ingresos <- rchisq(n = n, df = k)
  return(ingresos)
}
#ahora pasamos a ver que sucede cuando modificamos los grados de libertad, 


# Generamos 1000 ingresos con k=5...

# Caso 1: k=5
set.seed(42) # fijamos los muestreos aleatorios
ingresos_bajos_k <- simular_ingreso(n = 1000, k = 5)
print(paste("Media con k=5:", mean(ingresos_bajos_k)))
print(paste("Varianza con k=5:", var(ingresos_bajos_k)))

# Caso 2: k=20
set.seed(42) # fijamos los muestreos aleatorios:
ingresos_altos_k <- simular_ingreso(n = 1000, k = 20)
print(paste("Media con k=20:", mean(ingresos_altos_k)))
print(paste("Varianza con k=20:", var(ingresos_altos_k)))


# y ahora vemos los histogramas generados para comparar 

hist(ingresos_bajos_k, main = "Ingresos con k=5", xlab = "Ingreso (Y)", breaks = 50)
hist(ingresos_altos_k, main = "Ingresos con k=20", xlab = "Ingreso (Y)", breaks = 50)

#punto 2


#Función demanda_cd
demanda_cd <- function(Y, p1, p2, alpha1, alpha2) {
  #defino las demandas optimas primero
  x1_optimo <- (alpha1 * Y) / p1
  x2_optimo <- (alpha2 * Y) / p2
  #las agrupo en un vector
  demandas_vector <- c(x1 = x1_optimo, x2 = x2_optimo)
  utilidad_indirecta <- (x1_optimo^alpha1) * (x2_optimo^alpha2) # Utilidad Indirecta: U* = (x1*)^alpha1 * (x2*)^alpha2
  return(list(
    demandas = demandas_vector,
    utilidad_indirecta = utilidad_indirecta
  ))
}

#punto 3


set.seed(42) # fijamos los muestreos aleatorios:

Hogares <- 10000 # Número de hogares
grados <- 10  # Grados de libertad para el Ingreso (Y)
p1_fijo <- 1.5
p2_fijo <- 2.5
alpha1_fijo <- 0.4
alpha2_fijo <- 0.6


Y_simulados <- simular_ingreso(n = Hogares, k = grados)

#ahora armo vectores con los x1*,x2* y de utilidad indirecta
x1_optimo_vec <- (alpha1_fijo * Y_simulados) / p1_fijo
x2_optimo_vec <- (alpha2_fijo * Y_simulados) / p2_fijo
U_star_vec <- (x1_optimo_vec^alpha1_fijo) * (x2_optimo_vec^alpha2_fijo)

# 5. los junto todo en un data frame
simulacion_df <- data.frame(
  Y = Y_simulados,
  x1_opt = x1_optimo_vec,
  x2_opt = x2_optimo_vec,
  U_star = U_star_vec
)
# Calculamos medias y cuartiles sobre el data frame resultante
medias <- sapply(simulacion_df[, c("x1_opt", "x2_opt", "U_star")], mean)
cuartiles <- sapply(simulacion_df[, c("x1_opt", "x2_opt", "U_star")], quantile)

print(medias)
print(cuartiles)


# VISUALIZACIÓN DE DISTRIBUCIONES (HISTOGRAMAS)

par(mfrow = c(1, 3)) # Configura 3 gráficos en una fila

hist(simulacion_df$x1_opt, breaks = 50, col = "skyblue", 
     main = expression(paste("Demanda ", x[1]^"*")), 
     xlab = expression(x[1]^"*"), border = "white")

hist(simulacion_df$x2_opt, breaks = 50, col = "lightgreen", 
     main = expression(paste("Demanda ", x[2]^"*")), 
     xlab = expression(x[2]^"*"), border = "white")

hist(simulacion_df$U_star, breaks = 50, col = "salmon", 
     main = expression(paste("Utilidad ", U^"*")), 
     xlab = expression(U^"*"), border = "white")

# MEDIAS
media_vector <- c(
  mean(simulacion_df$x1_opt),
  mean(simulacion_df$x2_opt),
  mean(simulacion_df$U_star)
)
print(media_vector)

# CUARTILES
cuartiles_x1 <- quantile(simulacion_df$x1_opt)
cuartiles_x2 <- quantile(simulacion_df$x2_opt)
cuartiles_U  <- quantile(simulacion_df$U_star)
print(cuartiles_x1)
print(cuartiles_x2)
print(cuartiles_U)


#Punto 4


#Función prob_bajo_consumo
prob_bajo_consumo <- function(simulacion_df, c, j) {
  columna_demanda <- if (j == 1) "x1_opt" else "x2_opt" #que demanda queremos
  demanda_j <- simulacion_df[[columna_demanda]]   
  conteo_bajo_consumo <- sum(demanda_j < c)    #sumamos los valores que cumplen esta condicion
  probabilidad <- conteo_bajo_consumo / nrow(simulacion_df)  #calculamos la proba
  
  return(probabilidad)
}

# Probabilidad de que el consumo de Bien 1 sea menor que 2 (P(x1* < 2))
c_umbral <- 2
prob_x1_bajo <- prob_bajo_consumo(simulacion_df, c = c_umbral, j = 1)

print(prob_x1_bajo)

# Probabilidad de que el consumo de Bien 2 sea menor que 2 (P(x2* < 2))
prob_x2_bajo <- prob_bajo_consumo(simulacion_df, c = c_umbral, j = 2)
print(prob_x2_bajo)


#punto 5


# 1. Definir el nuevo precio (p1')
shock_precio <- 0.20 # Aumento del 20%
p1_shock <- p1_fijo * (1 + shock_precio)

#Demanda del Bien 1 después del shock: x1*' = (alpha1 * Y) / p1'
x1_opt_despues <- (alpha1_fijo * Y_simulados) / p1_shock

#Demanda del Bien 2 después del shock: x2*' (p2 y alpha2 no cambian)/ propiedad de las cobb douglas
x2_opt_despues <- (alpha2_fijo * Y_simulados) / p2_fijo
# Nota: La utilidad indirecta U* también cambia, pero no nos piden nada al respecto (todo pelota)

# El data frame de la simulación original ya existe: 'simulacion_df'
x1_opt_antes <- simulacion_df$x1_opt 

# Media de x1* antes y después
media_antes <- mean(x1_opt_antes)
media_despues <- mean(x1_opt_despues)
media_antes
media_despues

# Cuartiles de x1* antes y después
cuartiles_antes <- quantile(x1_opt_antes)
cuartiles_despues <- quantile(x1_opt_despues)
cuartiles_antes
cuartiles_despues


par(mfrow = c(1, 2)) 

# Histograma de la demanda x1* ANTES del shock (p1 = 1.5)
hist(x1_opt_antes, breaks = 50, col = "skyblue", 
     main = expression(paste("Demanda ", x[1]^"*", " ANTES (p1=1.5)")), 
     xlab = expression(x[1]^"*"), border = "white")

# Histograma de la demanda x1* DESPUÉS del shock (p1' = 1.8)
hist(x1_opt_despues, breaks = 50, col = "salmon", 
     main = expression(paste("Demanda ", x[1]^"*", " DESPUÉS (p1'=1.8)")), 
     xlab = expression(x[1]^"*"), border = "white")

#punto 6

# armo un data frame combinado para plotear el impacto superpuesto
df_comparacion <- data.frame(
  x1_demanda = c(x1_opt_antes, x1_opt_despues),
  Escenario = factor(rep(c("Antes del Shock (p1=1.5)", "Después del Shock (p1'=1.8)"), each = Hogares))
)

# 2. Graficar las distribuciones de densidad
densidad_shock <- ggplot(df_comparacion, aes(x = x1_demanda, fill = Escenario)) +
  geom_density(alpha = 0.5) + # Densidad para ver la forma
  
  # Líneas para las medias
  geom_vline(xintercept = media_antes, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = media_despues, color = "red", linetype = "dashed") +
  
  # Etiquetas y título
  labs(
    title = expression(paste("Distribución de Demanda ", x[1]^"*", " Antes y Después del Shock de Precios")),
    x = expression(paste("Demanda Óptima ", x[1]^"*")),
    y = "Densidad",
    fill = "Escenario de Precios"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

print(densidad_shock)


#punto 7

# Parámetros Beta para la Heterogeneidad
a_beta <- 5 # Forma 1
b_beta <- 5 # Forma 2

# Generación de la Heterogeneidad
alpha1_heterogeneo <- rbeta(n = Hogares, shape1 = a_beta, shape2 = b_beta)
alpha2_heterogeneo <- 1 - alpha1_heterogeneo

# Generación de Ingresos
Y_simulados <- simular_ingreso(n = Hogares, k = grados)


# punto 3
# Demanda de Bienes (Vectorizada)
x1_optimoH_vec <- (alpha1_heterogeneo * Y_simulados) / p1_fijo
x2_optimoH_vec <- (alpha2_heterogeneo * Y_simulados) / p2_fijo
U_star_vec <- (x1_optimoH_vec^alpha1_heterogeneo) * (x2_optimoH_vec^alpha2_heterogeneo)

# Data Frame de Resultados
simulacion_df <- data.frame(
  Y = Y_simulados,
  x1_optH = x1_optimoH_vec, # Demanda x1 Heterogénea
  x2_optH = x2_optimoH_vec, # Demanda x2 Heterogénea
  U_star = U_star_vec       # Utilidad Heterogénea
)

# Medias y Cuartiles
medias <- sapply(simulacion_df[, c("x1_optH", "x2_optH", "U_star")], mean)
cuartiles <- sapply(simulacion_df[, c("x1_optH", "x2_optH", "U_star")], quantile)

cat("\n--- PUNTO 3: Estadísticas con Heterogeneidad (Media alpha1 = 0.5) ---\n") 
print(medias)
print(cuartiles)
cat("----------------------------------------------------------------------\n")

par(mfrow = c(1, 3))

hist(simulacion_df$x1_optH, breaks = 50, col = "skyblue",
     main = expression(paste("Demanda ", x[1]^"*")),
     xlab = expression(x[1]^"*"), border = "white")

hist(simulacion_df$x2_optH, breaks = 50, col = "lightgreen",
     main = expression(paste("Demanda ", x[2]^"*")),
     xlab = expression(x[2]^"*"), border = "white")

hist(simulacion_df$U_star, breaks = 50, col = "salmon",
     main = expression(paste("Utilidad ", U^"*")),
     xlab = expression(U^"*"), border = "white")

# punto 4
c_umbral <- 2
prob_x1H_bajo <- prob_bajo_consumo(simulacion_df, c = c_umbral, j = 1)
prob_x2H_bajo <- prob_bajo_consumo(simulacion_df, c = c_umbral, j = 2)

cat("\n--- PUNTO 4: Probabilidad de Bajo Consumo (Umbral c=2) ---\n")
cat(paste("P(x1* < 2):", prob_x1H_bajo, "\n"))
cat(paste("P(x2* < 2):", prob_x2H_bajo, "\n"))
cat("----------------------------------------------------------\n")


# punto 5 
shock_precio <- 0.20
p1_shock <- p1_fijo * (1 + shock_precio)

# Demanda post-shock (
x1H_opt_despues <- (alpha1_heterogeneo * Y_simulados) / p1_shock
x2H_opt_despues <- (alpha2_heterogeneo * Y_simulados) / p2_fijo
x1H_opt_antes <- simulacion_df$x1_optH

# Medias y Cuartiles de x1* post-shock
media_antes <- mean(x1H_opt_antes)
media_despues <- mean(x1H_opt_despues)
cuartiles_antes <- quantile(x1H_opt_antes)
cuartiles_despues <- quantile(x1H_opt_despues)

# Visualización separada (Histogramas)
par(mfrow = c(1, 2))

hist(x1H_opt_antes, breaks = 50, col = "skyblue",
     main = expression(paste("Demanda ", x[1]^"*", " ANTES (p1=1.5)")),
     xlab = expression(x[1]^"*"), border = "white")

hist(x1H_opt_despues, breaks = 50, col = "salmon",
     main = expression(paste("Demanda ", x[1]^"*" , " DESPUÉS (p1'=1.8)")),
     xlab = expression(x[1]^"*"), border = "white")

# punto 6
# Data frame combinado para plotear la densidad superpuesta
df_comparacion <- data.frame(
  x1H_demanda = c(x1H_opt_antes, x1H_opt_despues), 
  Escenario = factor(rep(c("Antes del Shock (p1=1.5)", "Después del Shock (p1'=1.8)"), each = Hogares))
)

# Gráfico de densidad superpuesto
densidad_shock <- ggplot(df_comparacion, aes(x = x1H_demanda, fill = Escenario)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = media_antes, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = media_despues, color = "red", linetype = "dashed") +
  labs(
    title = expression(paste("PUNTO 6: Demanda ", x[1]^"*", " Antes vs. Después (Heterogeneidad)")),
    x = expression(paste("Demanda Óptima ", x[1]^"*")),
    y = "Densidad",
    fill = "Escenario de Precios"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

print(densidad_shock)

