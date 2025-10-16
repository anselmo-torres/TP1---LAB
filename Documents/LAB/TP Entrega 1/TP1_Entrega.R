#Comenzamos bajando las librerías a utilizar

library(tidyverse)
library(WDI)

#Definimos los países con los que vamos a trabajar y los años para los que buscamos los datos

paises <- c("AR","BR","CL","CO","MX","PE","UY","PY","BO","EC")   # ajustá a gusto. Arg, brasil, chile, colombia, mexico, peru, uruguay, paraguay, bolivia y ecuador
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

      