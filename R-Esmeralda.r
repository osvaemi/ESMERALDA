library(tidyverse) #Sirve para manipular datos
library(sf) #Sirve para importar y exportar archivos, como para hacer mapas estáticos
library(plotly) # Sirve para hacer gráficos interactivos
library(ggrepel) # Sirve para mejorar visualizaciones de datos
library(leaflet) #Sirve para hacer mapas interactivos 
library(geojsonio) # Sirve para generar archivos .geojson (que luego exportaremos a Kepler para tener una visualización estilizada)
library(readxl) # Sirve para leer archivos excel y transformarlos en csv
library(scales) # Lo usamos para cambiar números enteros en porcentajes (en los gráficos)
library(gridExtra) # Lo usamos para graficar distintos plots side-by-side

# Recuerden instalar todos estas liberías si es que aún no lo tienen en sus computadoras con la función "install.packages()" 
# antes de correr el código (ejemplo: install.packages(tidyverse))#

##### Vamos a comenzar con algunos datos sobre nuestra problemática #####

Poblacionsexo <- read_excel("Poblacionsexo.xlsx")
View(Poblacionsexo)

poblacion <- Poblacionsexo %>%
  mutate(POBLACION=TOTALES) %>%
  select(PARTIDO, POBLACION, Femenino, Masculino) %>%
  filter(POBLACION > 300000, POBLACION < 2000000)

P <- ggplot(data = poblacion, aes(x=PARTIDO, y=POBLACION))+
  geom_bar(stat="identity", aes(fill=PARTIDO))+
  scale_y_continuous(labels = comma)+  #Esta función la usamos para cambiar la notación científica de Y
  labs(title = "Población por Partido RMBA",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Partido",
       y= "Población")

ggplotly(P)  #Ya tenemos el gráfico interactivo de la población total por partido.

poblacion_masc <- Poblacionsexo %>%
  select(PARTIDO, Masculino)%>%
  filter(Masculino<1000000, Masculino > 200000)

View(poblacion_masc)

PM <- ggplot(data = poblacion_masc, aes(x=PARTIDO, y=Masculino))+
  geom_bar(aes(fill=Masculino), stat="identity")+
  labs(title = "Población masculina por Partido RMBA",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Partido",
       y= "Población",
       fill= "Población Masculina")+
  scale_y_continuous(labels = comma)+
  scale_fill_continuous(labels = comma)  #Esta función la usamos para cambiar la notación científica de fill
  

ggplotly(PM) #Tenemos población masculina por partido, excluyendo a CABA, partidos menores y el Total, ya que sólo queremos enfocarnos en los grandes partidos del Conurbano.

poblacion_fem <- Poblacionsexo %>%
  select(PARTIDO, Femenino)%>%
  filter(Femenino<1000000, Femenino>200000)
View(poblacion_fem)

PF <- ggplot(data = poblacion_fem, aes(x=PARTIDO, y=Femenino))+
  geom_bar(aes(fill=Femenino), stat="identity")+
  labs(title = "Población femenina por Partido RMBA",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Partido",
       y= "Población",
       fill= "Población Femenina")+
  scale_y_continuous(labels = comma)+
  scale_fill_continuous(labels = comma)

ggplotly(PF) #Ahora tenemos población femenina por partido

PMF <- grid.arrange(PM, PF, ncol=1) #Finalmente una comparación entre los sexos.

#### Esta no podemos hacerla interactiva, porque usamos grid.arrange antes, pero es bastante clara de por sí.

##### PODEMOS VER QUE SAN MARTÍN ES UNO DE LOS DISTRITOS MÁS POBLADOS Y TIENE MAYOR POBLACIÓN DE MUJERES QUE DE HOMBRES #####

##### ¿Qué medio de transporte utilizan las mujeres de la Región Metropolitana de Buenos Aires? #####

Viajessexo <- read_excel("Viajessexo.xlsx")
View(Viajessexo)

VS <- ggplot(data=Viajessexo, aes(x=modotransporte, y=porcentaje))+
  geom_bar(aes(fill=sexo), stat="identity", position = "dodge")+  #"position=dodge" nos sirve para decirle a ggplot que queremos ver las barras lado a lado.
  labs(title = "Colectivo u otros medios según sexo RMBA",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Modo de transporte",
       y= "Porcentaje",
       fill= "Sexo")+
  scale_y_continuous(labels=percent)

ggplotly(VS)

#Ahora vamos a ver el mismo grafico pero viendo hombres y mujeres en una sola barra, separado por modo de transporte

VS2 <- ggplot(Viajessexo, aes(fill=modotransporte, y=porcentaje, x=sexo)) + 
  geom_bar(position="stack", stat="identity")+
  labs(title = "Colectivo u otros medios según sexo RMBA",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Modo de transporte",
       y= "Porcentaje",
       fill= "Sexo")+
  scale_y_continuous(labels=percent)
ggplotly(VS2)

##### UTILIZAN MÁS LOS COLECTIVOS LAS MUJERES QUE LOS HOMBRES #####

##### ¿Qué problemas tienen los usuarios? #####

# Cuánto esperan

Tiempoespera <- read_excel("Tiempoespera.xlsx")
View(Tiempoespera)

TE <- ggplot(data=Tiempoespera)+
  geom_bar(aes(x=USUARIOS, y=ESPERA), stat = "identity")+
  coord_flip()+
  scale_x_continuous(labels=percent)+
  labs(title = "Tiempo de espera de los usuarios",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Usuarios",
       y= "Tiempo de espera")
  
ggplotly(TE) # Acá vemos cómo la mayoría de los usuarios esperan entre 5 y más de 30 minutos el colectivo.

# Cuánto viajan

Tiempoviaje <- read_excel("Tiempoviaje.xlsx")
View(Tiempoviaje)

TV <- ggplot(data=Tiempoviaje)+
  geom_bar(aes(x=Duracion_viaje, y=porcentaje, fill=Duracion_viaje), stat = "identity")+
  scale_y_continuous(labels=percent)+
  theme_minimal() + # Función de ggrepel para que las visualizaciones sean más lindas
  labs(title = "Tiempo de viaje",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Duración del viaje",
       y= "Usuarios")

ggplotly(TV) # Acá vemos que hay también que, si bien podría no ser un problema de todos los viajes, la duración de los mismos es algo a trabajar para disminuir constantemente.

#Saturación colectivo

Modo_transporte <- read_excel("Modo transporte.xlsx")
View(Modo_transporte)

MT <- ggplot(data=Modo_transporte, aes(x=MODOTRANSPORTE, y= PORCENTAJE)) +
  geom_col(aes(fill = PUBLICOPRIVADO))+
  geom_text(aes(label = scales::percent(PORCENTAJE), #Acá le ponemos el porcentaje a cada barra, para que sea más sencillo de comprender
                           y = PORCENTAJE),
                       vjust = 1.5)+
  scale_y_continuous(labels=percent)+
  theme_minimal()+
  labs(title = "Modo de transporte",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Porcentaje Usuarios",
       y= "Modo de transporte",
       fill= "Tipo de Transporte")

MT

ggplotly(MT) #Este gráfico es bastante ancho, por lo que sugerimos verlo en pantalla completa.

##### IDENTIFICAMOS TRES PROBLEMAS: TIEMPO DE ESPERA, TIEMPO DE VIAJE Y SATURACIÓN DEL COLECTIVO #####

##### ¿Cuánto viajan los usuarios? #####

Frecuenciaviaje <- read_excel("Frecuenciaviaje.xlsx")
View(Frecuenciaviaje) 

FV <- ggplot(data=Frecuenciaviaje, aes(x=frecuencia_viaje, y=porcentaje))+
  geom_bar(stat = "identity", aes(fill=frecuencia_viaje))+
  scale_y_continuous(labels=percent)+
  theme_minimal()+
  labs(title = "Frecuencia de viaje",
       subtitle = "RMBA 2010",
       caption = "Fuente: elaboración propia en base a encuesta ENMODO 2010",
       x= "Frecuencia de viajes por semana",
       y= "Porcentaje de usuarios",
       fill= "Cuánto viajan por semana")

FV

ggplotly(FV)

##### VEMOS QUE LA GENTE VIAJA EN SU MAYORÍA 5 VECES POR SEMANA, POR LO QUE INFERIMOS QUE ES PARA TRABAJAR
##### ES POR ESTO QUE NOS CENTRAREMOS EN RELAJAR LA HORA PICO #####

##### ES POR TODOS LOS DATOS ANTERIORES QUE DECIDIMOS ENFOCAR NUESTRA POLÍTICA PÚBLICA #####
##### EN EL COLECTIVO Y SU USUARIO PRINCIPAL: LAS MUJERES TRABAJADORAS #####

##### EL COLECTIVO ES EL MEDIO MÁS UTILIZADO, POR ESO CREEMOS QUE EL PRIMER PASO HACIA #####
##### UNA SOLUCIÓN INTEGRAL DEL TRANSPORTE URBANO ES MEJORANDOLO #####

##### LEEMOS NUESTRO DATASET DE LAS LÍNEAS NACIONALES QUE RECORREN #####
##### LA REGIÓN METROPOLITANA DE BUENOS AIRES PARA ENCONTRAR LAS QUE SON OBJETO DE NUESTRA POLÍTICA #####

lineas_nac <- st_read("https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/84947471-9c1e-4a23-8a2e-03a8c87c056f/download/lineasbusrmbajurisdiccionnacional.geojson")

##### LIMPIAMOS EL DATASET PARA QUE NOS FILTRE ÚNICAMENTE LAS LÍNEAS QUE RECORREN SAN MARTÍN - CAPITAL FEDERAL #####

lineas_san_martin <- lineas_nac %>% filter( LINEA == "78" & RAMAL == "B" | LINEA =="87" & RAMAL == "A" | LINEA == "176" & RAMAL == "A" | LINEA == "111" & (RAMAL == "A" |  RAMAL == "C")  
                                            |   LINEA == "161" & RAMAL == "B"| LINEA == "169" & (RAMAL == "A" |  RAMAL == "B")  | LINEA == "175" )

##### NUEVAMENTE FILTRAMOS LAS QUE HACEN EL RECORRIDO MUNICIPALIDAD DE SAN MARTÍN - NODO FEDERICO LACROZE #####

linea_sanmartin2 <- lineas_nac %>%
  filter(SENTIDO == "VUELTA") %>%
  filter((LINEA == "78" & RAMAL == "B") | (LINEA == "176" & RAMAL == "A") | (LINEA == "87" & RAMAL == "A") | (LINEA == "111" & RAMAL == "A") | (LINEA == "111" & RAMAL == "C") ) %>%
  drop_na()

##### HACEMOS UN MAPA EN LEAFLET PARA VER EL RECORRIDO SOBRE UN MAPA REAL DE LAS LINEAS SELECCIONADAS #####

linea_sanmartin2 <- st_transform(x = linea_sanmartin2,crs = 4326) #Transformamos la proyección a epsg 4326 (esto es necesario para trabajar en leaflet)
linea_sanmartin2 <- st_zm(linea_sanmartin2, drop = T, what = "ZM") #Reconfiguramos la proyección

leaflet() %>% #Llamamos a leaflet
  addTiles() %>% #Cargamos el mapa de fondo
  addPolylines(data = linea_sanmartin2) #Le agregamos nuestro dataset

##### AHORA UN MAPA CON LAS MISMAS LINEAS, EN R #####

lineas_sanmartin_mapa <- linea_sanmartin2 %>%
  ggplot() +
  geom_sf(data = linea_sanmartin2, aes(color = LINEA, group = LINEA )) +
  scale_fill_viridis_c() +
  labs(title = "Recorridos San Martín - Lacroze",
       subtitle = "Líneas de Colectivo nacionales",
       fill = "Colectivo")
lineas_sanmartin_mapa

##### LE PEDIMOS A R QUE NOS GENERE EL ARCHIVO GEOJSON PARA TRABAJAR LUEGO EN KEPLER #####

geojsonio::geojson_write(linea_sanmartin2, file = "LineasSM_xKepler.geojson")
