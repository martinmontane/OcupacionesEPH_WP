# Author: @martinmontane
# Mail: martinmontaneb@gmail.com
# Using pacman to install all packages. Make sure you have it installed
require(pacman)
# Important! This code uses data.table::fcase(). You need to have installed
# the development version of data.table
# Load packages
librerias <-
  c(
    'data.table',
    'foreign',
    'magrittr',
    'ggplot2',
    'ggthemes',
    'ggraph',
    'RColorBrewer',
    'igraph',
    'viridis',
    'tidygraph',
    'ggrepel',
    "httr",
    "writexl",
    "viridis",
    "graphlayouts",
    "sandwich",
    "lmtest",
    "graphlayouts",
    "oaqc",
    "ggpubr",
    "jtools",
    "huxtable",
    "officer",
    "flextable"
  )
p_load(librerias, character.only = TRUE)
# Get functions that will help us handle the Encuesta Permanente de Hogares (EPH)
source("https://raw.githubusercontent.com/martinmontane/OcupacionesEPH_WP/master/getEphs.R",encoding = "UTF-8")
source('https://raw.githubusercontent.com/martinmontane/OcupacionesEPH_WP/master/auxiliares.R')
# Directory to save the EPH files. It must exits (function will not create the folders if they do not exist)
ephDir <-
  "C:/Users/marti/OneDrive/Documents/Encuesta Permamente de Hogares (EPH-Argentina)/Oficial/"
datos <- getEPHS(dir = ephDir)
# National industry classification codes for Argentina
clasificadorCAES <-
  fread(
    "https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/Conversores/caesnuevo4d_caesviejo.csv"
  )
# We only keep observations from the City of Buenos Aires
datos <- datos[ESTADO %in% c(1:3) & REGION == 1]
gc()

# Industry codes have suffered some changes throughout the sample. Here we transform all codes to the first version of the
# Clasificador de Actividades para Encuestas Sociodemográficas
datos <-
  datos[, CAT_ACT := as.numeric(ifelse(
    ANO4 > 2011 & !is.na(PP04B_CAES),
    PP04B_CAES,
    ifelse(ANO4 > 2011 &
             is.na(PP04B_CAES), PP04B_COD, NA)
  ))]
clasificadorCAES$CAES_nuevo <-
  as.numeric(clasificadorCAES$CAES_nuevo)
clasificadorCAES$CAES_viejo_4d <-
  as.numeric(clasificadorCAES$CAES_viejo_4d)
datos <-
  datos[, CAT_ACT := ifelse(!CAT_ACT %in% na.omit(clasificadorCAES$CAES_nuevo),
                            NA,
                            CAT_ACT)]
datos <- datos[clasificadorCAES, on = c('CAT_ACT' = 'CAES_nuevo'),
               CAT_ACT := ifelse(ANO4 <= 2011, NA, i.CAES_viejo_4d)]
datos <- datos[, CAT_ACT := ifelse(ANO4 <= 2011, PP04B_COD, CAT_ACT)]
datos <-
  datos[, CAT_ACT := ifelse(!CAT_ACT %in% clasificadorCAES$CAES_viejo_4d, NA, CAT_ACT)]
datos <-
  datos[, CAT_ACT := ifelse(nchar(CAT_ACT) == 3, paste0(0, CAT_ACT), CAT_ACT)]

# We convert 4-digit CAES code to activity letter, which is the greatest aggregation available in the classificator
clasificadorCAESLetra <-
  fread(
    "https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/Conversores/caes_viejo_4d_a_letra.csv",
    encoding = "UTF-8"
  )
clasificadorCAESLetra <- clasificadorCAESLetra[nchar(CAES_VIEJO) > 2]
clasificadorCAESLetra <-
  clasificadorCAESLetra[, CAES_VIEJO := ifelse(nchar(CAES_VIEJO) == 3, paste0('0', CAES_VIEJO), CAES_VIEJO)]
datos <- datos[clasificadorCAESLetra, on = c('CAT_ACT' = 'CAES_VIEJO'),
               LETRA_ACT := Letra]
datos <- datos[, PONDERA_W := ifelse(ANO4 < 2016, PONDERA, PONDIIO)]
# We summarise wages at the year-industry level to create a dummy variable that tells us whether
# the industry was paying above the mean
resumenLetras <-
  datos[P21 > 0 &
          PP3E_TOT > 0, list(Variable = sum((P21 / PP3E_TOT) * PONDERA_W) / sum(PONDERA_W)),
        by = .(LETRA_ACT, ANO4)]

promedioAnual <- datos[P21 > 0 & PP3E_TOT > 0,
                       list(Variable = sum((P21 / PP3E_TOT) * PONDERA_W) /
                              sum(PONDERA_W)),
                       by = .(ANO4)]
resumenLetras <-
  resumenLetras[promedioAnual, on = 'ANO4', SALARIO_AVE := i.Variable]
resumenLetras <-
  resumenLetras[, arribaPromedio := ifelse(Variable > SALARIO_AVE, TRUE, FALSE)]
datos <- datos[resumenLetras, on = c('LETRA_ACT', 'ANO4'),
               SECTOR_ABOVE_AVG := ifelse(!is.na(LETRA_ACT), arribaPromedio, NA)]
# Here we do some data wrangling to keep only individuals that have more than one observation employed
# and create a variable (TRIMESTRES) that measures the difference between the observation of the
# individual and the last observation that same individual.
# We also create 2-digits occupation and industries variables to identify whether the
# job transition includes a short-lived unemployment or inactivity spell. Finally,
# we code the occupational category of the individual in each observation
datos <- datos %>%
  .[, CODUSU := trimws(CODUSU, which = 'both')] %>%
  .[, ID := paste(CODUSU, NRO_HOGAR, COMPONENTE, sep = '_')] %>%
  .[, nActivo := sum(ESTADO == 1), by = c('ID')] %>%
  .[nActivo > 1] %>%
  .[order(ID, ANO4, TRIMESTRE)] %>%
  .[, TRIMESTRES := ((ANO4 - shift(ANO4, type = 'lag')) * 4) + (TRIMESTRE -
                                                                  shift(TRIMESTRE, type = 'lag')), by = c('ID')] %>%
  .[, TRIMESTRES := ifelse(is.na(TRIMESTRES), 0, TRIMESTRES)] %>%
  .[, PP04D_COD := substr(PP04D_COD, 1, 2)] %>%
  .[, CAT_ACT := substr(CAT_ACT, 1, 2)] %>%
  .[, saltoInactividad := ifelse(
    ESTADO == 1 & shift(type = 'lag', n = 1, x = ESTADO) == 3 &
      shift(type = 'lag', n = 2, x = ESTADO) == 1,
    'Inactividad',
    ifelse(
      ESTADO == 1 & shift(type = 'lag', n = 1, x = ESTADO) == 2 &
        shift(type = 'lag', n = 2, x = ESTADO) == 1,
      'Desocupado',
      'Otro'
    )
  ), by = c('ID')] %>%
  .[, CAT_OCUP := ifelse(
    CAT_OCUP %in% c(1, 2, 4),
    'Cuentapropista',
    ifelse(
      CAT_OCUP == 3 & (PP07H == 1 | PP07I == 1),
      'Asalariado registrado',
      ifelse(
        CAT_OCUP == 3 &
          !(PP07H == 1 | PP07I == 1),
        'Asalariado no registrado',
        ifelse(!ESTADO %in% 1, 'No ocupado', 'Otro')
      )
    )
  )]
gc()
datos[which(saltoInactividad %in% c('Desocupado')) - 2, saltoInactividad :=
        'Desocupado']
datos[which(saltoInactividad %in% c('Inactividad')) - 2, saltoInactividad :=
        'Inactividad']
# We create a data.table that has only the transitions that go through a period of unemployment
saltosInactividad <-
  datos[saltoInactividad %in% c('Desocupado', 'Inactividad')]
# We subset so to keep only with the observations that did not go through a period of unemployment
datos <- datos[!ID %in% unique(saltosInactividad$ID)]
  # Here the code detects job switches if ellapsed trimester is greater than experience with employer
  # We can't compute 24T method as in other papers in the literature since we have bracketed
  # information on the job experience. Given the short time frame, we detect a job change
  # if the stated employer experience is less than the number of total quarters since the individual
  # entered the sample.
  # Job tenure is measured through different question in the EPH. PP07A is used for
  # waged workers, except for those employed in the domestic services activity sector.
  # PP05H is used for non-waged workers, except for those employed in the domestic services activity sector
  # Finally, PP04B3_ANO and PP04B3_MES give as information on the job tenure of
  # domestic services workers
  datos <- datos %>%
  .[CAT_OCUP %in% c("Asalariado no registrado", "Asalariado registrado") &
      !CAT_ACT %in% 95,
    EXP_EMP := fcase(
      PP07A == 1 ,
      0 / 3,
      PP07A == 2 ,
      3 / 3 ,
      PP07A == 3 ,
      6 / 3,
      PP07A == 4,
      12 / 3,
      PP07A == 5 ,
      36 / 3,
      PP07A == 6,
      60 / 3,
      PP07A == 9,
      9,
      PP07A == 0 ,
      9
    )] %>%
  .[, EXP_EMP := ifelse(EXP_EMP %in% 9, NA, EXP_EMP)] %>%
  .[!CAT_OCUP %in% c("Asalariado no registrado", "Asalariado registrado") &
      !CAT_ACT %in% 95,
    EXP_EMP := fcase(
      PP05H == 1 ,
      0 / 3,
      PP05H == 2 ,
      3 / 3,
      PP05H == 3,
      6 / 3,
      PP05H == 4,
      12 / 3,
      PP05H == 5,
      36 / 3,
      PP05H == 6,
      60 / 3,
      PP05H == 9,
      9,
      PP05H == 0,
      9
    )] %>%
  .[, EXP_EMP := ifelse(EXP_EMP %in% 9, NA, EXP_EMP)] %>%
  .[, PP04B3_ANO := ifelse(PP04B3_ANO < 0 |
                             PP04B3_ANO %in% 99, NA, PP04B3_ANO)] %>%
  .[, PP04B3_MES := ifelse(PP04B3_MES < 0 |
                             PP04B3_MES %in% 99, NA, PP04B3_MES)] %>%
  .[CAT_ACT %in% 95, EXP_EMP := (PP04B3_ANO * 12 + PP04B3_MES) / 3] %>%
  .[, saltoLaboral := ifelse(!TRIMESTRES %in% 0 &
                               TRIMESTRES > EXP_EMP & !is.na(EXP_EMP),
                             TRUE,
                             FALSE), by = "ID"] %>%
  .[, transicion := any(saltoLaboral), by = "ID"] %>%
  .[transicion == TRUE] %>%
  .[, trim := zoo::as.yearqtr(paste(ANO4, TRIMESTRE, sep = "-"))] %>%
  .[, firstTrans := min(trim[saltoLaboral %in% TRUE]), by = "ID"] %>%
  .[, saltoLaboral := ifelse(trim == firstTrans, TRUE, FALSE), by = "ID"] %>%
  .[, apariciones := .N, by = "ID"]
datos <-
  datos[which(saltoLaboral == TRUE) - 1, saltoLaboral := TRUE, by = "ID"]
datos <- datos[saltoLaboral == TRUE]
datos <- datos[, apariciones := .N, by = c('ID')]
# rbind transitions detected by the two methodologies and add information about the last employment observation
saltosInactividad <- saltosInactividad %>%
  .[, OCUP_LAG := shift(type = 'lag', n = 1, x = PP04D_COD), by = c("ID")] %>%
  .[, ACT_LAG := shift(type = 'lag', n = 1, x = CAT_ACT), by = c("ID")] %>%
  .[, W_LAG := shift(type = 'lag', n = 1, x = P21), by = c("ID")] %>%
  .[, H_LAG := shift(type = 'lag', n = 1, x = PP3E_TOT), by = c("ID")] %>%
  .[, CAT_OCUP_LAG := shift(type = 'lag', n = 1, x = CAT_OCUP), by = c("ID")] %>%
  .[, UNEM_TRANS := TRUE] %>%
  .[, TRIM_LAG := shift(type = 'lag', n = 1, x = TRIMESTRE), by = c("ID")] %>%
  .[, YEAR_LAG := shift(type = 'lag', n = 1, x = ANO4), by = c("ID")]  %>%
  .[, IDIMPP_LAG := shift(type = 'lag', n = 1, x = IDIMPP), by = c("ID")] %>%
  .[, SECTOR_ABOVE_AVG_LAG := shift(type = 'lag', n = 1, x = SECTOR_ABOVE_AVG), by =
      c("ID")] %>%
  .[, nOcupLag := shift(type = "lag", n = 1, x = PP03C), by = "ID"] %>%
  .[!is.na(W_LAG)]
datos <- datos %>%
  .[, OCUP_LAG := shift(type = 'lag', n = 1, x = PP04D_COD), by = c("ID")] %>%
  .[, ACT_LAG := shift(type = 'lag', n = 1, x = CAT_ACT), by = c("ID")] %>%
  .[, CAT_OCUP_LAG := shift(type = 'lag', n = 1, x = CAT_OCUP), by = c("ID")] %>%
  .[, W_LAG := shift(type = 'lag', n = 1, x = P21), by = c("ID")] %>%
  .[, H_LAG := shift(type = 'lag', n = 1, x = PP3E_TOT), by = c("ID")] %>%
  .[, TRIM_LAG := shift(type = 'lag', n = 1, x = TRIMESTRE), ] %>%
  .[, YEAR_LAG := shift(type = 'lag', n = 1, x = ANO4), ] %>%
  .[, UNEM_TRANS := FALSE]  %>%
  .[, IDIMPP_LAG := shift(type = 'lag', n = 1, x = IDIMPP), by = c("ID")] %>%
  .[, SECTOR_ABOVE_AVG_LAG := shift(type = 'lag', n = 1, x = SECTOR_ABOVE_AVG), by =
      c("ID")] %>%
  .[, nOcupLag := shift(type = "lag", n = 1, x = PP03C), by = "ID"] %>%
  .[!is.na(W_LAG)]
datos <- rbind(datos, saltosInactividad, fill = TRUE)
# Load spanish-english translation of the CNO's codes
cno <- readxl::read_excel(path = 'CNOTranslate.xlsx', sheet = 1)
# Data frame that contains all transitions that have information on the CNO code before and after the transition
datosOcupaciones <-
  datos[datos$PP04D_COD %in% cno$Código &
          datos$OCUP_LAG %in% cno$Código, ]

# Transition matrix for the ocupation codes. The idea is to plot the breakdown of departures/arrivals
# for each of the occupations
matrizTransicionesOcupaciones <-
  unclass(table(datosOcupaciones$PP04D_COD,
                datosOcupaciones$OCUP_LAG))
propSalidas <-
  data.table(
    Sector = rownames(matrizTransicionesOcupaciones),
    Valor = apply(matrizTransicionesOcupaciones, 1, sum) /
      sum(matrizTransicionesOcupaciones)
  )
propLlegadas <-
  data.table(
    Sector = colnames(matrizTransicionesOcupaciones),
    Valor = apply(matrizTransicionesOcupaciones, 2, sum) /
      sum(matrizTransicionesOcupaciones)
  )
SalidasLlegadasSector <-
  propSalidas[propLlegadas, on = c('Sector'), valor2 := i.Valor]
colnames(SalidasLlegadasSector) <-
  c('Sector', 'PropSalida', 'PropLlegada')
cno$IDDouble <- as.double(cno$Código)
SalidasLlegadasSector$Sector <-
  as.double(SalidasLlegadasSector$Sector)
SalidasLlegadasSector <- SalidasLlegadasSector[as.data.table(cno),
                                               on = c('Sector' = 'IDDouble'),
                                               cnoIng := Desc_eng]
SalidasLlegadasSector$Agrupación <-
  ifelse(
    SalidasLlegadasSector$PropSalida > 0.025 |
      SalidasLlegadasSector$PropLlegada > 0.025,
    SalidasLlegadasSector$cnoIng,
    NA
  )
salidaEntradaPlot <-
  ggplot(SalidasLlegadasSector, aes(x = PropSalida, y = PropLlegada)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_text_repel(aes(label = Agrupación), force = 4, size = 5) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = 'Share of job-to-job transitions (old occupation)',
       y = 'Share of job-to-job transitions (new occupation)') +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
ggsave(
  filename = 'png/salidaEntradaPlot.png',
  dpi = 300,
  scale = 1.5,
  salidaEntradaPlot
)
ggsave(
  filename = 'svg/salidaEntradaPlot.svg',
  dpi = 300,
  scale = 1.5,
  salidaEntradaPlot
)

# Transition matrix for the activity codes. The idea is to plot the breakadown of departures/arrivals
# for each of the 2-digit activity codes
matrizTransicionesActividades <- unclass(table(datos$CAT_ACT,
                                               datos$ACT_LAG))
propSalidas <-
  data.table(
    Sector = rownames(matrizTransicionesActividades),
    Valor = apply(matrizTransicionesActividades, 1, sum) /
      sum(matrizTransicionesActividades)
  )
propLlegadas <-
  data.table(
    Sector = colnames(matrizTransicionesActividades),
    Valor = apply(matrizTransicionesActividades, 2, sum) /
      sum(matrizTransicionesActividades)
  )
SalidasLlegadasSector <-
  propSalidas[propLlegadas, on = c('Sector'), valor2 := i.Valor]
colnames(SalidasLlegadasSector) <-
  c('Sector', 'PropSalida', 'PropLlegada')
# Industry codes
caes <-
  fread(
    "https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/CAES0_2D.csv"
  )
caes$`2d` <- as.numeric(caes$`2d`)
SalidasLlegadasSector$Sector <-
  as.numeric(SalidasLlegadasSector$Sector)
SalidasLlegadasSector <- SalidasLlegadasSector[caes,
                                               on = c('Sector' = '2d'),
                                               actIng := DescEnglish]
SalidasLlegadasSector$actIng <-
  ifelse(
    SalidasLlegadasSector$PropSalida > 0.025 |
      SalidasLlegadasSector$PropLlegada > 0.025,
    SalidasLlegadasSector$actIng,
    NA
  )
salidaEntradaPlot <-
  ggplot(SalidasLlegadasSector, aes(x = PropSalida, y = PropLlegada)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_text_repel(aes(label = actIng), force = 4, size = 5) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = 'Share of job-to-job transitions (old industry code)',
       y = 'Share of job-to-job transitions (new industry code)') +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
ggsave(
  filename = 'png/salidaEntradaPlotAct.png',
  dpi = 300,
  scale = 1.5,
  salidaEntradaPlot
)
ggsave(
  filename = 'svg/salidaEntradaPlotAct.svg',
  dpi = 300,
  scale = 1.5,
  salidaEntradaPlot
)

# get distance between occupations based on flows
flujosOcupaciones <- as.data.table(table(datosOcupaciones$PP04D_COD,
                                         datosOcupaciones$OCUP_LAG))
flujosOcupaciones <-
  getDistanceMeasureProduccion(
    sectorSalida = flujosOcupaciones$V2,
    sectorLlegada = flujosOcupaciones$V1,
    flow = flujosOcupaciones$N,
    simmetry = "max"
  )
flujosOcupaciones <- flujosOcupaciones[,
                                       sectores := dplyr::mutate(flujosOcupaciones,
                                                                 sectores = mapply(
                                                                   c,
                                                                   as.character(sectorSalida),
                                                                   as.character(sectorLlegada),
                                                                   SIMPLIFY = F
                                                                 ))$sectores]
flujosOcupaciones <- flujosOcupaciones[,
                                       sectores := lapply(sectores, sort)]

# Histogram showing the distribution of the occupation similarity measure
medidaDistanciaHist <-
  ggplot(flujosOcupaciones[RijCorregido > -1 &
                             !duplicated(sectores)]) +
  geom_histogram(aes(x = RijCorregido)) +
  theme_minimal() +
  labs(x = "", y = "")
ggsave(
  filename = 'png/distribucionHistogramaOcupaciones.png',
  medidaDistanciaHist,
  dpi = 300,
  scale = 2
)
ggsave(
  filename = 'svg/distribucionHistogramaOcupaciones.svg',
  medidaDistanciaHist,
  dpi = 300,
  scale = 2
)


# Occupation heatmap
heatmap <- ggplot(
  flujosOcupaciones[!is.nan(RijCorregido)],
  aes(
    x = sectorSalida,
    y = sectorLlegada,
    fill = RijCorregido,
    label = round(RijCorregido, 2)
  )
) +
  geom_tile(color = "white") +
  scale_fill_viridis() +
  labs(fill = "", x = "", y = "") +
  theme(axis.text = element_text(size = 12, color = "black"))
ggsave(filename = 'png/heatmapOcupacionesFull.png',
       heatmap,
       dpi = 300,
       scale = 2)
ggsave(filename = 'svg/heatmapOcupacionesFull.svg',
       heatmap,
       dpi = 300,
       scale = 2)

# Occupational space graph
colnames(flujosOcupaciones)[3] <- c('weight')
flujosGrafo <-
  flujosOcupaciones[!sectorSalida %in% c("02", "03", "04", "05", "06", "07") &
                      !sectorLlegada %in% c("02", "03", "04", "05", "06", "07") &
                      !duplicated(sectores) & weight > 0]
flujosGrafo <-
  flujosGrafo[order(weight, decreasing = TRUE)][!sectorSalida == sectorLlegada]
grafo <- graph_from_data_frame(flujosGrafo, directed = FALSE)
g_mst <- as_tbl_graph(grafo)
colnames(cno)[3] <- 'Agrupacion'

g_mst <- g_mst %>%
  activate(nodes) %>%
  mutate(name = as.numeric(name)) %>%
  dplyr::left_join(cno[, c('Código', 'Agrupacion')], by = c('name' = 'Código'))
g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::mutate(Agrupacion = gsub(
    x = Agrupacion,
    pattern = '* occupations*',
    replacement = ''
  )) %>%
  dplyr::mutate(Agrupacion = as.factor(Agrupacion))

# Strength
strengthGrafo <- sort(strength(grafo))

strengthGrafo <- data.table(ocupacion = names(strengthGrafo),
                            strength = strengthGrafo)
strengthGrafo <- strengthGrafo[order(strength)]
strengthGrafo$ocupacion <- as.numeric(strengthGrafo$ocupacion)
cno <- as.data.table(cno)
strengthGrafo <-
  strengthGrafo[cno, on = c('ocupacion' = 'Código'), Descripcion := Agrupacion]
strengthGrafo$Descripcion = gsub(x = strengthGrafo$Descripcion,
                                 pattern = '* occupations*',
                                 replacement = '')
strengthGrafo$ocupacion <- factor(strengthGrafo$ocupacion,
                                  levels = strengthGrafo$ocupacion[order(strengthGrafo$strength)])
flujosOcupacionesVolume <-
  as.data.table(table(datosOcupaciones$PP04D_COD,
                      datosOcupaciones$OCUP_LAG))
flujosOcupacionesVolume <- melt(flujosOcupacionesVolume,
                                measure.vars = c("V1", "V2"))
flujosOcupacionesVolume <-
  flujosOcupacionesVolume[, sum(N), by = value]
strengthGrafo <- strengthGrafo[flujosOcupacionesVolume,
                               , on = c("ocupacion" = "value")]
dataPlot <- strengthGrafo[V1 > 50]
dataPlot$Descripcion <- factor(dataPlot$Descripcion)
dataPlot$Descripcion <-
  reorder(dataPlot$Descripcion, dataPlot$strength)
strengthPlot <- ggplot(dataPlot) +
  geom_bar(aes(x = Descripcion, y = strength),
           fill = '#016392',
           stat = 'identity') +
  geom_text(aes(
    x = Descripcion,
    y = strength,
    label = Descripcion,
    hjust = -0.1
  )) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 20)) +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.y = element_blank())
ggsave(
  'png/strengthPlot50.png',
  dpi = 300,
  strengthPlot,
  width = 10,
  height = 7
)
ggsave('svg/strengthPlot50.svg',
       dpi = 300,
       strengthPlot,
       scale = 2)

dataPlot <- strengthGrafo[V1 > 100]

g_plot <- g_mst %>%
  activate(nodes) %>%
  mutate(name = as.character(name)) %>%
  left_join(dataPlot, by = c("name" = "ocupacion")) %>%
  filter(!is.na(Descripcion))

grafoBackbone <- layout_as_backbone(g_plot, keep = 0.1)
set.seed(1)
grafoMST <-
  ggraph(
    layout = 'manual',
    graph = g_plot,
    x = grafoBackbone$xy[, 1],
    y = grafoBackbone$xy[, 2]
  ) +
  geom_edge_link(aes(width = weight), show.legend = FALSE, color = 'grey90') +
  scale_edge_width(range = c(0, 3)) +
  geom_node_point(aes(size = strength(g_plot)),
                  color = 'grey30',
                  show.legend = FALSE) +
  geom_node_text(
    aes(label = Descripcion),
    size = 4,
    check_overlap = TRUE,
    repel = TRUE
  ) +
  theme_minimal() +
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color = '', x = "", y = "")
ggsave(filename = 'png/grafo_OcupacionesFull.png',
       dpi = 300,
       scale = 1.5,
       grafoMST)
ggsave(filename = 'svg/grafo_OcupacionesFull.svg',
       dpi = 300,
       scale = 1.5,
       grafoMST)

dataPlot <- strengthGrafo[V1 > 50]
dataPlot$ocupacion <- as.numeric(dataPlot$ocupacion)
g_plot <- g_mst %>%
  activate(nodes) %>%
  mutate(strength = NULL) %>%
  left_join(dataPlot, by = c("name" = "ocupacion")) %>%
  filter(!is.na(strength))
grafoBackbone <- layout_as_backbone(g_plot, keep = 0.1)
set.seed(1)
grafoMST <-
  ggraph(
    layout = 'manual',
    graph = g_plot,
    x = grafoBackbone$xy[, 1],
    y = grafoBackbone$xy[, 2]
  ) +
  geom_edge_link(aes(width = weight), show.legend = FALSE, color = 'grey90') +
  scale_edge_width(range = c(0, 3)) +
  geom_node_point(aes(size = strength(g_plot)),
                  color = 'grey30',
                  show.legend = FALSE) +
  geom_node_text(
    aes(label = Descripcion),
    size = 3,
    check_overlap = TRUE,
    repel = TRUE
  ) +
  theme_minimal() +
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color = '', x = "", y = "")
ggsave(filename = 'png/grafo_Ocupaciones50.png',
       dpi = 300,
       scale = 1,
       grafoMST)
ggsave(filename = 'svg/grafo_Ocupaciones50.svg',
       dpi = 300,
       scale = 1,
       grafoMST)

# get distance between industries based on flows
flujosActividades <-
  as.data.table(table(datos$CAT_ACT, datos$ACT_LAG))
flujosActividades <-
  getDistanceMeasureProduccion(
    sectorSalida = flujosActividades$V2,
    sectorLlegada = flujosActividades$V1,
    flow = flujosActividades$N,
    simmetry = "max"
  )
flujosActividades <- flujosActividades[,
                                       sectores := dplyr::mutate(flujosActividades,
                                                                 sectores = mapply(
                                                                   c,
                                                                   as.character(sectorSalida),
                                                                   as.character(sectorLlegada),
                                                                   SIMPLIFY = F
                                                                 ))$sectores]
flujosActividades <- flujosActividades[,
                                       sectores := lapply(sectores, sort)]

# Heatmap de Actividades
heatmap <- ggplot(
  flujosActividades[!is.nan(RijCorregido)],
  aes(
    x = sectorSalida,
    y = sectorLlegada,
    fill = RijCorregido,
    label = round(RijCorregido, 2)
  )
) +
  geom_tile(color = "white") +
  scale_fill_viridis() +
  labs(fill = "", x = "", y = "") +
  theme(axis.text = element_text(size = 12, color = "black"))
ggsave(filename = 'png/heatmapActividadesFull.png',
       heatmap,
       dpi = 300,
       scale = 2)
ggsave(filename = 'svg/heatmapActividadesFull.svg',
       heatmap,
       dpi = 300,
       scale = 2)

# Create industry space
caes <-
  fread(
    "https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/CAES0_2D.csv"
  )
caes$`2d` <- as.character(caes$`2d`)
colnames(flujosActividades)[3] <- c('weight')
flujosGrafo <-
  flujosActividades[!sectorSalida %in% c("02", "05", "14", "23", "30", "75") &
                      !sectorLlegada %in% c("02", "05", "14", "23", "30", "75") &
                      !duplicated(sectores) & weight > 0]
flujosGrafo <-
  flujosGrafo[order(weight, decreasing = TRUE)][!sectorSalida == sectorLlegada]
grafo <- graph_from_data_frame(flujosGrafo, directed = FALSE)
g_mst <- as_tbl_graph(grafo)

g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::left_join(caes[, c('2d', 'DescEnglish', "LetraEnglish")], by =
                     c('name' = '2d'))
g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::mutate(DescEnglish = as.factor(DescEnglish))

strengthGrafo <- sort(strength(g_mst))

strengthGrafo <- data.table(actividad = names(strengthGrafo),
                            strength = strengthGrafo)
strengthGrafo <- strengthGrafo[order(strength)]
strengthGrafo <-
  strengthGrafo[caes, on = c('actividad' = '2d'), DescEnglish := DescEnglish]
strengthGrafo$actividad <- factor(strengthGrafo$actividad,
                                  levels = strengthGrafo$actividad[order(strengthGrafo$strength)])
flujosActividadesVolume <-
  as.data.table(table(datos$CAT_ACT, datos$ACT_LAG))
flujosActividadesVolume <- melt(flujosActividadesVolume,
                                measure.vars = c("V1", "V2"))
flujosActividadesVolume <-
  flujosActividadesVolume[, sum(N), by = value]
strengthGrafo <-
  strengthGrafo[flujosActividadesVolume[!value %in% "01"],
                , on = c("actividad" = "value")]
dataPlot <- strengthGrafo[V1 > 50]

g_mst <- g_mst %>%
  activate(nodes) %>%
  left_join(dataPlot, by = c("name" = "actividad")) %>%
  filter(!is.na(strength))

grafoBackbone <- layout_as_backbone(g_mst, keep = 0.5)
set.seed(1)
grafoMST <-
  ggraph(
    layout = 'manual',
    graph = g_mst,
    x = grafoBackbone$xy[, 1],
    y = grafoBackbone$xy[, 2]
  ) +
  geom_edge_link(aes(width = weight), show.legend = FALSE, color = 'grey90') +
  scale_edge_width(range = c(0, 2)) +
  geom_node_point(aes(size = strength), show.legend = FALSE) +
  geom_node_text(
    aes(label = DescEnglish.x),
    size = 4,
    check_overlap = TRUE,
    repel = TRUE
  ) +
  theme_minimal() +
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color = '', x = "", y = "")
ggsave(filename = 'png/grafo_Actividades50.png',
       dpi = 300,
       scale = 1.5,
       grafoMST)
ggsave(filename = 'svg/grafo_Actividades50.svg',
       dpi = 300,
       scale = 1.5,
       grafoMST)

dataPlot$DescEnglish <- factor(dataPlot$DescEnglish)
dataPlot$DescEnglish <-
  reorder(dataPlot$DescEnglish, dataPlot$strength)
strengthPlot <- ggplot(dataPlot) +
  geom_bar(aes(x = DescEnglish, y = strength),
           fill = '#016392',
           stat = 'identity') +
  geom_text(aes(
    x = DescEnglish,
    y = strength,
    label = DescEnglish,
    hjust = -0.1
  )) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 20)) +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.y = element_blank())
ggsave(
  'png/strengthPlotAct50.png',
  dpi = 300,
  strengthPlot,
  width = 10,
  height = 7
)
ggsave('svg/strengthPlotAct50.svg',
       dpi = 300,
       strengthPlot,
       scale = 2)


# Labeling of educational level
listaEducacion <-
  list(
    "Primaria Completa",
    'Primaria Completa',
    'Primaria Completa',
    'Secundaria Incompleta',
    'Secundaria Completa',
    'Superior Incompleta',
    'Superior Completa'
  )
names(listaEducacion) <- c(1, 7, 2, 3, 4, 5, 6)
datos <- datos[, NIVEL_ED := dplyr::recode(NIVEL_ED, !!!listaEducacion)]

# Adding distance measure to dataset
datos <- datos[flujosOcupaciones,
               on = c('OCUP_LAG' = 'sectorSalida', 'PP04D_COD' = 'sectorLlegada'),
               RijOcupaciones := weight]
datos <-
  datos[flujosActividades, on = c('ACT_LAG' = 'sectorSalida', 'CAT_ACT' =
                                    'sectorLlegada'),
        RijActividad := weight]

datos$ANO4 <- factor(datos$ANO4)
datos$TRIMESTRE <- factor(datos$TRIMESTRE)
datos$IDTRIM <- paste(datos$ANO4, datos$TRIMESTRE, sep = '-')
datos$IDTRIM <- factor(datos$IDTRIM)
datos <-
  datos[, NEW_NIVEL_ED := forcats::fct_relevel(NIVEL_ED, "Sin instrucción")]
GET(
  "https://github.com/martinmontane/EPHelper/raw/master/utils/Inflacion.xlsx",
  write_disk(excelfile <- tempfile(fileext = ".xlsx"))
)
inflacion <- readxl::read_excel(excelfile, 1) %>%
  as.data.table(.) %>%
  .[complete.cases(.)] %>%
  .[, list(IPC = mean(IPC)), by = c('Year', 'Trimestre')]

datos$ANO4 <- as.numeric(as.character(datos$ANO4))
datos$TRIMESTRE <- as.numeric(as.character(datos$TRIMESTRE))
datos <- datos[inflacion,
               on = c('ANO4' = 'Year',
                      'TRIMESTRE' = 'Trimestre'),
               IPC_ACTUAL := IPC]
datos <- datos[inflacion,
               on = c('YEAR_LAG' = 'Year',
                      'TRIM_LAG' = 'Trimestre'),
               IPC_LAG := IPC]
datos <-
  datos[, CAT_SEQ := ifelse(
    saltoInactividad %in% c('Desocupado', 'Inactividad'),
    'Inactividad',
    ifelse(PP04D_COD == OCUP_LAG, 'Misma ocupacion', 'Distinta ocupacion')
  )]
datos <- datos[, VariacionHoraria := (PP3E_TOT / H_LAG) - 1]
datos$ANO4 <- factor(datos$ANO4)
datos$difTrimestre <-
  (as.numeric(as.character(datos$ANO4)) - datos$YEAR_LAG) * 4 +
  (datos$TRIMESTRE - datos$TRIM_LAG)


datos$CAT_EDAD <-
  cut(
    datos$CH06,
    breaks = seq(20, 70, by = 5) ,
    include.lowest = TRUE,
    labels = c(
      "20 to 25",
      "25 to 30",
      "30 to 35",
      "35 to 40",
      "40 to 45",
      "45 to 50",
      "50 to 55",
      "55 to 60",
      "60 to 65",
      "65 to 70"
    )
  )
datos$NIVEL_ED <-
  factor(
    datos$NIVEL_ED,
    levels = c(
      "Primaria Completa",
      "Secundaria Incompleta",
      "Secundaria Completa",
      "Superior Incompleta",
      "Superior Completa"
    ),
    labels = c(
      "Complete\nelementary school",
      "High school\ndropout",
      "Complete\nHigh school",
      "Some university\nstudies",
      "Complete\nuniversity studies"
    )
  )


datosParaFlujo <- data.table(datos)
datos <- data.table(datos) %>%
  .[P21 > 0 &
      W_LAG > 0 & PP3E_TOT > 0 & H_LAG > 0 &
      PP3E_TOT < 999 & H_LAG < 999] %>%
  .[, variacion := ((P21 / PP3E_TOT) / (W_LAG / H_LAG)) - 1] %>%
  .[, WHoraReal_Despues := (P21 / PP3E_TOT) * (unique(IPC_ACTUAL[ANO4 ==
                                                                   2019 & TRIMESTRE == 2]) / IPC_ACTUAL)] %>%
  .[, WHoraReal_Antes := (W_LAG / H_LAG) * (unique(IPC_ACTUAL[ANO4 == 2019 &
                                                                TRIMESTRE == 2]) / IPC_LAG)]

datos <-
  datos[, variacionReal := ((variacion + 1) / (IPC_ACTUAL / IPC_LAG)) - 1]
datos$variacionRealTrimestral <-
  ((datos$variacionReal + 1) ^ (1 / datos$difTrimestre)) - 1


# Medida de distancia según edad y nivel educativo
boxplotEdadOcupaciones <-
  ggplot(datosParaFlujo[!is.na(CAT_EDAD)],
         aes(group = CAT_EDAD, x = CAT_EDAD, y = RijOcupaciones)) +
  geom_boxplot(fatten = 4,
               alpha = 0.8,
               outlier.alpha = 0) +
  # geom_jitter(alpha=0.1)+
  theme_fivethirtyeight() +
  # scale_fill_brewer(palette = "Set1",name="Age group") +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y  = element_text(size = 24)
  ) +
  labs(title = "Occupation similarity", y = "Similarity measure (-1 to 1)")
boxplotEdadActividades <-
  ggplot(datosParaFlujo[!is.na(CAT_EDAD)],
         aes(group = CAT_EDAD, x = CAT_EDAD, y = RijActividad)) +
  geom_boxplot(fatten = 4, alpha = 0.8) +
  # geom_jitter(alpha=0.1)+
  theme_fivethirtyeight() +
  # scale_fill_brewer(palette = "Set1",name="Age group") +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 24)
  ) +
  labs(title = "Industry similarity")
grafico <-
  ggarrange(boxplotEdadOcupaciones, boxplotEdadActividades, legend = "none")
ggsave(
  filename = 'png/boxplotEdadSimilarity.png',
  plot = grafico,
  dpi = 300,
  scale = 2.5
)

boxplotNivelEdOcupaciones <-
  ggplot(datosParaFlujo[!is.na(NIVEL_ED)],
         aes(group = NIVEL_ED, x = NIVEL_ED, y = RijOcupaciones)) +
  geom_boxplot(fatten = 4,
               alpha = 0.8,
               outlier.alpha = 0) +
  # geom_jitter(alpha=0.1)+
  theme_fivethirtyeight() +
  # scale_fill_brewer(palette = "Set1",name="Age group") +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 14),
    axis.title.y  = element_text(size = 30)
  ) +
  labs(title = "Occupation similarity", y = "Similarity measure (-1 to 1)")
boxplotNivelEdActividades <-
  ggplot(datosParaFlujo[!is.na(NIVEL_ED)],
         aes(group = NIVEL_ED, x = NIVEL_ED, y = RijActividad)) +
  geom_boxplot(fatten = 4, alpha = 0.8) +
  # geom_jitter(alpha=0.1)+
  theme_fivethirtyeight() +
  # scale_fill_brewer(palette = "Set1",name="Age group") +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 24)
  ) +
  labs(title = "Industry similarity")
grafico <-
  ggarrange(boxplotNivelEdOcupaciones,
            boxplotNivelEdActividades,
            legend = "none")
ggsave(
  filename = 'png/boxplotNivelEdSimilarity.png',
  plot = grafico,
  dpi = 300,
  scale = 3.3
)



regOcupaciones <-
  lm(datosParaFlujo[!is.na(CAT_EDAD)], formula = RijOcupaciones ~ CAT_EDAD + NIVEL_ED)
regActividades <-
  lm(datosParaFlujo[!is.na(CAT_EDAD)], formula = RijActividad ~ CAT_EDAD + NIVEL_ED)
export_summs(
  regOcupaciones,
  regActividades,
  model.names = c("Occupational similarity", "Industry similarity"),
  to.file = "docx",
  file.name = "tablasCoeficientes/TableEdadNivelEd.docx"
)



# Main regression results
datosOcupacion <- datosParaFlujo
datosIndustria <- datosParaFlujo
  flujosOcupaciones <-
    as.data.table(table(datosOcupaciones$PP04D_COD[!is.na(datosOcupaciones$PP04D_COD) &
                                                     !is.na(datosOcupaciones$OCUP_LAG)],
                        datosOcupaciones$OCUP_LAG[!is.na(datosOcupaciones$PP04D_COD) &
                                                    !is.na(datosOcupaciones$OCUP_LAG)]))
  flujosActividades <-
    as.data.table(table(datosIndustria$CAT_ACT[!is.na(datosIndustria$CAT_ACT) &
                                                 !is.na(datosIndustria$ACT_LAG)],
                        datosIndustria$ACT_LAG[!is.na(datosIndustria$ACT_LAG) &
                                                 !is.na(datosIndustria$CAT_ACT)]))
  flujosOcupaciones <-
    getDistanceMeasureProduccion(
      sectorSalida = flujosOcupaciones$V2,
      sectorLlegada = flujosOcupaciones$V1,
      flow = flujosOcupaciones$N,
      simmetry = "none"
    )
  flujosActividades <-
    getDistanceMeasureProduccion(
      sectorSalida = flujosActividades$V2,
      sectorLlegada = flujosActividades$V1,
      flow = flujosActividades$N,
      simmetry = "none"
    )
  
  datos <- datos[flujosOcupaciones, on = c('OCUP_LAG' = 'sectorSalida',
                                           'PP04D_COD' = 'sectorLlegada'),
                 `:=` (RijOcupaciones = RijCorregido,
                       n = flow)]
  datos <- datos[flujosActividades, on = c('CAT_ACT' = 'sectorSalida',
                                           'ACT_LAG' = 'sectorLlegada'),
                 `:=` (RijActividad = RijCorregido)]
  
  
  # Medida de distancia basada en las trayectorias de los que NO pasaron por desempleo / inactividad
  
  flujosOcupacionesEmployed <-
    as.data.table(table(datosOcupaciones$PP04D_COD[!(datosOcupaciones$saltoInactividad %in% c("Desocupado", "Inactividad")) &
                                                     !is.na(datosOcupaciones$PP04D_COD) &
                                                     !is.na(datosOcupaciones$OCUP_LAG)],
                        datosOcupaciones$OCUP_LAG[!(datosOcupaciones$saltoInactividad %in% c("Desocupado", "Inactividad")) &
                                                    !is.na(datosOcupaciones$PP04D_COD) &
                                                    !is.na(datosOcupaciones$OCUP_LAG)]))
  flujosOcupacionesEmployed <-
    getDistanceMeasureProduccion(
      sectorSalida = flujosOcupacionesEmployed$V2,
      sectorLlegada = flujosOcupacionesEmployed$V1,
      flow = flujosOcupacionesEmployed$N,
      simmetry = "none"
    )
  
  datos <-
    datos[flujosOcupacionesEmployed, on = c('OCUP_LAG' = 'sectorSalida',
                                            'PP04D_COD' = 'sectorLlegada'),
          `:=` (RijOcupacionesEmp = RijCorregido,
                nEmp = flow)]
  flujosActividadesEmployed <-
    as.data.table(table(datosIndustria$CAT_ACT[!datosIndustria$saltoInactividad %in%
                                                 c("Desocupado", "Inactividad") &
                                                 !is.na(datosIndustria$CAT_ACT) & !is.na(datosIndustria$ACT_LAG)],
                        datosIndustria$ACT_LAG[!datosIndustria$saltoInactividad %in%
                                                 c("Desocupado", "Inactividad") &
                                                 !is.na(datosIndustria$CAT_ACT) & !is.na(datosIndustria$ACT_LAG)]))
  flujosActividadesEmployed <-
    getDistanceMeasureProduccion(
      sectorSalida = flujosActividadesEmployed$V2,
      sectorLlegada = flujosActividadesEmployed$V1,
      flow = flujosActividadesEmployed$N,
      simmetry = "none"
    )
  datos <-
    datos[flujosActividadesEmployed, on = c('CAT_ACT' = 'sectorSalida',
                                            'ACT_LAG' = 'sectorLlegada'),
          RijActividadEmp := i.RijCorregido]
  
  flujosOcupacionesUnem <-
    as.data.table(table(datosOcupaciones$PP04D_COD[(datosOcupaciones$saltoInactividad %in% c("Desocupado", "Inactividad")) &
                                                     !is.na(datosOcupaciones$PP04D_COD) &
                                                     !is.na(datosOcupaciones$OCUP_LAG)],
                        datosOcupaciones$OCUP_LAG[(datosOcupaciones$saltoInactividad %in% c("Desocupado", "Inactividad")) &
                                                    !is.na(datosOcupaciones$PP04D_COD) &
                                                    !is.na(datosOcupaciones$OCUP_LAG)]))
  flujosOcupacionesUnem <-
    getDistanceMeasureProduccion(
      sectorSalida = flujosOcupacionesUnem$V2,
      sectorLlegada = flujosOcupacionesUnem$V1,
      flow = flujosOcupacionesUnem$N,
      simmetry = "none"
    )
  
  datos <-
    datos[flujosOcupacionesUnem, on = c('OCUP_LAG' = 'sectorSalida',
                                        'PP04D_COD' = 'sectorLlegada'),
          `:=` (RijOcupacionesUnemp = RijCorregido,
                nEmp = flow)]
  flujosActividadesUnem <-
    as.data.table(table(datosIndustria$CAT_ACT[datosIndustria$saltoInactividad %in%
                                                 c("Desocupado", "Inactividad") &
                                                 !is.na(datosIndustria$CAT_ACT) & !is.na(datosIndustria$ACT_LAG)],
                        datosIndustria$ACT_LAG[datosIndustria$saltoInactividad %in%
                                                 c("Desocupado", "Inactividad") &
                                                 !is.na(datosIndustria$CAT_ACT) & !is.na(datosIndustria$ACT_LAG)]))
  flujosActividadesUnem <-
    getDistanceMeasureProduccion(
      sectorSalida = flujosActividadesUnem$V2,
      sectorLlegada = flujosActividadesUnem$V1,
      flow = flujosActividadesUnem$N,
      simmetry = "none"
    )
  datos <-
    datos[flujosActividadesUnem, on = c('CAT_ACT' = 'sectorSalida',
                                        'ACT_LAG' = 'sectorLlegada'),
          RijActividadUnemp := i.RijCorregido]
  
  # Regresiones
  
  formulas <- list()
  formulas[[1]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijOcupaciones*log(WHoraReal_Antes) +  CAT_OCUP_LAG  + CAT_OCUP + ANO4  + CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[2]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijOcupacionesEmp*log(WHoraReal_Antes)+ ANO4  + CAT_OCUP_LAG+ CAT_OCUP +  CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[3]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijActividad*log(WHoraReal_Antes) + CAT_OCUP_LAG  + CAT_OCUP + ANO4  + CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[4]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijActividadEmp*log(WHoraReal_Antes)+ ANO4  + CAT_OCUP_LAG+ CAT_OCUP +  CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[5]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijActividad*log(WHoraReal_Antes) + RijOcupaciones*log(WHoraReal_Antes) + CAT_OCUP_LAG  + CAT_OCUP + ANO4  + CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[6]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijActividadEmp*log(WHoraReal_Antes) + RijOcupacionesEmp*log(WHoraReal_Antes)  + ANO4  + CAT_OCUP_LAG+ CAT_OCUP +  CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[7]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijOcupacionesUnemp*log(WHoraReal_Antes)+ ANO4  + CAT_OCUP_LAG+ CAT_OCUP +  CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[8]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijActividadUnemp*log(WHoraReal_Antes)+ ANO4  + CAT_OCUP_LAG+ CAT_OCUP +  CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  formulas[[9]] <-
    "log(WHoraReal_Despues) ~ log(WHoraReal_Antes) + RijActividadUnemp*log(WHoraReal_Antes) + RijOcupacionesUnemp*log(WHoraReal_Antes)  + ANO4  + CAT_OCUP_LAG+ CAT_OCUP +  CH06 + I(NIVEL_ED) + PP3E_TOT + H_LAG"
  names(formulas) <- c(
    'nedel_Ocup',
    'nedel_OcupEmp',
    "nedel_Act",
    "nedel_ActEmp",
    "nedel_OcupAct",
    "nedel_OcupEmpActEmp",
    "nedel_OcupUnemp",
    "nedel_ActUnemp",
    "nedel_OcupUnemActUnemp"
  )
  
  grupos <- list(
    'pooled' = 'datos',
    'pooled1JobWorkersFT' = 'datos[nOcupLag %in% 1 & PP03C %in% 1 & PP3E_TOT >= 35 & H_LAG >= 35]',
    'pooledEmp' = 'datos[!saltoInactividad%in%c("Desocupado","Inactividad")]',
    'pooledEmp1JobWorkersFT' = 'datos[!saltoInactividad%in%c("Desocupado","Inactividad") & nOcupLag %in% 1 & PP03C %in% 1 & PP3E_TOT >= 35 & H_LAG >= 35]',
    'pooledUnemp' = 'datos[saltoInactividad%in%c("Desocupado","Inactividad")]'
  )
  regresiones <- lapply(formulas, function(x) {
    lapply(grupos, function(y) {
      summary(lm(formula = as.formula(x),
                 data = eval(parse(text = y))))
    })
  })
  
  export_summs(
    regresiones$nedel_Ocup$pooled,
    regresiones$nedel_Act$pooled,
    regresiones$nedel_OcupAct$pooled,
    model.names = c(
      "Occupational similarity",
      "Industry similarity",
      "Both similarity measures"
    ),
    to.file = "docx",
    file.name = "tablasCoeficientes/DistanceAllGroupAll.docx"
  )

  