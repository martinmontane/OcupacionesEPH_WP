# Carga de librerías
librerias <- c('data.table','foreign','magrittr','ggplot2','ggthemes','ggraph',
               'RColorBrewer', 'igraph', 'viridis','ggraph','tidygraph','ggrepel',"httr","readxl","viridis")
lapply(librerias, require, character.only=TRUE)
options(java.parameters = "-Xmx8000m")
setDTthreads(4)
source("https://raw.githubusercontent.com/martinmontane/EPHelper/master/getEphs.R")
source('https://raw.githubusercontent.com/martinmontane/EPHelper/master/auxiliares.R')
# Directorio donde guardar las EPHs. Debe existir y tener al menos 3gb de espacio
# en el disco rígido
ephDir <- "DirectorioEPH"
datos <- getEPHS(dir = ephDir)
clasificadorCAES <- fread("https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/Conversores/caesnuevo4d_caesviejo.csv")
datos <- datos[ESTADO %in% c(1:3) & REGION==1]
gc()
datos <- datos[,CAT_ACT:=as.numeric(ifelse(ANO4 >2011 & !is.na(PP04B_CAES),PP04B_CAES,
                                           ifelse(ANO4 > 2011 & is.na(PP04B_CAES),PP04B_COD,NA)))]
clasificadorCAES$CAES_nuevo <- as.numeric(clasificadorCAES$CAES_nuevo)
clasificadorCAES$CAES_viejo_4d <- as.numeric(clasificadorCAES$CAES_viejo_4d)
datos <- datos[,CAT_ACT:=ifelse(!CAT_ACT %in% na.omit(clasificadorCAES$CAES_nuevo),NA,CAT_ACT)]
datos <- datos[clasificadorCAES,on=c('CAT_ACT'='CAES_nuevo'),
               CAT_ACT:=ifelse(ANO4<=2011,NA,i.CAES_viejo_4d)] 
datos <- datos[,CAT_ACT := ifelse(ANO4<=2011, PP04B_COD, CAT_ACT)]
datos <- datos[,CAT_ACT := ifelse(!CAT_ACT %in% clasificadorCAES$CAES_viejo_4d,NA,CAT_ACT)]
datos <- datos[,CAT_ACT := ifelse(nchar(CAT_ACT)==3,paste0(0,CAT_ACT),CAT_ACT)]

clasificadorCAESLetra <- fread("https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/Conversores/caes_viejo_4d_a_letra.csv", encoding = "UTF-8")
clasificadorCAESLetra <- clasificadorCAESLetra[nchar(CAES_VIEJO)>2]
clasificadorCAESLetra <- clasificadorCAESLetra[,CAES_VIEJO:=ifelse(nchar(CAES_VIEJO)==3,paste0('0',CAES_VIEJO), CAES_VIEJO)]
datos <- datos[clasificadorCAESLetra,on=c('CAT_ACT'='CAES_VIEJO'),
               LETRA_ACT:=Letra]
datos <- datos[,PONDERA_W:=ifelse(ANO4<2016,PONDERA,PONDIIO)]
resumenLetras <- datos[P21>0 & PP3E_TOT>0,list(Variable=sum((P21/PP3E_TOT)*PONDERA_W)/sum(PONDERA_W)),
                       by=.(LETRA_ACT, ANO4)]

promedioAnual <- datos[P21>0 & PP3E_TOT>0,
                       list(Variable=sum((P21/PP3E_TOT)*PONDERA_W)/sum(PONDERA_W)),
                       by=.(ANO4)]
resumenLetras <- resumenLetras[promedioAnual,on='ANO4',SALARIO_AVE:=i.Variable]
resumenLetras <- resumenLetras[,arribaPromedio:=ifelse(Variable>SALARIO_AVE,TRUE,FALSE)]
datos <- datos[resumenLetras, on=c('LETRA_ACT','ANO4'),
               SECTOR_ABOVE_AVG:=ifelse(!is.na(LETRA_ACT),arribaPromedio,NA)]

datos <- datos %>%
  .[,CODUSU:=trimws(CODUSU,which = 'both')] %>%
  .[,ID:=paste(CODUSU,NRO_HOGAR,COMPONENTE,sep='_')] %>%
  .[,nActivo:=sum(ESTADO==1), by= c('ID')] %>%
  .[nActivo>1] %>%
  .[order(ID,ANO4,TRIMESTRE)] %>%
  .[, TRIMESTRES:=((ANO4-shift(ANO4,type = 'lag'))*4)+(TRIMESTRE-shift(TRIMESTRE, type='lag')), by=c('ID')] %>%
  .[, TRIMESTRES:=ifelse(is.na(TRIMESTRES), 0, TRIMESTRES)] %>%
  .[,PP04D_COD:=substr(PP04D_COD,1,2)] %>%
  .[,CAT_ACT := substr(CAT_ACT, 1, 2)]%>%
  .[,saltoInactividad:=ifelse(ESTADO == 1 & shift(type = 'lag',n = 1,x = ESTADO) == 3 &
                                shift(type = 'lag',n = 2,x = ESTADO) == 1, 'Inactividad',
                              ifelse(ESTADO == 1 & shift(type = 'lag',n = 1,x = ESTADO) == 2 &
                                       shift(type = 'lag',n = 2,x = ESTADO) == 1,'Desocupado','Otro')), by=c('ID')] %>%
  .[,CAT_OCUP:=ifelse(CAT_OCUP %in% c(1,2,4),'Cuentapropista',
                      ifelse(CAT_OCUP == 3 & (PP07H == 1 | PP07I ==1),'Asalariado registrado',
                             ifelse(CAT_OCUP == 3 & !(PP07H == 1 | PP07I ==1), 'Asalariado no registrado',
                                    ifelse(!ESTADO %in% 1, 'No ocupado','Otro'))))]
gc()

datos[which(saltoInactividad %in% c('Desocupado'))-2, saltoInactividad:='Desocupado']
datos[which(saltoInactividad %in% c('Inactividad'))-2, saltoInactividad:='Inactividad']
saltosInactividad <- datos[saltoInactividad %in% c('Desocupado','Inactividad')]
datos <- datos[!ID %in% unique(saltosInactividad$ID)]

datos <- datos %>%
  .[, EXP_EMP:=ifelse(PP07A %in% c(1,2), 2/3,
                      ifelse(PP07A == 3, 5/3,
                             ifelse(PP07A == 4,9/3,
                                    ifelse(PP07A == 5, 36/3,72))))] %>% 
  .[, saltoLaboral := ifelse(!TRIMESTRES %in% 0 & (TRIMESTRES-EXP_EMP) > 0, TRUE,FALSE), by='ID'] 
datos <- datos[which(saltoLaboral==TRUE)-1,saltoLaboral:=TRUE]
datos <- datos[saltoLaboral==TRUE]
datos <- datos[,apariciones:=.N, by=c('ID')]
datos <- datos[apariciones==2]

saltosInactividad <- saltosInactividad %>%
  .[, OCUP_LAG:=shift(type = 'lag',n = 1,x = PP04D_COD), by=c("ID")] %>%
  .[, ACT_LAG:=shift(type = 'lag',n = 1,x = CAT_ACT), by=c("ID")] %>%
  .[, W_LAG:=shift(type = 'lag',n = 1,x = P21), by=c("ID")] %>%
  .[, H_LAG:=shift(type = 'lag',n = 1,x = PP3E_TOT), by=c("ID")] %>%
  .[, CAT_OCUP_LAG:=shift(type = 'lag',n = 1,x = CAT_OCUP), by=c("ID")] %>%
  .[, UNEM_TRANS:=TRUE] %>%
  .[, TRIM_LAG:=shift(type = 'lag',n = 1,x = TRIMESTRE), by=c("ID")] %>%
  .[, YEAR_LAG:=shift(type = 'lag',n = 1,x = ANO4), by=c("ID")]  %>%
  .[, IDIMPP_LAG:=shift(type = 'lag',n = 1,x =IDIMPP), by=c("ID")] %>%
  .[, SECTOR_ABOVE_AVG_LAG:=shift(type = 'lag',n = 1,x =SECTOR_ABOVE_AVG), by=c("ID")] %>%
  .[!is.na(W_LAG)]
datos <- datos %>%
  .[, OCUP_LAG:=shift(type = 'lag',n = 1,x = PP04D_COD), by=c("ID")] %>%
  .[, ACT_LAG:=shift(type = 'lag',n = 1,x = CAT_ACT), by=c("ID")] %>%
  .[, CAT_OCUP_LAG:=shift(type = 'lag',n = 1,x = CAT_OCUP), by=c("ID")] %>%
  .[, W_LAG:=shift(type = 'lag',n = 1,x = P21), by=c("ID")] %>%
  .[, H_LAG:=shift(type = 'lag',n = 1,x = PP3E_TOT), by=c("ID")] %>%
  .[, TRIM_LAG:=shift(type = 'lag',n = 1,x = TRIMESTRE),] %>%
  .[, YEAR_LAG:=shift(type = 'lag',n = 1,x = ANO4),] %>%
  .[, UNEM_TRANS:=FALSE]  %>%
  .[,IDIMPP_LAG:=shift(type = 'lag',n = 1,x =IDIMPP), by=c("ID")] %>%
  .[, SECTOR_ABOVE_AVG_LAG:=shift(type = 'lag',n = 1,x =SECTOR_ABOVE_AVG), by=c("ID")] %>%
  .[!is.na(W_LAG)]

datos <- rbind(datos, saltosInactividad, fill=TRUE)  
cno <- readxl::read_excel(path = 'CNOTranslate.xlsx',sheet = 1)
cno$Código <- ifelse(nchar(cno$Código)==1,paste0('0',cno$Código),cno$Código)
datosOcupaciones <- datos[datos$PP04D_COD %in% cno$Código & datos$OCUP_LAG %in% cno$Código,]

# Análisis de salidas y entradas por ocupaciones
matrizTransicionesOcupaciones <- unclass(table(datosOcupaciones$PP04D_COD[!datosOcupaciones$PP04D_COD==datosOcupaciones$OCUP_LAG],
                                               datosOcupaciones$OCUP_LAG[!datosOcupaciones$PP04D_COD==datosOcupaciones$OCUP_LAG]))
propSalidas <- data.table(Sector= rownames(matrizTransicionesOcupaciones),
                          Valor=apply(matrizTransicionesOcupaciones,1,sum)/sum(matrizTransicionesOcupaciones))
propLlegadas <- data.table(Sector= colnames(matrizTransicionesOcupaciones),
                           Valor=apply(matrizTransicionesOcupaciones,2,sum)/sum(matrizTransicionesOcupaciones))
SalidasLlegadasSector <- propSalidas[propLlegadas,on=c('Sector'),valor2:=i.Valor]
colnames(SalidasLlegadasSector) <- c('Sector','PropSalida','PropLlegada')
cno$IDDouble <- as.double(cno$Código)
SalidasLlegadasSector$Sector <- as.double(SalidasLlegadasSector$Sector)
SalidasLlegadasSector <- SalidasLlegadasSector[as.data.table(cno),
                                               on=c('Sector'='IDDouble'),
                                               cnoIng:=Desc_eng]
SalidasLlegadasSector$Agrupación <- ifelse(SalidasLlegadasSector$PropSalida>0.025 |SalidasLlegadasSector$PropLlegada>0.025,
                                           SalidasLlegadasSector$cnoIng,
                                           NA)
salidaEntradaPlot <- ggplot(SalidasLlegadasSector, aes(x=PropSalida, y=PropLlegada)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_text_repel(aes(label=Agrupación),force = 4, size=5) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  labs(x='Share of job-to-job transitions (old occupation)',
       y='Share of job-to-job transitions (new occupation)') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))
ggsave(filename = 'png/salidaEntradaPlot.png',dpi=300,scale=2,salidaEntradaPlot)
ggsave(filename = 'svg/salidaEntradaPlot.svg',dpi=300,scale=2,salidaEntradaPlot)

# Metodo Neffke (2017) para ocupaciones y actividades. Full sample (ver más abajo)
flujosOcupaciones <- as.data.table(table(datosOcupaciones$PP04D_COD,
                                         datosOcupaciones$OCUP_LAG))
flujosOcupaciones <- getDistanceMeasureProduccion(sectorSalida = flujosOcupaciones$V2,
                                                  sectorLlegada = flujosOcupaciones$V1,
                                                  flow = flujosOcupaciones$N)
flujosOcupaciones <- flujosOcupaciones[,
                                       sectores:=dplyr::mutate(flujosOcupaciones,sectores= mapply(c, as.character(sectorSalida), as.character(sectorLlegada), SIMPLIFY = F))$sectores]
flujosOcupaciones <- flujosOcupaciones[,
                                       sectores:=lapply(sectores,sort)]
flujosActividades <- as.data.table(table(datos$CAT_ACT,datos$ACT_LAG))
flujosActividades <- getDistanceMeasureProduccion(sectorSalida = flujosActividades$V2,
                                                  sectorLlegada = flujosActividades$V1,
                                                  flow = flujosActividades$N)
# Histograma de distancia por sector de ocupación
medidaDistanciaHist <- ggplot(flujosOcupaciones[RijCorregido>-1 & !duplicated(sectores)]) +
  geom_histogram(aes(x=RijCorregido)) +
  theme_minimal() +
  labs(x="",y="")
ggsave(filename = 'png/distribucionHistogramaOcupaciones.png',medidaDistanciaHist,dpi=300,scale=2)
ggsave(filename = 'svg/distribucionHistogramaOcupaciones.svg',medidaDistanciaHist,dpi=300,scale=2)

# Heatmap de ocupaciones 
heatmap <- ggplot(flujosOcupaciones[!sectorSalida==71 & !sectorLlegada==71],aes(x=sectorSalida,y=sectorLlegada, fill=RijCorregido, label=round(RijCorregido,2))) +
  geom_tile(color=NA) +
  scale_fill_viridis() +
  labs(fill="", x="",y="")
ggsave(filename = 'png/heatmapOcupacionesFull.png',heatmap,dpi=300,scale=2)
ggsave(filename = 'svg/heatmapOcupacionesFull.svg',heatmap,dpi=300,scale=2)

# Construcción de grafo y análisis de strength
colnames(flujosOcupaciones)[3] <- c('weight')
flujosGrafo <- flujosOcupaciones[!sectorSalida %in% c("02","03","04","05","06","07") & !sectorLlegada %in% c("02","03","04","05","06","07") & !duplicated(sectores) &weight >0 ]
flujosGrafo<-flujosGrafo[order(weight,decreasing = TRUE)][!sectorSalida==sectorLlegada]
grafo <- graph_from_data_frame(flujosGrafo,directed = FALSE)
g_mst <- as_tbl_graph(grafo)
colnames(cno)[3] <- 'Agrupacion'
g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::left_join(cno[,c('Código','Agrupacion')],by=c('name'='Código'))
g_mst <- g_mst %>%
  activate(nodes) %>%
  mutate(Agrupacion = gsub(x=Agrupacion,pattern = '* occupations*',replacement = '')) %>%
  mutate(Agrupacion=as.factor(Agrupacion)) 
 
# Strength
strengthGrafo <- sort(strength(grafo))

strengthGrafo <- data.table(ocupacion=names(strengthGrafo),
                            strength = strengthGrafo)
strengthGrafo <- strengthGrafo[order(strength)]
cno <- data.table(cno)
strengthGrafo <- strengthGrafo[cno,on=c('ocupacion'='Código'), Descripcion:=Agrupacion]
strengthGrafo$Descripcion = gsub(x=strengthGrafo$Descripcion,pattern = '* occupations*',replacement = '')
strengthGrafo$ocupacion <- factor(strengthGrafo$ocupacion,
                                  levels = strengthGrafo$ocupacion[order(strengthGrafo$strength)])
flujosOcupacionesVolume <- as.data.table(table(datosOcupaciones$PP04D_COD,
                                         datosOcupaciones$OCUP_LAG))
flujosOcupacionesVolume <- melt(flujosOcupacionesVolume,
                          measure.vars = c("V1","V2"))
flujosOcupacionesVolume <- flujosOcupacionesVolume[,sum(N),by=value]                          
strengthGrafo <- strengthGrafo[flujosOcupacionesVolume,
                               ,on=c("ocupacion"="value")]
dataPlot <- strengthGrafo[V1>50]
dataPlot$Descripcion <-factor(dataPlot$Descripcion) 
dataPlot$Descripcion <- reorder(dataPlot$Descripcion,dataPlot$strength)
strengthPlot <- ggplot(dataPlot) +
  geom_bar(aes(x=Descripcion, y =strength), fill='#016392', stat = 'identity') +
  geom_text(aes(x=Descripcion, y=strength,label=Descripcion, hjust=-0.1)) +
  coord_flip() +
  scale_y_continuous(limits = c(0,20)) +
  theme_minimal() +
  labs(x="",y="") +
  theme(axis.text.y=element_blank())
ggsave('png/strengthPlot50.png',dpi = 300,strengthPlot, width=10,height=7)
ggsave('svg/strengthPlot50.svg',dpi = 300,strengthPlot, scale=2)

dataPlot <- strengthGrafo[V1>100]
dataPlot$Descripcion <-factor(dataPlot$Descripcion) 
dataPlot$Descripcion <- reorder(dataPlot$Descripcion,dataPlot$strength)
strengthPlot <- ggplot(dataPlot) +
  geom_bar(aes(x=Descripcion, y =strength), fill='#016392', stat = 'identity') +
  geom_text(aes(x=Descripcion, y=strength,label=Descripcion, hjust=-0.1)) +
  coord_flip() +
  scale_y_continuous(limits = c(0,15)) +
  theme_minimal() +
  labs(x="",y="") +
  theme(axis.text.y=element_blank())
ggsave('png/strengthPlot100.png',dpi = 300,strengthPlot, scale=2)
ggsave('svg/strengthPlot100.svg',dpi = 300,strengthPlot, scale=2)

dataPlot <- strengthGrafo
g_plot <- g_mst %>%
  activate(nodes) %>%
  left_join(dataPlot,by=c("name"="ocupacion"))
  
set.seed(1)
grafoMST <- ggraph(layout = 'nicely',graph = g_plot) + 
  geom_edge_link(aes(width=weight),show.legend = FALSE, color='grey90') +
  scale_edge_width(range = c(0,3)) +
  geom_node_point(aes(size=strength(g_plot)),color='grey30', show.legend = FALSE) +
  geom_node_text(aes(label=Descripcion), size=3,check_overlap = TRUE,repel = TRUE) +
  theme_minimal() + 
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color='', x="", y="")
ggsave(filename = 'png/grafo_OcupacionesFull.png', dpi=300, scale = 2, grafoMST)
ggsave(filename = 'svg/grafo_OcupacionesFull.svg', dpi=300, scale = 2, grafoMST)

dataPlot <- strengthGrafo[V1>100]
g_plot <- g_mst %>%
  activate(nodes) %>%
  mutate(strength=NULL) %>%
  left_join(dataPlot,by=c("name"="ocupacion")) %>%
  filter(!is.na(strength))

set.seed(1)
grafoMST <- ggraph(layout = 'nicely',graph = g_plot) + 
  geom_edge_link(aes(width=weight),show.legend = FALSE, color='grey90') +
  scale_edge_width(range = c(0,3)) +
  geom_node_point(aes(size=strength(g_plot)),color='grey30', show.legend = FALSE) +
  geom_node_text(aes(label=Descripcion), size=3,check_overlap = TRUE,repel = TRUE) +
  theme_minimal() + 
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color='', x="", y="")
ggsave(filename = 'png/grafo_Ocupaciones100.png', dpi=300, scale = 2, grafoMST)
ggsave(filename = 'svg/grafo_Ocupaciones100.svg', dpi=300, scale = 2, grafoMST)

# Redefinición de las categorías de nivel educativo
listaEducacion <- list("Sin instrucción",'Sin instrucción','Primaria Completa','Secundaria Incompleta','Secundaria Completa','Superior Incompleta','Superior Completa')
names(listaEducacion) <- c(1,7,2,3,4,5,6)
datos <- datos[,NIVEL_ED:=dplyr::recode(NIVEL_ED,!!!listaEducacion)]
datos <- datos[flujosOcupaciones,
               on=c('OCUP_LAG'='sectorSalida','PP04D_COD'='sectorLlegada'),
               RijOcupaciones:=weight]
datos <- datos[flujosActividades,on=c('ACT_LAG'='sectorSalida','CAT_ACT'='sectorLlegada'),
               RijActividad:=RijCorregido]

datos$ANO4 <- factor(datos$ANO4)
datos$TRIMESTRE <- factor(datos$TRIMESTRE)
datos$IDTRIM <- paste(datos$ANO4,datos$TRIMESTRE,sep='-')
datos$IDTRIM <- factor(datos$IDTRIM)
datos <- datos[,NEW_NIVEL_ED:=forcats::fct_relevel(NIVEL_ED,"Sin instrucción")]
GET("https://github.com/martinmontane/EPHelper/raw/master/utils/Inflacion.xlsx",
    write_disk(excelfile <- tempfile(fileext = ".xlsx")))
inflacion <- read_excel(excelfile, 1) %>%
  as.data.table(.) %>%
  .[complete.cases(.)] %>%
  .[,list(IPC=mean(IPC)), by=c('Year','Trimestre')]
datos <- data.table(datos) %>% 
  .[P21>0 & W_LAG >0 & PP3E_TOT>0 & H_LAG>0 & PP3E_TOT<999 & H_LAG<999] %>%
  .[,variacion:=((P21/PP3E_TOT)/(W_LAG/H_LAG))-1]
datos$ANO4 <- as.numeric(as.character(datos$ANO4))
datos$TRIMESTRE <- as.numeric(as.character(datos$TRIMESTRE))
datos <- datos[inflacion,
               on=c('ANO4'='Year',
                    'TRIMESTRE'='Trimestre'),
               IPC_ACTUAL:=IPC]
datos <- datos[inflacion,
               on=c('YEAR_LAG'='Year',
                    'TRIM_LAG'='Trimestre'),
               IPC_LAG:=IPC]
datos <- datos[,variacionReal:=((variacion+1)/(IPC_ACTUAL/IPC_LAG))-1]
datos <- datos[,CAT_SEQ:=ifelse(saltoInactividad %in% c('Desocupado','Inactividad'),'Inactividad',
                                ifelse(PP04D_COD==OCUP_LAG, 'Misma ocupacion','Distinta ocupacion'))]
datos <- datos[,VariacionHoraria:=(PP3E_TOT/H_LAG)-1]
datos$ANO4 <- factor(datos$ANO4)
datos$difTrimestre <- (as.numeric(as.character(datos$ANO4)) - datos$YEAR_LAG)*4+
  (datos$TRIMESTRE-datos$TRIM_LAG)

datos$variacionRealTrimestral <- ((datos$variacionReal+1)^(1/datos$difTrimestre))-1

datos <- datos[,WReal:=P21*unique(IPC_ACTUAL[ANO4==2019 & TRIMESTRE ==2])/IPC_ACTUAL]
salariosTrimestrales <- datos[,list(WReal=mean(WReal)),
                              by=.(ANO4,TRIMESTRE,CAT_SEQ)]
salariosTrimestrales$TRIM <- paste(salariosTrimestrales$ANO4,' Q',
                                   salariosTrimestrales$TRIMESTRE, sep='')
salariosTrimestrales$TRIM <- zoo::as.yearqtr(salariosTrimestrales$TRIM)
serieCompleta <-seq(as.Date("2003-07-01"), by="quarter", length.out = 64)
serieCompleta <- zoo::as.yearqtr(serieCompleta)
missingQtr <- serieCompleta[!serieCompleta %in% salariosTrimestrales$TRIM]
salariosTrimestrales <- rbind(salariosTrimestrales,
                              data.table(data.frame(TRIM=zoo::as.yearqtr(rep(missingQtr,length(unique(salariosTrimestrales$CAT_SEQ)))),
                                                    WReal=rep(NA,length(missingQtr)*length(unique(salariosTrimestrales$CAT_SEQ))),
                                                    CAT_SEQ=rep(unique(salariosTrimestrales$CAT_SEQ),length(missingQtr)))),
                              fill=TRUE) 
salariosTrimestrales$CAT_SEQ <- as.character(salariosTrimestrales$CAT_SEQ)
salariosTrimestrales$CAT_SEQ <- plyr::mapvalues(salariosTrimestrales$CAT_SEQ,
                                                c('Misma ocupacion','Distinta ocupacion','Inactividad'),
                                                c('Same occupation','Different occupation','Unemployment or inactivity'))
plotSalariosTransicion <- ggplot(salariosTrimestrales) +
  geom_line(aes(x=TRIM, y=WReal, color=CAT_SEQ), size=1.5) +
  theme_fivethirtyeight() +
  scale_color_wsj() +
  theme(axis.text.x = element_text(angle=90)) +
  zoo::scale_x_yearqtr(n = 14,format = 'Q%q-%Y') +
  labs(color='')
ggsave(filename = 'png/SalariosSegúnTransición.png',
       plot = plotSalariosTransicion,
       dpi=300)

ggsave(filename = 'svg/SalariosSegúnTransición.svg',
       plot = plotSalariosTransicion,
       dpi=300)

datos <- datos[,distintaOcupacion:= ifelse(!PP04D_COD==OCUP_LAG, TRUE, FALSE)]
datos <- datos[,distintaActividad:= ifelse(!ACT_LAG==CAT_ACT, TRUE, FALSE)]

# Distintas  maneras de medir la distancia entre ocupaciones
listaModelos <- list()
datosOcupacion <- datosOcupaciones
datosIndustria <- datos

  
  flujosOcupaciones <- as.data.table(table(datosOcupaciones$PP04D_COD,
                                           datosOcupaciones$OCUP_LAG))
  flujosActividades <- as.data.table(table(datosIndustria$CAT_ACT,
                                           datosIndustria$ACT_LAG))
  flujosOcupaciones <- getDistanceMeasureProduccion(sectorSalida = flujosOcupaciones$V2,
                                                    sectorLlegada = flujosOcupaciones$V1,
                                                    flow = flujosOcupaciones$N)
  flujosActividades <- getDistanceMeasureProduccion(sectorSalida = flujosActividades$V2,
                                                    sectorLlegada = flujosActividades$V1,
                                                    flow = flujosActividades$N)
  
  datos <- datos[flujosOcupaciones, on=c('OCUP_LAG'='sectorSalida',
                                         'PP04D_COD'='sectorLlegada'),
                 RijOcupaciones:=i.RijCorregido]
  datos <- datos[flujosActividades, on=c('CAT_ACT'='sectorSalida',
                                         'ACT_LAG'='sectorLlegada'),
                 RijActividad:=i.RijCorregido]
  
  
  # Medida de distancia basada en las trayectorias de los que pasaron por desempleo / inactividad

  # Medida de distancia basada en las trayectorias de los que NO pasaron por desempleo / inactividad
  
  flujosOcupacionesEmployed <- as.data.table(table(datosOcupaciones$PP04D_COD[!(datosOcupaciones$UNEM_TRANS == TRUE) & datosOcupaciones$CAT_OCUP %in% c('Asalariado registrado') & datosOcupaciones$CAT_OCUP_LAG %in% c('Asalariado registrado')],
                                                   datosOcupaciones$OCUP_LAG[!(datosOcupaciones$UNEM_TRANS ==TRUE) & datosOcupaciones$CAT_OCUP %in% c('Asalariado registrado') & datosOcupaciones$CAT_OCUP_LAG %in% c('Asalariado registrado')]))
  flujosOcupacionesEmployed <- getDistanceMeasureProduccion(sectorSalida = flujosOcupacionesEmployed$V2,
                                                            sectorLlegada = flujosOcupacionesEmployed$V1,
                                                            flow = flujosOcupacionesEmployed$N)
  datos <- datos[flujosOcupacionesEmployed, on=c('OCUP_LAG'='sectorSalida',
                                                 'PP04D_COD'='sectorLlegada'),
                 RijOcupacionesEmp:=i.RijCorregido]
  
  
  flujosActividadesEmployed <- as.data.table(table(datosIndustria$CAT_ACT[!datosIndustria$UNEM_TRANS & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP_LAG %in% c('Asalariado registrado')],
                                                   datosIndustria$ACT_LAG[!datosIndustria$UNEM_TRANS & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP_LAG %in% c('Asalariado registrado')]))
  flujosActividadesEmployed <- getDistanceMeasureProduccion(sectorSalida = flujosActividadesEmployed$V2,
                                                            sectorLlegada = flujosActividadesEmployed$V1,
                                                            flow = flujosActividadesEmployed$N)
  datos <- datos[flujosActividadesEmployed, on=c('CAT_ACT'='sectorSalida',
                                                 'ACT_LAG'='sectorLlegada'),
                 RijActividadEmp:=i.RijCorregido]
  
  # Regresiones
  formulas <- cbind(expand.grid(c('variacionRealTrimestral'),
                                c('RijOcupaciones','RijOcupacionesEmp'),
                                c('RijActividad','RijActividadEmp')))
  formulas <- formulas[c(1,4),]
  formulas <- paste(formulas$Var1,'~',formulas$Var2,'+',formulas$Var3,'+ SECTOR_ABOVE_AVG_LAG + CAT_OCUP_LAG + ANO4 + SECTOR_ABOVE_AVG + CAT_OCUP')
  
  names(formulas) <- c('VarRealTrim_RijOcup_RijAct',
                       'varRealTrim_RijOcupEmp_RijActEmp')
  
  
  grupos <- list('pooled'='datos',
                 'pooledEmp'='datos[!UNEM_TRANS==TRUE]')
  
  regresiones <- lapply(formulas,function(x){
    lapply(grupos,function(y) {
      tidyRegOutput(reg = lm(formula=as.formula(x),
                             data=eval(parse(text=y))),coef.int = 0.9)
      
    })
  })
  
  names(regresiones) <- names(formulas)
  
  
  for(i in 1:length(regresiones)) {
    for(j in 1:length(regresiones[[i]])) {
      xlsx::write.xlsx(file = "RegresionesSalida.xlsx",
                       x = as.data.table(regresiones[[i]][[j]]),
                       sheetName = paste0('Modelo_',i,'_',j),
                       row.names = FALSE,
                       append = TRUE)
    }
  }
  
  salida <- lapply(regresiones,function(x){
    grupo <- rep(names(x),sapply(x,function(y) nrow(y)))
    x <- rbindlist(x)
    x$grupo <- grupo
    return(x)
  })
  
  modelo  <- rep(names(salida),sapply(salida,function(x) nrow(x)))
  
  listaModelos <- rbindlist(salida)
  listaModelos$modelo <- modelo



coeficientePlot  <- ggplot(data=listaModelos[grepl('*RijOcc*',term) ],
                           aes(x = grupo,y = estimate, ymin = lb, ymax = ub))+
  geom_pointrange()+
  geom_hline(yintercept =0, linetype=2) +
  xlab('Variables Dependientes')+ ylab("Coeficiente asociado a la similitud entre ocupaciones\nIntervalo de 90% de confianza")+
  geom_errorbar(width=0.5,cex=1)+ coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y=element_blank(),
        axis.text.x=element_text(size=12),
        axis.ticks.y=element_blank(),
        axis.text=element_text(color='black'),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180))  +
  facet_grid(modelo~ grupo,scales = 'free',switch ="y")

ggsave(filename = 'png/coeficientesPlot.png',dpi=300,scale=1.8,coeficientePlot) 
ggsave(filename = 'svg/coeficientesPlot.svg',dpi=300,scale=1.8,coeficientePlot) 
