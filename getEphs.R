getEPHS <- function(years=c(2003:2019),
                    trims=c(1:4),
                    ind=TRUE,
                    dir=NULL,
                    asDataTable=TRUE) {
  if (!is.loaded("data.table")) library(data.table)
  if (!is.loaded("foreign")) library(foreign)
  
  ephsDisponibles <- data.table(year=c(rep(2003,2),rep(2004:2006,each=4),rep(2007,3),rep(2008:2014,each=4),rep(2015,2),rep(2016,3),rep(2017:2018,each=4),rep(2019,3)),
                                trim=c(3,4,rep(1:4,3),1,2,4,rep(1:4,7),1,2,2,3,4,rep(1:4,2),1,2,3),
                                linkDescarga=c('https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t303_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t403_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t104_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t204_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t304_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t404_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t105_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t205_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t305_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t405_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t106_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t206_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t306_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t406_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t107_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t207_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t407_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t108_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t208_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t308_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t408_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t109_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t209_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t309_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t409_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t110_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t210_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t310_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t410_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t111_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t211_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t311_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t411_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t112_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t212_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t312_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t412_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t113_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t213_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t313_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/t413_dta.zip',
                                               'https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/EPHsSAV/t114_dta.zip',
                                               'https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/EPHsSAV/t214_dta.zip',
                                               'https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/EPHsSAV/t314_dta.zip',
                                               'https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/EPHsSAV/t414_dta.zip',
                                               'https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/EPHsSAV/t115_dta.zip',
                                               'https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/EPHsSAV/t215_dta.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2doTrim_2016_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3erTrim_2016_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_4toTrim_2016_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_1er_Trim_2017_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2_Trim_2017_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3_Trim_2017_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_4_Trim_2017_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_1_Trim_2018_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2_Trim_2018_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3_Trim_2018_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_4_Trim_2018_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_1_Trim_2019_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_2_Trim_2019_txt.zip',
                                               'https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3_Trim_2019_txt.zip'))
  
  ephsDisponibles <- ephsDisponibles[year %in% years & trim %in% trims]
  
  ephsDisponibles[,nickname:= paste0('t',trim,substr(year,3,4))]
  
  if(is.null(dir)) stop('Hay que indicar un directorio donde desean guardarse los archivos')
  if(dir.exists(dir)) {
    if(ind) {
      archivos <-list.files(dir,
                            pattern = '*ind*',ignore.case = TRUE)
    } else {
      archivos <-list.files(dir,
                            pattern = '*hog',ignore.case = TRUE)
    }
    
    ephsDescargadas <- tolower(regmatches(archivos,regexpr('(?i)t\\d{3}',archivos)))
    ephsFaltantes <- ephsDisponibles[!nickname %in% ephsDescargadas]
    if(nrow(ephsFaltantes)==0){
      cat("Todas los microdatos de la EPH ya están descargadas en el directorio\r")
    } else {
      
      for(i in 1:nrow(ephsFaltantes)) {
        temp <- paste(dir,'/',ephsFaltantes[i,'nickname'],'.zip',sep='')
        download.file(unlist(ephsFaltantes[i,'linkDescarga']),temp)
        unzip(temp,exdir = dir)
        
      }
      cat("Todas los microdatos de la EPH ya se terminaron de descargar en el directorio\r")
    }
    filesToRead <- list.files(path=dir,
                              pattern = paste0(ephsDisponibles$nickname,'|',toupper(ephsDisponibles$nickname),collapse = "|"))
    if(ind) {
      filesToRead <- filesToRead[grepl(pattern = '*individual*',x = filesToRead,ignore.case = TRUE)]
      datos <- lapply(filesToRead,function(x){
        if(grepl(pattern = 'dta',x = x)) {
          x <- as.data.table(foreign::read.dta(paste0(dir,'/',x),convert.factors = FALSE))
          colnames(x) <- toupper(colnames(x))
          return(x) 
        } else {
          data.table::fread(paste0(dir,'/',x))
        }
      })
      datos <- rbindlist(datos, fill=TRUE)
    } else {
      filesToRead <- filesToRead[grepl(pattern = '*Hogar*',x = filesToRead,ignore.case = TRUE)]
      datos <- lapply(filesToRead,function(x){
        if(grepl(pattern = 'dta',x = x)) {
          x <- as.data.table(foreign::read.dta(paste0(dir,'/',x),convert.factors = FALSE))
          colnames(x) <- toupper(colnames(x))
          return(x) 
        } else {
          data.table::fread(paste0(dir,'/',x))
        }
      })
      datos <- rbindlist(datos, fill=TRUE)
    }
    if(!asDataTable) {
      datos <- as.data.frame(datos)
    }
    return(datos)
  }
  
}