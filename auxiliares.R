fill_matrix <- function(x, names = NULL, replaceValue=0) {
  if(!is.null(names)){
    if(!all(names %in% dimnames(x)[[1]])){
      faltantes_rows <- names[!names %in% rownames(x)]
      x <- rbind(x,matrix(data = rep(0, ncol(x)*length(faltantes_rows)), ncol = ncol(x),
                          dimnames = list(faltantes_rows,dimnames(x)[[2]])))
    } 
    if (!all(names %in% dimnames(x)[[2]])){
      faltantes_cols <-  names[!names %in% colnames(x)]
      x <- cbind(x,matrix(data = rep(0, nrow(x)*length(faltantes_cols)), nrow = nrow(x),
                          dimnames = list(dimnames(x)[[1]],faltantes_cols)))
    }
    x <- x[order(rownames(x)), ]
    x <- x[,order(colnames(x))] 
  } else {
    if(!dim(x)[1] %in% dim(x[2])) {
      faltantes_rows <- colnames(x)[!colnames(x) %in% rownames(x)]
      faltantes_cols <-  rownames(x)[!rownames(x) %in% colnames(x)]
      tot_sectores <- nrow(x)+length(faltantes_rows)
      x <- rbind(x,matrix(data = rep(replaceValue, ncol(x)*length(faltantes_rows)), ncol = ncol(x),
                          dimnames = list(faltantes_rows,dimnames(x)[[2]])))
      x <- cbind(x,matrix(data = rep(replaceValue, nrow(x)*length(faltantes_cols)), nrow = nrow(x),
                          dimnames = list(dimnames(x)[[1]],faltantes_cols)))
      x <- x[order(rownames(x)), ]
      x <- x[,order(colnames(x))]  
    }
  }
  return(x)
}

fill_gaps <- function(df, max_gap = 12){
  df <- arrange(df, ide_trabajador, mesInicio, mesFinal) %>%
    group_by(ide_trabajador, relacion) %>%
    mutate(threshold = ifelse(mesInicio-lag(mesFinal) > max_gap, FALSE, TRUE),
           remove = ifelse(threshold & lead(threshold), TRUE, FALSE),
           threshold = lead(threshold))
  df <- df[!df$remove | is.na(df$remove),]
  df[which(df$threshold), 'mesFinal'] <-  df[which(df$threshold) + 1, 'mesFinal']
  df <- df[-(which(df$threshold) + 1),]
  df$threshold <- df$remove <- NULL
  return(df)
}

createDistanceMatrix <- function(sectorOut, sectorIn, method, weights = NULL, returnLiftMatrix=FALSE) {
  transiciones <- unclass(table(sectorOut, sectorIn))
  transiciones <- fill_matrix(transiciones) 
  if(method == "outward") {
    lift <- transiciones / rowSums(transiciones)
    prop_total <- apply(transiciones,2,function(x) { sum(x) / sum(transiciones)})
    lift <- t(apply(lift,1,function(x){ (x*100)/(prop_total*100) }))
  } else if(method == "inward") { 
    lift <- t(transiciones) / colSums(transiciones)
    prop_total <- apply(transiciones,1,function(x) { sum(x) / sum(transiciones)})
    lift <- t(apply(lift,1,function(x){ (x*100)/(prop_total*100) }))
  }
  lift[lift<=1] <- 0
  lift[lift>1] <- 1
  lift[is.nan(lift)] <- 0
  liftMatrix <- lift
  probabilidades <- matrix(nrow = nrow(lift), ncol = ncol(lift),dimnames = dimnames(lift))
  # fviz_dist(probabilidades, show_labels = TRUE)
  for(sector_A in 1:nrow(lift)) {
    for(sector_B in 1:nrow(lift)) {
      condicional <- sum(lift[,sector_A] == 1 & lift[,sector_B] == 1 ) / sum(lift[,sector_B])
      if(all(is.na(c(probabilidades[sector_A, sector_B], probabilidades[sector_B, sector_A])))){
        probabilidades[sector_A, sector_B] <- condicional
      } else if(!is.na(probabilidades[sector_A, sector_B])){
        minimo <- min(condicional, probabilidades[sector_A, sector_B])
        probabilidades[sector_A, sector_B] <- probabilidades[sector_B, sector_A] <- minimo
      } else {
        minimo <- min(condicional, probabilidades[sector_B, sector_A])
        probabilidades[sector_A, sector_B] <- probabilidades[sector_B, sector_A] <- minimo
      }
    }
  }
  diag(probabilidades) <- 0
  if(returnLiftMatrix) {
    return(list(probabilidades = probabilidades,lift = liftMatrix))
  } else {
    probabilidades
  }
}

tidyRegOutput <- function(reg,coef.int) {
  library(plyr)
  if (class(reg) == 'lm.cluster') {
    term <- names(reg$lm_res$coefficients)
    estimate <- coef(reg)
    resumen <- summary(reg)
    std.error <- resumen[,2]
    statistic <- resumen[,3]
    p.value <- resumen[,4]
    lb<-confint(reg, level = coef.int)[,1]
    ub<-confint(reg, level = coef.int)[,2]
    roundEstimate <- round(estimate, 2)
    significancePvalue <- ifelse(p.value<0.01,paste0(roundEstimate,'***'),
                                 ifelse(p.value<0.05, paste0(roundEstimate,'**'),
                                        ifelse(p.value<0.1,paste0(roundEstimate,'*'),roundEstimate)))
    salida <- tibble(term,estimate,std.error,statistic,p.value, lb, ub,
                     coefPvalue=significancePvalue)
    return(salida)
  } else if(class(reg) == 'lm') {
    term <- rownames(summary(reg)$coefficients)
    estimate <- coef(reg)
    resumen <- summary(reg)$coefficients
    std.error <- resumen[,2]
    statistic <- resumen[,3]
    p.value <- resumen[,4]
    lb <- confint(reg, level = coef.int)[,1]
    ub <- confint(reg, level = coef.int)[,2]
    roundEstimate <- round(estimate, 2)
    significancePvalue <- ifelse(p.value<0.01,paste0(roundEstimate,'***'),
                                 ifelse(p.value<0.05, paste0(roundEstimate,'**'),
                                        ifelse(p.value<0.1,paste0(roundEstimate,'*'),roundEstimate)))
    salida <- data.table(term,estimate,std.error,statistic,p.value, lb, ub,
                         coefPvalue=significancePvalue)
    
    salida <- rbindlist(list(salida,
                        data.table(term='R2',estimate=summary(reg)$r.squared),
                        data.table(term='n',estimate=summary(reg)$df[2]+summary(reg)$df[1])),
                        fill=TRUE)
    return(salida)
  } else if(class(reg) == 'sarlm'){
    term <- rownames(summary(reg)$Coef)
    estimate <- coef(reg)[-1]
    resumen <- summary(reg)$Coef
    std.error <- resumen[,2]
    statistic <- resumen[,3]
    p.value <- resumen[,4]
    lb<-confint(reg, level = coef.int)[-1,1]
    ub<-confint(reg, level = coef.int)[-1,2]
    roundEstimate <- round(estimate, 2)
    significancePvalue <- ifelse(p.value<0.01,paste0(roundEstimate,'***'),
                                 ifelse(p.value<0.05, paste0(roundEstimate,'**'),
                                        ifelse(p.value<0.1,paste0(roundEstimate,'*'),roundEstimate)))
    salida <- tibble(term,estimate,std.error,statistic,p.value, lb, ub,
                     coefPvalue=significancePvalue)
    return(salida)
  } else if(any(class(reg) %in% c('rma'))) {
    term <- names(coef(reg))[-1]
    estimate <- coef(reg)[-1]
    std.error <- reg$se[-1]
    statistic <- reg$zval[-1]
    p.value <- reg$pval[-1]
    lb <- reg$ci.lb[-1]
    ub <- reg$ci.ub[-1]
    salida <- tibble(term,estimate,std.error,statistic,p.value, lb, ub)
  } else if(any(class(reg) %in% c('permutest.rma.uni'))) {
    term <- rownames(coef(reg))[-1]
    estimate <- coef(reg)[-1,1]
    std.error <- coef(reg)[-1,2]
    statistic <- coef(reg)[-1,3]
    p.value <- coef(reg)[-1,4]
    lb <- coef(reg)[-1,5]
    ub <- coef(reg)[-1,6]
    roundEstimate <- round(estimate, 2)
    significancePvalue <- ifelse(p.value<0.01,paste0(roundEstimate,'***'),
                                 ifelse(p.value<0.05, paste0(roundEstimate,'**'),
                                        ifelse(p.value<0.1,paste0(roundEstimate,'*'),roundEstimate)))
    salida <- tibble(term,estimate,std.error,statistic,p.value, lb, ub,
                     coefPvalue=significancePvalue)
  }
}

compareModels <- function(models, names, intervals) {
  tidyModels <- list()
  for (i in 1:length(models)) {
    tidyModels <- c(tidyModels,
                    list(tidyRegOutput(models[[i]],intervals[[i]]) %>%
                           select(term,coefPvalue) %>%
                           `colnames<-`(c("Estimate",names[[i]]))))
  }
  tidyModels<-join_all(tidyModels, by='Estimate', type='left')
}

getDistanceMeasure <- function(sectorSalida,sectorLlegada,flow,method='neffke2017'){
  if(method=='neffke2017') {
    dt <- data.table(sectorSalida, sectorLlegada, flow)
    dt <- dcast(dt,sectorSalida ~ sectorLlegada,value.var = 'flow')
    dt <- as.matrix(dt)
    rowNames <- dt[,1]
    dt <- dt[,-1]
    dt <- apply(dt,2,as.numeric)
    rownames(dt) <- rowNames
    dt <- fill_matrix(dt,replaceValue = 0) 
    dt[is.na(dt)] <- 0
    dt[upper.tri(dt,diag = FALSE)] <- dt[upper.tri(dt,diag = FALSE)] + t(dt)[upper.tri(t(dt),diag = FALSE)]
    dt <- ( dt*sum(dt) ) / ( outer(rowSums(dt),rowSums(dt), FUN = '*') )
    sectores <- rownames(dt)
    dt <- as.data.table((dt - 1)/(dt + 1))
    dt$sectorSalida <- sectores
    dt <- melt(dt,id.vars='sectorSalida')
    colnames(dt) <- c('sectorSalida','sectorLlegada','RijCorregido')
    return(dt)
  } else {
  dt <- data.table(sectorSalida, sectorLlegada, flow)
  totalLlegadas <- dt[,list(totalLlegada=sum(flow)),by='sectorLlegada']
  totalSalidas <- dt[,list(totalSalida=sum(flow)),by='sectorSalida']
  dt <- dcast(dt,sectorSalida ~ sectorLlegada,value.var = 'flow')
  dt[,2:ncol(dt)] <-(dt[,-1]*sum(dt[,-1]))/ outer(totalLlegadas$totalLlegada, totalSalidas$totalSalida, FUN = "*")
  dt[,2:ncol(dt)] <- as.data.table(as.matrix(dt[,2:ncol(dt)] - 1) / as.matrix(dt[,2:ncol(dt)] + 1))
  dt <- melt(dt,id.vars = 'sectorSalida',variable.name = 'sectorLlegada',value.name = 'RijCorregido')
  return(dt)
  }
}

getDistanceMeasureProduccion <- function(sectorSalida,sectorLlegada,flow,method='neffke2017'){
  if(method=='neffke2017') {
    dt <- data.table(sectorSalida, sectorLlegada, flow)
    dt <- dt[!(sectorSalida %in% 'NA' | sectorLlegada %in% 'NA')]
    dt <- dcast(dt,sectorSalida ~ sectorLlegada,value.var = 'flow')
    dt <- as.matrix(dt)
    rowNames <- dt[,1]
    dt <- dt[,-1]
    dt <- apply(dt,2,as.numeric)
    rownames(dt) <- rowNames
    dt <- fill_matrix(dt,replaceValue = 0) 
    dt[is.na(dt)] <- 0
    dt[upper.tri(dt,diag = FALSE)] <- dt[upper.tri(dt,diag = FALSE)] + t(dt)[upper.tri(t(dt),diag = FALSE)]
    dt <- ( dt*sum(dt) ) / ( outer(rowSums(dt),rowSums(dt), FUN = '*') )
    dt[lower.tri(dt,diag = FALSE)] <- t(dt)[lower.tri(t(dt),diag = FALSE)]
    sectores <- rownames(dt)
    dt <- as.data.table((dt - 1)/(dt + 1))
    dt$sectorSalida <- sectores
    dt <- melt(dt,id.vars='sectorSalida')
    colnames(dt) <- c('sectorSalida','sectorLlegada','RijCorregido')
    return(dt)
  } else {
    dt <- data.table(sectorSalida, sectorLlegada, flow)
    totalLlegadas <- dt[,list(totalLlegada=sum(flow)),by='sectorLlegada']
    totalSalidas <- dt[,list(totalSalida=sum(flow)),by='sectorSalida']
    dt <- dcast(dt,sectorSalida ~ sectorLlegada,value.var = 'flow')
    dt[,2:ncol(dt)] <-(dt[,-1]*sum(dt[,-1]))/ outer(totalLlegadas$totalLlegada, totalSalidas$totalSalida, FUN = "*")
    dt[,2:ncol(dt)] <- as.data.table(as.matrix(dt[,2:ncol(dt)] - 1) / as.matrix(dt[,2:ncol(dt)] + 1))
    dt <- melt(dt,id.vars = 'sectorSalida',variable.name = 'sectorLlegada',value.name = 'RijCorregido')
    return(dt)
  }
}
