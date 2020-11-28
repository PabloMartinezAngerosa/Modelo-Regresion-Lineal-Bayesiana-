source("ModelosBayesianos.R")

# Tabla que muestra descripcion de variables
MostrarTablaDescriptiva = function() {
  
  descrip <- utils::read.table('descripcion_var.txt', sep='\t',
                                header=TRUE,
                                row.names=NULL, stringsAsFactors=FALSE,
                                dec = ",",
                                encoding = "UTF-8",
                                check.names=FALSE)

                         
  base::print(xtable::xtable(descrip,
                            caption = "Detalle de las variables en la base de datos.",
                            type="latex",),
              caption.placement = "bottom")
}

# Summary con la media, minimo y maximo de las variables de la base
MostrarTablaResumen  <-  function() {
  
  resumen <- utils::read.table('summary.txt', sep='\t',
                               header=TRUE,
                               row.names=NULL, stringsAsFactors=FALSE,
                               dec = ",",
                               encoding = "UTF-8",
                               check.names=FALSE)
  
  base::print(xtable::xtable(resumen,
                             caption = "Resumen de todas las variables.",
                             type="latex"),
              caption.placement = "bottom")
}


# Muestra Figura correlación entre HC y NOX
MostrarGraficoCorrelacion  <-  function() {
  
  datos <- utils::read.table('polucion.txt', sep='\t',
                           header=TRUE,
                           row.names=NULL, stringsAsFactors=FALSE,
                           dec = ",")

  disp <- ggplot2::ggplot(datos, ggplot2::aes(x = HC, y = NOX)) + 
        ggplot2::geom_point() +
        ggplot2::labs(x="HC", y="NOX", 
                      title = "")
return(disp)
}

# Muestra tabla con parametros de la distribucion previa del modelo bayesiano empirico
MostrarTablaPreviaBayesianoEmpirico  <-  function() {

  prev <- utils::read.table('previa.txt', sep='\t',
                            header=TRUE,
                            row.names=NULL, stringsAsFactors=FALSE,
                            dec = ",",
                            encoding = "UTF-8",
                            check.names=FALSE)
  
  base::print(xtable::xtable(prev,
                             caption = "Parametros de las distribuciones 
                             normales previas para el modelo bayesiano empirico.",
                             type="latex"),
              caption.placement = "bottom")
}

#Muestra tabla de Rhat y n_eff del modelo bayesiano
MostrarTablaConvergenciaBayesiano <-  function() {

  convergencia_bayesiano <- utils::read.table('convergencia_bayesiano.txt', sep='\t',
                            header=TRUE,
                            row.names=NULL, stringsAsFactors=FALSE,
                            dec = ",",
                            encoding = "UTF-8",
                            check.names=FALSE)

  base::print(xtable::xtable(convergencia_bayesiano,
                             caption = "Indicadores de convergencia para los
                             modelos bayesianos.",
                             type="latex"),
              caption.placement = "bottom")

}

#Muestra tabla intervalos de credibilidad de los modelos bayesianos
MostrarTablaIntervalosCredibilidadBayesianos  <-  function() {
  
  intervalos_credibilidad_bayesiano <- utils::read.table('intervalos_credibilidad_bayesiano.txt', sep=',',
                                              header=TRUE,
                                              row.names=NULL, stringsAsFactors=FALSE,
                                              dec = ",",
                                              encoding = "UTF-8",
                                              check.names=FALSE)
  
  base::print(xtable::xtable(intervalos_credibilidad_bayesiano,
                             caption = "Intervalos de credibilidad para los 
                             parametros de los modelos bayesianos.",
                             type="latex"),
              caption.placement = "bottom")
  
}

# Muestra tabla RMSE de todos los modelos
MostrarTablaRMSE  <-  function() {

  RMSEModelos <- utils::read.table('RMSE.txt', sep=',',
                                    header=TRUE,
                                    row.names=NULL, stringsAsFactors=FALSE,
                                    dec = ",",
                                    encoding = "UTF-8",
                                    check.names=FALSE)

  base::print(xtable::xtable(RMSEModelos,
                             caption = "RMSE para todos los modelos.",
                             type="latex"),
              caption.placement = "bottom")
  
}

# Muestra tabla media, sd de los parametros posteriori de todos los modelos y 
# betas estimados para ML
MostrarTablaResumenPosteriori <-  function() {
  
  ResumenPosteriori <- utils::read.table('summary_posteriori.txt', sep=',',
                                   header=TRUE,
                                   row.names=NULL, stringsAsFactors=FALSE,
                                   dec = ",",
                                   encoding = "UTF-8",
                                   check.names=FALSE)
  
  base::print(xtable::xtable(ResumenPosteriori,
                             caption = "Media y desvio de los parametros a 
                             posteriori de los modelos bayesianos y estimaciones 
                             para modelos lineales.",
                             type="latex"),
              caption.placement = "bottom")
  
}

# Muestra grafico de densidades posterioris
MostrarGraficosDensidadesBayesianas <- function() {
  
  PlotDensidadBayes()
  
}

# Muestra grafico posteriori predictiva bayes y bayes empirico
MostrarGraficosPosterioriPredictiva <- function() {
  
  PlotPosterioriPredictivas()
}

# Muestra grafico estadisticos mediana, IQR y maximo para bayes y bayes empirico
MostrarGraficosEstadisticos <- function() {
  
  PlotEstadisticos()
}


