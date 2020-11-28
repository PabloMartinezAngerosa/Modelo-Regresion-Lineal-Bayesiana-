source("ModeloLM.R")

# Modelo con previas default
modeloBayesiano <- rstanarm::stan_glm(MORT ~ PREC + JANT + 
                                      JULT + EDUC + NONW + SO, 
                                      data = datosML,
                                      family = gaussian(link = "identity"),
                                      seed = 12345)

# Modelo Inferencia bayesiana empirica
modeloBayesianoEmpirico <- rstanarm::stan_glm(MORT ~ PREC + JANT + 
                                              JULT + EDUC + NONW + SO, 
                                              data = datosML,
                                              family = gaussian(link = "identity"),
                                              prior_intercept = rstanarm::normal(1122.79104, 101.12442),
                                              prior = rstanarm::normal(base::c(2.23936, -1.03784, -2.10246,
                                                                               -12.41246, 3.89109, 0.30278),
                                                                       base::c(0.52209, 0.48114, 0.98114,
                                                                               5.36332, 0.63640, 0.06265)),
                                              prior_aux = rstanarm::normal(26.19,0.5),
                                              seed = 12345)
PlotDensidadBayes <-  function() {
  bayesplot::bayesplot_grid(bayesplot::mcmc_dens(modeloBayesiano), 
                            bayesplot::mcmc_dens(modeloBayesianoEmpirico))
}

PlotPosterioriPredictivas <- function() {
  
  pred<-rstanarm::posterior_predict(modeloBayesiano,draws = 100)
  predEmpirico<-rstanarm::posterior_predict(modeloBayesianoEmpirico,draws = 100)
  
  bayesplot::bayesplot_grid(bayesplot::ppc_dens_overlay(datosML$MORT,pred), 
                            bayesplot::ppc_dens_overlay(datosML$MORT,predEmpirico))
}

PlotEstadisticos <- function() {
  
  bayesplot::bayesplot_grid(bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "median"),
                            bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "median"),   
                            bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "IQR"),
                            bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "IQR"),  
                            bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "max"),
                            bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "max"))
}

