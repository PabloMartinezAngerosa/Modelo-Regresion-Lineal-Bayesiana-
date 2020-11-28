source("ModelosBayesianos.R")

# Analizamos la performance del Modelo Bayesiano y el Modelo Bayesiano Empirico
base::summary(modeloBayesiano)
rstanarm::prior_summary(modeloBayesiano)

base::summary(modeloBayesianoEmpirico)
rstanarm::prior_summary(modeloBayesianoEmpirico)


modeloBayesiano
modeloBayesiano$coefficients
# Intervalos de credibilidad para los parametros
modeloBayesiano$stanfit

modeloBayesianoEmpirico
modeloBayesianoEmpirico$coefficients
# Intervalos de credibilidad para los parametros
modeloBayesianoEmpirico$stanfit

# Cálculo RMSE Bayesiano
predictBayesiano <- predict(modeloBayesiano, datosML, interval="predict")
RMSEBayesiano = sqrt(mean((datosML$MORT - predictBayesiano)^2))

# Cálculo RMSE Bayesiano Empirico
predictBayesianoEmpirico <- predict(modeloBayesianoEmpirico, datosML, interval="predict")
RMSEBayesianoEmpirico = sqrt(mean((datosML$MORT - predictBayesianoEmpirico)^2))


# Diagnostico de las Cadenas Modelo Bayesiano
# Parece darse la convergencia de las cadenas
bayesplot::mcmc_trace(modeloBayesiano)
bayesplot::mcmc_trace(modeloBayesiano,pars = "(Intercept)")
bayesplot::mcmc_trace(modeloBayesiano,pars = "PREC")
bayesplot::mcmc_trace(modeloBayesiano,pars = "JANT")
bayesplot::mcmc_trace(modeloBayesiano,pars = "JULT")
bayesplot::mcmc_trace(modeloBayesiano,pars = "EDUC")
bayesplot::mcmc_trace(modeloBayesiano,pars = "NONW")
bayesplot::mcmc_trace(modeloBayesiano,pars = "SO")
bayesplot::mcmc_trace(modeloBayesiano,pars = "sigma")

# Diagnostico de las Cadenas Modelo Bayesiano Empírico
# Parece darse la convergencia de las cadenas
bayesplot::mcmc_trace(modeloBayesianoEmpirico)
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "(Intercept)")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "PREC")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "JANT")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "JULT")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "EDUC")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "NONW")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "SO")
bayesplot::mcmc_trace(modeloBayesianoEmpirico,pars = "sigma")

# Intervalos de credibilidad del 95% Modelo Bayesiano
bayesplot::mcmc_intervals(modeloBayesiano, prob = 0.95)
bayesplot::mcmc_intervals(modeloBayesiano ,pars=c("PREC",
                               "JANT",
                               "JULT",
                               "EDUC",
                               "NONW",
                               "SO"), prob = 0.95)
bayesplot::mcmc_intervals(modeloBayesiano ,pars=c("SO"), prob = 0.95)

# Intervalos de credibilidad del 95% Modelo Bayesiano Empírico
bayesplot::mcmc_intervals(modeloBayesianoEmpirico, prob = 0.95)
bayesplot::mcmc_intervals(modeloBayesianoEmpirico ,pars=c("PREC",
                                                  "JANT",
                                                  "JULT",
                                                  "EDUC",
                                                  "NONW",
                                                  "SO"), prob = 0.95)
bayesplot::mcmc_intervals(modeloBayesianoEmpirico ,pars=c("SO"), prob = 0.95)

# Histograma de los coeficientes Modelo Bayesiano
bayesplot::mcmc_hist(modeloBayesiano)
# Histograma de los coeficientes por cadena Modelo Bayesiano
bayesplot::mcmc_hist_by_chain(modeloBayesiano)
# Estimacion de densidad de los coeficientes Modelo Bayesiano
bayesplot::mcmc_dens(modeloBayesiano)

# Histograma de los coeficientes Modelo Bayesiano Empírico
bayesplot::mcmc_hist(modeloBayesianoEmpirico)
# Histograma de los coeficientes por cadena Modelo Bayesiano Empírico
bayesplot::mcmc_hist_by_chain(modeloBayesianoEmpirico)
# Estimacion de densidad de los coeficientes Modelo Bayesiano Empírico
bayesplot::mcmc_dens(modeloBayesianoEmpirico)

# Diagnostico de Modelo Bayesiano
# Predictivas posteriores
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "mean")
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "median")
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "var")
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "IQR")
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "min")
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "max")

q75 <- function(y) stats::quantile(y, 0.75)
bayesplot::pp_check(modeloBayesiano, plotfun = "stat", stat = "q75")

pred<-rstanarm::posterior_predict(modeloBayesiano,draws = 100)
bayesplot::ppc_dens_overlay(datosML$MORT,pred)

# Diagnostico de Modelo Bayesiano Empírico
# Predictivas posteriores
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "mean")
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "median")
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "var")
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "IQR")
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "min")
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "max")
bayesplot::pp_check(modeloBayesianoEmpirico, plotfun = "stat", stat = "q75")

predEmpirico<-rstanarm::posterior_predict(modeloBayesianoEmpirico,draws = 100)
bayesplot::ppc_dens_overlay(datosML$MORT,predEmpirico)

