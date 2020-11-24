source("modelLM.R")

# prior_aux = rstanarm::normal(26.19, 1.5)
prior_aux = rstanarm::exponential(2)

modelo1 <- rstanarm::stan_glm(MORT ~ PREC + JANT + 
                    JULT + EDUC + NONW + SO, 
                    data = datosML,
                    family = gaussian(link = "identity"),
                    prior_intercept = rstanarm::normal(1122.79104, 101.12442),
                    prior = rstanarm::normal(base::c(2.23936, -1.03784, -2.10246, 
                                                     -12.41246, 3.89109, 0.30278), 
                                             base::c(0.52209, 0.48114, 0.98114, 
                                                     5.36332, 0.63640, 0.06265)),
                    prior_aux = rstanarm::exponential(2),
                    seed = 12345)

base::summary(modelo1)
rstanarm::prior_summary(modelo1)

modelo1
modelo1$coefficients
modelo1$stanfit


###Diagnostico de las Cadenas
bayesplot::mcmc_trace(modelo1)
bayesplot::mcmc_trace(modelo1,pars = "(Intercept)")
bayesplot::mcmc_trace(modelo1,pars = "PREC")
bayesplot::mcmc_trace(modelo1,pars = "JANT")
bayesplot::mcmc_trace(modelo1,pars = "JULT")
bayesplot::mcmc_trace(modelo1,pars = "EDUC")
bayesplot::mcmc_trace(modelo1,pars = "NONW")
bayesplot::mcmc_trace(modelo1,pars = "SO")
bayesplot::mcmc_trace(modelo1,pars = "sigma")

##Intervalos de credibilidad del 95%
bayesplot::mcmc_intervals(modelo1, prob = 0.95)
bayesplot::mcmc_intervals(modelo1 ,pars=c("PREC",
                               "JANT", 
                               "JULT",
                               "EDUC",
                               "NONW",
                               "SO"), prob = 0.95)

bayesplot::mcmc_intervals(modelo1 ,pars=c("SO"), prob = 0.95)


bayesplot::mcmc_hist(modelo1)#Histograma de los coeficientes 
bayesplot::mcmc_hist_by_chain(modelo1)#Histograma de los coeficientes por cadena
bayesplot::mcmc_dens(modelo1)#Estimacion de densidad de los coeficientes

# Diagnostico de Modelo
##Predictivas posteriores
pred<-rstanarm::posterior_predict(modelo1,draws = 100)
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "mean")
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "median")
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "var")
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "IQR")
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "min")
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "max")

quant1 <- quantile(datosML$MORT, probs = seq(0,1, .01))
q98 <- function(y) quantile(y, 0.98)

bayesplot::pp_check(modelo1, plotfun = "stat", stat = "q98")


bayesplot::ppc_dens_overlay(datosML$MORT,pred)



