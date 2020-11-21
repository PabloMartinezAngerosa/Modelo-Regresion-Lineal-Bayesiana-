source("modelLM.R")


#TODO ajustar el mismo modelo en ML Bayessiano DONE!
#TODO model checking ?

library(rstanarm)
library(bayesplot)
library(ggplot2)
library(dplyr)

#TODO probar prioris basada en ML
modelo1 <- stan_glm(MORT ~ PREC + JANT + 
                    JULT + EDUC + NONW + SO, 
                    data = datosML,
                    family = gaussian(link = "identity"),
                    prior_intercept = normal(0,10),
                    prior = normal(0,10),
                    prior_aux = exponential(1),
                    seed = 12345)

summary(modelo1)
prior_summary(modelo1)

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
mcmc_intervals(modelo1 ,pars=c("PREC",
                               "JANT", 
                               "JULT",
                               "EDUC",
                               "NONW",
                               "SO"), prob = 0.95)


mcmc_hist(modelo1)#Histograma de los coeficientes 
mcmc_hist_by_chain(modelo1)#Histograma de los coeficientes por cadena
mcmc_dens(modelo1)#Estimacion de densidad de los coeficientes

# Diagnostico de Modelo
##Predictivas posteriores
pred<-posterior_predict(modelo1,draws = 100)

pp_check(modelo1, plotfun = "stat", stat = "mean")##Usando un estadistico para comparaci+on
pp_check(modelo1, plotfun = "stat", stat = "median")

ppc_dens_overlay(datosML$MORT,pred)#Sobreposicion de las yrep con los datos originales
