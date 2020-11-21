source("modelLM.R")


#TODO ajustar el mismo modelo en ML Bayessiano
#TODO model checking

library(rstanarm)
library(bayesplot)
library(ggplot2)
library(dplyr)


modelo1 <- stan_glm(MORT ~ ., data = datosML,
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


###Cadenas
mcmc_trace(modelo1)
mcmc_trace(modelo1,pars = "(Intercept)")
mcmc_trace(modelo1,pars = "PREC")
mcmc_trace(modelo1,pars = "JANT")
mcmc_trace(modelo1,pars = "JULT")
mcmc_trace(modelo1,pars = "EDUC")
mcmc_trace(modelo1,pars = "NONW")
mcmc_trace(modelo1,pars = "SO")
mcmc_trace(modelo1,pars = "sigma")

mcmc_intervals(modelo1 , prob = 0.95)##Intervalos de credibilidad del 95%

mcmc_hist(modelo1)#Histograma de los coeficientes 
mcmc_hist_by_chain(modelo1)#Histograma de los coeficientes por cadena
mcmc_dens(modelo1)#Estimacion de densidad de los coeficientes


##Predictivas posteriores
pred<-posterior_predict(modelo1,draws = 100)

pp_check(modelo1, plotfun = "stat", stat = "mean")##Usando un estadistico para comparaci+on
pp_check(modelo1, plotfun = "stat", stat = "median")

ppc_dens_overlay(datosML$MORT,pred)#Sobreposicion de las yrep con los datos originales
