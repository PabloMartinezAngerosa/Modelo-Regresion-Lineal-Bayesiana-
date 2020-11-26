source("modelLM.R")

# prior_aux = rstanarm::normal(26.19, 1.5)
# prior_aux = rstanarm::exponential(2)

# Modelo con previas default

modelo1 <- rstanarm::stan_glm(MORT ~ PREC + JANT + 
                    JULT + EDUC + NONW + SO, 
                    data = datosML,
                    family = gaussian(link = "identity"),
                    seed = 12345)

base::summary(modelo1)
rstanarm::prior_summary(modelo1)

# Estimates:
#   mean   sd     10%    50%    90% 
# (Intercept) 1123.3  101.7  994.1 1125.8 1248.4
# PREC           2.2    0.5    1.6    2.2    2.9
# JANT          -1.0    0.5   -1.7   -1.0   -0.4
# JULT          -2.1    1.0   -3.4   -2.1   -0.9
# EDUC         -12.5    5.5  -19.3  -12.5   -5.7
# NONW           3.9    0.6    3.1    3.9    4.7
# SO             0.3    0.1    0.2    0.3    0.4
# sigma         26.6    2.7   23.4   26.4   30.2

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

q75 <- function(y) stats::quantile(y, 0.75)
bayesplot::pp_check(modelo1, plotfun = "stat", stat = "q75")

bayesplot::ppc_dens_overlay(datosML$MORT,pred)

# Modelo Inferencia bayesiana empirica

modelo2 <- rstanarm::stan_glm(MORT ~ PREC + JANT + 
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

base::summary(modelo2)
rstanarm::prior_summary(modelo2)

# Estimates:
#   mean   sd     10%    50%    90% 
# (Intercept) 1121.4   56.2 1051.6 1120.7 1195.0
# PREC           2.2    0.3    1.9    2.2    2.6
# JANT          -1.0    0.3   -1.4   -1.0   -0.7
# JULT          -2.1    0.6   -2.8   -2.1   -1.4
# EDUC         -12.3    3.1  -16.2  -12.4   -8.4
# NONW           3.9    0.3    3.5    3.9    4.3
# SO             0.3    0.0    0.3    0.3    0.4
# sigma         19.5    1.4   17.8   19.4   21.2


#prior_aux: normal
# Estimates:
#   mean   sd     10%    50%    90% 
# (Intercept) 1123.2   67.4 1036.6 1122.2 1210.6
# PREC           2.2    0.4    1.8    2.2    2.7
# JANT          -1.0    0.3   -1.5   -1.0   -0.6
# JULT          -2.1    0.7   -3.0   -2.1   -1.2
# EDUC         -12.4    3.7  -17.1  -12.4   -7.7
# NONW           3.9    0.4    3.4    3.9    4.5
# SO             0.3    0.0    0.2    0.3    0.4
# sigma         27.2    0.8   26.3   27.0   28.3


modelo2
modelo2$coefficients
modelo2$stanfit


###Diagnostico de las Cadenas
bayesplot::mcmc_trace(modelo2)
bayesplot::mcmc_trace(modelo2,pars = "(Intercept)")
bayesplot::mcmc_trace(modelo2,pars = "PREC")
bayesplot::mcmc_trace(modelo2,pars = "JANT")
bayesplot::mcmc_trace(modelo2,pars = "JULT")
bayesplot::mcmc_trace(modelo2,pars = "EDUC")
bayesplot::mcmc_trace(modelo2,pars = "NONW")
bayesplot::mcmc_trace(modelo2,pars = "SO")
bayesplot::mcmc_trace(modelo2,pars = "sigma")

##Intervalos de credibilidad del 95%
bayesplot::mcmc_intervals(modelo2, prob = 0.95)
bayesplot::mcmc_intervals(modelo2 ,pars=c("PREC",
                                          "JANT", 
                                          "JULT",
                                          "EDUC",
                                          "NONW",
                                          "SO"), prob = 0.95)

bayesplot::mcmc_intervals(modelo2 ,pars=c("SO"), prob = 0.95)


bayesplot::mcmc_hist(modelo2)#Histograma de los coeficientes 
bayesplot::mcmc_hist_by_chain(modelo2)#Histograma de los coeficientes por cadena
bayesplot::mcmc_dens(modelo2)#Estimacion de densidad de los coeficientes

# Diagnostico de Modelo
##Predictivas posteriores
pred2<-rstanarm::posterior_predict(modelo2,draws = 100)
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "mean")
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "median")
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "var")
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "IQR")
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "min")
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "max")
bayesplot::pp_check(modelo2, plotfun = "stat", stat = "q75")

bayesplot::ppc_dens_overlay(datosML$MORT,pred2)


