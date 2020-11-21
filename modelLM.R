
datos <- utils::read.table('polucion.txt', sep='\t',
                           header=TRUE,
                           row.names=NULL, stringsAsFactors=FALSE,
                           dec = ",")


# Analisis descriptivo de los datos
base::which(base::is.na(datos))
utils::head(datos)
base::names(datos)
utils::str(datos)
base::dim(datos)

base::summary(datos[,1:16])

# Matriz de correlaciones
base::round(stats::cor(datos[,1:16],use='pairwise.complete.obs'), 2) 

# HC y NOX tienen correlacion de 0.98 se saca de la base NOX
noxCol = base::which(base::names(datos) == "NOX")
datos = datos[,-noxCol]

fittedGenearlLinearModel = stats::lm(MORT~. , data = datos)
base::summary(fittedGenearlLinearModel) 

# Analisis de Multicolinealidad, no hay valores >= 10 
# -> no hay multicolinealidad
car::vif(fittedGenearlLinearModel)

# Obtenemos modelo ajustados por step wise AIC
stepWiseFitted = stats::step(fittedGenearlLinearModel, 
                             direction= "both", 
                             trace=0)

# No se rechaza la normalidad
nortest::lillie.test(stepWiseFitted$residuals) 
stats::shapiro.test(stepWiseFitted$residuals) 

# No se rechaza la homogenidad de varianza
car::ncvTest(stepWiseFitted)
lmtest::bptest(stepWiseFitted)

# Observaciones influyentes 6, 28, 32, 37  
base::which(base::abs(stats::dffits(stepWiseFitted))>=2*base::sqrt(8/60))

# Probamos sacar las influyentes y ver el R^2
stepWiseFittedInf = stats::lm(formula = MORT ~ PREC + 
                              JANT + JULT + POPN + 
                              EDUC + NONW + SO, 
                              data = datos[-c(6, 28, 32, 37),])

# Analisis de Multicolinealidad, no hay valores >= 10 
# -> no hay multicolinealidad
car::vif(stepWiseFittedInf)

# No se rechaza la normalidad del modelo sin influyentes
nortest::lillie.test(stepWiseFittedInf$residuals) 
stats::shapiro.test(stepWiseFittedInf$residuals)

# No se rechaza la homogenidad de varianza sin influyentes
car::ncvTest(stepWiseFittedInf)
lmtest::bptest(stepWiseFittedInf)

# Observacion 2 es atipica
car::outlierTest(stepWiseFittedInf)

# Armamos el modelo nuevamente sin el atipico 
fittedModel <- stats::lm(formula = MORT ~ PREC + JANT + 
                         JULT + POPN + EDUC + NONW + SO, 
                         datos[-c(6, 28, 32, 37, 2),])

# volvemos a correr el stepwise
stepWiseFitted = stats::step(fittedModel, direction= "both", trace=0)

# Analisis de Multicolinealidad, no hay valores >= 10 
# -> no hay multicolinealidad
car::vif(stepWiseFitted)

# No se rechaza la normalidad del modelo
nortest::lillie.test(stepWiseFitted$residuals) 
stats::shapiro.test(stepWiseFitted$residuals)

# No se rechaza la homogenidad de varianza sin influyentes
car::ncvTest(stepWiseFitted)
lmtest::bptest(stepWiseFitted)

# Seleccionamos el modelo
modelML = stepWiseFitted
datosML = datos[-c(6, 28, 32, 37, 2),]

base::summary(modelML)




