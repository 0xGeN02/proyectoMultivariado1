##Caso1

Velocidad <- Caso_1$Velocidad
DistanciaFrenado <- Caso_1$DistanciaFrenado
VelocidadCuadrado <- Caso_1$VelocidadCuadrado

# Primero, creamos un dataframe con los datos
datos <- data.frame(
  Velocidad = Velocidad,
  DistanciaFrenado = DistanciaFrenado,
  VelocidadCuadrado = VelocidadCuadrado
)

library(ggplot2)

# Creamos el gráfico de dispersión para Velocidad y Distancia de Frenado
ggplot(datos) +
  geom_point(aes(x=Velocidad,y=DistanciaFrenado)) +
  labs(x="Velocidad", y="Distancia de Frenado")

# Creamos el gráfico de dispersión para Velocidad al Cuadrado y Distancia de Frenado
ggplot(datos) +
  geom_point(aes(x=VelocidadCuadrado,y=DistanciaFrenado)) +
  labs(x="Velocidad al Cuadrado", y="Distancia de Frenado")

# Calculamos la correlación de Pearson para ambas relaciones
correlacion1 <- cor(datos$Velocidad , datos$DistanciaFrenado)
print(paste("La correlación de Pearson entre Velocidad y Distancia de Frenado es:", correlacion1))

correlacion2 <- cor(datos$VelocidadCuadrado , datos$DistanciaFrenado)
print(paste("La correlación de Pearson entre Velocidad al Cuadrado y Distancia de Frenado es:", correlacion2))

# Grafica correlacion
round(cor(datos,method = "pearson"), 3)
corrplot::corrplot(cor(datos, method = "pearson"), type = "lower")

# Estimación del modelo 1

modelo1 <- lm(DistanciaFrenado ~ Velocidad, data = Caso_1)
summary(modelo1)

# Diagnosis del modelo 1

resid1 <- resid(modelo1)
zresid1 <- rstandard(modelo1)
DIstancia_Estimada1 <- fitted(modelo1)
ggplot(data = Caso_1, aes(DIstancia_Estimada1, zresid1)) + geom_point() + geom_hline(yintercept = 0) + theme_bw()
ggplot(data=Caso_1, mapping=aes(x=Velocidad, y=DistanciaFrenado, label=Coche)) +geom_point() + geom_text()
lmtest::bptest(modelo1)
car::dwt(modelo1, alternative = "two.sided")
qqnorm(resid1)
qqline(resid1)
shapiro.test(resid1)

# Estimación del modelo 2

modelo2 <- lm(DistanciaFrenado ~ VelocidadCuadrado, data = Caso_1)
summary(modelo2)

# Diagnosis del modelo 2

resid2 <- resid(modelo2)
zresid2 <- rstandard(modelo2)
DIstancia_Estimada2 <- fitted(modelo2)
ggplot(data = Caso_1, aes(DIstancia_Estimada2, zresid2)) + geom_point() + geom_hline(yintercept = 0) + theme_bw()
ggplot(data=Caso_1, mapping=aes(x=VelocidadCuadrado, y=DistanciaFrenado, label=Coche)) +geom_point() + geom_text(nudge_y = 7)
lmtest::bptest(modelo2)
car::dwt(modelo2, alternative = "two.sided")
qqnorm(resid2)
qqline(resid2)
shapiro.test(resid2)

#modelo3

# Definimos 'a'
a <- -7.5

# Estimamos el modelo lineal
modelo3 <- lm(DistanciaFrenado ~ I(Velocidad^2 / (2*a)), data = datos)

# Diagnosis del modelo 3

resid3 <- resid(modelo3)
zresid3 <- rstandard(modelo3)
DIstancia_Estimada3 <- fitted(modelo3)
ggplot(data = Caso_1, aes(DIstancia_Estimada3, zresid3)) + geom_point() + geom_hline(yintercept = 0) + theme_bw()
ggplot(data=Caso_1, mapping=aes(x=VelocidadCuadrado, y=DistanciaFrenado, label=Coche)) +geom_point() + geom_text(nudge_y = 7)
lmtest::bptest(modelo3)
car::dwt(modelo3, alternative = "two.sided")
qqnorm(resid3)
qqline(resid3)
shapiro.test(resid3)