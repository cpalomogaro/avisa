#####################################################################################
# Modelos de supervivencia
#####################################################################################
library(survival)
library(survminer)
library(ranger)

## Hago un dataframe con los avisos resueltos y sin resolver y asigno valor de 1 
#a los resueltos y 0 a los que siguen abiertos

resueltos$COORDENADA_OFICIAL_X <- resueltos$COORDENADA_OFICIAL_Y <- resueltos$FECHA_DE_RECEPCION <- resueltos$FECHA_DE_RESOLUCION <- NULL
resueltos$weekdayResolucion <- resueltos$HORA_DE_RECEPCION <- resueltos$HORA_DE_RESOLUCION <- resueltos$direccion <- NULL
sinresolver$COORDENADA_OFICIAL_X <- sinresolver$COORDENADA_OFICIAL_Y <- sinresolver$FECHA_DE_RECEPCION <- sinresolver$direccion <- sinresolver$HORA_DE_RECEPCION <- NULL

resueltos$solucionado <- 1
sinresolver$solucionado <- 0 
colnames(sinresolver)[colnames(sinresolver) == 'tiempo_sin_resolver'] <- 'tiempo'
colnames(resueltos)[colnames(resueltos) == 'tiempo_de_resolucion'] <- 'tiempo'
total <- rbind(resueltos, sinresolver)
total <- merge(total, ID_seccion, by= 'SECCION_ID')
total$tiempo <- as.numeric(total$tiempo)
total <- total[!is.na(total$tiempo),]

#Por problemas de memoria, escojo una muestra aleatoria de 5000 entradas 
set.seed(1234)
tmp <- sample_n(total, 5000)

#Análisis de supervivencia por Kaplan Meier
kaplan <- Surv(tmp$tiempo, tmp$solucionado)
kaplan

fit_kaplan <- survfit(kaplan ~ 1)
summary(fit_kaplan)

ggsurvplot(fit_kaplan, main = 'Kaplan Meyer Plot') + labs(
  x = "Tiempo (días)",
  y    = "Probabilidad de supervivencia"
)

# cuando se tienen en cuenta mÃ¡s de dos variables, como la secciÃ³n a la que pertenecen los avisos, 
# no se obtienen grÃ¡ficos muy informativos:
fit02 <- survfit(Surv(tiempo, solucionado) ~ SECCION, data=tmp)
ggsurvplot(fit02, pval=TRUE, 
           main="Kaplan-Meier para tiempo de resolucion") + labs(
             x = "Tiempo (días)",
             y    = "Probabilidad de supervivencia"
           )

#Por lo que se aplica un modelo de riesgos proporcionales, o regresiÃ³n de Cox
cox <- coxph(Surv(tiempo, solucionado) ~ SECCION, data=tmp)
summary(cox)

cox_fit <- survfit(cox)

#Si miramos el efecto de todas las variables:
cox_total <- coxph(Surv(tiempo, solucionado) ~ SECCION + TIPO_INCIDENCIA_ID + CANAL_DE_ENTRADA_ID +
ANOMALIA_ID + DISTRITO_ID + BARRIO_ID + weekday, data=tmp)
cox_total
cox_total_fit <- survfit(cox_total)

#Pruebo a estratificar en un modelo de riesgos no proporcionales
# mod3 <- coxph(Surv(tiempo, solucionado) ~ CANAL_DE_ENTRADA_ID+DISTRITO_ID + strata(SECCION), data=tmp)
# mod3_fit <- survfit(mod3)
# plot(mod3_fit)
#sale igual que fit02
#observaciones no independientes mediante clustering

mod4 <- coxph(Surv(tiempo, solucionado) ~ TIPO_INCIDENCIA_ID + CANAL_DE_ENTRADA_ID +
                ANOMALIA_ID + DISTRITO_ID + BARRIO_ID + weekday + cluster(SECCION), data=tmp)

mod4_fit <- survfit(mod4)

#Random Forest aplicado a an?lisis de supervivencia
r_fit_bmt <- ranger(Surv(tiempo, solucionado) ~ SECCION + TIPO_INCIDENCIA_ID + CANAL_DE_ENTRADA_ID + 
                      ANOMALIA_ID + DISTRITO_ID + BARRIO_ID + weekday, 
                    data=tmp,
                    importance = "permutation")

# media
death_times <- r_fit_bmt$unique.death.times
surv_prob <- data.frame(r_fit_bmt$survival)
avg_prob <- sapply(surv_prob,mean)

# Pinta para cada una de las incidencias
plot(r_fit_bmt$unique.death.times,r_fit_bmt$survival[1,], type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "death times",
     ylab = "sin resolver",
     main = "Curvas de supervivencia")

for(n in c(2:137)){
  lines(r_fit_bmt$unique.death.times, r_fit_bmt$survival[n,], type = "l", col = "red")
}
lines(death_times, avg_prob, lwd = 2)
legend(100, 0.2, legend = c('Media - black'))

vi <- data.frame(sort(round(r_fit_bmt$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

# Represento la curvas de supervivencia
km <- rep("KM", length(fit_kaplan$time))
km_df <- data.frame(fit_kaplan$time,fit_kaplan$surv,km)
names(km_df) <- c("Tiempo","Supervivencia","Modelo")

cox <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,cox)
names(cox_df) <- c("Tiempo","Supervivencia","Modelo")

cox_total <- rep("Cox_total",length(cox_total_fit$time))
cox_total_df <- data.frame(cox_total_fit$time,cox_total_fit$surv,cox_total)
names(cox_total_df) <- c("Tiempo","Supervivencia","Modelo")

# mod3 <- rep("mod3",length(mod3_fit$time))
# mod3_df <- data.frame(mod3_fit$time,mod3_fit$surv,mod3)
# names(mod3_df) <- c("Tiempo","Supervivencia","Modelo")

mod4 <- rep("Cox_cluster",length(mod4_fit$time))
mod4_df <- data.frame(mod4_fit$time,mod4_fit$surv,mod4)
names(mod4_df) <- c("Tiempo","Supervivencia","Modelo")

rf <- rep("RF",length(r_fit_bmt$unique.death.times))
rf_df <- data.frame(r_fit_bmt$unique.death.times,avg_prob,rf)
names(rf_df) <- c("Tiempo","Supervivencia","Modelo")

plot_df <- rbind(km_df, cox_df, cox_total_df, mod4_df, rf_df)

p <- ggplot(plot_df, aes(x = Tiempo, y = Supervivencia, color = Modelo))
p + geom_line() + ggtitle("ComparaciÃ³n de los modelos de supervivencia") 
