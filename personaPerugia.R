#lineas 
# write.csv(con.historia, "OperDataclean.csv", row.names = F)
# 
# g<- ggplot(data = datos%>%filter(sexo == "femenino", sueldo > 500, sueldo < 1400, year %in% c(2015,2016)), 
#            aes(x=sueldo, y=unmes, colour = year)) + geom_count(alpha = 0.5) +
#      geom_smooth(confint = F) + 
#      xlab("Salary") + 
#      ylab("Femenine - Probability of stay one month") + 
#      ggtitle("WOMEN - Salary vs probability of staying one month at work")
# 
# g<- ggplot(data = datos%>%filter(sexo == "masculino"), aes(x=sueldo, y=unmes)) + geom_count(alpha = 0.5) +
#      geom_smooth() + 
#      xlab("Salary") + 
#      ylab("Femenine - Probability of stay one month") + 
#      ggtitle("WOMEN - Salary vs probability of staying one month at work")


historic <- function(yearinicial = 2010, yearfinal = 2017){
     require(dplyr)
     require(tidyr)
     #detach(package:plyr)
     
     #agregar historial
     classes <- c(NUM = "factor", DSDNUM = "factor", NOMBRE = "character", DEPTO = "factor", 
                  FECHA = "character", MVTO = "factor", FMOVIM = "character", ADSDNUM = "factor", 
                  ADEPTO = "factor")

     
     historial.raw <- read.csv("historial.csv", colClasses = classes, 
                           na.strings = c("  -   -","","<NA>","NA NA"), 
                           stringsAsFactors = F)
     
     #quitar primer renglon
     historial.raw <- historial.raw[-1,]
     
     historial.raw$FECHA = as.Date(historial.raw$FECHA, "%d/%m/%Y")
     historial.raw$FMOVIM = as.Date(historial.raw$FMOVIM, "%d/%m/%Y")
     
     #renombrar movimientos A-alta, B-baja, C-cambio, R-reingreso
     mov.tipo <- data.frame("MVTO" = c("A","B","C","R"), "MOV.TIPO" = c("ALTA","BAJA","CAMBIO","ALTA"))
     historial.raw <- merge(historial.raw, mov.tipo, by = "MVTO")
     
     historial.raw <<- historial.raw[year(historial.raw$FECHA) >= yearinicial & year(historial.raw$FECHA) <= yearfinal,]%>%
          arrange(FECHA)
     
     #CAMBIOS EN EL STATUS DE LA PERSONA EN EL TIEMPO
     cambios <- historial.raw%>%
          filter(MOV.TIPO == "CAMBIO",  SUELDO >0)%>%
          group_by(NUM)%>%
          summarise("cambios.depto" = n_distinct(DEPTO)-1,
                    "reingresos" = n(),
                    "cambios.sueldo" = n_distinct(SUELDO)-1,
                    "sueldo.min" = min(SUELDO), "sueldo.max" = max(SUELDO),
                    "aumento" = sueldo.max - sueldo.min,
                    "dias.para.aumento" = ifelse(aumento >0, max(FECHA) - min(FECHA), 0))
     
     #dias entre su cambio y su baja (positivos es que salio despues de un cambio)
     cambio.salidas <- historial.raw%>%
          filter(MOV.TIPO %in% c("CAMBIO","BAJA"))%>%
          group_by(NUM, MOV.TIPO)%>%
          summarise("FECHA" = max(FECHA))%>%
          spread(MOV.TIPO, FECHA)%>%
          mutate("dias.salio.cambio" = BAJA-CAMBIO, 
                 "salio.por.cambio" = ifelse(dias.salio.cambio >= 0 & dias.salio.cambio <=15,1,0))%>%
          select(NUM, dias.salio.cambio, salio.por.cambio)
     
     cambio <<- merge(cambios, cambio.salidas, by="NUM", all.x = TRUE)
     
     
     #agregar a BD general los cambios durante su estancia, sueldo, puestos, deptos
     con.historia <- merge(personal.produccion, cambio, by = "NUM", all.x = TRUE)
     

     #corregir NA por sueldcon.historia actual en sueldo.max y sueldo.min
     con.historia$sueldo.max.fit <- ifelse(is.na(con.historia$sueldo.max),
                                           con.historia$sueldo,
                                           con.historia$sueldo.max)
     con.historia$sueldo.min.fit <- ifelse(is.na(con.historia$sueldo.min),
                                           con.historia$sueldo,
                                           con.historia$sueldo.min)

     #aumentos, cambios de depto, etc que son NA se convienten en cero
     con.historia[, 16:22][is.na(con.historia[, 16:22])] <- 0
     con.historia[, 25:26][is.na(con.historia[, 25:26])] <- 0
     
     #corregir nombre de puestos preliminares y pespuntadores A,B,C
     puestos <- read.csv("puestos.csv")
     con.historia <<- merge(con.historia, puestos, by = "puesto", all.x = TRUE)
}

explore.hist <- function(){
     library(tidyr)
     
     #dias trabajo promedio (NO PUDE RESOLVERLO)
     # a<- historial.raw%>%
     #      filter(MOV.TIPO != "CAMBIO")%>%
     #      group_by(NUM, MOV.TIPO)%>%
     #      select(NUM, MOV.TIPO,FECHA)%>%
     #      arrange(NUM,FECHA,MOV.TIPO)
}


limpia.general <- function(){
     library(lubridate)
     library(dplyr)
     library(Amelia)
     
     classes <- c(primer_nom = "character", segdo_nom = "character", ap_paterno = "character",
                  ap_materno = "character", telefono = "character", fingreso = "character", 
                  fnacido = "character", fbaja = "character",dsdnum = "factor")
     personal <- read.csv("personal.csv", colClasses = classes, 
                           na.strings = c("  -   -","","<NA>","NA NA"))
     missmap(personal,main = "Datos faltantes", col = c("yellow","gray"))
     
     #character to dates
     personal$fingreso = as.Date(personal$fingreso, "%d/%m/%Y")
     personal$fnacido = as.Date(personal$fnacido, "%d/%m/%Y")
     personal$fbaja = as.Date(personal$fbaja, "%d/%m/%Y")
     
     #corregir fecha baja del num 11340, unico error
     personal$fbaja[personal$num ==  11340] <- "2015-11-10"
     personal$motivo = paste(personal$motivo1, personal$motivo2)
     personal <- personal
     
     #corregir fecha de ingreso con numero de empleado
     ingresos <- personal[,c(4,20)]
     #cambiar fecha a numero y eliminar nas
     ingresos <- ingresos[!is.na(ingresos$fingreso),]
     ingresos$datenum <- as.numeric(ingresos$fingreso)
     
     #elimina outliers de màs de 2 sigma, en residuals
     fit <- lm(data = ingresos, datenum~num)
     ingresos$res <- fit$residuals
     sd2 <- sd(fit$residuals)*1
     ingresos$outlier <- ifelse(abs(ingresos$res)>sd2,1,0)
     
     #añade las fechas fitted y limpia las que existen correctas
     fit <- lm(data = ingresos[ingresos$outlier == 0,], datenum~num)
     newy <- data.frame("num" = personal$num)
     personal$fitted <- round(predict(fit, newdata = newy))
     personal$fingresofitted <- as.Date(ifelse(is.na(personal$fingreso),
                                                    personal$fitted,  personal$fingreso),
                                             origin = "1970-01-01")
     
     #calcula antiguedad al dia de hoy si aún trabajan
     personal$antiguedad <- ifelse(personal$estatus == "Alta", 
                                   max(personal$fingresofitted) - personal$fingresofitted,
                                   personal$fbaja - personal$fingresofitted)
     
     personal <- select(personal, -fitted)
     
     #sin fecha de naciimiento, edad = NA, no cero
     personal$edad[personal$edad == 0] <- NA
     personal$antiguedad[personal$antiguedad < 0] <- 0
     personal.raw <<- personal%>%arrange(num)
     
     #calcula fecha de ingreso con su numero de empleado, pues es consecutivo
     #a partir del num 8021, los registros se normalizan, se hacer un primer intento con esto
     # equivale al 2012 para acá
     personal <- personal.raw[personal.raw$num>8021,]
     #personal <- personal.raw
     
     deptogral <- read.csv("filtro_deptos.csv")
     personal <- merge(personal, deptogral, by = "subdepto")

     personal <- personal%>%
          mutate("edad.fit" = ifelse(!is.na(edad), ceiling(edad),
                                       ceiling(floor(mean(personal$edad, na.rm = T)))),
               "rango.edad" = ceiling(edad.fit/10), 
                 "meses.antiguedad" = floor(antiguedad/30),
                 "edo.civil.fit" = ifelse(!is.na(edo_civil),edo_civil,
                                          ifelse(rango.edad < 3, 3 , 1)))
     personal[is.na(personal$estudios),]$estudios <- "Ninguno"
     personal[is.na(personal$sexo),]$sexo <- "masculino"
     
     
     deptogral <- read.csv("grupo_puestos.csv")
     personal <- merge(personal, deptogral, by = "puesto")
     
     #CORREGIR LEON MAL ESCRITO
     personal$municipio <- as.factor(ifelse(personal$municipio %in% c("LEOH","LON","LEN","EON","LEO","L","NA"),
                                  paste("LEON"),paste(personal$municipio)))
     
     clean <<- personal
     
     #solo produccion 
     personal.produccion <<- personal%>%
          filter(depto == "PRODUCCION")%>%
          select("NUM" = num, antiguedad, depto.group, sexo, municipio, edo.civil.fit, 
                 sueldo, puesto, edad.fit, estudios, "ingreso" = fingresofitted, 
                 rango.edad, meses.antiguedad, grupo.puesto, nivel.critico)

     missmap(personal.produccion,main = "Faltantes despues de limpiar", col = c("yellow","gray"))
     
}


learning <- function(duracion = 3, prob.level = 0.7, boost = FALSE, fecha.inicio = "2016-01-01",
                     fecha.fin = "2016-12-31"){
     library(caret)
     
     set.seed(888)
     
     #agregar a BD general los cambios durante su estancia, sueldo, puestos, deptos
     for.fit <- con.historia %>%
          filter(ingreso >= fecha.inicio & ingreso <= fecha.fin)
     
     #probabilidad que duren mas de "duracion" meses
     for.fit$year.ingreso <- as.factor(year(for.fit$ingreso))
     for.fit$mes.ingreso <- month(for.fit$ingreso)
     for.fit$output <- as.factor(ifelse(for.fit$meses.antiguedad>=duracion,1,0))
     
     clean.data <<- for.fit%>%
          select(-c(ingreso, puesto, nivel.critico, meses.antiguedad, municipio, 
                    grupo.puesto, NUM, antiguedad, year.ingreso, aumento, estudios, 
                    depto.group, puesto.fit, sueldo.min, sueldo.max))
     #reorder, output first
     clean.data <- clean.data[,c(ncol(clean.data), 1:ncol(clean.data)-1)]
     
     inTrain <- createDataPartition(clean.data$output, p=0.75, list = F)
     
     dtrain <<- clean.data[inTrain,]
     dtest <<- clean.data[-inTrain,]
     
     glm.model <<- glm(output~.-1, family =  "binomial", data = dtrain)
     
     message("GLM")
     pred.glm <<- predict(glm.model, dtest, type = "response")
     pred.fac.glm <- ifelse(pred.glm>=prob.level,1,0)
     print(summary(glm.model))
     print(confusionMatrix(pred.fac.glm, dtest$output))
     
     if (boost){
          #tunning gradient boost
          gbmGrid <-  expand.grid(interaction.depth = c(1, 5), 
                                  n.trees = (1:10)*50, 
                                  shrinkage = 0.1,
                                  n.minobsinnode = c(1,5))
          #result - interaction depth = 1, n.tress=150, shrinkage = 0.1 and nminsbox =20

          fitControl <- trainControl(## 10-fold CV
               method = "repeatedcv",
               number = 10,
               ## repeated ten times
               repeats = 10)
          
          boost.model <<- train(output~., method = "gbm", data = dtrain, 
                                trControl = fitControl ,verbose = FALSE,
                                tuneGrid = gbmGrid)
          
          message("BOOST")
          pred.boost <<- predict(boost.model, dtest, type = "prob")
          pred.fac.boost <- ifelse(pred.boost[,2]>=prob.level,1,0)
          print(summary(boost.model))
          print(confusionMatrix(pred.fac.boost, dtest$output))
     }
}


probabilidadporsueldo <- function(duracion = 6){

     require(ggplot2)
     #probabilidad que duren mas de x meses
     lmpersonal$seismeses <- ifelse(lmpersonal$meses.antiguedad>=duracion,1,0)
     lmpersonal$year <- year(lmpersonal$ingreso)

     lmpersonal <<- lmpersonal
     
     #porcentaje de gente que cumplio 6 meses
     graficos <<- lmpersonal%>%
               group_by(year, sueldo, sexo,seismeses)%>%
               summarise(n=n())%>%
               mutate(freq = n/sum(n))
     View(graficos)
     ggplot(data = graficos)
     
     #modelo
     fit <- glm(seismeses~sueldo+factor(sexo), data = lmpersonal, family = "binomial")
     
     #plot
     dsexo = c("masculino","femenino")
     par(mfrow = c(1,2))
     for (i in dsexo){
          xsueldos <<- seq(800,2000,1)
          data <- data.frame(sueldo = xsueldos, sexo = i)
          yfit <- predict(fit, newdata = data,
                              type = "response", 
                              confidence = "interval")
          plot(yfit~xsueldos, pch = 16, main = i)
     }
     par(mfrow = c(1,1))
}

palabrascomunes <- function(){
     require(c(tm,SnowballC))
     palabras = NULL
     
     years <- c(2012:2016)
     for (i in years){
          motivos <- personal.raw[year(personal.raw$fingresofitted)== i,]$motivo
          write.csv(motivos, "motivos.csv", row.names = FALSE)          
          text <- readLines("motivos.csv")
          
          docs <- Corpus(VectorSource(text))
          
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          docs <- tm_map(docs, toSpace, "/")
          docs <- tm_map(docs, toSpace, "@")
          docs <- tm_map(docs, toSpace, "\\|")
          
          # Convert the text to lower case
          docs <- tm_map(docs, content_transformer(tolower))
          # Remove numbers
          docs <- tm_map(docs, removeNumbers)
          # Remove spanish common stopwords
          docs <- tm_map(docs, removeWords, stopwords("spanish"))
          # Remove your own stop word
          # specify your stopwords as a character vector
          docs <- tm_map(docs, removeWords, c("NA", "mas", "trabajo")) 
          # Remove punctuations
          docs <- tm_map(docs, removePunctuation)
          # Eliminate extra white spaces
          docs <- tm_map(docs, stripWhitespace)
          # Text stemming  (quita ing, ed, en inglés, en español causa problemas)
          #docs <- tm_map(docs, stemDocument)
          
          dtm <- TermDocumentMatrix(docs)
          m <- as.matrix(dtm)
          v <- sort(rowSums(m),decreasing=TRUE)
          d <- data.frame(year = i, razon = names(v), freq=v)
          palabras <- rbind(palabras, head(d, 50))
          
     }
     print(palabras)
     palabras <<- palabras
     
}

exploratory <- function(){

     #hasta 24 meses
     meses.personal <- filter(lmpersonal, meses.antiguedad <=1)
     a2016 <<- lmpersonal[year(lmpersonal$ingreso) == "2016",]
     
     #altas y bajas por semana
     
     
         
}

grafica <- function(datos = lmpersonal, 
                    outcome = "meses.antiguedad", 
                    predictor = "sexo", 
                    gr = "points"){
     a <- datos
     colNums <- match(outcome,names(a))
     y= a[,colNums]
     colNums <- match(predictor,names(a))
     x= a[,colNums]
     
     fit <- lm(data = a, y~x-1)
     print(summary(fit))
     if (gr == "points"){
          ggplot(data = a, aes(x=x, y=y, colour = "red", alpha = 0.2)) + 
               geom_point() + geom_smooth(method = "lm", colour = "navy") + 
               xlab(predictor) + ylab(outcome)
     } else {
          ggplot(data = a, aes(x=factor(x), y=y, fill = predictor)) + 
               geom_boxplot() + facet_grid(~year) + 
               xlab(predictor) + ylab(outcome)          
     }
     
}