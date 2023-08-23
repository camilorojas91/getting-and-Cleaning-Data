##Crea carpetas en el proyecto si no existen
if(!dir.exists("datos")){
  dir.create("datos")
}

if(!dir.exists("salidas")){
  dir.create("salidas")
}

library(rio)
library(dplyr)
library(tidyr)

####Test####
subject_test <- import("./datos/UCI HAR Dataset/test/subject_test.txt")
x_test <- import("./datos/UCI HAR Dataset/test/x_test.txt")
y_test <- import("./datos/UCI HAR Dataset/test/y_test.txt")

traer_archivos_test <- function(direccion) {
  
  direccion <- "./datos/UCI HAR Dataset/test/Inertial Signals"
  
  archivos <- list.files(direccion, pattern = "test.*\\.txt$", full.names = TRUE)
  return(archivos)
}

archivos_test <- traer_archivos_test(direccion)

datos_combinados_test <- NULL

for (i in archivos_test) {
  datos_test <- import(i)
  if (is.null(datos_combinados_test)) {
    datos_combinados_test <- datos_test
  } else {
    datos_combinados_test <- cbind(datos_combinados_test, datos_test)
  }
}

#####train####

subject_train <- import("./datos/UCI HAR Dataset/train/subject_train.txt")
x_train <- import("./datos/UCI HAR Dataset/train/x_train.txt")
y_train <- import("./datos/UCI HAR Dataset/train/y_train.txt")

traer_archivos_train <- function(direccion) {
  
  direccion <- "./datos/UCI HAR Dataset/train/Inertial Signals"
  
  archivos <- list.files(direccion, pattern = "train.*\\.txt$", full.names = TRUE)
  return(archivos)
}

archivos_train <- traer_archivos_train(direccion)

datos_combinados_train <- NULL

for (i in archivos_train) {
  datos_train <- import(i)
  if (is.null(datos_combinados_train)) {
    datos_combinados_train <- datos_train
  } else {
    datos_combinados_train <- cbind(datos_combinados_train, datos_train)
  }
}

for (i in 1153:1280) {
  nueva_col <- rep(NA,nrow(datos_combinados_train))
  colname <- paste0("V",i)
  datos_combinados_train[colname] <- as.numeric(nueva_col)
}
final_y <- rbind(y_test,y_train)
final_x<- rbind(x_test, x_train)
final_subject<- rbind(subject_test,subject_train)

colnames(datos_combinados_train) <- colnames(datos_combinados_test)
final_datos<- rbind(datos_combinados_test,datos_combinados_train)

base_final <- cbind(final_subject,final_y,final_x)

colnames(base_final)[1:2] <- c("sujeto", "actividad")

#analisis media y SD por usuario y 

base_final_1 <- base_final %>% 
  mutate(actividad = case_when(actividad == 1~ "WALKING",
                               actividad ==2~ "WALKING_UPSTAIRS",
                               actividad ==3~"WALKING_DOWNSTAIRS",
                               actividad ==4~ "SITTING",
                               actividad ==5~ "STANDING",
                               actividad ==6~ "LAYING",
                               TRUE~ NA)) %>% 
  group_by(sujeto,actividad) %>% 
  summarise_at(
    vars(-group_cols()), 
    list(media = ~ mean(., na.rm = TRUE), desviacion = ~ sd(., na.rm = TRUE))
  )


Basefinal <- as.data.frame(t(base_final_1))
colnames(Basefinal) <- paste(Basefinal[2,], "sujeto",Basefinal[1,])
Basefinal <- Basefinal[-1:-2,]

c <- names(Basefinal)

Basefinal<- as.data.frame(sapply(Basefinal, as.numeric))

base_resumen <- as.data.frame(colMeans(Basefinal))

base_resumen$sujeto_actividad <- row.names(base_resumen)

colnames(base_resumen) <- c("Media", "sujeto_actividad")

export(base_resumen,"./salidas/tabla_resuman.xlsx")
