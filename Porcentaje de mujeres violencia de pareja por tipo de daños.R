###Porcentaje de mujeres que experimentaron violencia de pareja por tipo de daños###
library(dplyr)
library(srvyr)

##Se prepara la base de datos##
#Es necesario contar con las dos tablas que contienen información de la sección XIII
xiii<-read.csv("sec_xiii.csv", header = TRUE)
xiii_2<-read.csv("sec_xiii_2.csv", header = TRUE)%>%select(ID_MUJ, starts_with("P13"))
sdm<-read.csv("sdm.csv", header = TRUE)%>%select(ID_MUJ, EDAD)
xiii<-list(xiii, xiii_2, sdm)%>%Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by="ID_MUJ"), .)


#Limpiando una notación extraña para facilitar la comprensión del código#
xiii$T_INSTRUM[xiii$T_INSTRUM=='A1\n']<-"A1"
xiii$T_INSTRUM[xiii$T_INSTRUM=='A2\n']<-"A2"
xiii$T_INSTRUM[xiii$T_INSTRUM=='B1\n']<-"B1"
xiii$T_INSTRUM[xiii$T_INSTRUM=='B2\n']<-"B2"
xiii$T_INSTRUM[xiii$T_INSTRUM=='C1\n']<-"C1"
xiii$T_INSTRUM[xiii$T_INSTRUM=='C2\n']<-"C2"

#Se construye un vector de indicadores y dos variables binarias para identificar a la población de interes: mujeres que están o han estado casadas o unidas y mujeres que han sufrido violencia de pareja#
hubo_pareja<-c("A1", "A2", "B1", "B2")
xiii<-xiii%>%mutate(violencia=case_when((if_any(P13_1_1:P13_1_36AB, ~. %in% c(1:3)))==1~1,TRUE~0))
xiii<-xiii%>%mutate(pareja=case_when(T_INSTRUM %in% hubo_pareja~1,TRUE~0))

#La variable grupo_edad se usa para agrupar por edades
xiii<-xiii%>%mutate(grupo_edad=case_when(
  EDAD>=15 & EDAD<=17 ~ '15 a 17',
  EDAD>=18 & EDAD<=19 ~ '18 a 19',
  EDAD>=20 & EDAD<=24 ~ '20 a 24',
  EDAD>=25 & EDAD<=29 ~ '25 a 29',
  EDAD>=30 & EDAD<=34 ~ '30 a 34',
  EDAD>=35 & EDAD<=39 ~ '35 a 39',
  EDAD>=40 & EDAD<=44 ~ '40 a 44',
  EDAD>=45 & EDAD<=49 ~ '45 a 49',
  EDAD>=50 & EDAD<=54 ~ '50 a 54',
  EDAD>=55 & EDAD<=59 ~ '55 a 59',
  EDAD>=60 & EDAD<=97 ~ '60 y más',
  EDAD>=98 ~ 'No especificada',
  TRUE ~ 'No especificada'
))

##Se especifica el modelo de la muestra##
design_xiii<-xiii%>%as_survey(id=UPM_DIS, strata=EST_DIS, weights=FAC_MUJ)

##Calculando el porcentaje de mujeres que experimentaron violencia de pareja por tipo de daños y estadísticos de significancia##
relativo_violencia_nacional<-design_xiii%>%filter(T_INSTRUM %in% hubo_pareja)%>%summarise("NOM_ENT"="Nacional", r=survey_ratio(violencia, pareja, vartype = c("se","cv","ci"), level = .95))
relativo_violencia_estados<-design_xiii%>%filter(T_INSTRUM %in% hubo_pareja)%>%group_by(NOM_ENT)%>%summarise(r=survey_ratio(violencia, pareja, vartype = c("se","cv","ci"), level = .95))
#Se combinan la tabla de resultados nacional y la tabla de resultados por estado en una sola#
indicador_violencia_pareja<-rbind(relativo_violencia_nacional, relativo_violencia_estados)

##Calculando el porcentaje de mujeres que experimentaron violencia de pareja por tipo de daño, por grupo de edad y estadísticos de significancia##
relativo_violencia_edad<-design_xiii%>%filter(T_INSTRUM %in% hubo_pareja)%>%group_by(grupo_edad)%>%summarise(r=survey_ratio(violencia, pareja, vartype = c("se","cv","ci"), level = .95))

