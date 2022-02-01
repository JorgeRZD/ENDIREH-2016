###Prevalencia de violencia total contra las mujeres de 15 años y más a lo largo de su vida###

##Se prepara la base de datos##
#Es necesario contar con las cinco tablas que contienen información sobre violencia en cada ámbito#
vi<-read.csv("sec_vi.csv", header = TRUE)%>%select(ID_MUJ:T_INSTRUM, starts_with("P6_6_"))
vii<-read.csv("sec_vii.csv", header = TRUE)%>%select(ID_MUJ, P7_3_1_1:P7_3_2_3, starts_with("P7_8_"), starts_with("P7_9_"))
viii<-read.csv("sec_viii.csv", header = TRUE)%>%select(ID_MUJ, starts_with("P8_1_"))
x<-read.csv("sec_x.csv", header = TRUE)%>%select(ID_MUJ, starts_with("P10_1_"))
xiii<-read.csv("sec_xiii.csv", header=TRUE)%>%select(ID_MUJ, starts_with("P13_1_"), FAC_VIV:EST_DIS)
secciones<-list(vi, vii, viii, x, xiii)%>%Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by="ID_MUJ"), .)

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

#Se construyen variables binarias para indicar la presencia de al menos un acto de violencia en cada ámbito, 
#una variable binaria que indique la presencia de un acto de violencia en cualquier ámbito y 
#una constante que se usará para calcular la prevalencia a nivel de la población
secciones<-secciones%>%mutate(violencia_familiar=case_when((if_any(P10_1_1:P10_1_18, ~. %in% c(1:3))) ~ 1, TRUE ~ 0),
                              violencia_pareja=case_when((if_any(P13_1_1:P13_1_36AB, ~. %in% c(1:3))) ~ 1, TRUE ~ 0),
                              violencia_escolar=case_when((if_any(P6_6_1:P6_6_17, ~. ==1)) ~ 1, TRUE ~ 0), 
                              violencia_laboral=case_when((if_any(P7_3_1_1:P7_9_18, ~. ==1)) ~ 1, TRUE ~ 0),
                              violencia_comunitaria=case_when((if_any(P8_1_1:P8_1_15, ~. ==1)) ~ 1, TRUE ~ 0),
                              dummy=1, violencia_total=case_when((if_any(violencia_familiar:violencia_comunitaria, ~. ==1)) ~ 1, TRUE ~ 0))

##Se especifica el modelo de la muestra##
design_secciones<-secciones%>%as_survey(ID=UPM_DIS, strata=EST_DIS, weights=FAC_MUJ)

##Calculando el porcentaje de mujeres que experimentaron violencia en cualquier ámbito y estadísticos de significancia##
prevalencia_nacional<-design_secciones%>%summarise(r=survey_ratio(violencia_total, dummy, vartype = c("se","cv","ci"), level = .95))
Prevalencia_estata<-design_secciones%>%group_by(NOM_ENT)%>%summarise(r=survey_ratio(violencia_total, dummy, vartype = c("se","cv","ci"), level = .95))

##Calculando el porcentaje de mujeres que experimentaron violencia en cualquier ámbito, por grupo de edad y estadísticos de significancia##
Prevalencia_estata<-design_secciones%>%group_by(grupo_edad)%>%summarise(r=survey_ratio(violencia_total, dummy, vartype = c("se","cv","ci"), level = .95))

