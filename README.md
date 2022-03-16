# Stage-M2
MS-nutrition
# Chargement des packages ----

library(tidyverse)
library(readr)
library(survey)
options("survey.lonely.psu" = "adjust")
library(lpSolveAPI)
library(ggplot2)

# Chargement des donn?es ----

conso_compo_alim_vf_mad_datagouv2021 <- read_delim("conso-compo-alim-vf-mad-datagouv2021.csv", 
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


description_indiv <- read_delim("description-indiv.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)


correspondance_packageINCA3 <- read_delim("correspondance_packageINCA3.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


R_contraintes_nut_input <- read_delim("R_contraintes_nut_input.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)


R_data_compo_input <- read_delim("R_data_compo_input.xls", 
                                 delim = ";", escape_double = FALSE, locale = locale(grouping_mark = "."), 
                                 trim_ws = TRUE)
str(R_data_compo_input)

#  Consommation moyenne journaliere de poisson chez les hommes adultes ----

## S?lection des variables d'int?r?t ##
data <- conso_compo_alim_vf_mad_datagouv2021%>%
  select(NOIND, gpe_INCA3, qte_conso_pond, R24_nombre)

data2 <- description_indiv%>%
  select(pop3, NOIND, sex_PS, tage_PS,ech,pop3,zae,NOMEN,strate,fpc1,fpc2,fpc3,pond_indiv_adu_pop3)

data3 <- correspondance_packageINCA3%>%
  filter(format == "format_gpe_INCA3")%>%
  select(code, libelle)%>%
  mutate(gpe_INCA3=paste0("gpe_",code)) #fonction paste qui te permet de concat?ner des ?l?ments : ici je concat?ne "gpe_" avec chaque code

## Fusion des variables selectionn?es dans un tableau + filtre pop + calcul moyenne

table <-left_join(data2, data)%>%
  filter(tage_PS %in% 7:9, gpe_INCA3 == 18 )%>%
  group_by(NOIND)%>%
  summarise(mean_conso = sum(qte_conso_pond/R24_nombre, na.rw=TRUE))%>%
  full_join(data2 %>%filter(tage_PS %in% 7:9&pop3==1))%>%
  mutate(mean_conso=replace_na(mean_conso, 0))

mean(table$mean_conso)


#####  Analyse INCA3 avec prise en compte du plan d'?chantillonnage : ----


plan_ech <- svydesign(data = table, id=~zae+NOMEN+NOIND,strata = ~strate,fpc=~fpc1+fpc2+fpc3, weights = ~pond_indiv_adu_pop3)

svymean(~mean_conso, design = plan_ech , na.rm = T)[[1]] #[[pour avoir que le premier chiffre]]
sqrt(svyvar(~mean_conso,design= plan_ech))

# autre facon de faire 

bygroupe <-left_join(data2, data)%>%
  filter(tage_PS %in% 7:9 & pop3==1)%>%
  group_by(NOIND, gpe_INCA3)%>%
  summarise(mean_conso = sum(qte_conso_pond/R24_nombre, na.rw=TRUE))%>%
  pivot_wider(names_from = gpe_INCA3, values_from = mean_conso, values_fill = 0, names_prefix = "gpe_")%>%
  pivot_longer(cols = starts_with("gpe"), names_to = "gpe_INCA3", values_to = "quantite")#%>%
#group_by(gpe_INCA3)%>%
#summarise(moy = mean(quantite))

# autre facon de faire

qt_bygroupe = bygroupe %>%
  left_join(description_indiv%>%
              select(NOIND,pop3,ech,pop3,zae,NOMEN,strate,
                     fpc1,fpc2,fpc3,pond_indiv_adu_pop3), on="NOIND")%>%
  group_by(gpe_INCA3)%>%
  nest()%>% #Ici tu cr?es des sous-listes par groupes d'aliments et sexe
  mutate(data_pond=map(.x=data,.f=~svydesign(id=~zae+NOMEN+NOIND,strata = ~strate,data = .x,
                                             fpc=~fpc1+fpc2+fpc3, weights = ~pond_indiv_adu_pop3)))%>% #map te permet de cr?er un design (data_pond) pour chaque ?l?ment de la liste (.x correspond au groupe 1, puis groupe 2, etc.) 
  mutate(
    mean=map_dbl(.x=data_pond,~svymean(~quantite,design = .x)[[1]]),
    std=map_dbl(.x=data_pond,~sqrt(svyvar(~quantite,design=.x))[[1]]))%>%
  select(gpe_INCA3, mean, std)


# ajout du nom des aliments 

qt_bygroupe = bygroupe %>%
  left_join(description_indiv%>%
              select(NOIND,pop3,ech,pop3,zae,NOMEN,strate,
                     fpc1,fpc2,fpc3,pond_indiv_adu_pop3), on="NOIND")%>%
  group_by(gpe_INCA3)%>%
  nest()%>% #Ici tu cr?es des sous-listes par groupes d'aliments et sexe
  mutate(data_pond=map(.x=data,.f=~svydesign(id=~zae+NOMEN+NOIND,strata = ~strate,data = .x,
                                             fpc=~fpc1+fpc2+fpc3, weights = ~pond_indiv_adu_pop3)))%>% #map te permet de cr?er un design (data_pond) pour chaque ?l?ment de la liste (.x correspond au groupe 1, puis groupe 2, etc.) 
  mutate(
    mean=map_dbl(.x=data_pond,~svymean(~quantite,design = .x)[[1]]),
    std=map_dbl(.x=data_pond,~sqrt(svyvar(~quantite,design=.x))[[1]]))%>%
  select(gpe_INCA3, libelle, mean, std)%>%
  left_join(data3, on="gpe_INCA3")




#####    Apport nutritionnels INCA3 par sexe ---- 


conso <- conso_compo_alim_vf_mad_datagouv2021 %>%
  select(NOIND, gpe_INCA3, qte_conso_pond, R24_nombre, aliment_code_INCA3,aliment_libelle_INCA3, aet:glucides)%>%
  filter(!aliment_code_INCA3%in%c(3049, 3393,3724)) #Supprimer les aliments avec NA ds la compo

indiv <- description_indiv%>%
  select(pop3, NOIND, sex_PS, tage_PS,ech,pop3,zae,NOMEN,strate,fpc1,fpc2,fpc3,pond_indiv_adu_pop3)


by_alim <-left_join(conso, indiv)%>%
  filter(tage_PS %in% 7:9 & pop3==1)%>%
  group_by(NOIND)%>%
  mutate(across(aet:glucides, ~.*qte_conso_pond/100, .names = "qt_tot_{.col}" ))%>% 
  summarize(across(qt_tot_aet:qt_tot_glucides, ~sum(./R24_nombre, na.rw=TRUE)))%>% #na.rw=TRUE permet de supprimer les lignes qui ont des NA, pas ouf si on veut voir ou il y a des erreurs
  pivot_longer(cols = qt_tot_aet:qt_tot_glucides, names_to = "nutriment", values_to = "apport")


apport_moy = by_alim %>%
  left_join(description_indiv, on="gpe_INCA3"%>%
              select(NOIND,pop3, sex_PS,ech,pop3,zae,NOMEN,strate,
                     fpc1,fpc2,fpc3,pond_indiv_adu_pop3), on="NOIND")%>%
  group_by(sex_PS, nutriment)%>%
  nest()%>% #Ici tu cr?es des sous-listes par groupes d'aliments et sexe
  mutate(data_pond=map(.x=data,.f=~svydesign(id=~zae+NOMEN+NOIND,strata = ~strate,data = .x,
                                             fpc=~fpc1+fpc2+fpc3, weights = ~pond_indiv_adu_pop3)))%>% #map te permet de cr?er un design (data_pond) pour chaque ?l?ment de la liste (.x correspond au groupe 1, puis groupe 2, etc.) 
  
  mutate(
    n=map_dbl(.x=data,~nrow(.x)),
    mean=map_dbl(.x=data_pond,~svymean(~apport,design = .x)[[1]]),
    std=map_dbl(.x=data_pond,~sqrt(svyvar(~apport,design=.x))[[1]]))%>%
  select(sex_PS, nutriment, mean, std, n)





####      Exercice de PL avec quelques donnees ---- 


data_compO <- data.frame(Food=c("Alim1", "Alim2", "Alim3")) %>%  
  mutate(CostPerServing=c(.18, .23, .05), VitaminA=c(107, 500, 0), Calories=c(72, 121, 65)) %>%
  dplyr :: rename(VitaminAmin=VitaminA) %>% # Rename the variable which will correspond to the minimal constraint  
  dplyr :: rename(Caloriesmin=Calories) %>%  
  mutate(VitaminAmax=VitaminAmin) %>%  
  mutate(Caloriesmax=Caloriesmin)%>%
  pivot_longer(cols = CostPerServing:Caloriesmax, names_to = "nutrient")%>%
  group_by(Food, nutrient)%>%
  pivot_wider(names_from = Food, values_from = value, values_fill = 0)

data_type <- data.frame(type=c("obj_fct", "const", "const", "const", "const"))%>%
  mutate(const_val=c(NA, 5000, 2000, 50000, 2500))%>%
  mutate(const_type=c(NA, ">=", ">=", "<=", "<="))

data_compo <- cbind(data_compO, data_type)
mod <- make.lp(nrow = nrow(data_compo%>%filter(type=="const")),ncol=3)
for (i in 1:3){
  set.column(mod, i, data_compo%>%filter(type=="const")%>%ungroup()%>%pull(i+1))}
set.constr.type(mod, data_compo%>%filter(type =="const")%>%ungroup()%>%pull(const_type))
set.rhs(mod, data_compo%>%filter(type=="const")%>%ungroup%>%pull(const_val))
set.objfn(mod,  data_compo%>%filter(type=="obj_fct")%>%ungroup%>%select(Alim1:Alim3)%>%as.numeric())
solve(mod)
get.variables(mod) 







####    exo optimisation avec plus de donn?es ----

input_compo <- R_data_compo_input%>%
  select(Food_item_code, Food_group, Food_group_code, MEAN_QTE_all, nrj:cost, FG_100:FG_800, P5, P95)%>% # selection des bonnes variables
  # Calcul des contraintes en poucentage d'energbie pour les macronut
  mutate(GLU_pctNRJmin=GLU*4-R_contraintes_nut_input%>%filter(NUT=="GLU_pctNRJmin")%>%pull(NRJ_reco)/100*nrj)%>% 
  mutate(LIPIDE_pctNRJmin=LIPIDE*9-R_contraintes_nut_input%>%filter(NUT=="LIPIDE_pctNRJmin")%>%pull(NRJ_reco)/100*nrj)%>%
  mutate(AG_SATU_pctNRJmax=AG_SATU*9-R_contraintes_nut_input%>%filter(NUT=="AG_SATU_pctNRJmax")%>%pull(NRJ_reco)/100*nrj)%>%
  mutate(GLU_pctNRJmax=GLU*4-R_contraintes_nut_input%>%filter(NUT=="GLU_pctNRJmax")%>%pull(NRJ_reco)/100*nrj)%>%
  mutate(LIPIDE_pctNRJmax=LIPIDE*9-R_contraintes_nut_input%>%filter(NUT=="LIPIDE_pctNRJmax")%>%pull(NRJ_reco)/100*nrj)%>%
  mutate(across(nrj:cost, ~./100 ))%>% #pour avoir les nutriments par gramme
  select(Food_item_code, Food_group, Food_group_code, nrj:cost, GLU_pctNRJmin:LIPIDE_pctNRJmax, FG_100:FG_800, P5, P95)%>%
  #Pour passer les nutriments en ligne et les aliments en colonne : 
  pivot_longer(cols = nrj:P95, names_to = "nutrient")%>%select(-Food_group,-Food_group_code)%>%
  pivot_wider(names_from = Food_item_code, values_from = value, values_fill = 0)%>%
  rename("NUT"="nutrient")


#Calcul l'apport ÃÂ©nergÃÂ©tique tot
nrj_tot <- R_data_compo_input%>%
  select(Food_item_code, MEAN_QTE_all, nrj)%>%
  mutate(nrj_tot = nrj*MEAN_QTE_all/100)%>%
  summarize(app_tot = sum(nrj_tot, na.rw=TRUE))%>%pull(app_tot)

#creation de la table pour le modele
P95_table <- R_data_compo_input%>%distinct(P95_FG,Food_group_code) #pour extraire les P95 par food_groupe

table_pl = input_compo%>%
  left_join(R_contraintes_nut_input, on="NUT")%>%
  select(NUT:F_850, minmax:NRJ_reco)%>%
  # Boucle ifelse pour ajouter les contraintes sur les nutriments 
  mutate(const_type=ifelse(minmax=="min",">=",
                           ifelse(minmax=="max","<=",
                                  ifelse(minmax=="eq", "=",NA))
  ))%>%
  left_join(P95_table,by=c("NUT"="Food_group_code"))%>%
  # Boucle pour ajouter le type de variable, contrainte, fonction objective et seuil a ne pas depasser (rangelimit pour les P5 et P95)
  mutate(type=ifelse(NUT=="cost", "obj_fct",ifelse(NUT %in% c("P5","P95"),"range_limit","const")))%>%
  # boucle pour ajouter la colonne rhs : si la colonne NUT commence par FG_ alors on met les valeurs de la colonne P95_FG sinon les valeurs de la colonne RDA
  mutate(rhs=ifelse(grepl("^FG_", NUT), P95_FG,RDA))%>%
  # Pour mettre la contrainte isocalorique
  mutate(rhs=ifelse(NUT=="nrj",nrj_tot ,rhs))%>%
  select(NUT:F_850, const_type, type, rhs)


## modele optimisation du cout 

table_const=table_pl%>%filter(type=="const"& !is.na(const_type)) #filtre pour les nutriments de type const et qui ont une valeur dans la colonne const_type, comme ca on enleve les NA

model <- make.lp(nrow = nrow(table_const),ncol= ncol(table_pl%>%select(starts_with("F_"))))
for(i in 1:ncol(table_pl%>%select(starts_with("F_")))){
  set.column(model, i, table_const%>%pull(i+1))}

set.constr.type(model,table_const%>%pull(const_type)) # ajout des types de contraintes
set.bounds(model,lower = table_pl%>%filter(NUT=="P5")%>%select(starts_with("F_"))%>%as.numeric(),
           upper = table_pl%>%filter(NUT=="P95")%>%select(starts_with("F_"))%>%as.numeric()
) # ajout des contraintes sur les percentiles 
set.rhs(model, table_const%>%pull(rhs)) #ajout des valeurs limite pour chaque nutriments
set.objfn(model,table_pl%>%filter(type=="obj_fct")%>%select(starts_with("F_"))%>%as.numeric())

dimnames(model)[[1]]=table_const%>%pull(NUT) # pour avoir le noms des nutriments 
dimnames(model)[[2]]=colnames(table_pl%>%select(starts_with("F_")))# pour avoir le nom des aliments 

solve(model)
get.variables(model)
get.objective(model)
get.sensitivity.rhs(model) # pour voir quelle(s) contraintes est activ(s), plus on s'eloigne de 0 plus la contraintes exerce une tension sur le modele
write.lp(model,file="test.txt")



# Deux facons pour calculer les apports optimaux des aliments 
res1=data.frame(qty_opt=get.variables(model),
                dimnames(model)[[2]])%>%
  rename( "Food_item_code"="dimnames.model...2..")%>%
  left_join(R_data_compo_input, on="Food_item_code")%>%
  mutate(across(nrj:cost, ~.*qty_opt/100, .names= "intake_opt_{.col}"))%>%
  summarise(across(starts_with("intake_opt_"), ~sum(.)))


res=data.frame(qty_opt=get.variables(model),
               dimnames(model)[[2]])%>%
  rename( "Food_item_code"="dimnames.model...2..")%>%
  left_join(R_data_compo_input, on="Food_item_code")%>%
  pivot_longer(cols=nrj:cost,names_to = "nutrient",values_to = "compo")%>%
  mutate(app=compo*qty_opt/100)%>%
  group_by(nutrient)%>%
  summarize(app_tot=sum(app))%>%
  left_join(R_contraintes_nut_input,by=c("nutrient"="NUT"))%>%
  mutate(pct_reco=app_tot/RDA*100)%>%
 # calcul en pourcentage d'Ã©nergie des macronutriments
 mutate(app_nrj=
           ifelse(nutrient %in% c("GLU","PROT"),app_tot*4/nrj_tot*100,
                  ifelse(nutrient %in% c("LIPIDE","AG_SATU"),app_tot*9/nrj_tot*100,
                  NA)))






### EXO minimisation des deviations ---------


# data management pour creer les matrices des contraintes de deviation
f<- R_data_compo_input%>%select(Food_item_code, MEAN_QTE_all)%>%rename("Food_item"="Food_item_code")

food <- R_data_compo_input%>%
  select(Food_item_code, MEAN_QTE_all)%>%
  mutate(qte_obs = 1/MEAN_QTE_all)%>%
  left_join(f, on="MEAN_QTE_all")%>%
  select(Food_item_code, Food_item, qte_obs)%>%
  pivot_wider(names_from=Food_item,values_from= qte_obs,values_fill = 0)

dev_neg <- R_data_compo_input%>%
  select(Food_item_code, MEAN_QTE_all)%>%
  mutate(qte_obs = MEAN_QTE_all/MEAN_QTE_all)%>%
  left_join(f, on="MEAN_QTE_all")%>%
  select(Food_item_code, Food_item, qte_obs)%>%
  mutate(Food_item=gsub("F_","Devneg_",Food_item))%>%
  pivot_wider(names_from=Food_item,values_from= qte_obs,values_fill = 0)
  

dev_pos <- R_data_compo_input%>%
  select(Food_item_code, MEAN_QTE_all)%>%
  mutate(qte_obs = -(MEAN_QTE_all/MEAN_QTE_all))%>%
  left_join(f, on="MEAN_QTE_all")%>%
  select(Food_item_code, Food_item, qte_obs)%>%
  mutate(Food_item=gsub("F_","Devpos_",Food_item))%>%
  pivot_wider(names_from=Food_item,values_from= qte_obs,values_fill = 0)
 
# agregation des matrices en une seule
dev_const <- food%>%
  left_join(dev_pos)%>%left_join(dev_neg)%>%
  rename("NUT"="Food_item_code")

input_table <- bind_rows(input_compo,dev_const)%>%
  mutate_all(~replace(., is.na(.), 0))

#ajout des colonne "type de contrainte" et "valeur des contraintes"
table_dev <- table_const%>%
  select(NUT,const_type, rhs)


fct_obj <- input_table%>%
  filter(NUT =="nrj")%>%
  select(F_110:Devneg_850)%>%
  pivot_longer(cols=F_110:Devneg_850, names_to="obj_fct", values_to="nrj")%>%
  mutate(NUT=ifelse(grepl("^F_", obj_fct), 0, 1))%>%select(obj_fct, NUT)%>%
  pivot_wider(names_from=obj_fct, values_from = NUT)%>%
  mutate(NUT= "obj_fct")

input_table1 <- input_table%>%
  left_join(table_dev)%>%
  mutate(const_type=ifelse(grepl("^F_", NUT),"=",const_type))%>%
  mutate(rhs=ifelse(grepl("^F_", NUT), 1, rhs))%>%
  left_join(P95_table,by=c("NUT"="Food_group_code"))%>%
  mutate(rhs=ifelse(grepl("^FG_", NUT), P95_FG,rhs))%>%
  mutate(rhs=ifelse(NUT=="nrj",nrj_tot ,rhs))%>%
  filter(!NUT %in%c("cost"))%>%
  bind_rows(fct_obj)%>%
  mutate(value= ifelse(const_type %in% c("=", "<=", ">="), "const",
         ifelse(grepl("^FG_", NUT), "const",
                ifelse(NUT=="obj_fct", "obj_fct",
                       NA))))%>%
  select(NUT:rhs, value)



# Modele pour minimiser les deviations 


salutation <- input_table1%>%filter(value=="const"& !is.na(const_type))



the_model <- make.lp(nrow = nrow(salutation), ncol= ncol(input_table1%>%select(starts_with("F_"),starts_with("Dev"))))
   for(i in 1:ncol(input_table1%>%select(starts_with("F_"),starts_with("Dev")))){
  set.column(the_model, i, salutation%>%pull(i+1))}

set.constr.type(the_model, salutation%>%pull(const_type))
set.bounds(the_model, lower = input_table1%>%filter(NUT=="P5")%>%select(starts_with("F_"),starts_with("Dev"))%>%as.numeric(), 
           upper = c(input_table1%>%filter(NUT=="P95")%>%select(starts_with("F_"))%>%as.numeric(),rep("Inf",ncol(input_table1%>%select(starts_with("Dev"))))))#il faut que le maximum soit different de 0 sinon le modele est infaisable donc on met "Inf"
set.rhs(the_model, salutation%>%pull(rhs))
set.objfn(the_model, input_table1%>%filter(value=="obj_fct")%>%select(starts_with("F_"),starts_with("Dev"))%>%as.numeric())

dimnames(the_model)[[1]]=salutation%>%pull(NUT) # pour avoir le noms des nutriments 
dimnames(the_model)[[2]]=colnames(input_table%>%select(starts_with("F_"), starts_with("Dev")))# pour avoir le nom des aliments 

solve(the_model)
get.variables(the_model)
get.objective(the_model)
get.sensitivity.rhs(the_model)

###      Graphique : organisation des resultats

result1=data.frame(qty_opt=get.variables(the_model),
                   dimnames(the_model)[[2]])%>%
  rename( "Food_item_code"="dimnames.the_model...2..")%>%
  left_join(R_data_compo_input, on="Food_item_code")%>%
  filter(grepl("F_",Food_item_code))%>%
  select(qty_opt, MEAN_QTE_all, Food_group, Food_group_code)%>%
  mutate(var_qty = qty_opt-MEAN_QTE_all)%>%
  group_by(Food_group,Food_group_code)%>%
  summarize(var_qty=sum(var_qty))


# variation de la consommation des groupes d'aliments entre diet optimisÃ©e et diet observÃ©e

result1=result1%>%
 mutate(Food_group=factor(Food_group,levels=result1%>%arrange(Food_group_code)%>%pull(Food_group)))


ggplot(data = result1, aes(x=Food_group, y=var_qty,fill=Food_group)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Fruits and vegetables"="green", "Starches"="orange", "red", "gray", "blue", "pink", "yellow"))+
  labs(y = "Variation de la quantitÃ© optimisÃ©e (g)",
       x = "Groupes d'aliments",
       title = "Variation de la consommation des groupes d'aliments entre diet observÃ©e et optimisÃ©e") +
  theme_bw()

library(colortools)
pizza(setColors("tomato", 12))

# Apport en pourcentage de la recommandation

result2=data.frame(qty_opt=get.variables(the_model),
               dimnames(the_model)[[2]])%>%
  rename( "Food_item_code"="dimnames.the_model...2..")%>%
  left_join(R_data_compo_input, on="Food_item_code")%>%
  pivot_longer(cols=nrj:cost,names_to = "nutrient",values_to = "compo")%>%
  filter(grepl("F_",Food_item_code))%>%
  mutate(app=compo*qty_opt/100)%>%
  group_by(nutrient)%>%
  summarise(app_tot=sum(app))%>%
  left_join(R_contraintes_nut_input,by=c("nutrient"="NUT"))%>%
  mutate(pct_reco=app_tot/RDA*100)%>%
  # calcul en pourcentage d'Ã©nergie des macronutriments
  mutate(app_nrj=
           ifelse(nutrient %in% c("GLU","PROT"),app_tot*4/nrj_tot*100,
                  ifelse(nutrient %in% c("LIPIDE","AG_SATU"),app_tot*9/nrj_tot*100,
                         NA)))


get.sensitivity.rhs(the_model)
