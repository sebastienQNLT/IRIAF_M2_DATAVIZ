options(scipen=999)

library(dplyr)

dvf<-read.csv2("base_DVF.csv",sep=";")

dvf<-dvf %>% select(-X)

dvf.corrected<-dvf %>% 
  #mutate(Code_postal=sprintf("%05d", Code_postal)) %>% 
  mutate(No_voie=as.character(No_voie),
         Code_departement=as.character(Code_departement),
         Code_postal=as.character(Code_postal),
        ) %>% 
    mutate(
      Code_departement_domtom=case_when(substr(Code_postal,1,2)=="97"~substr(Code_postal,1,3),
                                 TRUE~substr(Code_postal,1,2)
                                 ))

# test
# dvf.corrected %>% filter(as.numeric(Code_postal)>=97000) %>%
#  group_by(Code_departement,Code_departement_domtom) %>% count


quantile(dvf.corrected$Surface_reelle_bati,probs = c(0.9,.95,.99),na.rm = TRUE)
quantile(dvf.corrected$Valeur_fonciere,probs = c(0.9,.95,.99),na.rm = TRUE)
quantile(dvf.corrected$nblots,probs = c(0.9,.95,.99),na.rm = TRUE)
quantile(dvf.corrected$prix_m2,probs = c(0.9,.95,.99),na.rm = TRUE)


dvf.without.anomaly<-dvf.corrected %>% 
  filter(Surface_reelle_bati<=300) %>% 
  filter(nblots<=3) %>% 
  filter(Valeur_fonciere<=2000000)

quantile(dvf.without.anomaly$Surface_reelle_bati,probs = c(0.9,.95,.99),na.rm = TRUE)
quantile(dvf.without.anomaly$Valeur_fonciere,probs = c(0.9,.95,.99),na.rm = TRUE)
quantile(dvf.without.anomaly$nblots,probs = c(0.9,.95,.99),na.rm = TRUE)
quantile(dvf.without.anomaly$prix_m2,probs = c(0.9,.95,.99),na.rm = TRUE)
dvf.without.anomaly %>% summary

write.csv2(dvf.without.anomaly,"dvf_propre.csv")

#DATA QUALITY ----
#comptage des valeurs manquantes
dvf %>% summary
dvf.corrected %>% summary


#statistiques descriptives

# prix_m2 / valeur_foncière surface_reelle
# distribution du nb lots
# distribution du type_local



#repartition geo au niveau département / commune 

#saisonnalité dans le nb de ventes

#variation n/n-1