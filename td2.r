options(scipen=999)

library(dplyr)
library(lubridate)
library(leaflet)
library(plotly)
library(geojsonio)

#geocodage du fichier avec https://adresse.data.gouv.fr/csv

dvf<-read.csv2("dvf_propre.geocoded.csv",sep=";")

dvf<-dvf %>% 
  mutate(
  Date_mutation_d=dmy(Date_mutation), 
  Code_departement=sprintf("%02d", Code_departement)
  )%>% 
  mutate( periode=format(Date_mutation_d,"%Y%m"))



(kpi1<-dvf %>% filter(year(Date_mutation_d)=="2019") %>% count)
(kpi2<-dvf %>% filter(year(Date_mutation_d)=="2019" & Type_local=="Appartement") %>%
    summarize(prix_m2_mean=mean(prix_m2)))
(kpi3<-dvf %>% filter(year(Date_mutation_d)=="2019" & Type_local=="Maison") %>%
    summarize(prix_m2_mean=mean(prix_m2)))

prix_m2.dpt<-dvf %>% group_by(Code_departement) %>% summarize(prix_m2_mean=mean(prix_m2))
(prix_m2.commune<-dvf %>% group_by(result_citycode) %>% summarize(prix_m2_mean=mean(prix_m2)))

#graphiques ----
dvf %>% group_by(periode) %>% summarise(n=n()) %>% 
    plot_ly(x = ~periode, y = ~n, type='scatter',mode = 'lines')

dvf %>% group_by(periode,Type_local) %>% summarise(n=n()) %>% 
    plot_ly(x = ~periode, y = ~n,type='bar',color=~Type_local)

# cartes---- 
dpt <- geojsonio::geojson_read("departements.geojson", what = "sp")
prix_m2.dpt<-dvf %>% group_by(Code_departement) %>% summarize(prix_m2_mean=mean(prix_m2))
dpt.plus.prixm2 <- raster::merge(dpt, prix_m2.dpt, by.x="code", by.y="Code_departement")

bins <- c(0,1000,1500,2000,2500, 10000, Inf)
pal <- colorBin("YlOrRd", domain =dpt.plus.prixm2$prix_m2_mean , bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g €/m<sup>2</sup>",
  dpt.plus.prixm2$code, round(dpt.plus.prixm2$prix_m2_mean,0)
) %>% lapply(htmltools::HTML)


leaflet(dpt.plus.prixm2) %>% 
  addPolygons(
    fillColor = ~pal(prix_m2_mean),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    label = labels)

#carto2 -----
data<-dvf %>% filter(Code_departement=="79") %>% dplyr::select(latitude,longitude,Valeur_fonciere,Type_local)

bins <- c(0,50000,100000,125000,150000, 300000, Inf)
pal <- colorBin("YlOrRd", domain =dpt.plus.prixm2$prix_m2_mean , bins = bins)


leaflet(data) %>% addTiles() %>%
  addCircleMarkers(~as.numeric(longitude), ~as.numeric(latitude), 
                   label=~as.character(paste0(Type_local," / prix : ",Valeur_fonciere,"€")),
                   color = ~pal(Valeur_fonciere))

#prophet----
library(prophet)
df<-dvf %>% group_by(Date_mutation_d) %>% summarise(m=mean(prix_m2)) %>% rename(ds=Date_mutation_d,y=m)

m <- prophet(df)
future <- make_future_dataframe(m, periods = 180)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)

# R
prophet_plot_components(m, forecast)
