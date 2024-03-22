#fonctions necessaires au Rmardown 5_etape_2_analyse_station 


#### Création df de travail pour la station considérée## 
# A partir des fichiers csv téléchargés sur l'IED Carhyce 

df_creation<- function(station) {
  
  #importation des exports IED Carhyce 
  
  TC<-read.csv2("raw_data/1_Export_tables_calcaires.csv")
  AS<-read.csv2("raw_data/1_Export_depots_argilo_sableux.csv")
  CCE<-read.csv2("raw_data/1_Export_cotes_calcaires_est.csv")
  A<-read.csv2("raw_data/1_Export_ardennes.csv")
  
  # jointure des df des différents HER 
  
  IED_joint<-rbind(TC,AS,CCE,A)
  
  
  # mise en forme
  
  IED_joint<-IED_joint %>% 
    clean_names() %>%  # fonction qui génère des noms de colonne propres 
    mutate(date_realisation=as.Date(date_realisation, # changement de format 
                                    version_protocole=as.factor(version_protocole),
                                    nom_her_ou_dom_dominant=as.factor(nom_her_ou_dom_dominant))) 
  
  # remplacement du code et libellé de station pour la Slack et l'Helpe 
  
  ## Remplacement des codes/noms de la Slack à Ambleteuse par ceux de la Slack à Rinxent et de l'Helpe mineure à maroilles par l'Helpe mineure à grand fayt 
  
  IED_joint<-IED_joint %>% 
    mutate(code_station=as.character(code_station)) %>% 
    mutate(code_station = recode(code_station, "1090000" = "1000477", 
                             "1006000" = "1001131", 
                             .default = code_station),
           localisation_station_de_mesure = recode(localisation_station_de_mesure, "LA SLACK À AMBLETEUSE (62)" = "LA SLACK À RINXENT (62)", 
                                "L'HELPE MINEURE À MAROILLES (59)" = "HELPE MINEURE À GRAND FAYT (59)", 
                                .default = localisation_station_de_mesure)) 
  
 #filtre sur le code station souhaité   
  
  df_carhyce <-IED_joint %>% 
    filter(code_station == station)
  
  return(df_carhyce)
  
}

#### Mise en forme du data frame de données syrah_ce#### 
# A partir de table d'attributs fournie par K.Kreutzenberger, joignant données Carhyce et données SYRAH-CE 

miseenforme_syrah<-function(station){
  
  #importation fichier excel données syrah-ce 
  
  syrah<-read.xlsx("raw_data/3_syrah_export_2012_2017_DISC.xlsx")
  
  #modification des noms de colonnes pour avoir des noms "propres" 
  
  syrah<-syrah %>% 
    clean_names() # fonction du package "janitor" qui remplace automatiquement les noms de colonnes par des noms "propres" 
  
  #remplacement des noms/codes de stations dans le data frame de données Syrah   
  
  syrah<-syrah %>% 
    mutate(code_station=as.character(code_station)) %>% 
    dplyr::mutate(code_station = recode(code_station, "1090000" = "1000477", "1006000"= "1001131", 
                                        .default = code_station),
                  localisation_station_de_mesure = recode(localisation_station_de_mesure, "LA SLACK À AMBLETEUSE (62)" = "LA SLACK À RINXENT (62)", 
                                                          "L'HELPE MINEURE À MAROILLES (59)" = "HELPE MINEURE À GRAND FAYT (59)", 
                                                          .default = localisation_station_de_mesure),
                  x2012_toponyme_s = recode(x2012_toponyme_s, "LA SLACK À AMBLETEUSE (62)" = "LA SLACK À RINXENT (62)", 
                                            "L'HELPE MINEURE À MAROILLES (59)" = "HELPE MINEURE À GRAND FAYT (59)", 
                                            .default = x2012_toponyme_s),
                  x2017_toponyme_s= recode(x2017_toponyme_s, "LA SLACK À AMBLETEUSE (62)" = "LA SLACK À RINXENT (62)", 
                                           "L'HELPE MINEURE À MAROILLES (59)" = "HELPE MINEURE À GRAND FAYT (59)", 
                                           .default = x2017_toponyme_s),
                  x2017_disc_toponyme_s = recode(x2017_disc_toponyme_s, "LA SLACK À AMBLETEUSE (62)" = "LA SLACK À RINXENT (62)", 
                                                 "L'HELPE MINEURE À MAROILLES (59)" = "HELPE MINEURE À GRAND FAYT (59)", 
                                                 .default = x2017_disc_toponyme_s)) 
  
  syrah<-syrah %>% 
    mutate(date_realisation=as.Date(date_realisation, origin = "1899-12-30"),
      x2012_ratio_drai=as.numeric(x2012_ratio_drai),
           x2017_ratio_drai=as.numeric(x2017_ratio_drai),
           x2012_taux_vcom=as.numeric(x2012_taux_vcom),
           x2017_taux_vcom=as.numeric(x2017_taux_vcom),
           x2017_disc_ratio_arti=as.numeric(x2017_disc_ratio_arti),
           x2017_surf_agri=as.numeric(x2017_surf_agri),
           x2012_taux_recti=as.numeric(x2012_taux_recti),
           x2017_taux_recti=as.numeric(x2017_taux_recti),
           x2012_taux_ur=as.numeric(x2012_taux_ur),
           x2017_taux_ur=as.numeric(x2017_taux_ur),
           x2012_taux_dig=as.numeric(x2012_taux_dig),
           x2017_taux_dig=as.numeric(x2017_taux_dig),
           x2017_surf_artif=as.numeric(x2017_surf_artif),
           x2012_ratio_irri=as.numeric(x2012_ratio_irri),
           x2017_ratio_irri=as.numeric(x2017_ratio_irri),
           x2012_long_vcom = as.numeric(x2012_long_vcom),
           x2017_long_vcom = as.numeric(x2017_long_vcom),
           x2012_taux_fcht= as.numeric(x2012_taux_fcht),
           x2017_taux_fcht= as.numeric(x2017_taux_fcht)
           
           )
  
syrah_sta<-syrah %>% 
  filter(code_station==station) %>% 
  filter(date_realisation==max(date_realisation))

return(syrah_sta)

}



#### Création de la carte de localisation de la station #### 

map_localisation<-function (station) {
  
  map_dpt<-st_read(dsn = "sig/5_departements.shp", stringsAsFactors = FALSE) # couche shp des départements 
  
  map_station<-st_read(dsn= "sig/test.shp", stringsAsFactors = FALSE, quiet=FALSE) # couche des stations dispo sur l'IED fin 2023 
  
  
  map_dpt_geom<-map_dpt$geometry #objet avec uniquement la géométrie des départements 
  plot(map_dpt_geom) # plot des départements pour vérifier 
  
  test<- map_station %>%   #selection uniquement de la station qui nous intéresse et de la colonne géométrie 
    filter(Code_sta == station) %>% 
    select(geometry)
  plot(test) # plot de la station pour vérifier 
  
  final_map<- ggplot()+
    geom_sf(data= map_dpt_geom)+
    geom_sf(data= test, color="red")+
    ggtitle("Localisation de la station dans les Hauts-de-France")+
    theme(plot.title = element_text(hjust = 0.5, size=12))
  
 return(final_map) 
  
}