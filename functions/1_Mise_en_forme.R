# fonctions necessaires au Rmarkdown 1_Mise en forme 
## Pour les explications, cf Rmarkdown  



####Calcul du Qb_Q2#### 

function_Qb_Q2 <- function(code_region, Q2_fr, hydro_carhyce){
  
  # info sites hydro 
  
  info_sites_hydro <- get_hydrometrie_sites(     # fonction d'importation du package hubeau 
    list(code_region = code_region)) %>% # code de la région hdf 
    select(code_site:type_site, code_departement, libelle_commune, libelle_cours_eau, surface_bv) #l'information indispensable au calcul final du Qb_Q2 est ici la surface de BV
  
  
  # Q2 sites hydro (totaux)
  
  Q2_fr<-Q2_fr %>% 
    mutate( Code_sta=as.character(Code_sta),
           Surface_BV_hydro= as.numeric(Surface_BV_hydro),
           Surface_BV_topo= as.numeric(Surface_BV_topo),
           Q2_m3 = as.numeric(Q2_m3),
           )
  
  
  #filtre Q2 sur les sites d'interet: 
  
  hydro_carhyce<-hydro_carhyce %>% 
    mutate(code_site_hydro =substr(code_sta_hydro, 1,8))
  
  
  ## Filtre de Q2_hdf sur les sites d'interet 
  
  sites_interet<-unique(hydro_carhyce$code_site_hydro) 
  
  Q2_fr<-Q2_fr %>% 
    filter(Code_sta %in% sites_interet) # filtre des Q23 sur ces codes de sites (Attention aux éventuels codes de sites rassemblant plusieurs codes stations)
  
  
  # 1ère jointure entre hydro_carhyce et carhyce_hdf_ofb pour récupérer la surface de BV amont aux stations carhyce : 
  
  carhyce_hdf_ofb_bv<-carhyce_hdf_ofb %>%  #selection des colonnes de code station et surface de BV 
    select(code_sta_HM=Code_sta, surface_bv_km2, Date, Q_plein_bord)
  
  hydro_carhyce<-hydro_carhyce %>% 
    mutate(code_sta_HM = as.character(code_sta_HM))
  
  hydro_carhyce_vf<-left_join(carhyce_hdf_ofb_bv, hydro_carhyce, by="code_sta_HM")
  
  
  # 2ème jointure entre hydro_carhyce_vf et Q_interet pour récupérer les Q2 et surface de BV amont aux stations hydro : 
  
  Q2_fr<-Q2_fr %>% 
    rename(code_site_hydro=Code_sta)
  
  hydro_carhyce_vf<-hydro_carhyce_vf %>% 
    mutate(code_site_hydro =substr(code_sta_hydro, 1,8))
  
  hydro_carhyce_vf<-left_join(hydro_carhyce_vf, 
                          Q2_fr, 
                          by="code_site_hydro") 
  
  
  # Application de la formule de Myer 
  
  hydro_carhyce_vf <- hydro_carhyce_vf %>%
    mutate(Q2_carhyce = case_when(
      superposition == TRUE ~ Q2_m3,
      #si les deux stations sont superposées, on prends le Q2 de la station hydro
      superposition == FALSE ~ as.numeric(Q2_m3 *(surface_bv_km2/ Surface_BV_topo) ^ 0.8)
    )) #sinon on le calcule avec la formule de Myer 
  
  
  # calcul du rapport Qb_Q2 
  
  hydro_carhyce_vf<-hydro_carhyce_vf %>% 
    mutate(Qb_Q2= Q_plein_bord/Q2_carhyce)
  
  
  
  return(hydro_carhyce_vf)
  
}


#### Calcul Q2 à partir données de débit journalier#### 

function_QT<-function(code_station, debut, fin, Tr){
  
  #importation via l'API hub'eau des données de débits 
  
  debits <- get_hydrometrie_obs_elab(     # fonction d'importation du package hubeau 
    list(code_entite = code_station,   # code de la station 
         date_debut_obs_elab = debut,  #date de début de l'importation 
         date_fin_obs_elab=fin,  # date de fin de l'importation 
         grandeur_hydro_elab = "QmJ")) %>%  # grandeur importée (QmJ= débits moyens journaliers, QmM= débits moyens mensuels)
    select(code_station:resultat_obs_elab) %>%  #sélection des colonnes d'intêret 
    mutate(annee = lubridate::ymd(date_obs_elab), # création d'une colonne année au format date 
           annee = lubridate::year(annee))
  
  # calcul des débits maximums pour chaque année : 
  
  debits<-debits %>% 
    dplyr::group_by(annee) %>% 
    dplyr::summarise(crue=max(resultat_obs_elab)) 
  
  # calcul du débit de crue de période de retour Tr et affichage du graphique et des informations sur le modèle (vérifier que la p-value est significative)
  
  probplotQT<-ProbPlot(data_obs=debits$crue, T_rp=Tr )
  
  return(unname(probplotQT[[2]]))
  
}




