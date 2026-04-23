###########################################################################################################################
#######                                  Combinaison des data iNaturalist et GBif                                   #######
###########################################################################################################################



############################################# téléchargement des packages #################################################

# installation si nécessaire:
# install.packages(c("rgbif", "rnaturalearth", "ggplot2", "rinat","raster", "dplyr", "sf"))

# chargement des packages
library(rgbif)                                                    # accès aux données de GBIF
library(rnaturalearth)                                            # cartographie des pays
library(ggplot2)                                                  # faire les graphique
library(rinat)                                                    # accès aux données de iNaturalist
library(raster)                                                   # analyse spatiale
library(dplyr)                                                    # manipulation des tableaux
library(sf)                                                       # cartographie moderne

# faire comprendre à R de faire les calculs comme si tout était sur une surface plane (géométrie Euclidienne)
sf_use_s2(FALSE)                                                  # évite message d'erreurs



#####################################################  Carte: Suisse   #####################################################

# téléchargement de la carte de la suisse 
Switzerland <- ne_countries(
  scale = "medium",                                               # niveau de définition                       
  returnclass = "sf",                                             # format de l'objet
  country = "Switzerland"                                         # pays ciblé
)

# visualisation préliminaire de la carte suisse vide
x11()
ggplot(data = Switzerland) +                                      # pays précédemment ciblé
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  theme_classic(
)                                                                 # style de graphique épuré sans quadrillage 



################################################ liste d'espèce à traiter ##################################################

liste <- c("Coccinella septempunctata", "Harmonia axyridis")      # choix des espèces à étudier
resultats <- list()                                               # endroit où stocker les tableaux



################################################## boucle de traitement ####################################################

for (nom in liste) {                                              # création d'une boucle 
  
  message("--- Traitement de : ", nom, " ---")


# traitement pour les données gBif
  gbif_raw <- occ_data(                                           # téléchargement des données gBif 
    scientificName = nom,                                         # une espèce après l'autre de la liste prédéfinie
    hasCoordinate = TRUE,                                         # filtre que les occurences avec données géographiques
    limit = 5000,                                                 # nombre maximum d'occurences à télécharger (5000)
    country = "CH")$data                                          # extraction du tableau des occurences 

# transformation des points gBif en objets spatiaux
  data_spatiale_gbif <- st_as_sf(
    gbif_raw,                                                     # donnée brute avec point qui dépassent
    coords = c("decimalLongitude", "decimalLatitude"),            # coordonnées des points
    crs = 4326                                                    # format GPS standard (similaire à la carte)
  )

# garder uniquement les points gBif dans les contours suisses
  data_suisse_gbif <- st_intersection(data_spatiale_gbif, Switzerland)

# visualisation des points gBif sur la carte de la Suisse
  print(
  ggplot(data = Switzerland) +                                    # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_sf(                                                        # ajout des points occurrences sur la carte
    data = data_suisse_gbif,                                      # source des data 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#b50072",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title= paste("Répartition", nom, "(Gbif)"),                   # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude"                                                # titre axe y
  ) +
  theme_classic())  

# création du tableau gBif
  coords_gbif <- st_coordinates(data_suisse_gbif)                 # extraction des coordonnées GPS
  data_gbif <- data.frame(
    species   = nom,                                              # nom espèce 
    latitude  = coords_gbif[, "Y"],                               # latitude (X)
    longitude = coords_gbif[, "X"],                               # longitude (Y)
    date_obs  = as.Date(data_suisse_gbif$eventDate),              # date simple (J-M-A) sans heure
    source    = "gbif"                                            # source des données (gBif)
  )

# vérification du tableau gBif
  summary(data_gbif)


# traitement pour les données iNaturalist
  inat_raw <- get_inat_obs(
    maxresults = 5000,                                            # limitation du nombre d'occurences téléchargées 
    query = nom,                                                  # nom espèce à rechercher (Coccinella septempunctata)
    place_id = "switzerland",                                     # restriction aux occurences situées en suisse
  )

# transformation des points iNaturalist en objets spatiaux
  data_spatiale_inat <- st_as_sf(
  inat_raw,                                                       # donnée brute avec point qui dépassent
  coords = c("longitude", "latitude"),                            # coordonnées des points
  crs = 4326)                                                     # format GPS standard (similaire à la carte)

# garder uniquement les points iNaturalist dans les contours suisses
  data_suisse_inat <- st_intersection(data_spatiale_inat, Switzerland)   

# visualisation des points iNaturalist sur la carte de la Suisse
print(                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      #  création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_sf(                                                        # ajout des points occurrences sur la carte
    data = data_suisse_inat,                                      # source des data 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#ff6cc9",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title=paste("Répartition", nom, "(iNaturalist)"),             # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude")                                               # titre axe y
 +
  theme_classic())                                                # style de graphique épuré sans quadrillage 

# création du tableau iNaturalist
  coords_inat <- st_coordinates(data_suisse_inat)                 # extraction des coordonnées GPS
  data_inat <- data.frame(
    species   = nom,                                              # nom espèce 
    latitude  = coords_inat[, "Y"],                               # latitude (X)
    longitude = coords_inat[, "X"],                               # longitude (Y)
    date_obs  = as.Date(data_suisse_inat$observed_on),            # date simple (J-M-A) sans heure
    source    = "inat"                                            # source des données (iNat)
  )

# vérification du tableau gBif
  summary(data_inat)


# stockage des tableaux de l'espèce dans 'résultat'
  resultats[[nom]] <- rbind(data_gbif, data_inat)
}



########################################## combinaison en un seul grand tableau ############################################

# création du tableau final en combinant tous les tableaux 
full_data <- bind_rows(resultats)                                 # 'bind_rows' (bout à bout) VS 'merge' (ajout colonne)        

# vérification du tableau global
head(full_data)                                                   # affiche les 6 premières lignes du tableau 
table(full_data$source, useNA = "ifany")                          # calcul N(gbif)=7337, N(iNat)=4190  & NA=0

# vérification des espèces
table(full_data$species)                                          # pas d'espèce parasite (e.g. papillon)

# vérification des dates
summary(full_data$date_obs)                                       # grand pannel temporel (1880-2026) pour étude temps

# suppression des observations sans dates vides pour éviter les problèmes d'analyse (NA=210)
full_data <- full_data %>% filter(!is.na(date_obs))
summary(full_data$date_obs)                                       # NA=0

# suppression des doublons car gBif synchronisent souvent les données avec iNat
full_data_unique <- full_data %>%
  distinct(species,latitude,longitude,date_obs,.keep_all = TRUE)
table(full_data$source, useNA = "ifany")                          # calcul N(gbif)=7129, N(iNat)=4188  & NA=0

# graphique final 
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = full_data,                                             # source des data 
    aes(x = longitude, y = latitude, fill = species),             # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    color = "white",                                              # couleur de la bordure des points
    alpha = 0.8
  ) +
  scale_fill_manual(                                              # choix des couleurs pour les points iNat & Gbif 
    values = c("Harmonia axyridis" = "#f08800",                 # orange pour Harmonia axyridis
    "Coccinella septempunctata" = "#b50072"),                   # rose pour Coccinella septempunctata
    name = "Espèces",                                             # titre de la légende des couleurs
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Harmonia axyridis (Gbif & iNat)",          # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 
