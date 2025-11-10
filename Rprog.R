library(rgbif)
library(readxl)
library(tidyr)
library(dplyr)
setwd("~/Stage/Projet-ConservES/Bioblitz")
donnes=read.csv("code_landscape_FR-RN_clemence (1).csv",fileEncoding = "Windows-1252")

#occ_count(scientificName = 'Agrimonia eupatoria',country="DE",year="*,2023")
donnes$GBIF_OCC_TOT=occ_count(scientificName = donnes$ScientificName,year="*,2023")


# calcul nombre occurence total GBIF entre 1000 et 2023 avec critère staut de l'occurence=présent --------

#on enlève les 1073 premières lignes qui n'ont pas les noms d'espèces
donnes2 = donnes %>% slice_tail(n=2004)
#on transfomre le liste des expece en un array
species_list2 <- donnes2$ScientificName
# on definit une fonction qui pour une sp données rend le nombre d'occurence
get_count <- function(sp) {
  tryCatch({
    occ_count(scientificName = sp, year = "1000,2023")
  }, error = function(e) NA)  # renvoie NA si une requête échoue
}

#test pour les 100 premières lignes
donnes3 =donnes2 %>% slice_head(n=100)
species_list3 <- donnes3$ScientificName
#on applique la fonction get_count a la liste des noms sc
counts3 <- sapply(species_list3, get_count)
#on constuir tableau recap pour les 100 1ères lignes
df_counts3 <- data.frame(
  ScientificName = species_list3,
  OccurrenceCount = counts3
)
#pour le tableau entier(en ayant déja enlevé les lignes 1 à 1073 incluse)
counts2 = sapply(species_list2,get_count)
df_counts2 = data.frame(
  ScientificName = species_list2,
  OccurrenceCount = counts2
)
#rends le début de la base de données coupé précédemment pour pouvoir tt recoller ensuite
donnes0=donnes %>% slice_head(n=1072)

# calcul nombre occurence en prenant en compte le pays entre 1000 et 2023 --------
#on repare de donnes2 où on a enlevé les lignes dont on a pas le nom de l'espèce
#on transfomre le liste des espece en un array
species_list2 <- donnes2$ScientificName
#on extrait la liste des pays
country_list2 <- substr(donnes2$EventID, 1, 2)
# on definit une fonction qui pour une sp données rend le nombre d'occurence en fonction du pays
get_count2 <- function(sp,ct) {
  tryCatch({
    occ_count(scientificName = sp, year = "1000,2023", country=ct)
  }, error = function(e) NA)  # renvoie NA si une requête échoue
}

#test pour les 100 premières lignes
donnes3 =donnes2 %>% slice_head(n=100)
species_list3 <- donnes3$ScientificName
country_list3 <- substr(donnes3$EventID, 1, 2)
#on combine les deux arrays
comb3 <- data.frame(species = species_list3, country = country_list3)
#on applique comme dans le cas précédent
countsctry3 <- apply(comb3, 1, function(row) get_count2(row['species'], row['country']))
df_counts3 <- data.frame(
  ScientificName = species_list3,country=country_list3,
  OccurrenceCountCountry = countsctry3
)
#après un essaie avec juste 100lignes qui se calcule plus rapidement et on compare resultats avec ce qu'on trve manuellemt sur GBIF
#compraison concluante qd dans GBIF on cherche l'espèce, puis on va dans occurences, puis à gauche, on appplique les critères : statut de l'occurence : présent, année entre 1000 et 2023 et pays =pays souhaité

#calcul pour table complète :
species_list2 <- donnes2$ScientificName
country_list2 <- substr(donnes2$EventID, 1, 2)
#on combine les deux arrays
comb2 <- data.frame(species = species_list2, country = country_list2)
#on applique comme dans le cas précédent
countsctry2 <- apply(comb2, 1, function(row) get_count2(row['species'], row['country']))
df_countsctry2 <- data.frame(
  ScientificName = species_list2,country=country_list2,
  OccurrenceCountCountry = countsctry2
)


# bilan intermédiaire -----------------------------------------------------

#on a récupéré deux tableaux :
#dans le premier on a deux colonnes, une avec le nom scientifique, correspondatn a la liste des noms scientifiques 
#des colonnes 1073 à 3077 et la deuxieme colonne corresponds au nomber d'observation dans gbif de cette espece entre 1000 et 2023
#dans le deuxieme tableau, on a 3 colonne : les noms scientifique comme precedent, 
#le pays où a été fait l'observation et le nmbr d'observation dans gbif de l'espece donnee dans le pays donné entre 1000 et 2023

# "puzzle" ----------------------------------------------------------------
#le but de cette section est de recoller les colonnes obtenues précédemment pour obtenir le tableau de base complété !
#pour cela on va utiliser le module dplyr avec les astuces de la feuille : https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-transformation.pdf
A2=donnes0 %>% select(GBIF_OCC_TOT)
A3=donnes0 %>% select(GBIF_OCC_PAYS)
B2=df_counts2 %>% select(OccurrenceCount) %>% rename_at("OccurrenceCount",~"GBIF_OCC_TOT")
B3=df_countsctry2 %>% select(OccurrenceCountCountry) %>% rename_at("OccurrenceCountCountry",~"GBIF_OCC_PAYS")
A2$GBIF_OCC_TOT <- as.character(A2$GBIF_OCC_TOT)
B2$GBIF_OCC_TOT <- as.character(B2$GBIF_OCC_TOT)
A3$GBIF_OCC_PAYS <- as.character(A3$GBIF_OCC_PAYS)
B3$GBIF_OCC_PAYS <- as.character(B3$GBIF_OCC_PAYS)
A2B2=bind_rows(A2,B2)
A3B3=bind_rows(A3,B3)
donnesterm <- donnes
donnesterm$GBIF_OCC_TOT...14=A2B2$GBIF_OCC_TOT
donnesterm$GBIF_OCC_PAYS...15=A3B3$GBIF_OCC_PAYS
