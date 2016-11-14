# script du jour 4 de la formation.

library(tidyverse)
reserve <- read_csv2("./reserve-assemblee-2013.csv", 
                     col_types = cols(
                       ID_SUBVENTION = col_integer(),
                       NOM_BENEFICIAIRE = col_character(),
                       DESCRIPTION_SUBVENTION = col_character(),
                       ADRESSE_BENEFICIAIRE = col_character(),
                       PROGRAMME_PJL_FINANCE = col_character(),
                       MONTANT_SUBVENTION = col_integer(),
                       PRENOM_NOM_DEPUTE_NOSDEPUTES = col_character(),
                       ID_DEPUTE_AN = col_character(),
                       ID_NOSDEPUTES = col_integer(),
                       NOM_DEPUTE_NOSDEPUTES = col_character(),
                       PRENOM_DEPUTE_NOSDEPUTES = col_character(),
                       SEXE_DEPUTE = col_character(),
                       DATE_NAISSANCE_DEPUTE = col_date(format = ""),
                       LIEU_NAISSANCE_DEPUTE = col_character(),
                       ID_DEPARTEMENT = col_character(),
                       DEPARTEMENT_NOSDEPUTES = col_character(),
                       NUMERO_CIRCONSCRIPTION = col_integer(),
                       DATE_DEBUT_MANDAT = col_date(format = ""),
                       DATE_FIN_MANDAT = col_date(format = ""),
                       MANDAT_CLOS = col_integer(),
                       GROUPE_SIGLE = col_character(),
                       PARTI_RATTACHEMENT_FINANCIER = col_character(),
                       PROFESSION = col_character(),
                       PLACE_HEMICYCLE = col_integer(),
                       URL_DEPUTE_AN = col_character(),
                       SLUG_NOSDEPUTES = col_character(),
                       URL_DEPUTE_NOSDEPUTES = col_character(),
                       API_NOSDEPUTES = col_character(),
                       NB_MANDATS = col_integer()
                     ))

library(stringr)

reserve <- reserve %>% 
  # création d'un code circo sur deux caractères
  mutate(CodeCirco = str_pad(NUMERO_CIRCONSCRIPTION, 2, "left", "0")) %>% 
  mutate(CodeCirco = paste(ID_DEPARTEMENT, CodeCirco, sep = "-")) # concaténation

# barplot classique
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(stat = "count")

# barplot stacked avec couleur par groupe politique
reserve %>% 
  ggplot(aes(x = 1)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count", position = position_fill())
