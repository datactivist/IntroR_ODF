# on veut transformer un messy data, le résultat du premier tour de l'élection présidentielle de 2012, en tidy data
# Source : https://www.data.gouv.fr/fr/datasets/election-presidentielle-2012-resultats-572126/

P2012T1 <- read_csv2("./presidentielle2012T1.csv", 
                     col_types = cols(
                       `Code du département` = col_character(),
                       `Libellé du département` = col_character(),
                       `Code de la commune` = col_character(),
                       `Libellé de la commune` = col_character(),
                       Inscrits = col_integer(),
                       Abstentions = col_integer(),
                       `% Abs/Ins` = col_double(),
                       Votants = col_integer(),
                       `% Vot/Ins` = col_double(),
                       `Blancs et nuls` = col_integer(),
                       `% BlNuls/Ins` = col_double(),
                       `% BlNuls/Vot` = col_double(),
                       Exprimés = col_integer(),
                       `% Exp/Ins` = col_double(),
                       `% Exp/Vot` = col_double(),
                       Sexe = col_logical(),
                       Nom = col_character(),
                       Prénom = col_character(),
                       Voix = col_integer(),
                       `% Voix/Ins` = col_double(),
                       `% Voix/Exp` = col_double(),
                       Sexe_1 = col_logical(),
                       Nom_1 = col_character(),
                       Prénom_1 = col_character(),
                       Voix_1 = col_integer(),
                       `% Voix/Ins_1` = col_double(),
                       `% Voix/Exp_1` = col_double(),
                       Sexe_2 = col_character(),
                       Nom_2 = col_character(),
                       Prénom_2 = col_character(),
                       Voix_2 = col_integer(),
                       `% Voix/Ins_2` = col_double(),
                       `% Voix/Exp_2` = col_double(),
                       Sexe_3 = col_character(),
                       Nom_3 = col_character(),
                       Prénom_3 = col_character(),
                       Voix_3 = col_integer(),
                       `% Voix/Ins_3` = col_double(),
                       `% Voix/Exp_3` = col_double(),
                       Sexe_4 = col_character(),
                       Nom_4 = col_character(),
                       Prénom_4 = col_character(),
                       Voix_4 = col_integer(),
                       `% Voix/Ins_4` = col_double(),
                       `% Voix/Exp_4` = col_double(),
                       Sexe_5 = col_logical(),
                       Nom_5 = col_character(),
                       Prénom_5 = col_character(),
                       Voix_5 = col_integer(),
                       `% Voix/Ins_5` = col_double(),
                       `% Voix/Exp_5` = col_double(),
                       Sexe_6 = col_character(),
                       Nom_6 = col_character(),
                       Prénom_6 = col_character(),
                       Voix_6 = col_integer(),
                       `% Voix/Ins_6` = col_double(),
                       `% Voix/Exp_6` = col_double(),
                       Sexe_7 = col_character(),
                       Nom_7 = col_character(),
                       Prénom_7 = col_character(),
                       Voix_7 = col_integer(),
                       `% Voix/Ins_7` = col_double(),
                       `% Voix/Exp_7` = col_double(),
                       Sexe_8 = col_character(),
                       Nom_8 = col_character(),
                       Prénom_8 = col_character(),
                       Voix_8 = col_integer(),
                       `% Voix/Ins_8` = col_double(),
                       `% Voix/Exp_8` = col_double(),
                       Sexe_9 = col_character(),
                       Nom_9 = col_character(),
                       Prénom_9 = col_character(),
                       Voix_9 = col_integer(),
                       `% Voix/Ins_9` = col_double(),
                       `% Voix/Exp_9` = col_double()
                     ))

# glimpse(P2012T1)

# library(dplyr)
# library(tidyr)

## on ne garde que les colonnes de nom

noms <- P2012T1 %>% 
  select(starts_with("Nom"))

## on ne garde que la première ligne

noms <- noms %>% 
  slice(1)

## On transforme en vecteur 
noms <- as.vector(t(noms))

## on ne garde que les colonnes utiles

df <- P2012T1 %>% 
  select(`Code du département`, `Code de la commune`, Inscrits, Votants, Exprimés, starts_with("Voix")) 

## on change les noms des colonnes pour y mettre le nom des candidats
names(df)[6:15] <- noms

glimpse(df)
# c'est du tidy data !

# charger le package stringr pour les manipulations de chaînes de caractère
library(stringr)

# il faut normaliser la longueur des codes département et codes communes avec str_pad
df <- df %>% 
  mutate(CodeDpt = str_pad(`Code du département`, 2, "left", "0")) %>% 
  mutate(CodeCommune = str_pad(`Code de la commune`, 3, "left", "0")) 
  
# on concatène code dpt et code commune pour obtenir un code Insee

df <- df %>% 
  mutate(CodeInsee = paste0(CodeDpt, CodeCommune))

# on se débarasse des variables devenues inutiles
df <- df %>% 
  select(-`Code du département`, -`Code de la commune`)

##### pivoter large/long et vice-versa

# large vers long

df_long <- df %>% 
  gather(Candidat, Voix, JOLY:HOLLANDE)
  

# long vers large

df_large <- df_long %>% 
  spread(Candidat, Voix)
