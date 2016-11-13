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

df_long %>% 
  group_by(CodeInsee) %>%  #les opérations qui suivent sont par commune
  arrange(-Voix) %>% #on trie par ordre décroissant du nombre de voix dans chaque commune
  slice(1:2) %>% #on ne garde que les deux premières lignes
  ungroup %>% #le reste se fait sur l'ensemble de la table
  arrange(-Inscrits) %>% 
  #tri sur l'ensemble des inscrits 
  mutate(pourcent_ins = Voix / Inscrits * 100)

# long vers large

df_large <- df_long %>% 
  spread(Candidat, Voix)

# calculer le % des inscrits

df_long <- df_long %>% 
  mutate(pourcent.ins = Voix / Inscrits * 100)


# on s'amuse à aller et venir entre formats long et large, et à séparer et réunifier des colonnes 
df_long %>% 
  gather(type, valeur, Voix, pourcent.ins) %>%
  unite(Candidat, Candidat, type, sep = "_") %>% 
  spread(Candidat, valeur) %>% 
  gather(Candidat, Voix, ARTHAUD_pourcent.ins:SARKOZY_Voix) %>% 
  separate(Candidat, c("Nom", "type"), sep = "_")


df_idf <- df_long %>% 
  # ne garder que les lignes des départements d'Ile-de-France
  # %in% est un opérateur 
  filter(CodeDpt %in% c("75", "77", "78", "91", "92", "93", "94", "95"))

df_hollande <- df_long %>% 
  filter(Candidat %in% "HOLLANDE")

df_hollande %>% 
  arrange(desc(pourcent.ins))

# sélectionner les communes dans lesquelles Le Pen obtient plus de 20 % des inscrits, tout en conservant les résultats des autres candidats

df_lepenistes <- df_large %>% 
  mutate(LePen_ins = `LE PEN` / Inscrits * 100) %>% 
  filter(LePen_ins > 20)

df_large %>% 
  mutate_at(vars(ARTHAUD:SARKOZY), funs(. / Inscrits * 100))

# agrégation par département

df_large %>% 
  group_by(CodeDpt) %>% 
  summarise(Inscrits = sum(Inscrits))

df_large %>% 
  group_by(CodeDpt) %>% 
  summarise_at(vars(Inscrits, Exprimés, Votants, ARTHAUD:SARKOZY), funs(sum(.)))


## agrégation

ze <- read_csv2("https://frama.link/ze_csv")

df_large <- left_join(df_large, ze, by = c("CodeInsee" = "CODGEO"))

df_ze_lepen <- df_large %>% 
  group_by(LIBZE2010) %>% 
  summarise_at(vars(Inscrits, Exprimés, Votants, ARTHAUD:SARKOZY), funs(sum(.))) %>% 
  mutate(LePen_ins = `LE PEN` / Inscrits * 100) %>% 
  arrange(desc(LePen_ins))

write_excel_csv(df_ze_lepen, path = "./resultats_ze.csv", na = "")


df_large %>% 
  mutate(Abstention_ins = (Inscrits-Votants) / Inscrits * 100) %>% 
  ggplot(aes(x = Abstention_ins)) +
  geom_density() +
  theme_bw()

df_large %>% 
  mutate(Abstention_ins = (Inscrits - Votants) / Inscrits * 100) %>% 
  ggplot(aes(x = Abstention_ins)) +
  geom_histogram() +
  theme_bw()

df_large %>% 
  mutate(Abstention_ins = (Inscrits - Votants) / Inscrits * 100,
         LePen_exp = (`LE PEN` / Exprimés * 100)) %>% 
  ggplot(aes(x = Abstention_ins,
             y = LePen_exp)) +
  geom_point(alpha = 0.1)

pdf("./monimage.pdf")
df_large %>% 
  mutate(Abstention_ins = (Inscrits - Votants) / Inscrits * 100,
         LePen_exp = (`LE PEN` / Exprimés * 100)) %>% 
  ggplot(aes(x = Abstention_ins,
             y = LePen_exp)) +
  geom_point(alpha = 0.1) +
  geom_smooth()
dev.off()
