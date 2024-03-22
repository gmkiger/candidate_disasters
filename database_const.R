#### Database Construction ####
install.packages("geobr")
install.packages("stringi")
install.packages("stringr")
library(geobr)
library(stringi)
library(stringr)
library(tidyverse)
library(readxl)

#raw files
raw_96 <- read.csv("Data/1996_candidates.csv")
raw_00 <- read.csv("Data/2000_candidates.csv")
raw_04 <- read.csv("Data/2004_candidates.csv")
raw_08 <- read.csv("Data/2008_candidates.csv")
raw_12 <- read.csv("Data/2012_candidates.csv")
raw_16 <- read.csv("Data/2016_candidates.csv")
raw_20 <- read.csv("Data/2020_candidates.csv")

#candidates per city per year - in other file
cands_96 <- raw_96 %>% group_by(Município) %>%
  summarise(count=n()) %>%
  rename("1996" = "count") %>%
  mutate(city = stri_trans_general(Município, "Latin-ASCII")) %>%
  select(2:3) %>%
  mutate(city = tolower(city))

#merging into one 

df_cand <- cands_00 %>% full_join(cands_04) %>% 
  full_join(cands_08) %>%
  full_join(cands_12) %>%
  full_join(cands_16) %>%
  full_join(cands_20) %>%
  select(city, "2000", "2004", "2008", "2012", "2016", "2020") 

df_fix <- df_cand %>% arrange(city) 
df_fix <- df_fix %>% 
  mutate(fix = if_else(is.na(df_fix$"2000")|
                         is.na(df_fix$"2004")|
                         is.na(df_fix$"2008")|
                         is.na(df_fix$"2012")|
                         is.na(df_fix$"2016")|
                         is.na(df_fix$"2020"),1,0)) %>%
  filter(fix == 1)


test <- df_cand %>% filter(str_detect(city,"este"))

#gis stuff 
muni_raw <- read_municipality(year = 2010) 
muni_fix1 <- muni_raw %>%
  mutate(city = stri_trans_general(name_muni, "Latin-ASCII"))

#fixing names of cities for merge 


muni2 <- muni_fix1 %>% filter(str_detect(city,"'"))
muni3 <- muni_fix1 %>% filter(str_detect(city,"este")) %>% 
  arrange(city) 

muni4 <- as.data.frame(muni3$city)

write.csv(muni4, "muni4.csv")
write.csv(test,"test.csv")

#renaming 
raw_12 <- raw_12 %>% rename(elec_year = "Ano.de.eleição", 
                            office = "Cargo",
                            coalition = "Coligação",
                            color_race = "Cor.raça",
                            app_status = "Detalhe.da.situação.de.candidatura",
                            marital_status = "Estado.civil",
                            age_range = "Faixa.etária",
                            federation = "Federação",
                            gender = "Gênero",
                            edu = "Grau.de.instrução",
                            city = "Município",
                            nationality = "Nacionalidade",
                            social_name = "Nome.social",
                            occupation = "Ocupação",
                            re_election = "Reeleição",
                            region = "Região",
                            party = "Sigla.partido",
                            app_status2 = "Situação.de.candidatura",
                            total = "Situação.de.totalização",
                            election_type = "Tipo.eleição",
                            uf = "UF",
                            cand = "Quantidade.de.candidatos",
                            elected = "Quantidade.de.candidatos.eleitos",
                            round_2 = "Quantidade.de.candidatos.para.o.2º.turno",
                            not_elec = "Quantidade.de.candidatos.não.eleitos",
                            sub_cand = "Quantidade.de.candidatos.suplentes",
                            uninformed = "Quantidade.de.candidatos.não.informados")

cities_12 <- unique(raw_12$city)

#### disaster data 
disaster <- read_excel("Brazilian_disaster_datasets.xlsx")

#checking if cities are untreated
city_d <- unique(disaster$Municipality)
city2 <- disaster %>% 
  group_by(Municipality) %>% 
  slice(1) %>% 
  ungroup()
