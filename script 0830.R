library(openxlsx)
library(tidyverse)




#import files 5-siffrig SNI

dat <- read.csv("F:/Digital spetskompetens/Flödesanalys/Skill relatedness/matriser_csv/SNI 2007/sni07_5_relatedness.csv",
                header = TRUE, sep = "\t", fileEncoding = "utf-8"
      )


#import definition av it-bransch

itsni <-  read.xlsx(xlsxFile = "F:/Digital spetskompetens/Flödesanalys/Skill relatedness/sni_urval.xlsx")


#klassifikation av relatedness
grupp_namn <- c("Ej klassad", 
                    "Orelaterade",
                    "Svagt relaterade",
                    "Måttligt relaterade",
                    "Starkt relaterade",
                    "Mycket starkt relaterade")

grupp_num_range <- c("*",
                    "-1,0 - 0,1", 
                     "0,1 - 0,33",
                     "0,33 - 0,8",
                     "0,8 - 0,95",
                     "0,95 - 1,0"
                     )
grupp <- grupp_namn

grupp_join <- cbind.data.frame(grupp, grupp_num_range)


#filtrera fram 10 största skill relatedness för respektive it-delbransch
res1 <- dat %>%
  filter(sni07_5_i %in% itsni$Detaljgrupp,
         sni07_5_i != sni07_5_j) %>%
  group_by(sni07_5_i) %>%
  slice_max(rel_index, n = 10) %>%
  mutate(sni07_5_i_text = str_sub(as.character(sni07_5_i_text), 8)) %>%
  mutate(sni07_5_j_text = str_sub(as.character(sni07_5_j_text), 8)) %>%
  mutate(itbransch = factor(case_when(
    sni07_5_j %in% itsni$Detaljgrupp == 1 ~ "IT-bransch",
                              TRUE ~ "Annan bransch"), 
    levels = c("IT-bransch", "Annan bransch"))) %>%
  mutate(grupp = case_when(rel_index <= 0.1 ~ "Orelaterade",
                           rel_index > 0.1 & rel_index <= 0.33  ~ "Svagt relaterade",
                           rel_index > 0.33 & rel_index <= 0.8  ~ "Måttligt relaterade",
                           rel_index > 0.8 & rel_index <= 0.95  ~ "Starkt relaterade",
                           rel_index > 0.95 & rel_index <= 1  ~ "Mycket starkt relaterade",
                           TRUE ~ "Ej klassad" )) %>%
  mutate(grupp = factor(grupp,
                        levels = c(
                    grupp_namn))) %>%
  left_join(grupp_join, by = "grupp" ) %>%
  ungroup()


res_hogutb <- dat %>%
  filter(sni07_5_i %in% itsni$Detaljgrupp,
         sni07_5_i != sni07_5_j) %>%
  group_by(sni07_5_i) %>%
  slice_max(rel_index_hogutb, n = 10) %>%
  mutate(sni07_5_i_text = str_sub(as.character(sni07_5_i_text), 8)) %>%
  mutate(sni07_5_j_text = str_sub(as.character(sni07_5_j_text), 8)) %>%
  mutate(itbransch = factor(case_when(
    sni07_5_j %in% itsni$Detaljgrupp == 1 ~ "IT-bransch",
    TRUE ~ "Annan bransch"), 
    levels = c("IT-bransch", "Annan bransch"))) %>%
  mutate(grupp = case_when(rel_index_hogutb <= 0.1 ~ "Orelaterade",
                           rel_index_hogutb > 0.1 & rel_index_hogutb <= 0.33  ~ "Svagt relaterade",
                           rel_index_hogutb > 0.33 & rel_index_hogutb <= 0.8  ~ "Måttligt relaterade",
                           rel_index_hogutb > 0.8 & rel_index_hogutb <= 0.95  ~ "Starkt relaterade",
                           rel_index_hogutb > 0.95 & rel_index_hogutb <= 1  ~ "Mycket starkt relaterade",
                           TRUE ~ "Ej klassad" )) %>%
  mutate(grupp = factor(grupp,
                        levels = c(
                          grupp_namn))) %>%
  left_join(grupp_join, by = "grupp" ) %>%
  ungroup()


res_hogink <- dat %>%
  filter(sni07_5_i %in% itsni$Detaljgrupp,
         sni07_5_i != sni07_5_j) %>%
  group_by(sni07_5_i) %>%
  slice_max(rel_index_hogink, n = 10) %>%
  mutate(sni07_5_i_text = str_sub(as.character(sni07_5_i_text), 8)) %>%
  mutate(sni07_5_j_text = str_sub(as.character(sni07_5_j_text), 8)) %>%
  mutate(itbransch = factor(case_when(
    sni07_5_j %in% itsni$Detaljgrupp == 1 ~ "IT-bransch",
    TRUE ~ "Annan bransch"), 
    levels = c("IT-bransch", "Annan bransch"))) %>%
  mutate(grupp = case_when(rel_index_hogink <= 0.1 ~ "Orelaterade",
                           rel_index_hogink > 0.1 & rel_index_hogink <= 0.33  ~ "Svagt relaterade",
                           rel_index_hogink > 0.33 & rel_index_hogink <= 0.8  ~ "Måttligt relaterade",
                           rel_index_hogink > 0.8 & rel_index_hogink <= 0.95  ~ "Starkt relaterade",
                           rel_index_hogink > 0.95 & rel_index_hogink <= 1  ~ "Mycket starkt relaterade",
                           TRUE ~ "Ej klassad" )) %>%
  mutate(grupp = factor(grupp,
                        levels = c(
                          grupp_namn))) %>%
  left_join(grupp_join, by = "grupp" ) %>%
  ungroup()

  

#export
  
res <- list("10_mest_related" = res1, res_hogutb, res_hogink)
write.xlsx(res, file="F:/Digital spetskompetens/Flödesanalys/Skill relatedness/results/results.xlsx", sheetName="10_mest_related", row.names=FALSE)
  

