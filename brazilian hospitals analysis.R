library(tidyverse)
library(dslabs)
library(ggthemes)
library(ggrepel)
library(ggmap)
library(maps)
library(mapdata)
library(rworldmap)



#download the last version of CNES database in http://cnes.datasus.gov.br/pages/downloads/arquivosBaseDados.jsp

#import population dataset

demo_brasil <- read_excel("demo_brasil.xlsx")


#Import following CNES datasets:  tbEstabelecimento202003, rlEstabComplementar202003, tbMunicipio202003, tbEquipamento202003, tbTipoEquipamento202003,
  #tbLeito202003, 

tbEstabelecimento202003 <- read_delim("bbdd/tbEstabelecimento202003.csv", 
                                      +     ";", escape_double = FALSE, col_types = cols(`TO_CHAR(DT_ATUALIZACAO,'DD/MM/YYYY')` = col_date(format = "%m/%d/%Y")), 
                                      +     trim_ws = TRUE)

rlEstabComplementar202003 <- read_delim("bbdd/rlEstabComplementar202003.csv", 
                                        +     ";", escape_double = FALSE, col_types = cols(CO_UNIDADE = col_character(), 
                                                                                           +         `TO_CHAR(DT_ATUALIZACAO,'DD/MM/YYYY')` = col_date(format = "%m/%d/%Y"), 
                                                                                           +         `TO_CHAR(DT_ATUALIZACAO_ORIGEM,'DD/MM/YYYY')` = col_date(format = "%m/%d/%Y")), 
                                        +     trim_ws = TRUE)

tbMunicipio202003 <- read_delim("bbdd/tbMunicipio202003.csv", 
                                +     ";", escape_double = FALSE, trim_ws = TRUE)


tbEquipamento202003 <- read_delim("bbdd/tbEquipamento202003.csv", 
                                  +     ";", escape_double = FALSE, trim_ws = TRUE)

tbLeito202003 <- read_delim("bbdd/tbLeito202003.csv", 
                            +     ";", escape_double = FALSE, trim_ws = TRUE)

#joining tables


dat <- inner_join(tbEstabelecimento202003, rlEstabComplementar202003, by = "CO_UNIDADE")

tbMunicipio202003 <- rename(tbMunicipio202003, c("CO_MUNICIPIO"="CO_MUNICIPIO_GESTOR"))



dat2 <- inner_join(dat, tbMunicipio202003, by = "CO_MUNICIPIO_GESTOR")

dat3 <- inner_join(dat2, tbLeito202003, by = "CO_LEITO")

dat3.1 <- inner_join(dat3, demo_brasil, by = "CO_SIGLA_ESTADO")

dat4 <- dat3.1 %>% group_by(CO_UNIDADE) %>% mutate(leitos_total_hospital = sum(QT_EXIST)) 

dat5 <- dat4 %>% group_by(CO_MUNICIPIO_GESTOR) %>% mutate(leitos_total_municipio = sum(QT_EXIST))

dat6 <- dat5 %>% group_by(CO_SIGLA_ESTADO) %>% mutate(leitos_total_estado = sum(QT_EXIST))

# Map

newmap <- getMap(resolution = "low")

plot(newmap, xlim = c(-90, -20), ylim = c(-30, 0), asp = 1)

points(dat6$NU_LONGITUDE, dat6$NU_LATITUDE, col = "blue", cex = .6)

#Plot 0

dat6 %>% group_by(state) %>%
  ggplot(aes(state, leitos_totais_estado, fill = taxa_leitos_estado)) +
  geom_bar(stat="identity") +
  xlab("State") +
  ylab("Beds' Rate") +
  ggtitle("Beds by State") + 
  coord_flip() +
  scale_fill_gradient(low = "green", high = "blue", na.value = NA)

# Plot 1 Beds' rate vs GDP in U$D by states

dat6 %>% 
  group_by(CO_SIGLA_ESTADO) %>% 
  ggplot() +
  geom_point(aes(x = taxa_leitos_estado, y = GDP_per_capita, col=Region), size = 3) +
  geom_text(aes(taxa_leitos_estado, GDP_per_capita, label = CO_SIGLA_ESTADO, nudge_x = 0.05)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Beds rate by population (log scale)") +
  ylab("GDP in U$D (log scale)") +
  ggtitle("Beds' rate vs U$D GDP")


# Plot 2: boxplot distibution hospitals for beds and states

dat6 %>% filter(QT_EXIST >= 10) %>% 
  ggplot(aes(CO_SIGLA_ESTADO, leitos_total_hospital, fill = Region)) +
  geom_boxplot() +
  geom_point(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")
