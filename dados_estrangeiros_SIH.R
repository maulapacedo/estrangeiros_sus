######         DATA SUS            ########
## COOPERAÇÃO INTERNACIONAL TÉCNICA ## 
## Autora: Paula Macedo Barros 
## Data criação: 27/07/2022
##Variaveis 

#Setar diretório 
setwd("~/COBRADI/DATASUS")

# Options
rm(list = ls())

# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(dplyr, tidyr, stringr, lubridate, ggplot2, scales,
               data.table, devtools, stringr, showtext, geobr)

# Loading Google fonts (https://fonts.google.com/)
font_add_google("Signika Negative")
showtext_auto()

# Data SIH-SUS -----------------------------------------------------
#Abrir bases do SUS direto do GITHUB
remotes::install_github("rfsaldanha/microdatasus", force = T)
library(microdatasus)

#Abrir dados do Sistema de Informacoes Hospitalares Descentralizada(SIH)
#SIH apenas para o ano de 2021 
data_sih <- fetch_datasus(year_start = 2021,
                          year_end = 2021,
                          month_start = 1,
                          month_end = 12,
                          uf = "all",
                          information_system = "SIH-RD")

#Processar os dados SIH 
sih <- process_sih(data_sih)

#Filtrar dados apenas para estrangeiros 
sih_estrangeiros <- sih |>
  filter(NACIONAL != "Brasil") |> 
  select(UF_ZI, ANO_CMPT, MES_CMPT, ESPEC, CGC_HOSP, N_AIH, IDENT, CEP, MUNIC_RES,
         munResStatus, munResTipo, munResNome, munResUf, munResLat, munResLon, munResAlt, 
         munResArea, NACIONAL, SEXO, MARCA_UTI, UTI_MES_TO, QT_DIARIAS, DIAR_ACOM,
         PROC_REA, PROC_SOLIC, VAL_SH, VAL_SP, VAL_TOT, VAL_UTI, MARCA_UTI, US_TOT,
         DT_INTER, DT_SAIDA, DIAG_PRINC, DIAG_SECUN, COBRANCA, GESTAO, DIAS_PERM,
         RACA_COR, MUNIC_MOV, COD_IDADE, IDADE, DIAS_PERM, MORTE, CAR_INT, GESTRISCO,
         HOMONIMO, GESTOR_COD, GESTOR_TP, GESTOR_CPF, CNES, INSC_PN, SEQ_AIH5, CNAER,
         VINCPREV, CID_ASSO, CID_MORTE, COMPLEX, FINANC, REMESSA, VAL_SH_FED, VAL_SP_FED, 
         VAL_SH_GES, VAL_SP_GES, VAL_UCI, MARCA_UCI) |> 
  #Transformar variáveis para Números
  mutate(VAL_UTI = as.numeric(VAL_UTI),
         VAL_TOT = as.numeric(VAL_TOT),
         VAL_SH = as.numeric(VAL_SH),
         VAL_SP = as.numeric(VAL_SP),
         US_TOT = as.numeric(US_TOT), 
         VAL_SH_FED = as.numeric(VAL_SH_FED), 
         VAL_SP_FED = as.numeric(VAL_SP_FED), 
         VAL_SH_GES = as.numeric(VAL_SH_GES),
         VAL_SP_GES = as.numeric(VAL_SP_GES),
         VAL_UCI = as.numeric(VAL_UCI), 
         munResNome = str_to_upper(munResNome),
         munResUf = str_to_upper(munResUf),
         NACIONAL = str_to_upper(NACIONAL),
         DT_INTER = as.Date(DT_INTER),
         DT_SAIDA = as.Date(DT_SAIDA)) 


##Salvando os dados para facilitar carregamento da base 
#Gerando arquivo em um diretorio temporario
arq_estrangeiros <- tempfile(fileext = ".RData")

save(sih_estrangeiros, file = arq_estrangeiros)

#Verificando se o arquivo foi salvo no diretorio 
file.exists(arq_estrangeiros)

#apagando sih_estangeiros do ambiente de trabalho
rm(sih_estrangeiros)

#verificando a existencia do objeto
exists(sih_estrangeiros)

#carregando sih_estrangeiros
load(file = arq_estrangeiros)
ls()
print(load(file = arq_estrangeiros))


##Salvando em xlsx
openxlsx::write.xlsx(sih_estrangeiros, "sih_estrangeiros.xlsx", rowNames = TRUE)

#------------------------------------------------------------------------------#

#Carregar Base Estrangeiros 
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/sih_estrangeiros.xlsx")

# Data municipalities IBGE -----------------------------------------------------

#Load data
municipios_br <- geobr::read_municipality("all", 
                                              year = 2020)

municipios_br <- municipios_br |> 
  mutate(
    munResNome = stringr::str_to_upper(name_muni),
    munResUf = stringr::str_to_upper(name_state), 
    MUNIC_RES = as.character(code_muni)
  )

#Data structure
glimpse(municipios_brasil)

#Save data
readr::write_rds(municipios_brasil, 
                 "Dados/IBGE/municipios_brasil.rds",
                 compress = "gz")


# Merge municipios do IBGE com a BASE ESTRANGEIROS -----------------------------
estrangeiros_leftjoin <- left_join(x = sih_estrangeiros,
                                  y = municipios_br, 
                                  by = c("munResNome" = "munResNome",
                                         "munResUf" = "munResUf"))

sum(is.na(estrangeiros_leftjoin$name_muni)) #como consertar essas 548 observações???? 

#Plotar mapa
mapa_br <- geobr::read_state(year = 2020)

mapa_estrangeiros <- mapa_br |> 
  ggplot() + 
  geom_sf(
    fill = "#F0F6F6", col = "#4E4B5C"
  ) +
  geom_sf_label(
    data = estrangeiros_leftjoin,
    aes(x = munResLon, y = munResLat),
    col = "#E27396"
  ) + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_minimal() +
  theme(text = element_text(family = "Signika Negative")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 24))

mapa_estrangeiros
##
estrangeiros <- estrangeiros_lefjoin |> 
  filter(is.na(CEP) == FALSE)


#Somar quantidade de municipios NA 
sum(is.na(estrangeiros_2$name_muni))

#Salvar dados 
write_xlsx(estrangeiros_2, "~\\Paula\\DATASUS\\bases\\estrangeiros_2.xlsx")




#Plot do Mapa
ggplot(estrangeiros_2) +
  geom_sf(aes(fill = MUNIC_RES)) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1)


mapa_estrangeiros

#Filtrar dados para regiões de fronteira (RS, PR, SC, MT, MS, AC, RO, RR, MA...) ---------

##Dados para o Rio Grande do Sul 
data_sih_RS <- fetch_datasus(year_start = 2021,
                          year_end = 2021,
                          uf = "RS",
                          information_system = "SIH-RD")

#_________________________________________________________________________________________#

#Tabela Justificativa do Auditor para a Aceitação de Atendimento sem o número do Cartão Nacional de Saúde 

table(sih_estrangeiros$AUD_JUST)

sum(table(sih_estrangeiros$AUD_JUST))

dados_sinan <- readr::read_csv("~/Paula/DATASUS/dados/microdados_violencia.csv")

View(head(data_sih, 50))


View(sih_estrangeiros)
table(sih_estrangeiros$NACIONAL)
table(sih_estrangeiros$MUNIC_RES)
table(sih_estrangeiros$munResUf)
View(sih_estrangeiros)
table(sih_estrangeiros$MUNIC_RES)
table(sih_estrangeiros$munResStatus)
