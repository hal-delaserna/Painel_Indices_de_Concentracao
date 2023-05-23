library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(reactable)
library(DT) # para download de dados pelo usuário
library(concstats)


# busyIndicator(text = "Calculation in progress ... ", wait = 0)
options(shiny.reactlog = TRUE, launch.browser = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                           %%%
###     Carregamento VPM DBAMB ----
#%%                                           %%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

VPM <-
  read.table(
    "./data/DBAMB_QuantidadeEhValordaProducaoMineralComercializada.csv",
    header = TRUE, sep = "\t",dec = ".",
    fill = TRUE, quote = "", 
    # colClasses = c("integer","character","character","character","character","character","numeric","numeric"),
    fileEncoding = 'UTF-8', encoding = 'UTF-8')


VPM <- 
  VPM[VPM$Ano.Base.Ral > 2011,]


VPM$id_mun_UF <-
  paste(
    VPM$Municipio |> 
      gsub(pattern = "-| {1,}|'|\"", replacement = "") |> 
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') |> 
      toupper(),
    VPM$Sigla.Estado, sep = "_")

# Impondo coluna de núcleos CNPJs 

VPM$CPF.CNPJ.Nucleos <- 
  ifelse(str_length(VPM$CPF.CNPJ.Titular) > 14,
         str_extract(VPM$CPF.CNPJ.Titular, "^.{1,10}"),
         VPM$CPF.CNPJ.Titular
  )

# Ajustes das variáveis ----

colnames(VPM) <-
  colnames(VPM) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
colnames(VPM)[c(4,9)] <- c("UF","Valor.Producao.Comercializada.Produto")

# VPM$Substancia.RAL <- 
#   VPM$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM$Substancia.AMB <-
  VPM$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM$Produto.Comercializado <-
  VPM$Produto.Comercializado |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM$Municipio <-
  VPM$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

# VPM$Nome <-
#   VPM$Nome |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()


# Formatando colunas de grandezas como numeric

VPM$Valor.Producao.Comercializada.Produto <- 
  gsub(VPM$Valor.Producao.Comercializada.Produto, 
       pattern = ",", replacement = "") |> as.numeric()

VPM$Valor.Producao.Comercializada.Substancia.AMB <- 
  gsub(VPM$Valor.Producao.Comercializada.Substancia.AMB, 
       pattern = ",", replacement = "") |> as.numeric()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                           %%%
###     Carregamento IBGE ----
#%%                                           %%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source('./Rscripts/geocod.R')

# Unindo Geocod-VPM

VPM <- 
  left_join(VPM,
            geocod[,-c(1:3,13)], by = c("id_mun_UF"))


# _____ Variáveis de sessão ----
uf <-
  c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

RegiaoIntermediaria <- unique(geocod$Região.Intermediária.IBGE)
RegiaoImediata <- unique(geocod$Região.Imediata.IBGE)
Substancia <- sort(unique(VPM$Substancia.AMB))
Produto <- sort(unique(VPM$Produto.Comercializado))



# Impondo IDs de agregação ----

VPM$id_ANO_SUBSTANCIA <- 
  paste(VPM$Ano.Base.Ral, VPM$Substancia.AMB) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_SUBSTANCIA_UF <- 
  paste0(VPM$Ano.Base.Ral, VPM$Substancia.AMB, VPM$UF) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_SUBSTANCIA_MESO <- 
  paste0(VPM$Ano.Base.Ral, VPM$Substancia.AMB, VPM$Cod_RgInt) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_SUBSTANCIA_MICRO <- 
  paste0(VPM$Ano.Base.Ral, VPM$Substancia.AMB, VPM$Cod_RgImed) |> gsub(pattern = " ", replacement = "")


VPM$id_ANO_PRODUTO <- 
  paste(VPM$Ano.Base.Ral, VPM$Produto.Comercializado) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_PRODUTO_UF <- 
  paste0(VPM$Ano.Base.Ral, VPM$Produto.Comercializado, VPM$UF) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_PRODUTO_MESO <- 
  paste0(VPM$Ano.Base.Ral, VPM$Produto.Comercializado, VPM$Cod_RgInt) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_PRODUTO_MICRO <- 
  paste0(VPM$Ano.Base.Ral, VPM$Produto.Comercializado, VPM$Cod_RgImed) |> gsub(pattern = " ", replacement = "")

# Colunas de VPMs setoriais-regionais ----

# _____ VPM_Produto ----  
VPM <- 
  left_join(
    filter(VPM, Valor.Producao.Comercializada.Produto > 0),
    summarise(
      group_by(
        VPM,
        id_ANO_PRODUTO),  
      "VPM_Produto" = sum(Valor.Producao.Comercializada.Produto, na.rm = T)),
    by = c('id_ANO_PRODUTO'))


# _____ VPM_Produto_UF ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_PRODUTO_UF),  
      "VPM_Produto_UF" = sum(Valor.Producao.Comercializada.Produto, na.rm = T)),
    by = c('id_ANO_PRODUTO_UF'))



# _____ VPM_Produto_Meso ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_PRODUTO_MESO),  
      "VPM_Produto_MESO" = sum(Valor.Producao.Comercializada.Produto, na.rm = T)),
    by = c('id_ANO_PRODUTO_MESO'))



# _____ VPM_Produto_MICRO ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_PRODUTO_MICRO),  
      "VPM_Produto_MICRO" = sum(Valor.Producao.Comercializada.Produto, na.rm = T)),
    by = c('id_ANO_PRODUTO_MICRO'))


# _____ VPM_Substancia ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA),  
      "VPM_Substancia" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T),
      "p" = median(Valor.Producao.Comercializada.Produto/
                     Quantidade.Producao.Comercializada...Produto, na.rm = T),
      "SD" = sd(Valor.Producao.Comercializada.Produto/
                  Quantidade.Producao.Comercializada...Produto, na.rm = T),
      "CV" = (sd(Valor.Producao.Comercializada.Produto/
                   Quantidade.Producao.Comercializada...Produto, na.rm = T))/
        (median(Valor.Producao.Comercializada.Produto/
                  Quantidade.Producao.Comercializada...Produto, na.rm = T))),
    by = c('id_ANO_SUBSTANCIA'))



# _____ VPM_Substancia_UF ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_UF),  
      "VPM_Substancia_UF" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T)),
    by = c('id_ANO_SUBSTANCIA_UF'))


# _____ VPM_Substancia_Meso ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_MESO),  
      "VPM_Substancia_MESO" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T)),
    by = c('id_ANO_SUBSTANCIA_MESO'))

# _____ VPM_Substancia_Micro ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_MICRO),  
      "VPM_Substancia_MICRO" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T)),
    by = c('id_ANO_SUBSTANCIA_MICRO'))









# _____ Mkt_Share_Produto_VPM_BR ----
MS_Produto_VPM_BR <-
  summarise(
    group_by(VPM,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             Produto.Comercializado),
    "MS_Produto" = (Valor.Producao.Comercializada.Produto /
                      VPM_Produto)
  ) |> na.omit()

# _____ Mkt_Share_Produto_VPM_UF ----

MS_Produto_VPM_UF <-
  summarise(
    group_by(
      VPM,
      CPF.CNPJ.Nucleos,
      Ano.Base.Ral,
      UF,
      Produto.Comercializado
    ),
    "MS_Produto" = (Valor.Producao.Comercializada.Produto /
                      VPM_Produto_UF)
  ) |> na.omit()

# _____ Mkt_Share_Produto_VPM_MESO ----

MS_Produto_VPM_MESO <-
  summarise(
    group_by(
      VPM,
      CPF.CNPJ.Nucleos,
      Ano.Base.Ral,
      UF,
      Região.Intermediária.IBGE,
      Produto.Comercializado
    ),
    "MS_Produto" = (Valor.Producao.Comercializada.Produto /
                      VPM_Produto_MESO)
  ) |> na.omit()

# _____ Mkt_Share_Produto_VPM_MICRO ----

MS_Produto_VPM_MICRO <-
  summarise(
    group_by(
      VPM,
      CPF.CNPJ.Nucleos,
      Ano.Base.Ral,
      UF,
      Região.Imediata.IBGE,
      Produto.Comercializado
    ),
    "MS_Produto" = (Valor.Producao.Comercializada.Produto /
                      VPM_Produto_MICRO)
  ) |> na.omit()


# _____ Mkt_Share_Substancia_VPM_BR ----
MS_Substancia_VPM_BR <-
  summarise(
    group_by(VPM,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             Substancia.AMB),
    "MS_Substancia" = (
      Valor.Producao.Comercializada.Substancia.AMB /
        VPM_Substancia)
  ) |> na.omit()


# _____ Mkt_Share_Substancia_VPM_UF ----

MS_Substancia_VPM_UF <-
  summarise(
    group_by(VPM,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             UF,
             Substancia.AMB),
    "MS_Substancia" = (
      Valor.Producao.Comercializada.Substancia.AMB /
        VPM_Substancia_UF
    )
  ) |> na.omit()

# _____ Mkt_Share_Substancia_VPM_MESO ----

MS_Substancia_VPM_MESO <-
  summarise(
    group_by(VPM,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             Região.Intermediária.IBGE,
             UF,
             Substancia.AMB),
    "MS_Substancia" = (
      Valor.Producao.Comercializada.Substancia.AMB /
        VPM_Substancia_MESO
    )
  ) |> na.omit()

# _____ Mkt_Share_Substancia_VPM_MICRO ----

MS_Substancia_VPM_MICRO <-
  summarise(
    group_by(VPM,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             UF,
             Região.Intermediária.IBGE,
             Região.Imediata.IBGE,
             Substancia.AMB),
    "MS_Substancia" = (
      Valor.Producao.Comercializada.Substancia.AMB /
        VPM_Substancia_MICRO
    )
  ) |> na.omit()








#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                           %%%
###        Carregamento CNAE 2.0 ----
#%%                                           %%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CNAE_Subclasses_2_0 <-
#   read.table(file = './data/CNAE_2_0.csv', 
#              header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
#              colClasses = c('character'), 
#              fileEncoding = "latin1")
# 
# colnames(CNAE_Subclasses_2_0) <- 
#   c("subclasse", "subclasse.descrição", "classe", 
#     "classe.descrição", "grupo", "grupo.descrição", "divisão", 
#     "divisão.descrição", "seção", "seção.descrição", "grupamento", 
#     "grande.Grupamento")



