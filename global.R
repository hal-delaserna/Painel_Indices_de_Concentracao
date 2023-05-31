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
  VPM[VPM$Ano.Base.Ral > 2011 & VPM$Ano.Base.Ral < 2022,]


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

Titulo <- 
  paste0("Preços Reais (base ",  max(VPM$Ano.Base.Ral), ")")



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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                           %%%
###        Carregamento IPA ----
#%%                                           %%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

IPA <- # ipeadatar::ipeadata(code = "IGP_IPAI")
  read.table(file = "./data/IPA.csv", header = TRUE, sep = ";", 
             quote = "", dec = ".")

IPA$year <- 
  lubridate::year(IPA$date)

IPA <- 
  IPA[,c("year", "value")]


IPA$fator <- NA
for (i in 1:nrow(IPA)) {
  
  # fator de deflação
  IPA$fator[i] <- 
    as.numeric(
      IPA[IPA$year == max(IPA$year),]$value/
        IPA[i,c("value")])
}
# unindo VPM ao IPA
  VPM <- 
    left_join(VPM, IPA, by = c("Ano.Base.Ral" = "year"))


# Colunas de VPMs setoriais-regionais --------------------------------------------

# _____ VPM_Substancia ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA),  
    "VPM_Substancia" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T),
    "p_Substancia" = median(Valor.Producao.Comercializada.Substancia.AMB/
                                 Quantidade.Producao.Comercializada...Substancia, na.rm = T) |> round(2),
    "SD_Substancia" = sd(Valor.Producao.Comercializada.Substancia.AMB/
                              Quantidade.Producao.Comercializada...Substancia, na.rm = T) |> round(2),
    "CV_Substancia" = ((sd(Valor.Producao.Comercializada.Substancia.AMB/
                               Quantidade.Producao.Comercializada...Substancia, na.rm = T))/
      (median(Valor.Producao.Comercializada.Substancia.AMB/
                Quantidade.Producao.Comercializada...Substancia, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA'))

  VPM$p_Substancia_Real <- 
    round(VPM$p_Substancia*VPM$fator, digits = 2)


# _____ VPM_Substancia_UF ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_UF),  
    "VPM_Substancia_UF" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T),
    "p_Substancia_UF" = median(Valor.Producao.Comercializada.Substancia.AMB/
                              Quantidade.Producao.Comercializada...Substancia, na.rm = T)|> round(2),
    "SD_Substancia_UF" = sd(Valor.Producao.Comercializada.Substancia.AMB/
                           Quantidade.Producao.Comercializada...Substancia, na.rm = T)|> round(2),
    "CV_Substancia_UF" = ((sd(Valor.Producao.Comercializada.Substancia.AMB/
                            Quantidade.Producao.Comercializada...Substancia, na.rm = T))/
      (median(Valor.Producao.Comercializada.Substancia.AMB/
                Quantidade.Producao.Comercializada...Substancia, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA_UF'))

  VPM$p_Substancia_UF_Real <- 
    round(VPM$p_Substancia_UF*VPM$fator,2)

# _____ VPM_Substancia_Meso ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_MESO),  
    "VPM_Substancia_MESO" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T),
    "p_Substancia_MESO" = median(Valor.Producao.Comercializada.Substancia.AMB/
                                 Quantidade.Producao.Comercializada...Substancia, na.rm = T)|> round(2),
    "SD_Substancia_MESO" = sd(Valor.Producao.Comercializada.Substancia.AMB/
                              Quantidade.Producao.Comercializada...Substancia, na.rm = T)|> round(2),
    "CV_Substancia_MESO" = ((sd(Valor.Producao.Comercializada.Substancia.AMB/
                               Quantidade.Producao.Comercializada...Substancia, na.rm = T))/
      (median(Valor.Producao.Comercializada.Substancia.AMB/
                Quantidade.Producao.Comercializada...Substancia, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA_MESO'))
  
  VPM$p_Substancia_MESO_Real <- 
    round(VPM$p_Substancia_MESO*VPM$fator,2)
  

# _____ VPM_Substancia_Micro ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_MICRO),  
    "VPM_Substancia_MICRO" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T),
    "p_Substancia_MICRO" = median(Valor.Producao.Comercializada.Substancia.AMB/
                                   Quantidade.Producao.Comercializada...Substancia, na.rm = T)|> round(2),
    "SD_Substancia_MICRO" = sd(Valor.Producao.Comercializada.Substancia.AMB/
                                Quantidade.Producao.Comercializada...Substancia, na.rm = T)|> round(2),
    "CV_Substancia_MICRO" = ((sd(Valor.Producao.Comercializada.Substancia.AMB/
                                 Quantidade.Producao.Comercializada...Substancia, na.rm = T))/
      (median(Valor.Producao.Comercializada.Substancia.AMB/
                Quantidade.Producao.Comercializada...Substancia, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA_MICRO'))

  VPM$p_Substancia_MICRO_Real <- 
    round(VPM$p_Substancia_MICRO*VPM$fator,2)
  
# MARKET SHARE --------------------------------------------

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
    ,"p_Substancia" = unique(p_Substancia, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_Real, na.rm = T)
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
        VPM_Substancia_UF)
    ,"p_Substancia" = unique(p_Substancia_UF, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia_UF, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia_UF, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_UF_Real, na.rm = T)
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
        VPM_Substancia_MESO)
    ,"p_Substancia" = unique(p_Substancia_MESO, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia_MESO, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia_MESO, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_MESO_Real, na.rm = T)
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
        VPM_Substancia_MICRO)
    ,"p_Substancia" = unique(p_Substancia_MICRO, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia_MICRO, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia_MICRO, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_MICRO_Real, na.rm = T)
  ) |> na.omit()


# rm(VPM)



