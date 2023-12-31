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
###     Carregamento IBGE ----
#%%                                           %%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source('./Rscripts/geocod.R')


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








#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###               CARREGAMENTO PBRUTA DBAMB ----
#%%%%%%%%%%%%%%%                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PBruta <-
  read.table(
    "./data/CubosDBAMB_MovimentacaoProducaoBruta.csv",
    header = TRUE, sep = ",",dec = ".",
    fill = TRUE, quote = "", 
    # colClasses = c("integer","character","character","character","character","character","numeric","numeric"),
    fileEncoding = 'UTF-8', encoding = 'UTF-8')


# Ajustes das variáveis ----

colnames(PBruta) <-
  colnames(PBruta) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
colnames(PBruta)[c(6,5)] <- c("UF","Municipio")


# Formatando colunas de grandezas como numeric

PBruta$Ano.Base.Ral <- 
  PBruta$Ano.Base.Ral |> as.numeric()

PBruta$Valor.Venda.com.Ajuste.por.Minerio <- 
  PBruta$Valor.Venda.com.Ajuste.por.Minerio |> as.numeric()

PBruta$Quantidade.Venda.com.Ajuste <- 
  PBruta$Quantidade.Venda.com.Ajuste |> as.numeric()


PBruta$p <- 
  round(
    (PBruta$Valor.Venda.com.Ajuste.por.Minerio/
      PBruta$Quantidade.Venda.com.Ajuste), 2)
  


# Subsetting ----
PBruta <- 
  PBruta[PBruta$Ano.Base.Ral > 2011 & 
           PBruta$Ano.Base.Ral < 2022 & 
           PBruta$p > 0 & is.na(PBruta$p) == FALSE,]


PBruta$id_mun_UF <-
  paste(
    PBruta$Municipio |> 
      gsub(pattern = "-| {1,}|'|\"", replacement = "") |> 
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') |> 
      toupper(),
    PBruta$UF, sep = "_")

# Impondo coluna de núcleos CNPJs 

PBruta$CPF.CNPJ.Nucleos <- 
  ifelse(str_length(PBruta$CPF.CNPJ.Titular) > 14,
         str_extract(PBruta$CPF.CNPJ.Titular, "^.{1,10}"),
         PBruta$CPF.CNPJ.Titular
  )

# PBruta$Substancia.RAL <- 
#   PBruta$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

PBruta$Substancia.AMB <-
  PBruta$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

PBruta$Municipio <-
  PBruta$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

# Unindo Geocod-PBruta

PBruta <- 
  left_join(PBruta,
            geocod[,-c(1:3,13)], by = c("id_mun_UF"))

# unindo PBruta ao IPA
PBruta <- 
  left_join(PBruta, IPA, by = c("Ano.Base.Ral" = "year"))


# Impondo IDs de agregação ----

PBruta$id_ANO_SUBSTANCIA <- 
  paste(PBruta$Ano.Base.Ral, PBruta$Substancia.AMB) |> gsub(pattern = " ", replacement = "")

PBruta$id_ANO_SUBSTANCIA_UF <- 
  paste0(PBruta$Ano.Base.Ral, PBruta$Substancia.AMB, PBruta$UF) |> gsub(pattern = " ", replacement = "")

PBruta$id_ANO_SUBSTANCIA_MESO <- 
  paste0(PBruta$Ano.Base.Ral, PBruta$Substancia.AMB, PBruta$Cod_RgInt) |> gsub(pattern = " ", replacement = "")

PBruta$id_ANO_SUBSTANCIA_MICRO <- 
  paste0(PBruta$Ano.Base.Ral, PBruta$Substancia.AMB, PBruta$Cod_RgImed) |> gsub(pattern = " ", replacement = "")

PBruta$id_ANO_SUBSTANCIA_MET <- 
  paste0(PBruta$Ano.Base.Ral, PBruta$Substancia.AMB, PBruta$Cod_Reg_Metropolitana) |> gsub(pattern = " ", replacement = "")



# Colunas de PBrutas setoriais-regionais --------------------------------------------

# _____ Venda_PBruta_Substancia ----
PBruta <- 
  left_join(
    PBruta,
    summarise(
      group_by(
        PBruta,
        id_ANO_SUBSTANCIA),  
      "Venda_PBruta_Substancia" = sum(Valor.Venda.com.Ajuste.por.Minerio, na.rm = T),
      "p_Substancia" = median(p, na.rm = T),
      "SD_Substancia" = sd(p, na.rm = T)|> round(2),
      "CV_Substancia" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA'))

  PBruta$p_Substancia_Real <- 
    round(PBruta$p_Substancia*PBruta$fator, digits = 2)


# _____ PBruta_Substancia_UF ----
PBruta <- 
  left_join(
    PBruta,
    summarise(
      group_by(
        PBruta,
        id_ANO_SUBSTANCIA_UF),  
      "Venda_PBruta_Substancia_UF" = sum(Valor.Venda.com.Ajuste.por.Minerio, na.rm = T),
      "p_Substancia_UF" = median(p, na.rm = T),
      "SD_Substancia_UF" = sd(p, na.rm = T)|> round(2),
      "CV_Substancia_UF" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA_UF'))

  PBruta$p_Substancia_UF_Real <- 
    round(PBruta$p_Substancia_UF*PBruta$fator,2)

# _____ Venda_PBruta_Substancia_Meso ----
PBruta <- 
  left_join(
    PBruta,
    summarise(
      group_by(
        PBruta,
        id_ANO_SUBSTANCIA_MESO),  
    "Venda_PBruta_Substancia_MESO" = sum(Valor.Venda.com.Ajuste.por.Minerio, na.rm = T),
    "p_Substancia_MESO" = median(p, na.rm = T),
    "SD_Substancia_MESO" = sd(p, na.rm = T)|> round(2),
    "CV_Substancia_MESO" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA_MESO'))
  
  PBruta$p_Substancia_MESO_Real <- 
    round(PBruta$p_Substancia_MESO*PBruta$fator,2)
  

# _____ Venda_PBruta_Substancia_Micro ----
PBruta <- 
  left_join(
    PBruta,
    summarise(
      group_by(
        PBruta,
        id_ANO_SUBSTANCIA_MICRO),  
      "Venda_PBruta_Substancia_MICRO" = sum(Valor.Venda.com.Ajuste.por.Minerio, na.rm = T),
      "p_Substancia_MICRO" = median(p, na.rm = T),
      "SD_Substancia_MICRO" = sd(p, na.rm = T)|> round(2),
      "CV_Substancia_MICRO" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
    by = c('id_ANO_SUBSTANCIA_MICRO'))

  PBruta$p_Substancia_MICRO_Real <- 
    round(PBruta$p_Substancia_MICRO*PBruta$fator,2)
  
  
# _____ Venda_PBruta_Substancia_MET ----
  PBruta <- 
    left_join(
      PBruta,
      summarise(
        group_by(
          PBruta,
          id_ANO_SUBSTANCIA_MET),  
        "Venda_PBruta_Substancia_MET" = sum(Valor.Venda.com.Ajuste.por.Minerio, na.rm = T),
        "p_Substancia_MET" = median(p, na.rm = T),
        "SD_Substancia_MET" = sd(p, na.rm = T)|> round(2),
        "CV_Substancia_MET" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
      by = c('id_ANO_SUBSTANCIA_MET'))
  
  PBruta$p_Substancia_MET_Real <- 
    round(PBruta$p_Substancia_MET*PBruta$fator,2)  
  
  
  
# MARKET SHARE BRUTA --------------------------------------------

# _____ Mkt_Share_Substancia_PBruta_BR ----
MS_Substancia_PBruta_BR <-
  summarise(
    group_by(PBruta,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             Substancia.AMB),
     "MS_Substancia" = (
      Valor.Venda.com.Ajuste.por.Minerio /
        Venda_PBruta_Substancia)
    ,"p_Substancia" = unique(p_Substancia, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_Real, na.rm = T)
  ) |> na.omit()


# _____ Mkt_Share_Substancia_PBruta_UF ----

MS_Substancia_PBruta_UF <-
  summarise(
    group_by(PBruta,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             UF,
             Substancia.AMB),
     "MS_Substancia" = (
      Valor.Venda.com.Ajuste.por.Minerio /
        Venda_PBruta_Substancia_UF)
    ,"p_Substancia" = unique(p_Substancia_UF, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia_UF, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia_UF, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_UF_Real, na.rm = T)
  ) |> na.omit()

# _____ Mkt_Share_Substancia_PBruta_MESO ----

MS_Substancia_PBruta_MESO <-
  summarise(
    group_by(PBruta,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             Região.Intermediária.IBGE,
             UF,
             Substancia.AMB),
     "MS_Substancia" = (
      Valor.Venda.com.Ajuste.por.Minerio /
        Venda_PBruta_Substancia_MESO)
    ,"p_Substancia" = unique(p_Substancia_MESO, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia_MESO, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia_MESO, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_MESO_Real, na.rm = T)
  ) |> na.omit()

# _____ Mkt_Share_Substancia_PBruta_MICRO ----

MS_Substancia_PBruta_MICRO <-
  summarise(
    group_by(PBruta,
             CPF.CNPJ.Nucleos,
             Ano.Base.Ral,
             UF,
             Região.Intermediária.IBGE,
             Região.Imediata.IBGE,
             Substancia.AMB),
     "MS_Substancia" = (
      Valor.Venda.com.Ajuste.por.Minerio /
        Venda_PBruta_Substancia_MICRO)
    ,"p_Substancia" = unique(p_Substancia_MICRO, na.rm = T)
    ,"SD_Substancia" = unique(SD_Substancia_MICRO, na.rm = T)
    ,"CV_Substancia" = unique(CV_Substancia_MICRO, na.rm = T)
    ,"p_Substancia_Real" = unique(p_Substancia_MICRO_Real, na.rm = T)
  ) |> na.omit()

  
  
# _____ Mkt_Share_Substancia_PBruta_MET ----
  
  MS_Substancia_PBruta_MET <-
    summarise(
      group_by(PBruta,
               CPF.CNPJ.Nucleos,
               Ano.Base.Ral,
               UF,
               Reg_Metropolitana,
               Substancia.AMB),
      "MS_Substancia" = (
        Valor.Venda.com.Ajuste.por.Minerio /
          Venda_PBruta_Substancia_MET)
      ,"p_Substancia" = unique(p_Substancia_MET, na.rm = T)
      ,"SD_Substancia" = unique(SD_Substancia_MET, na.rm = T)
      ,"CV_Substancia" = unique(CV_Substancia_MET, na.rm = T)
      ,"p_Substancia_Real" = unique(p_Substancia_MET_Real, na.rm = T)
    ) |> na.omit()
  
  




  
  
  
  
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  ###               CARREGAMENTO PBENEFICIADA DBAMB ----
  #%%%%%%%%%%%%%%%                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  PBeneficiada <-
    read.table(
      "./data/CubosDBAMB_MovimentacaoProducaoBeneficiada.csv",
      header = TRUE, sep = ",",dec = ".",
      fill = TRUE, quote = "", 
      # colClasses = c("integer","character","character","character","character","character","numeric","numeric"),
      fileEncoding = 'UTF-8', encoding = 'UTF-8')
  
  
  # Ajustes das variáveis ----
  
  colnames(PBeneficiada) <-
    colnames(PBeneficiada) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  colnames(PBeneficiada)[c(5,4)] <- c("UF","Municipio")
  
  
  # Formatando colunas de grandezas como numeric
  
  PBeneficiada$Ano.Base.Ral <- 
    PBeneficiada$Ano.Base.Ral |> as.numeric()
  
  PBeneficiada$Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor <- 
    PBeneficiada$Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor |> as.numeric()
  
  PBeneficiada$Quantidade.Venda.com.Ajuste <- 
    PBeneficiada$Quantidade.Venda.com.Ajuste |> as.numeric()
  
  
  PBeneficiada$p <- 
    round(
      (PBeneficiada$Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor/
         PBeneficiada$Quantidade.Venda.com.Ajuste), 2)
  
  
  
  # Subsetting ----
  PBeneficiada <- 
    PBeneficiada[PBeneficiada$Ano.Base.Ral > 2011 & 
             PBeneficiada$Ano.Base.Ral < 2022 & 
             PBeneficiada$p > 0 & is.na(PBeneficiada$p) == FALSE,]
  
  
  PBeneficiada$id_mun_UF <-
    paste(
      PBeneficiada$Municipio |> 
        gsub(pattern = "-| {1,}|'|\"", replacement = "") |> 
        iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') |> 
        toupper(),
      PBeneficiada$UF, sep = "_")
  
  # Impondo coluna de núcleos CNPJs 
  
  PBeneficiada$CPF.CNPJ.Nucleos <- 
    ifelse(str_length(PBeneficiada$CPF.CNPJ.Titular) > 14,
           str_extract(PBeneficiada$CPF.CNPJ.Titular, "^.{1,10}"),
           PBeneficiada$CPF.CNPJ.Titular
    )
  
  # PBeneficiada$Substancia.RAL <- 
  #   PBeneficiada$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  PBeneficiada$Substancia.AMB <-
    PBeneficiada$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  PBeneficiada$Municipio <-
    PBeneficiada$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  # Unindo Geocod-PBeneficiada
  
  PBeneficiada <- 
    left_join(PBeneficiada,
              geocod[,-c(1:3,13)], by = c("id_mun_UF"))
  
  # unindo PBeneficiada ao IPA
  PBeneficiada <- 
    left_join(PBeneficiada, IPA, by = c("Ano.Base.Ral" = "year"))
  
  
  # Impondo IDs de agregação ----
  
  PBeneficiada$id_ANO_SUBSTANCIA <- 
    paste(PBeneficiada$Ano.Base.Ral, PBeneficiada$Substancia.AMB) |> gsub(pattern = " ", replacement = "")
  
  PBeneficiada$id_ANO_SUBSTANCIA_UF <- 
    paste0(PBeneficiada$Ano.Base.Ral, PBeneficiada$Substancia.AMB, PBeneficiada$UF) |> gsub(pattern = " ", replacement = "")
  
  PBeneficiada$id_ANO_SUBSTANCIA_MESO <- 
    paste0(PBeneficiada$Ano.Base.Ral, PBeneficiada$Substancia.AMB, PBeneficiada$Cod_RgInt) |> gsub(pattern = " ", replacement = "")
  
  PBeneficiada$id_ANO_SUBSTANCIA_MICRO <- 
    paste0(PBeneficiada$Ano.Base.Ral, PBeneficiada$Substancia.AMB, PBeneficiada$Cod_RgImed) |> gsub(pattern = " ", replacement = "")
  
  PBeneficiada$id_ANO_SUBSTANCIA_MET <- 
    paste0(PBeneficiada$Ano.Base.Ral, PBeneficiada$Substancia.AMB, PBeneficiada$Cod_Reg_Metropolitana) |> gsub(pattern = " ", replacement = "")
  
  
  
  # Colunas de PBeneficiadas setoriais-regionais --------------------------------------------
  
  # _____ Venda_PBeneficiada_Substancia ----
  PBeneficiada <- 
    left_join(
      PBeneficiada,
      summarise(
        group_by(
          PBeneficiada,
          id_ANO_SUBSTANCIA),  
        "Venda_PBeneficiada_Substancia" = sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor, na.rm = T),
        "p_Substancia" = median(p, na.rm = T),
        "SD_Substancia" = sd(p, na.rm = T)|> round(2),
        "CV_Substancia" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
      by = c('id_ANO_SUBSTANCIA'))
  
  PBeneficiada$p_Substancia_Real <- 
    round(PBeneficiada$p_Substancia*PBeneficiada$fator, digits = 2)
  
  
  # _____ PBeneficiada_Substancia_UF ----
  PBeneficiada <- 
    left_join(
      PBeneficiada,
      summarise(
        group_by(
          PBeneficiada,
          id_ANO_SUBSTANCIA_UF),  
        "Venda_PBeneficiada_Substancia_UF" = sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor, na.rm = T),
        "p_Substancia_UF" = median(p, na.rm = T),
        "SD_Substancia_UF" = sd(p, na.rm = T)|> round(2),
        "CV_Substancia_UF" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
      by = c('id_ANO_SUBSTANCIA_UF'))
  
  PBeneficiada$p_Substancia_UF_Real <- 
    round(PBeneficiada$p_Substancia_UF*PBeneficiada$fator,2)
  
  # _____ Venda_PBeneficiada_Substancia_Meso ----
  PBeneficiada <- 
    left_join(
      PBeneficiada,
      summarise(
        group_by(
          PBeneficiada,
          id_ANO_SUBSTANCIA_MESO),  
        "Venda_PBeneficiada_Substancia_MESO" = sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor, na.rm = T),
        "p_Substancia_MESO" = median(p, na.rm = T),
        "SD_Substancia_MESO" = sd(p, na.rm = T)|> round(2),
        "CV_Substancia_MESO" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
      by = c('id_ANO_SUBSTANCIA_MESO'))
  
  PBeneficiada$p_Substancia_MESO_Real <- 
    round(PBeneficiada$p_Substancia_MESO*PBeneficiada$fator,2)
  
  
  # _____ Venda_PBeneficiada_Substancia_Micro ----
  PBeneficiada <- 
    left_join(
      PBeneficiada,
      summarise(
        group_by(
          PBeneficiada,
          id_ANO_SUBSTANCIA_MICRO),  
        "Venda_PBeneficiada_Substancia_MICRO" = sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor, na.rm = T),
        "p_Substancia_MICRO" = median(p, na.rm = T),
        "SD_Substancia_MICRO" = sd(p, na.rm = T)|> round(2),
        "CV_Substancia_MICRO" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
      by = c('id_ANO_SUBSTANCIA_MICRO'))
  
  PBeneficiada$p_Substancia_MICRO_Real <- 
    round(PBeneficiada$p_Substancia_MICRO*PBeneficiada$fator,2)
  
  
  # _____ Venda_PBeneficiada_Substancia_MET ----
  PBeneficiada <- 
    left_join(
      PBeneficiada,
      summarise(
        group_by(
          PBeneficiada,
          id_ANO_SUBSTANCIA_MET),  
        "Venda_PBeneficiada_Substancia_MET" = sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor, na.rm = T),
        "p_Substancia_MET" = median(p, na.rm = T),
        "SD_Substancia_MET" = sd(p, na.rm = T)|> round(2),
        "CV_Substancia_MET" = ((sd(p, na.rm = T))/(median(p, na.rm = T)))|> round(2)),
      by = c('id_ANO_SUBSTANCIA_MET'))
  
  PBeneficiada$p_Substancia_MET_Real <- 
    round(PBeneficiada$p_Substancia_MET*PBeneficiada$fator,2)  
  
  
  
  # MARKET SHARE BENEFICIADA--------------------------------------------
  
  # _____ Mkt_Share_Substancia_PBeneficiada_BR ----
  MS_Substancia_PBeneficiada_BR <-
    summarise(
      group_by(PBeneficiada,
               CPF.CNPJ.Nucleos,
               Ano.Base.Ral,
               Substancia.AMB),
      "MS_Substancia" = (
        Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor /
          Venda_PBeneficiada_Substancia)
      ,"p_Substancia" = unique(p_Substancia, na.rm = T)
      ,"SD_Substancia" = unique(SD_Substancia, na.rm = T)
      ,"CV_Substancia" = unique(CV_Substancia, na.rm = T)
      ,"p_Substancia_Real" = unique(p_Substancia_Real, na.rm = T)
    ) |> na.omit()
  
  
  # _____ Mkt_Share_Substancia_PBeneficiada_UF ----
  
  MS_Substancia_PBeneficiada_UF <-
    summarise(
      group_by(PBeneficiada,
               CPF.CNPJ.Nucleos,
               Ano.Base.Ral,
               UF,
               Substancia.AMB),
      "MS_Substancia" = (
        Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor /
          Venda_PBeneficiada_Substancia_UF)
      ,"p_Substancia" = unique(p_Substancia_UF, na.rm = T)
      ,"SD_Substancia" = unique(SD_Substancia_UF, na.rm = T)
      ,"CV_Substancia" = unique(CV_Substancia_UF, na.rm = T)
      ,"p_Substancia_Real" = unique(p_Substancia_UF_Real, na.rm = T)
    ) |> na.omit()
  
  # _____ Mkt_Share_Substancia_PBeneficiada_MESO ----
  
  MS_Substancia_PBeneficiada_MESO <-
    summarise(
      group_by(PBeneficiada,
               CPF.CNPJ.Nucleos,
               Ano.Base.Ral,
               Região.Intermediária.IBGE,
               UF,
               Substancia.AMB),
      "MS_Substancia" = (
        Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor /
          Venda_PBeneficiada_Substancia_MESO)
      ,"p_Substancia" = unique(p_Substancia_MESO, na.rm = T)
      ,"SD_Substancia" = unique(SD_Substancia_MESO, na.rm = T)
      ,"CV_Substancia" = unique(CV_Substancia_MESO, na.rm = T)
      ,"p_Substancia_Real" = unique(p_Substancia_MESO_Real, na.rm = T)
    ) |> na.omit()
  
  # _____ Mkt_Share_Substancia_PBeneficiada_MICRO ----
  
  MS_Substancia_PBeneficiada_MICRO <-
    summarise(
      group_by(PBeneficiada,
               CPF.CNPJ.Nucleos,
               Ano.Base.Ral,
               UF,
               Região.Intermediária.IBGE,
               Região.Imediata.IBGE,
               Substancia.AMB),
      "MS_Substancia" = (
        Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor /
          Venda_PBeneficiada_Substancia_MICRO)
      ,"p_Substancia" = unique(p_Substancia_MICRO, na.rm = T)
      ,"SD_Substancia" = unique(SD_Substancia_MICRO, na.rm = T)
      ,"CV_Substancia" = unique(CV_Substancia_MICRO, na.rm = T)
      ,"p_Substancia_Real" = unique(p_Substancia_MICRO_Real, na.rm = T)
    ) |> na.omit()
  
  
  
  # _____ Mkt_Share_Substancia_PBeneficiada_MET ----
  
  MS_Substancia_PBeneficiada_MET <-
    summarise(
      group_by(PBeneficiada,
               CPF.CNPJ.Nucleos,
               Ano.Base.Ral,
               UF,
               Reg_Metropolitana,
               Substancia.AMB),
      "MS_Substancia" = (
        Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor /
          Venda_PBeneficiada_Substancia_MET)
      ,"p_Substancia" = unique(p_Substancia_MET, na.rm = T)
      ,"SD_Substancia" = unique(SD_Substancia_MET, na.rm = T)
      ,"CV_Substancia" = unique(CV_Substancia_MET, na.rm = T)
      ,"p_Substancia_Real" = unique(p_Substancia_MET_Real, na.rm = T)
    ) |> na.omit()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  ###                    Variáveis de sessão ----
  #%%%%%%%%%%%%%%%                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  uf <-
    c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
  
  RegiaoIntermediaria <- unique(geocod$Região.Intermediária.IBGE)
  RegiaoImediata <- unique(geocod$Região.Imediata.IBGE)
  RegiaoMetropolitana <- na.omit(unique(geocod$Reg_Metropolitana))
  Substancia <- sort(unique(PBruta$Substancia.AMB))
  Produto <- sort(unique(PBeneficiada$Produto.Comercializado))
  
  Titulo <- 
    paste0("Preços Reais (base ",  max(PBruta$Ano.Base.Ral,na.rm = TRUE), ")")  
  
    rm(list = c("PBeneficiada",'PBruta'))
  
  