# HEADER ----
header <-
  dashboardHeader(title = "Painel de Índices de Concentração de Mercado", 
                  titleWidth = 550)
# SIDEBAR ----

sidebar <- dashboardSidebar(width = 200,
  sidebarMenu(
    menuItem("Hirchsman-Herfindahl (HHI)",
             menuSubItem(" Produção Bruta", tabName = "tab_HHI_Bruta", icon = icon("truck-monster")), #person-digging
             menuSubItem(" Produção Beneficiada", tabName = "tab_HHI_Beneficiada", icon = icon("industry")))
   ,menuItem("Relação de Concentração (CR4)", 
             tabName = "tab_CR4")))
# BODY ----

body <-
  dashboardBody(
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 12px; }")) #change the font size to 20
    ),
    tabItems(
    
# tab_HHI_Bruta ---- 
    tabItem(tabName = "tab_HHI_Bruta"
            ,tabsetPanel(

#_____ tab_HHI BR ---- 
              tabPanel("Brasil"
                       # ,fluidRow(box(width = 12, solidHeader = FALSE ))

                       
# __________Input Substância ----                  
 ,fluidRow(
   box(width = 2
      ,pickerInput(
        inputId = "id.Substancia.select"
        ,label = "Substância"
        ,choices = list(`Substancia` = Substancia)
        ,multiple = FALSE
        ,selected = "Areia"
        ),actionButton(inputId = "id.Atualizar.button.BR", label = "Ok")
      )
,box(width = 8
     ,reactableOutput(outputId = "id.HHI_Substancia_PBruta_BR")
     ))
 ,fluidRow(
   box(title = "Índice Hirchsman-Herfindahl (HHI)"
       ,solidHeader = TRUE
       ,status = "warning"
       ,width = 4
       ,height = '215px'
       ,plotOutput(outputId = "id.Graf.HHI_Substancia_PBruta_BR", height = "150px"))
   ,box(title = Titulo
        ,solidHeader = TRUE
        ,status = "info"
        ,width = 4
        ,height = '215px'
        ,plotOutput(outputId = "id.Graf.preco_Substancia_PBruta_BR", height = "150px"))
   ,box(title = "Coeficiente de Variação (preço)"
        ,solidHeader = TRUE
        ,status = "info"
        ,width = 4
        ,height = '215px'
        ,plotOutput(outputId = "id.Graf.CV_Substancia_PBruta_BR", height = "150px"))
   )
),
#_____ tab_HHI UF ---- 
tabPanel("UF"
# __________Input Substância ----
,fluidRow(
   box(width = 2
     ,pickerInput(
       inputId = "id.Substancia.select.UF"
       ,label = "Substância"
       ,choices = list(`Substancia` = Substancia)
       ,multiple = FALSE
       ,selected = "Areia"
     )
# __________ Input UF ----
     ,pickerInput(
       inputId = "id.UF.select"
       ,label = "UF"
       ,choices = list(`UF` = uf)
       ,multiple = FALSE)
,actionButton(inputId = "id.Atualizar.button.UF", label = "Ok")
)
,box(width = 8
     ,reactableOutput(outputId = "id.HHI_Substancia_PBruta_UF")
))
,fluidRow(
  box(title = "Índice Hirchsman-Herfindahl (HHI)"
      ,solidHeader = TRUE
      ,status = "warning"
      ,width = 4
      ,height = '215px'
      ,plotOutput(outputId = "id.Graf.HHI_Substancia_PBruta_UF", height = "150px"))
  ,box(title = Titulo
       ,solidHeader = TRUE
       ,status = "info"
       ,width = 4
       ,height = '215px'
       ,plotOutput(outputId = "id.Graf.preco_Substancia_PBruta_UF", height = "150px"))
  ,box(title = "Coeficiente de Variação (preço)"
       ,solidHeader = TRUE
       ,status = "info"
       ,width = 4
       ,height = '215px'
       ,plotOutput(outputId = "id.Graf.CV_Substancia_PBruta_UF", height = "150px"))
)
),
#_____ tab_HHI MESO ---- 
tabPanel("Mesorregião"
# __________Input Substância ----
,fluidRow(
  box(width = 2
     ,pickerInput(
       inputId = "id.Substancia.select.MESO"
       ,label = "Substância"
       ,choices = list(`Substancia` = Substancia)
       ,multiple = FALSE
       ,selected = "Areia")
# __________ Input UF.MESO ----
     ,pickerInput(
       inputId = "id.UF.MESO.select"
       ,label = "UF"
       ,choices = list(`UF` = uf)
       ,multiple = FALSE)
# __________ Input MESO ----
     ,pickerInput(
       inputId = "id.MESO.select"
       ,label = "Mesorregião"
       ,choices = list(`Mesorregião` = RegiaoIntermediaria)
       ,multiple = FALSE)
,actionButton(inputId = "id.Atualizar.button.MESO", label = "Ok")
 )
,box(width = 8
     ,reactableOutput(outputId = "id.HHI_Substancia_PBruta_MESO"))
)
,fluidRow(
  box(title = "Índice Hirchsman-Herfindahl (HHI)"
      ,solidHeader = TRUE
      ,status = "warning"
      ,width = 4
      ,height = '215px'
      ,plotOutput(outputId = "id.Graf.HHI_Substancia_PBruta_MESO", height = "150px"))
  ,box(title = Titulo
       ,solidHeader = TRUE
       ,status = "info"
       ,width = 4
       ,height = '215px'
       ,plotOutput(outputId = "id.Graf.preco_Substancia_PBruta_MESO", height = "150px"))
  ,box(title = "Coeficiente de Variação (preço)"
       ,solidHeader = TRUE
       ,status = "info"
       ,width = 4
       ,height = '215px'
       ,plotOutput(outputId = "id.Graf.CV_Substancia_PBruta_MESO", height = "150px"))
)
),
#_____ tab_HHI MICRO ---- 
tabPanel("Microrregião"
         # __________Input Substância ----
         ,fluidRow(
           box(width = 2
               ,pickerInput(
                 inputId = "id.Substancia.select.MICRO"
                 ,label = "Substância"
                 ,choices = list(`Substancia` = Substancia)
                 ,multiple = FALSE
                 ,selected = "Areia")
               # __________ Input UF.MICRO ----
               ,pickerInput(
                 inputId = "id.UF.MICRO.select"
                 ,label = "UF"
                 ,choices = list(`UF` = uf)
                 ,multiple = FALSE)
               # __________ Input MICRO ----
               ,pickerInput(
                 inputId = "id.MICRO.select"
                 ,label = "Microrregião"
                 ,choices = list(`Microrregião` = RegiaoImediata)
                 ,multiple = FALSE)
               ,actionButton(inputId = "id.Atualizar.button.MICRO", label = "Ok")
           )
           ,box(width = 8
                ,reactableOutput(outputId = "id.HHI_Substancia_PBruta_MICRO"))
         )
         ,fluidRow(
           box(title = "Índice Hirchsman-Herfindahl (HHI)"
               ,solidHeader = TRUE
               ,status = "warning"
               ,width = 4
               ,height = '215px'
               ,plotOutput(outputId = "id.Graf.HHI_Substancia_PBruta_MICRO", height = "150px"))
           ,box(title = Titulo
                ,solidHeader = TRUE
                ,status = "info"
                ,width = 4
                ,height = '215px'
                ,plotOutput(outputId = "id.Graf.preco_Substancia_PBruta_MICRO", height = "150px"))
           ,box(title = "Coeficiente de Variação (preço)"
                ,solidHeader = TRUE
                ,status = "info"
                ,width = 4
                ,height = '215px'
                ,plotOutput(outputId = "id.Graf.CV_Substancia_PBruta_MICRO", height = "150px"))
         )
         ),
#_____ tab_HHI MET ---- 
tabPanel("Região Metropolitana"
         # __________Input Substância ----
         ,fluidRow(
           box(width = 2
               ,pickerInput(
                 inputId = "id.Substancia.select.MET"
                 ,label = "Substância"
                 ,choices = list(`Substancia` = Substancia)
                 ,multiple = FALSE
                 ,selected = "Areia")
               # __________ Input UF.MET ----
               ,pickerInput(
                 inputId = "id.UF.MET.select"
                 ,label = "UF"
                 ,choices = list(`UF` = uf)
                 ,selected = "AL"
                 ,multiple = FALSE)
               # __________ Input MET ----
               ,pickerInput(
                 inputId = "id.MET.select"
                 ,label = "Reg. Metropolitana"
                 ,choices = list(`RegiaoMet` = RegiaoMetropolitana)
                 ,multiple = FALSE)
               ,actionButton(inputId = "id.Atualizar.button.MET", label = "Ok")
           )
           ,box(width = 8
                ,reactableOutput(outputId = "id.HHI_Substancia_PBruta_MET"))
         )
         ,fluidRow(
           box(title = "Índice Hirchsman-Herfindahl (HHI)"
               ,solidHeader = TRUE
               ,status = "warning"
               ,width = 4
               ,height = '215px'
               ,plotOutput(outputId = "id.Graf.HHI_Substancia_PBruta_MET", height = "150px"))
           ,box(title = Titulo
                ,solidHeader = TRUE
                ,status = "info"
                ,width = 4
                ,height = '215px'
                ,plotOutput(outputId = "id.Graf.preco_Substancia_PBruta_MET", height = "150px"))
           ,box(title = "Coeficiente de Variação (preço)"
                ,solidHeader = TRUE
                ,status = "info"
                ,width = 4
                ,height = '215px'
                ,plotOutput(outputId = "id.Graf.CV_Substancia_PBruta_MET", height = "150px"))
         )
)
))

# tab_HHI_Beneficiada ---- 
,tabItem(tabName = "tab_HHI_Beneficiada"
         ,fluidRow(box(width = 2)))

# tab_CR4 ---- 
,tabItem(tabName = "tab_CR4"
         ,fluidRow(box(width = 2)))
))


# dashboardPage ----

shinyUI(dashboardPage(header, sidebar, body, skin = 'yellow'))












