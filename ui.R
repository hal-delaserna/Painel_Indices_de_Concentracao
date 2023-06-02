# HEADER ----
header <-
  dashboardHeader(title = "Painel de Índices de Concentração", 
                  titleWidth = 350)

# SIDEBAR ----

sidebar <- dashboardSidebar(width = 200,
  sidebarMenu(
    menuItem("Hirchsman-Herfindahl (HHI)",
             tabName = "tab_HHI")
   ,menuItem("Relação de Concentração (CR4)", 
             tabName = "tab_CR4")
    ))

# BODY ----

body <-
  dashboardBody(
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 12px; }")) #change the font size to 20
    ),
    tabItems(
    
# tab_HHI ---- 
    tabItem(tabName = "tab_HHI"
            ,tabsetPanel(

#_____ tab_HHI BR ---- 
              tabPanel("Brasil"

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
         )
)

# tab_CR4 ---- 
,tabItem(tabName = "tab_CR4")
)))


# dashboardPage ----

shinyUI(dashboardPage(header, sidebar, body, skin = 'yellow'))












