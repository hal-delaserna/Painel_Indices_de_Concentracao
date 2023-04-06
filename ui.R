

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
    
# TAB_Graf_Extrativa_Divisao ---- 
    tabItem(tabName = "tab_HHI"


###              INPUT BOX ----

#__Box dos Inputs Substância-Produto----            

#__Input Substância ----                  
 ,box(width = 2
      ,pickerInput(
        inputId = "id.Substancia.select"
        ,label = "Substância"
        ,choices = list(`Substancia` = Substancia)
        ,multiple = FALSE
        ,selected = "Areia"
        )
#__Input UF ----                  
      ,pickerInput(
       inputId = "id.UF.select"
      ,label = "UF"
      ,choices = list(`UF` = uf)
      ,multiple = FALSE
    # ,selected = "TODAS"
        ),actionButton(inputId = "id.Atualizar.button", label = "Ok")
      )
    

,box(width = 5
     ,reactableOutput(outputId = "id.HHI_Substancia_VPM_UF")
     )

 ,box(width = 5
     ,height = 150
     ,plotOutput(outputId = "id.Graf.Estoque.Trimestre")
     )
),
  tabItem(tabName = "tab_CR4")
))


# dashboardPage ----

shinyUI(dashboardPage(header, sidebar, body, skin = 'yellow'))












