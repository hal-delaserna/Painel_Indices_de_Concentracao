

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
 ,box(width = 2
      ,pickerInput(
        inputId = "id.Substancia.select"
        ,label = "Substância"
        ,choices = list(`Substancia` = Substancia)
        ,multiple = FALSE
        ,selected = "Areia"
        ),actionButton(inputId = "id.Atualizar.button.BR", label = "Ok")
      )
    

,box(width = 5
     ,reactableOutput(outputId = "id.HHI_Substancia_VPM_BR")
     )

 ,box(width = 5
     ,height = '180px'
     ,plotOutput(outputId = "id.Graf.HHI_Substancia_VPM_BR", height = "150px")
     )),


#_____ tab_HHI UF ---- 
tabPanel("UF"
         
# __________Input Substância ----
,box(width = 2
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
       ,multiple = FALSE
       # ,selected = "TODAS"
     ),actionButton(inputId = "id.Atualizar.button.UF", label = "Ok")
)


,box(width = 5
     ,reactableOutput(outputId = "id.HHI_Substancia_VPM_UF")
)

,box(width = 5
     ,height = '180px'
     ,plotOutput(outputId = "id.Graf.HHI_Substancia_VPM_UF", height = "150px")
)),




#_____ tab_HHI MESO ---- 
tabPanel("Mesorregião"
         
# __________Input Substância ----
,box(width = 2
     ,pickerInput(
       inputId = "id.Substancia.select.MESO"
       ,label = "Substância"
       ,choices = list(`Substancia` = Substancia)
       ,multiple = FALSE
       ,selected = "Areia"
     )
# __________ Input UF.MESO ----
     ,pickerInput(
       inputId = "id.UF.MESO.select"
       ,label = "UF"
       ,choices = list(`UF` = uf)
       ,multiple = FALSE
       # ,selected = "TODAS"
     )
# __________ Input MESO ----
     ,pickerInput(
       inputId = "id.MESO.select"
       ,label = "Mesorregião"
       ,choices = list(`Mesorregião` = RegiaoIntermediaria)
       ,multiple = FALSE
   # ,selected = "TODAS"
 ),actionButton(inputId = "id.Atualizar.button.MESO", label = "Ok")
 )


,box(width = 5
     ,reactableOutput(outputId = "id.HHI_Substancia_VPM_MESO")
)

,box(width = 5
     ,height = '180px'
     ,plotOutput(outputId = "id.Graf.HHI_Substancia_VPM_MESO", height = "150px")
))



))









# tab_HHI ---- 
,tabItem(tabName = "tab_CR4")
))


# dashboardPage ----

shinyUI(dashboardPage(header, sidebar, body, skin = 'yellow'))












