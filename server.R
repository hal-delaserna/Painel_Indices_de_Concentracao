
  function(input, output, session) {

# HHI_Substancia_VPM_BR
    HHI_Substancia_VPM_BR <-
      eventReactive(input$id.Atualizar.button, {
        summarise(
          group_by(
            MS_Substancia_VPM_BR[MS_Substancia_VPM_BR$Substancia.AMB == input$id.Substancia.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos)
        ) |> arrange(desc(Ano.Base.Ral))
      }
      )       
    
# HHI_Substancia_VPM_UF
    HHI_Substancia_VPM_UF <-
      eventReactive(input$id.Atualizar.button, {
      summarise(
        group_by(
          MS_Substancia_VPM_UF[MS_Substancia_VPM_UF$Substancia.AMB == input$id.Substancia.select & 
                                 MS_Substancia_VPM_UF$UF == input$id.UF.select, ],
          Ano.Base.Ral),
        "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
        "N" = length(CPF.CNPJ.Nucleos)
      ) |> arrange(desc(Ano.Base.Ral))
        }
      )   

# HHI_Substancia_VPM_MESO
    HHI_Substancia_VPM_MESO <-
      eventReactive(input$id.Atualizar.button, {
        summarise(
          group_by(
            MS_Substancia_VPM_MESO[MS_Substancia_VPM_MESO$Substancia.AMB == input$id.Substancia.select & 
                                     MS_Substancia_VPM_MESO$UF == input$id.UF.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos)
        ) |> arrange(desc(Ano.Base.Ral))
      }
      )   
    
    
    
      
###             OBSERVERS PLOT ----
  #__ geom_col - output$id.Graf.Divisao.Trimestre ----  
    output$id.Graf.Estoque.Trimestre <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_UF(), aes(x = Ano.Base.Ral, y = HHI, group = 1)) + 
          geom_point() + geom_line() 
      })
    

#___ TABELA GERAL - output$id.Tb.CNAE ----  
 output$id.HHI_Substancia_VPM_UF <- 
   renderReactable({
     reactable(HHI_Substancia_VPM_UF()  
             ,defaultColDef = colDef(align = "center")
             ,theme = reactableTheme(cellStyle = list(fontSize = '11px')) 
             ,filterable = FALSE
             ,resizable = TRUE
             ,compact = FALSE
             ,striped = TRUE
             ,sortable = TRUE
             ,wrap = TRUE
             ,fullWidth = TRUE
             )
   })
 }







