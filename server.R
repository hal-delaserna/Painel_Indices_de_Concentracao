
## Expressões reativas ----   

  function(input, output, session) {

#_____ HHI_Substancia_VPM_BR ----
    HHI_Substancia_VPM_BR <-
      eventReactive(input$id.Atualizar.button.BR, {
        summarise(
          group_by(
            MS_Substancia_VPM_BR[MS_Substancia_VPM_BR$Substancia.AMB == input$id.Substancia.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos)
        ) |> arrange(desc(Ano.Base.Ral))
      }
      )  
    
#_____ HHI_Substancia_VPM_UF ----
    HHI_Substancia_VPM_UF <-
      eventReactive(input$id.Atualizar.button.UF, {
      summarise(
        group_by(
          MS_Substancia_VPM_UF[MS_Substancia_VPM_UF$Substancia.AMB == input$id.Substancia.select.UF & 
                                 MS_Substancia_VPM_UF$UF == input$id.UF.select, ],
          Ano.Base.Ral),
        "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
        "N" = length(CPF.CNPJ.Nucleos)
      ) |> arrange(desc(Ano.Base.Ral))
        }
      )   

#_____ HHI_Substancia_VPM_MESO ----
    HHI_Substancia_VPM_MESO <-
      eventReactive(input$id.Atualizar.button.MESO, {
        summarise(
          group_by(
            MS_Substancia_VPM_MESO[MS_Substancia_VPM_MESO$Substancia.AMB == input$id.Substancia.select.MESO & 
                                     MS_Substancia_VPM_MESO$Região.Intermediária.IBGE == input$id.MESO.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos)
        ) |> arrange(desc(Ano.Base.Ral))
      }
      )   
    


    observe({
      RegiaoIntermediaria <- 
        sort(
          unique(geocod[geocod$UF_sigla == input$id.UF.MESO.select,]$Região.Intermediária.IBGE)
          )
      
      # Can use character(0) to remove all choices
      if (is.null(RegiaoIntermediaria))
        RegiaoIntermediaria <- character(0)
      
      # Can also set the label and select items
      updatePickerInput(session = session, 
                        inputId = "id.MESO.select",
                        label = "Mesorregião",
                        choices = RegiaoIntermediaria
      )
    })    
    

    
        
      
###             OBSERVERS PLOT ----
#_____  geom_col - output$id.Graf.HHI_Substancia_VPM_BR ----  
    output$id.Graf.HHI_Substancia_VPM_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_BR(), aes(x = Ano.Base.Ral, y = HHI, group = 1)) + 
          geom_point() + geom_line() 
      })
    
    
#_____ geom_col - output$id.Graf.HHI_Substancia_VPM_UF ----  
    output$id.Graf.HHI_Substancia_VPM_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_UF(), aes(x = Ano.Base.Ral, y = HHI, group = 1)) + 
          geom_point() + geom_line() 
      })

#_____ geom_col - output$id.Graf.HHI_Substancia_VPM_MESO ----  
    output$id.Graf.HHI_Substancia_VPM_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MESO(), aes(x = Ano.Base.Ral, y = HHI, group = 1)) + 
          geom_point() + geom_line() 
      })        
    
    
    
###             OBSERVERS TABELA ----    
    
#_____ TABELA GERAL - output$id.Tb.HHI.BR ----  
 output$id.HHI_Substancia_VPM_BR <- 
   renderReactable({
     reactable(HHI_Substancia_VPM_BR()  
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
    
#_____ TABELA GERAL - output$id.Tb.HHI.UF ----  
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
    

#_____ TABELA GERAL - output$id.Tb.HHI.MESO ----  
output$id.HHI_Substancia_VPM_MESO <- 
  renderReactable({
    reactable(HHI_Substancia_VPM_MESO()  
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







