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
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })  
    
#_____ HHI_Substancia_VPM_UF ----
    HHI_Substancia_VPM_UF <-
      eventReactive(input$id.Atualizar.button.UF, {
      summarise(
        group_by(
          MS_Substancia_VPM_UF[MS_Substancia_VPM_UF$Substancia.AMB == input$id.Substancia.select.UF & 
                                 MS_Substancia_VPM_UF$UF == input$id.UF.select, ],
          Ano.Base.Ral),
        "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
        "N" = length(CPF.CNPJ.Nucleos),
        "p(nominal)" = unique(p_Substancia),
        "SD" = unique(SD_Substancia),
        "CV" = unique(CV_Substancia),
        "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
        })   

#_____ HHI_Substancia_VPM_MESO ----
    HHI_Substancia_VPM_MESO <-
      eventReactive(input$id.Atualizar.button.MESO, {
        summarise(
          group_by(
            MS_Substancia_VPM_MESO[MS_Substancia_VPM_MESO$Substancia.AMB == input$id.Substancia.select.MESO & 
                                     MS_Substancia_VPM_MESO$Região.Intermediária.IBGE == input$id.MESO.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })
    
    
    
    #_____ HHI_Substancia_VPM_MICRO ----
    HHI_Substancia_VPM_MICRO <-
      eventReactive(input$id.Atualizar.button.MICRO, {
        summarise(
          group_by(
            MS_Substancia_VPM_MICRO[MS_Substancia_VPM_MICRO$Substancia.AMB == input$id.Substancia.select.MICRO & 
                                     MS_Substancia_VPM_MICRO$Região.Imediata.IBGE == input$id.MICRO.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })
    
    
    #_____ HHI_Substancia_VPM_RMT ----
    HHI_Substancia_VPM_RMT <-
      eventReactive(input$id.Atualizar.button.RMT, {
        summarise(
          group_by(
            MS_Substancia_VPM_RMT[MS_Substancia_VPM_RMT$Substancia.AMB == input$id.Substancia.select.RMT & 
                                      MS_Substancia_VPM_RMT$Região.Metropolitana.IBGE == input$id.RMT.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })
#_____Formulário Hierárquico MESO
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
                        choices = RegiaoIntermediaria)
    })    

    
    #_____Formulário Hierárquico MICRO
    observe({
      RegiaoImediata <- 
        sort(
          unique(geocod[geocod$UF_sigla == input$id.UF.MICRO.select,]$Região.Imediata.IBGE)
        )
      
      # Can use character(0) to remove all choices
      if (is.null(RegiaoImediata))
        RegiaoImediata <- character(0)
      
      # Can also set the label and select items
      updatePickerInput(session = session, 
                        inputId = "id.MICRO.select",
                        label = "Microrregião",
                        choices = RegiaoImediata)
    })    
    
    
    #_____Formulário Hierárquico RMT
    observe({
      RegiaoMetropolitana <- 
        sort(
          unique(geocod[geocod$UF_sigla == input$id.UF.RMT.select,]$Região.Metropolitana.IBGE)
        )
      
      # Can use character(0) to remove all choices
      if (is.null(RegiaoMetropolitana))
        RegiaoMetropolitana <- character(0)
      
      # Can also set the label and select items
      updatePickerInput(session = session, 
                        inputId = "id.RMT.select",
                        label = "Região Metropolitana",
                        choices = RegiaoMetropolitana)
    })    
    
    
    
        
###             OBSERVERS PLOT ----
#_____  geom_col - output$id.Graf.HHI_Substancia_VPM_BR ----  
    output$id.Graf.HHI_Substancia_VPM_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_BR(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_VPM_BR ----  
    output$id.Graf.preco_Substancia_VPM_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_BR(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = 'grey') + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`)) + 
          labs(x = "Ano", y = "", caption = "⬤   Preço Real           O   Preço Nominal") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_VPM_BR ----  
    output$id.Graf.CV_Substancia_VPM_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_BR(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })

#_____  geom_col - output$id.Graf.HHI_Substancia_VPM_UF ----  
    output$id.Graf.HHI_Substancia_VPM_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_UF(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_VPM_UF ----  
    output$id.Graf.preco_Substancia_VPM_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_UF(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_VPM_UF ----  
    output$id.Graf.CV_Substancia_VPM_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_UF(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.HHI_Substancia_VPM_MESO ----  
    output$id.Graf.HHI_Substancia_VPM_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MESO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_VPM_MESO ----  
    output$id.Graf.preco_Substancia_VPM_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MESO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_VPM_MESO ----  
    output$id.Graf.CV_Substancia_VPM_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MESO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.HHI_Substancia_VPM_MICRO ----  
    output$id.Graf.HHI_Substancia_VPM_MICRO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MICRO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_VPM_MICRO ----  
    output$id.Graf.preco_Substancia_VPM_MICRO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MICRO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_VPM_MICRO ----  
    output$id.Graf.CV_Substancia_VPM_MICRO <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_MICRO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
    
#_____  geom_col - output$id.Graf.HHI_Substancia_VPM_RMT ----  
    output$id.Graf.HHI_Substancia_VPM_RMT <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_RMT(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_VPM_RMT ----  
    output$id.Graf.preco_Substancia_VPM_RMT <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_RMT(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_VPM_RMT ----  
    output$id.Graf.CV_Substancia_VPM_RMT <-
      renderPlot({
        ggplot(data = HHI_Substancia_VPM_RMT(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV)) + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme_bw()
      })
    
###             OBSERVERS TABELA ----    
    
#_____ TABELA GERAL - output$id.Tb.HHI.BR ----  
 output$id.HHI_Substancia_VPM_BR <- 
   renderReactable({
     reactable(HHI_Substancia_VPM_BR()  
             ,defaultColDef = colDef(align = "center")
             ,theme = reactableTheme(cellStyle = list(fontSize = '11px')) 
             ,filterable = FALSE
             ,resizable = FALSE
             ,compact = TRUE
             ,highlight = TRUE
             ,striped = TRUE
             ,sortable = TRUE
             ,wrap = TRUE
             ,fullWidth = TRUE)
   })
    
#_____ TABELA GERAL - output$id.Tb.HHI.UF ----  
output$id.HHI_Substancia_VPM_UF <- 
    renderReactable({
      reactable(HHI_Substancia_VPM_UF()  
              ,defaultColDef = colDef(align = "center")
              ,theme = reactableTheme(cellStyle = list(fontSize = '11px')) 
              ,filterable = FALSE
              ,resizable = FALSE
              ,compact = TRUE
              ,highlight = TRUE
              ,striped = TRUE
              ,sortable = TRUE
              ,wrap = TRUE
              ,fullWidth = TRUE)
      })    
    

#_____ TABELA GERAL - output$id.Tb.HHI.MESO ----  
output$id.HHI_Substancia_VPM_MESO <- 
  renderReactable({
    reactable(HHI_Substancia_VPM_MESO()  
              ,defaultColDef = colDef(align = "center")
              ,theme = reactableTheme(cellStyle = list(fontSize = '11px')) 
              ,filterable = FALSE
              ,resizable = FALSE
              ,compact = TRUE
              ,highlight = TRUE
              ,striped = TRUE
              ,sortable = TRUE
              ,wrap = TRUE
              ,fullWidth = TRUE)
  })
    
    
#_____ TABELA GERAL - output$id.Tb.HHI.MICRO ----  
    output$id.HHI_Substancia_VPM_MICRO <- 
      renderReactable({
        reactable(HHI_Substancia_VPM_MICRO()  
                  ,defaultColDef = colDef(align = "center")
                  ,theme = reactableTheme(cellStyle = list(fontSize = '11px')) 
                  ,filterable = FALSE
                  ,resizable = FALSE
                  ,compact = TRUE
                  ,highlight = TRUE
                  ,striped = TRUE
                  ,sortable = TRUE
                  ,wrap = TRUE
                  ,fullWidth = TRUE)
      })
    
    
#_____ TABELA GERAL - output$id.Tb.HHI.RMT ----  
    output$id.HHI_Substancia_VPM_RMT <- 
      renderReactable({
        reactable(HHI_Substancia_VPM_RMT()  
                  ,defaultColDef = colDef(align = "center")
                  ,theme = reactableTheme(cellStyle = list(fontSize = '11px')) 
                  ,filterable = FALSE
                  ,resizable = FALSE
                  ,compact = TRUE
                  ,highlight = TRUE
                  ,striped = TRUE
                  ,sortable = TRUE
                  ,wrap = TRUE
                  ,fullWidth = TRUE)
      })

}




















