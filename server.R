## Expressões reativas ----   

  function(input, output, session) {

#_____ HHI_Substancia_PBruta_BR ----
    HHI_Substancia_PBruta_BR <-
      eventReactive(input$id.Atualizar.button.BR, {
        summarise(
          group_by(
            MS_Substancia_PBruta_BR[MS_Substancia_PBruta_BR$Substancia.AMB == input$id.Substancia.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })  
    
#_____ HHI_Substancia_PBruta_UF ----
    HHI_Substancia_PBruta_UF <-
      eventReactive(input$id.Atualizar.button.UF, {
      summarise(
        group_by(
          MS_Substancia_PBruta_UF[MS_Substancia_PBruta_UF$Substancia.AMB == input$id.Substancia.select.UF & 
                                 MS_Substancia_PBruta_UF$UF == input$id.UF.select, ],
          Ano.Base.Ral),
        "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
        "N" = length(CPF.CNPJ.Nucleos),
        "p(nominal)" = unique(p_Substancia),
        "SD" = unique(SD_Substancia),
        "CV" = unique(CV_Substancia),
        "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
        })   

#_____ HHI_Substancia_PBruta_MESO ----
    HHI_Substancia_PBruta_MESO <-
      eventReactive(input$id.Atualizar.button.MESO, {
        summarise(
          group_by(
            MS_Substancia_PBruta_MESO[MS_Substancia_PBruta_MESO$Substancia.AMB == input$id.Substancia.select.MESO & 
                                     MS_Substancia_PBruta_MESO$Região.Intermediária.IBGE == input$id.MESO.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })
    
    
    
    #_____ HHI_Substancia_PBruta_MICRO ----
    HHI_Substancia_PBruta_MICRO <-
      eventReactive(input$id.Atualizar.button.MICRO, {
        summarise(
          group_by(
            MS_Substancia_PBruta_MICRO[MS_Substancia_PBruta_MICRO$Substancia.AMB == input$id.Substancia.select.MICRO & 
                                     MS_Substancia_PBruta_MICRO$Região.Imediata.IBGE == input$id.MICRO.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })
    
    
    #_____ HHI_Substancia_PBruta_MET ----
    HHI_Substancia_PBruta_MET <-
      eventReactive(input$id.Atualizar.button.MET, {
        summarise(
          group_by(
            MS_Substancia_PBruta_MET[MS_Substancia_PBruta_MET$Substancia.AMB == input$id.Substancia.select.MET & 
                                      MS_Substancia_PBruta_MET$Reg_Metropolitana == input$id.MET.select, ],
            Ano.Base.Ral),
          "HHI" = round(10000*hhi(MS_Substancia, na.rm = TRUE),0),
          "N" = length(CPF.CNPJ.Nucleos),
          "p(nominal)" = unique(p_Substancia),
          "SD" = unique(SD_Substancia),
          "CV" = unique(CV_Substancia),
          "p(real)" = unique(p_Substancia_Real)) |> arrange(desc(Ano.Base.Ral))
      })
#_____Formulário Hierárquico MESO_PBruta
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

    
    #_____Formulário Hierárquico MICRO_PBruta
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
    
    
    #_____Formulário Hierárquico MET_PBruta
    observe({
      RegiaoMetropolitana <- 
        sort(
          unique(geocod[geocod$UF_sigla == input$id.UF.MET.select,]$Reg_Metropolitana)
        )
      
      # Can use character(0) to remove all choices
      if (is.null(RegiaoMetropolitana))
        RegiaoMetropolitana <- character(0)
      
      # Can also set the label and select items
      updatePickerInput(session = session, 
                        inputId = "id.MET.select",
                        label = "Região Metropolitana",
                        choices = RegiaoMetropolitana)
    })    
    
    
    
        
###             OBSERVERS PLOT ----
#_____  geom_col - output$id.Graf.HHI_Substancia_PBruta_BR ----  
    output$id.Graf.HHI_Substancia_PBruta_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_BR(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_PBruta_BR ----  
    output$id.Graf.preco_Substancia_PBruta_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_BR(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = 'grey') + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`)) + 
          labs(x = "Ano", y = "", caption = "⬤   Preço Real           O   Preço Nominal") +
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_PBruta_BR ----  
    output$id.Graf.CV_Substancia_PBruta_BR <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_BR(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })

#_____  geom_col - output$id.Graf.HHI_Substancia_PBruta_UF ----  
    output$id.Graf.HHI_Substancia_PBruta_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_UF(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_PBruta_UF ----  
    output$id.Graf.preco_Substancia_PBruta_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_UF(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_PBruta_UF ----  
    output$id.Graf.CV_Substancia_PBruta_UF <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_UF(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.HHI_Substancia_PBruta_MESO ----  
    output$id.Graf.HHI_Substancia_PBruta_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MESO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_PBruta_MESO ----  
    output$id.Graf.preco_Substancia_PBruta_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MESO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_PBruta_MESO ----  
    output$id.Graf.CV_Substancia_PBruta_MESO <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MESO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.HHI_Substancia_PBruta_MICRO ----  
    output$id.Graf.HHI_Substancia_PBruta_MICRO <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MICRO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_PBruta_MICRO ----  
    output$id.Graf.preco_Substancia_PBruta_MICRO <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MICRO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_PBruta_MICRO ----  
    output$id.Graf.CV_Substancia_PBruta_MICRO <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MICRO(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV), color = "brown") + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
    
#_____  geom_col - output$id.Graf.HHI_Substancia_PBruta_MET ----  
    output$id.Graf.HHI_Substancia_PBruta_MET <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MET(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = HHI), color = "brown") + geom_point(aes(y = HHI)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.Preco_Substancia_PBruta_MET ----  
    output$id.Graf.preco_Substancia_PBruta_MET <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MET(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = `p(nominal)`), color = "grey") + geom_point(aes(y = `p(nominal)`), shape = 1) + 
          geom_line(aes(y = `p(real)`), color = "brown") + geom_point(aes(y = `p(real)`), shape = 4) + 
          labs(x = "Ano", y = "", caption = "X   Preço Real           O   Preço Nominal") +
          theme_bw(base_size = 14)
      })
    
#_____  geom_col - output$id.Graf.CV_Substancia_PBruta_MET ----  
    output$id.Graf.CV_Substancia_PBruta_MET <-
      renderPlot({
        ggplot(data = HHI_Substancia_PBruta_MET(),aes(x = Ano.Base.Ral)) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~.)) +
          geom_line(aes(y = CV)) + geom_point(aes(y = CV)) + 
          labs(x = "Ano", y = "") + 
          theme_bw(base_size = 14)
      })
    
###             OBSERVERS TABELA ----    
    
#_____ TABELA GERAL - output$id.Tb.HHI.BR ----  
 output$id.HHI_Substancia_PBruta_BR <- 
   renderReactable({
     reactable(HHI_Substancia_PBruta_BR()  
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
output$id.HHI_Substancia_PBruta_UF <- 
    renderReactable({
      reactable(HHI_Substancia_PBruta_UF()  
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
output$id.HHI_Substancia_PBruta_MESO <- 
  renderReactable({
    reactable(HHI_Substancia_PBruta_MESO()  
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
    output$id.HHI_Substancia_PBruta_MICRO <- 
      renderReactable({
        reactable(HHI_Substancia_PBruta_MICRO()  
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
    
    
#_____ TABELA GERAL - output$id.Tb.HHI.MET ----  
    output$id.HHI_Substancia_PBruta_MET <- 
      renderReactable({
        reactable(HHI_Substancia_PBruta_MET()  
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




















