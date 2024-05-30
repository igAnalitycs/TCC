inicio_server = function(input, output, session, dados, lista_inovacoes_mercado, lista_produtos) {
  
  #Carregando os dados -----------------------------------------------------------
  
  # Observe --------------------------------------------------------------------
  
  observe({
    req(input$input_geo)
    input_geo = input$input_geo
    
    marcas_geo = lista_produtos %>% 
      filter(geo == input$input_geo, is_launch_prod == 1) %>% 
      distinct(marca)
    
    updateSelectInput(
      session,
      'input_marca',
      "Selecione uma Marca:",
      choices = c("TODAS AS MARCAS", sort(marcas_geo$marca))
    )
  })
  
  #Output's--------------------------------------------------------------------------
  
  output$vendas_mes_marca_geo = renderHighchart({
    
    req(input$input_geo)
    req(input$input_marca)
    
    plot_vendas_geo_marca(dados = dados,
                          input_geo = input$input_geo,
                          input_marca = input$input_marca,
                          input_inovacao = 'mercado')

  })
  
  output$table_inovations = renderDT({
    req(input$input_geo)
    req(input$input_marca)
    
    tabela = lista_inovacoes_lancamento(dados = dados, 
                                        input_geo = input$input_geo, 
                                        input_marca = input$input_marca,
                                        input_inovacao = 'mercado')
    
    tabela = tabela %>% 
      mutate(mes = as.character(as.yearmon(mes_inicio_prod))) %>% 
      select(prod, mes, tempo_entre_inovacao) %>% 
      rename("Inovações" = "prod",
             "Mês de Lançamento" = "mes",
             "Ciclo Entre Inovações" = "tempo_entre_inovacao")
    ##Criando interatividade para a tabela
    datatable(
      tabela,
      caption = htmltools::tags$caption("CLIQUE NO PRODUTO PARA MAIORES OBSERVAÇÕES", style = "color:black"),
      selection = "single",
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        lengthMenu = NULL,
        deferRender = TRUE,
        language = list(
          info = "_START_ até _TOTAL_",
          search = "PESQUISAR:"
        ),
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c("copy", "excel"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#5B1217', 'color': 'white'});",
          "}"
        )
      )
    )
    
  })
  
  output$grafico_volume_produto = renderHighchart({
    req(input$table_inovations_rows_selected)
    req(input$input_marca)
    req(input$input_geo)
    #req(input$input_inovacao)
    
    plot_volume_produto(
      dados = dados,
      lista_inovacoes_mercado = lista_inovacoes_mercado,
      input_linha_tabela = input$table_inovations_rows_selected,
      input_marca = input$input_marca,
      input_geo = input$input_geo,
      input_inovacao = 'mercado'
    )
  })
  
  output$ui_botao_inovacao = renderUI({
    
    req(input$table_inovations_rows_selected)
    
    radioButtons(
      inputId = "btn_nivel_inovacao",
      label = "Apenas Inovações",
      choices = c("Sim", "Não"),
      selected = "column",
      inline = TRUE
    )
    
  })
  
  output$grafico_share_prod = renderHighchart({
    req(input$input_marca)
    req(input$input_geo)
    req(input$table_inovations_rows_selected)
    #req(input$input_inovacao)
    
    if(input$btn_nivel_inovacao == "Não"){
      plot = plot_share_por_produto(
        dados = dados,
        lista_inovacoes_mercado = lista_inovacoes_mercado,
        input_geo = input$input_geo, 
        input_marca = input$input_marca,
        input_linha_tabela = input$table_inovations_rows_selected,
        input_inovacao = 'mercado'
      )
    }
    
    if(input$btn_nivel_inovacao == "Sim"){
      plot = plot_share_por_produto_grupo(
        dados = dados,
        lista_inovacoes_mercado = lista_inovacoes_mercado,
        input_geo = input$input_geo, 
        input_marca = input$input_marca,
        input_linha_tabela = input$table_inovations_rows_selected,
        input_inovacao = 'mercado'
      )
    }
    
    return(plot)
  })
  
  output$grafico_comp_share_geo = renderHighchart({
    req(input$input_marca)
    req(input$input_geo)
    req(input$table_inovations_rows_selected)
    #req(input$input_inovacao)
    
    plot_share_comp_prod_geo(
      dados = dados,
      lista_inovacoes_mercado = lista_inovacoes_mercado,
      input_linha_tabela = input$table_inovations_rows_selected,
      input_marca = input$input_marca,
      input_geo = input$input_geo,
      input_inovacao = 'mercado'
    )
    
    
  })
  
  output$grafico_antes_depois = renderHighchart({
    
    req(input$input_marca)
    req(input$input_geo)
    req(input$table_inovations_rows_selected)
    
    plot_comp_share_before_after(
      dados = dados,
      lista_inovacoes_mercado = lista_inovacoes_mercado,
      input_linha_tabela = input$table_inovations_rows_selected,
      input_marca = input$input_marca,
      input_geo = input$input_geo,
      input_inovacao = 'mercado'
    )
    
  })
  
  output$tabela_prod_geo = renderDT({
    
    req(input$input_marca)
    req(input$input_geo)
    req(input$table_inovations_rows_selected)
    
    
    tabela = table_plot_geo(
      dados = dados,
      lista_inovacoes_mercado = lista_inovacoes_mercado,
      input_geo = input$input_geo, 
      input_marca = input$input_marca,
      input_linha_tabela = input$table_inovations_rows_selected,
      input_inovacao = 'mercado'
    )
    
    datatable(
      tabela %>% 
        select("Região","Data de Lançamento","Mês Share Maximo","Share Máximo",
               "Tempo Para alcançar o Share Máximo","Tempo Entre Inovações"),
      caption = htmltools::tags$caption(paste0("Produto: ", unique(tabela$produto)), style="color:black"),#Legenda
      selection = "single",#apenas uma linha pode ser selecionada na tabela
      rownames = FALSE,# números de linha na tabela
      extensions = "Buttons",
      options = list(
        columnDefs = list(list(className = "dt-center", targets = "_all")), #centralizar tudo
        lengthMenu = NULL,
        deferRender = TRUE,# Tabela carregada de forma mais rápida 
        language = list(
          info = "_START_até_TOTAL_",
          search = "PESQUISAR:"
        ),
        pageLength = 10,#Define o número padrão de linhas visíveis por página.
        dom = "Bfrtip",#botões de controle
        buttons = c("copy", "excel"),
        initComplete = JS(#Código personalizado quando a tabela é carregada
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#5B1217', 'color': 'white'});",
          "}"
        )
      )
    )
  })
  
  
  output$grafico_maturity_inno = renderHighchart({
    
    req(input$input_marca)
    req(input$input_geo)
    
    plot_maturity_inovations(
      dados = dados,
      lista_inovacoes_mercado = lista_inovacoes_mercado,
      input_marca = input$input_marca,
      input_geo = input$input_geo,
      input_inovacao = 'mercado'
    )
  })
  
  
  output$grafico_share_inno_prod = renderHighchart({
    
    req(input$input_marca)
    req(input$input_geo)
    req(input$btn_input_visao)
    
    plot_share_cycle_time_all_innovations_in_brand(
      dados = dados,
      lista_inovacoes_mercado = lista_inovacoes_mercado,
      btn_input_visao = input$btn_input_visao,
      input_marca = input$input_marca,
      input_geo = input$input_geo,
      input_inovacao = 'mercado'
    )
  })
  
  
}

