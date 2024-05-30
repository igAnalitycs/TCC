# Plot´s (INOVAÇÕES EM TODOS OS CANAIS) ======================================================================

plot_vendas_geo_marca = function(dados, input_geo, input_marca, input_inovacao){
  
  dados_plot = share_vendas_inovacao_mes_geo(
    dados = dados,
    input_geo = input_geo,
    input_marca = input_marca,
    input_inovacao = input_inovacao
  ) %>% 
    mutate( mes = as.character(mes),
            indice_preco_dif0 = ifelse(indice_preco, 1) == 0, NA_character_, paste0(round(indice_preco,1), "%"))
  
  if(input_marca == "TODAS AS MARCAS"){
    titulo = paste0("Share de vendas de marcas e inovaçoes por mês - ",input_geo)
    
    tooltip_vendas = list(pointFormat = paste0(
      "<style='display: block; font-size: 11px;'>Vendas em HL por Marca: <b> {point.vendas_in_hl:,.0f}</b><br>",
      "<style='display: block; font-size: 11px;'>Número de Inovações: <b>{point.inovacao}</b><br>",
      "<style='display: block; font-size: 11px;'>Numero de Produtos: <b>{point.n_prod}</b><br>"
    ))
    
    tooltip_share = list(pointFormat = paste0(
      "<style='display: block; font-size: 11px;'>",
      "{series.name} para todas as marcas: <b> {point.indice_preco:.2f}%</b><br>"
    ))
  } else{
    
    titulo = paste0("Contribuição e vendas da marca ", input_marca, " por mês - ", input_geo)
    
    tooltip_vendas = list(pointFormat = paste0(
      "<style='display: block; font-size: 11px;'>Vendas em HL da marca ", input_marca,":<b> {point.vendas_in_hl:,.0f}</b><br>",
      "<style='display: block; font-size: 11px;'>Número de Inovações na marca ", input_marca,":<b>{point.inovacao}</b><br>",
      "<style='display: block; font-size: 11px;'>Numero de Produtos na marca ", input_marca,":<b>{point.n_prod}</b><br>"
    ))
    
    tooltip_share = list(pointFormat = paste0(
      "<style='display: block; font-size: 11px;'>",
      "{series.name} para marca ",input_marca, ": <b> {point.y:.2f}%</b><br>"
    ))
    
  }
  
  minimo_vendas = min(dados_plot$vendas_in_hl)
  minimo_vendas = case_when(
    minimo_vendas < 1000 ~ 0,
    minimo_vendas >= 1000 & minimo_vendas < 5000 ~ 1000,
    minimo_vendas >= 5000 & minimo_vendas < 10000 ~ 5000,
    minimo_vendas >= 10000 & minimo_vendas < 50000 ~ 10000,
    minimo_vendas >= 50000 & minimo_vendas < 100000 ~ 50000,
    TRUE ~  minimo_vendas - 50000
  )
  minimo_vendas = case_when(
    input_geo == "GEO CO" & input_marca == "TODAS AS MARCAS" ~ 200000,
    input_geo == "GEO MG" & input_marca == "TODAS AS MARCAS" ~ 500000,
    input_geo == "GEO NE" & input_marca == "TODAS AS MARCAS" ~ 200000,
    input_geo == "GEO N0" & input_marca == "TODAS AS MARCAS" ~ 200000,
    input_geo == "GEO RJ+ES" & input_marca == "TODAS AS MARCAS" ~ 450000,
    input_geo == "GEO SP" & input_marca == "TODAS AS MARCAS" ~ 850000,
    input_geo == "GEO SUL" & input_marca == "TODAS AS MARCAS" ~ 500000,
    TRUE ~ minimo_vendas
  )
  
  maximo_vendas = max(dados_plot$vendas_in_hl)
  maximo_vendas = case_when(
    maximo_vendas < 100 ~ 100,
    maximo_vendas >= 100 & maximo_vendas < 1000 ~ 1000,
    maximo_vendas >= 1000 & maximo_vendas < 5000 ~ 5000,
    maximo_vendas >= 5000 & maximo_vendas < 10000 ~ 10000,
    maximo_vendas >= 10000 & maximo_vendas < 50000 ~ 50000,
    maximo_vendas >= 50000 & maximo_vendas < 100000 ~ 90000,
    TRUE ~  maximo_vendas + 100000
  )
  maximo_vendas = case_when(
    # input_geo == "GEO CO" & input_marca == "TODAS AS MARCAS" ~ 200000,
    # input_geo == "GEO MG" & input_marca == "TODAS AS MARCAS" ~ 500000,
    input_geo == "GEO NE" & input_marca == "TODAS AS MARCAS" ~ 900000,
    input_geo == "GEO N0" & input_marca == "TODAS AS MARCAS" ~ 850000,
    input_geo == "GEO RJ+ES" & input_marca == "TODAS AS MARCAS" ~ 1500000,
    input_geo == "GEO SP" & input_marca == "TODAS AS MARCAS" ~ 2100000,
    #input_geo == "GEO SUL" & input_marca == "TODAS AS MARCAS" ~ 500000,
    TRUE ~ maximo_vendas
  )
  
  maximo_share = max(dados_plot$indice_preco)
  maximo_share = case_when(
    maximo_share < 2 ~ maximo_share+1,
    maximo_share >=2 & maximo_share < 5 ~ maximo_share+2,
    maximo_share >= 5 & maximo_share < 10 ~  maximo_share+3,
    maximo_share >= 10 ~ maximo_share+5
  )
  
  
  plot = highchart() %>% 
    hc_add_series(
      type = "column",
      data = dados_plot,
      hcaes(
        y = indice_preco,
        x = mes,
        group = agrup,
        indice_preco_dif0 = indice_preco_dif0
        #color = c("#81b29a")
      ),
      yAxis = 1,
      tooltip = tooltip_share
      ) %>% 
    hc_plotOptions(#Gráfico de forma empilhada
      column = list(
        stacking = "normal",
        groupPadding = 0.1
      )
    ) %>% 
    hc_add_series(
      name = paste0("<b style=display: block;>", "Vendas em hl", '</b>'),
      data = dados_plot %>% distinct(vendas_in_hl, mes, inovacao, inovation_dif0, n_prod),
      type = "line",
      hcaes(
        y = vendas_in_hl, 
        x = mes,
        inovacao = inovacao,
        inovation_dif0 = inovation_dif0,
        n_prod = n_prod
        #color = c("#FF0000")
      ),
      yAxis = 0,
      tooltip = tooltip_vendas
    ) %>% 
    hc_xAxis(
      categories = unique(dados_plot$mes),
      crosshair = TRUE,
      showInLegend = F,
      type = "category",
      labels = list(rotation = -50,
                    style = list(fontSize = '12px'),
                    x = -10,
                    y = 25)
    ) %>% 
    hc_yAxis_multiples(
      list(lineWidth = 3, title = list(text = paste0("<style='display: block; font-size: 12px;'>", 
                                                     "Vendas em hl")), 
           min = minimo_vendas, max = maximo_vendas),
      list(showLastLabel = FALSE, opposite = TRUE, 
           min = 0, max = maximo_share,
           title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                      "% Share das Marcas")))
    ) %>%  
    hc_colors(c("#e6e222","#808080","#5B1217")) %>% 
    hc_tooltip(shared = TRUE) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        titulo, "</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_legend(
      itemStyle = list(
        fontSize = '15px',
        fontWeight = 'bold',
        color = '#5e5e5e'
      )
    ) %>% 
    hc_plotOptions(
      column = list(
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%",
          style = list(color = "#000000",fontWeight = "bold")
        )
      )
    )%>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    )
  
  grupos = dados_plot %>% 
    filter(!is.na(inovation_dif0)) %>% 
    pull(mes) %>% 
    unique() %>% 
    as.character()
  
  valores = dados_plot %>% 
    filter(!is.na(inovation_dif0)) %>% 
    distinct(mes, vendas_in_hl) %>% 
    pull(vendas_in_hl)
  
  inovation = dados_plot %>%
    filter(!is.na(inovation_dif0)) %>% 
    distinct(mes, inovation_dif0) %>% 
    pull(inovation_dif0)
  
  for (i in 1:length(grupos)){
    grupo_selected = grupos[[i]]
    valor = valores[[i]]
    
    ano = as.yearmon(grupos[[i]]) %>% year()
    mes = as.yearmon(grupos[[i]]) %>% month()
    
    if(input_marca != "TODAS AS MARCAS"){
      data_inicio = dados %>% 
        filter(geo == input_geo, marca == input_marca) %>% 
        arrange(mes_inicio_marca) %>% 
        summarise(
          primeiro_mes = first(mes_inicio_marca)
        ) %>% 
        pull(primeiro_mes)
      
      numero_meses = (ano - year(data_inicio))*12 + mes - month(data_inicio)
      x = numero_meses
    }
    else{
      data_inicio = dados %>% 
        filter(geo == input_geo, mes_inicio_prod == first(mes_inicio_prod)) %>% 
        pull(mes_inicio_prod) %>% 
        unique()
      
      numero_meses = (ano - year(data_inicio))*12 + mes - month(data_inicio)
      x = numero_meses
    }
    
    y = valor
    anotacao = paste0(inovation[[i]])
    
    lista = list(
      point = list(
        x = x,
        y = y,
        xAxis = 0,
        yAxis = 0
      ),
      text = anotacao,
      style = list(
        fontSize = "10px"
      )
    )
    
    plot = plot %>% 
      hc_add_annotation(#Cartão informativo e, cima do gráfico
        labels = list(lista),
        labelOptions = list(
          borderRadius = 0,
          backgroundColor = "#ffffff",
          allowOverlap = TRUE,
          #shape = 'path',
          overflow = 'justify',
          padding = 5,
          distance = 12,
          align = "center",
          verticalAlign = 'middle',
          fontSize = "8.5px"
        )
      )
    
  }
 

  return(plot)
  
}

# Plot´s (Contribuição de Produtos em Todos os Canais) ======================================================================


plot_volume_produto = function(dados, lista_inovacoes_mercado,input_geo, input_marca, input_inovacao, input_linha_tabela){
  
  if(input_inovacao == "mercado"){
    tabela = lista_inovacoes_mercado
  }
  else if(input_inovacao == "vendas"){
    tabela = lista_inovacoes_vendas
  }
  
  lista_inovacoes_mercado =lista_inovacoes_mercado %>% ungroup()
  
  
  if(input_marca == "TODAS AS MARCAS"){
    tabela = tabela %>% filter(geo == input_geo)
  }
  else{
    tabela = tabela %>% filter(geo == input_geo, marca == input_marca)
  }
  
  marca_selecionada = tabela[input_linha_tabela,]$marca
  produto = tabela[input_linha_tabela,]$prod
  
  share_marca_geo = dados %>% 
    filter(geo == input_geo) %>% 
    group_by(mes) %>% 
    mutate(total_vendas = sum(vendas_in_hl)) %>% 
    filter(marca == marca_selecionada) %>% 
    group_by(mes) %>% 
    summarise(
      share_marca = sum(vendas_in_hl)/mean(total_vendas)*100
    )
  
  share_prod_marca = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada) %>% 
    group_by(mes) %>% 
    mutate(total_vendas = sum(vendas_in_hl)) %>% 
    filter(prod == produto, mes >= mes_inicio_prod) %>% 
    group_by(mes) %>% 
    summarise(
      share_prod_marca = sum(vendas_in_hl)/mean(total_vendas)*100
    )
  
  share_prod_geo = dados %>% 
    filter(geo == input_geo) %>% 
    group_by(mes) %>% 
    mutate(vendas_total = sum(vendas_in_hl)) %>% 
    filter(prod == produto, mes >= mes_inicio_prod) %>% 
    group_by(mes) %>% 
    summarise(
      share_prod_geo = sum(vendas_in_hl)/mean(vendas_total)*100
    )
  
  dados_plot = share_marca_geo %>% 
    left_join(share_prod_geo, by = "mes") %>% 
    left_join(share_prod_marca, by = "mes") %>% 
    mutate(
      mes = as.character(mes)
    )
  
  titulo = paste0("Curva de Inovação por Marca ",produto, " Por Mês - ", input_geo)
  
  plot = highchart() %>% 
    hc_add_series(
      type = "column",
      name = "Share de Produtos na Marca",
      data = dados_plot,
      hcaes(
        y = share_prod_marca,
        x = mes
      ),
      yAxis = 1,
      labels = list(format = "{value:.2f}%"),
      tooltip = list(
        pointFormat = paste0(
          "<style='display: block; font-size: 11px;'>Produto: <b>",produto,"</b><br>",
          "<style='display: block; font-size: 11px;'>Share de Produtos na Marca: <b> {point.y:.4f}%</b><br>"
        )
      )
    ) %>% 
    hc_add_series(
      type = "line",
      name = "Share da Marca na Geo",
      data = dados_plot,
      hcaes(
        y = share_marca,
        x = mes
      ),
      yAxis = 0,
      labels = list(format = "{value:.2f}%"),
      tooltip = list(
        pointFormat = paste0(
          "<style='display: block; font-size: 11px;'>",
          "Share da Marca na Geo: <b> {point.y:.4f}%</b><br>"
        )
      )
    ) %>% 
    hc_add_series(
      type = "line",
      name = "Share de Produtos na Geo",
      data = dados_plot,
      hcaes(
        y = share_prod_geo,
        x = mes
      ),
      yAxis = 0,
      labels = list(format = "{value:.2f}%"),
      tooltip = list(
        pointFormat = paste0(
          "<style='display: block; font-size: 11px;'>",
          "Share de Produtos na Geo: <b> {point.y:.4f}%</b><br>"
        )
      )
    ) %>% 
    hc_xAxis(
      categories = unique(dados_plot$mes),
      crosshair = TRUE,
      showInLegend = F,
      type = "category",
      labels = list(rotation = -50,
                    style = list(fontSize = '12px'),
                    x = -10,
                    y = 25)
    ) %>% 
    hc_yAxis_multiples(#São 2 eixos Y
      list(lineWidth = 3, 
           title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                                     "% Share da Marca"))),
      list(showLastLabel = FALSE, opposite = TRUE,
           title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                      "% Share de Produtos")))
    ) %>%  
    hc_colors(c("#808080","#e6e222","#5B1217")) %>% 
    hc_tooltip(shared = TRUE) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        titulo, "</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_legend(
      itemStyle = list(
        fontSize = '15px',
        fontWeight = 'bold',
        color = '#5e5e5e'
      )
    ) %>% 
    hc_plotOptions(
      column = list(
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%"
        )
      ),
      line = list(
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%"
        )
      )
    )%>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    )
  
  return(plot)
}

plot_share_por_produto = function(dados, lista_inovacoes_mercado, input_geo, input_marca, input_linha_tabela, input_inovacao){

  if(input_inovacao == 'mercado'){
    is_launch = 'is_launch_prod'
    qnt_inovacao = 'num_inovacao'
  }else if(input_inovacao == 'vendas'){
    is_launch = 'is_launch_prod_sales'
    qnt_inovacao = 'num_inovacao_vendas'
  }
   
  lista_inovacoes_mercado =lista_inovacoes_mercado %>% ungroup()
  
  if(input_inovacao == 'mercado'){
    tabela = lista_inovacoes_mercado
  } else if(input_inovacao == 'vendas'){
    tabela = lista_inovacoes_vendas
  }
  if(input_marca == "TODAS AS MARCAS"){
    tabela = tabela %>% filter(geo == input_geo)
  }else{
    tabela = tabela %>% filter(geo == input_geo, marca == input_marca)
  }
  
  marca_selecionada = tabela[input_linha_tabela,]$marca
  
  mes_inicio_inovacao = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada, !!sym(qnt_inovacao) == 1) %>% 
    distinct(mes_inicio = mes_inicio_prod, prod)
  
  dados_plot = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada) %>% 
    group_by(mes) %>% 
    mutate(vendas_total = sum(vendas_in_hl)) %>% 
    group_by(mes, prod) %>% 
    summarise(
      indice_preco = sum(vendas_in_hl)/mean(vendas_total)*100
    ) %>% 
    ungroup()
  
  dados_plot = dados_plot %>% 
    left_join(mes_inicio_inovacao, by = c("prod")) %>% 
    group_by(prod) %>% 
    mutate(
      indice_preco = case_when(
        prod %in% mes_inicio_inovacao$prod & mes < mes_inicio ~ NA_real_,
        TRUE ~ indice_preco
      )
    ) %>% 
    ungroup() %>% 
    select(-mes_inicio) %>% 
    mutate(
      inovacao = ifelse(prod %in% mes_inicio_inovacao$prod, 1, 0)
    )
  
  paleta_cores_opacas = c("#99c9d6","#dab5e2", "#9dce9c","#b9e2ce","#b0c7d8","#c1bae6","#acf5ab", "#ffe395","#d58581", "#c396d5", "#FFA07A", "#FFC0CB", "#FFE4E1","#DDA0DD", "#B0E0E6", "#D8BFD8")
  paleta_cores_escuras = c("#3493ad","#b66bc5", "#3C9E3A","#74c69d","#618fb1","#8475cd","#59EB57", "#ffc72c","#ab0c04", "#872EAB")
  paleta_cores_inovacoes = c("#FFD700",'#8B0000',"#3493ad", '#2F4F4F', "#3C9E3A", '#F4A460',  "#b66bc5", '#FF69B4', "#3C9E3A",  '#20B2AA')
  
  lista_prod = dados_plot %>% 
    distinct(prod, inovacao)
  
  cores_inov = lista_prod %>% 
    filter(inovacao == 1)
  
  cores_inov = cores_inov %>% 
    bind_cols(cor = paleta_cores_inovacoes[1:nrow(cores_inov)],
              opacidade = rep(1, nrow(cores_inov))) #(bind_cols) colar colunas lado a lado no novo dataframe
  
  cores_base = lista_prod %>% 
    filter(inovacao == 0)
  
  if(nrow(cores_base) != 0){
    cores_base = cores_base %>% 
      bind_cols(cor = paleta_cores_opacas[1:nrow(cores_base)],
                opacidade = rep(0.3, nrow(cores_base)))
    
    cores = bind_rows(cores_base, cores_inov) %>% 
      arrange(prod)
  }else{
    cores = cores_inov %>% 
      arrange(prod)
  }
  
  meses = dados_plot %>% 
    arrange(mes) %>% 
    pull(mes) %>% 
    unique() %>% 
    as.character()
  
  dados_plot = dados_plot %>% 
    arrange(prod)
  
  plot = dados_plot %>% 
    mutate(mes = as.Date(mes),
           prod = ifelse(inovacao == 1, paste0(prod, "(Inovação)"), prod)) %>% 
    hchart(
      "area",
      hcaes(
        y = indice_preco,
        x= mes,
        group = prod
      ),
      marker = TRUE,
      tooltip = list(pointFormat = paste0(
        "<style='display: block; font-size: 11px;'>Produto: <b>{series.name}</b><br>",
        "<style='display: block; font-size: 11px;'>Share de Produtos na Maca: <b> {point.indice_preco:.4f}%</b><br>"
      )),
      color = cores$cor, fillOpacity = cores$opacidade
    ) %>% 
    hc_xAxis(
      lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
      title = list(text = "<span style='display: block; font-size: 12px;'> Month </span>"),
      dateTimeLabelFormats = list(day = " %m %Y"),
      typer = "datetime") %>% 
    hc_yAxis(
      labels = list(format = "{value}%"),
      lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
      title = list(text = "<span style='display: block; font-size: 12px;'> Share de Produtos </span>"),
      max = 100
    ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Percentual de Inovações da marca ", marca_selecionada," por Produto - ",input_geo,"</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_plotOptions(
      area = list(
        stacking = "percent",
        groupPadding = 0.1
      )
    ) %>% 
    hc_legend(
      itemStyle = list(
        fontSize = "12px",
        fontWeight = "bold",
        color = "#5e5e5e"
      )
    ) %>% 
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    )
  
  return(plot)
  
}

plot_share_por_produto_grupo = function(dados, lista_inovacoes_mercado, input_geo, input_marca, input_linha_tabela, input_inovacao){
  
  if(input_inovacao == 'mercado'){
    is_launch = 'is_launch_prod'
    qnt_inovacao = 'num_inovacao'
  }else if(input_inovacao == 'vendas'){
    is_launch = 'is_launch_prod_sales'
    qnt_inovacao = 'num_inovacao_vendas'
  }
  
  if(input_inovacao == 'mercado'){
    tabela = lista_inovacoes_mercado
  } else if(input_inovacao == 'vendas'){
    tabela = lista_inovacoes_vendas
  }
  
  lista_inovacoes_mercado = lista_inovacoes_mercado %>% ungroup()
  
  if(input_marca == "TODAS AS MARCAS"){
    tabela = tabela %>% 
      filter(geo == input_geo)
  }else{
    tabela = tabela %>% 
      filter(geo == input_geo, marca == input_marca)
  }
  
  marca_selecionada = tabela[input_linha_tabela,]$marca
  prod_inovacao = tabela$prod
  
  dados_plot = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada) %>% 
    group_by(mes) %>% 
    mutate(vendas_total = sum(vendas_in_hl)) %>% 
    filter(prod %in% prod_inovacao) %>% 
    group_by(mes, prod) %>% 
    summarise(
      indice_preco = sum(vendas_in_hl)/mean(vendas_total)*100
    ) %>% 
    ungroup()
  
  paleta_cores = c("#ffc72c","#5B1217","gray","#3493ad","#b66bc5", "#3C9E3A","#74c69d","#618fb1","#8475cd","#59EB57", "#ab0c04", "#872EAB",
                   "#99c9d6","#dab5e2", "#9dce9c","#b9e2ce","#b0c7d8","#c1bae6","#acf5ab", "#ffe395","#d58581", "#c396d5", "#FFA07A", "#FFC0CB", "#FFE4E1","#DDA0DD", "#B0E0E6", "#D8BFD8")
  
  plot = dados_plot %>% 
    mutate(mes = as.Date(mes)) %>% 
    hchart(
      "area",
      hcaes(
        y = indice_preco,
        x = mes,
        group = prod
      ),
      marker = TRUE,
      tooltip = list(pointFormat = paste0(
        "<style='display: block; font-size: 11px;'>Produto: <b> {series.name}</b> <br>",
        "<style='display: block; font-size: 11px;'>Share de Produtos na Maca: <b> {point.indice_preco:.4f}%</b><br>"
      ))
    ) %>%
    hc_xAxis(
      linecolor = "#f7f7f7", gridLineColor = "#f4f4f4",
      title = list(text = "<span style='display: block; font-size: 12px;'> Month </span>"),
      dateTimeLabelFormats = list(day = "%m %Y"),
      type = "datetime"
      ) %>% 
    hc_yAxis(
      labels = list(format = "{value}%"),
      linecolor = "#f7f7f7", gridLineColor = "#f4f4f4",
      title = list(text = "<span style='display: block; font-size: 12px;'> Share de Produtos </span>")
    ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Percentual de Inovações da marca ", marca_selecionada," por Produto - ",input_geo,"</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_colors(paleta_cores) %>% 
    hc_legend(
      itemStyle = list(fontSize = "12px",
                       fontWeight = "bold",
                       color = "#5e5e5e")
    ) %>% 
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    )
    
  return(plot)
  
}

plot_share_comp_prod_geo = function(dados,lista_inovacoes_mercado, input_geo, input_marca, input_inovacao,input_linha_tabela){
  
  if(input_inovacao == 'mercado'){
    tabela = lista_inovacoes_mercado
  } else if(input_inovacao == 'vendas'){
    tabela = lista_inovacoes_vendas
  }
  
  lista_inovacoes_mercado = lista_inovacoes_mercado %>% ungroup()
  
  if(input_marca == "TODAS AS MARCAS"){
    tabela = tabela %>%  filter(geo == input_geo)
  } else{
    tabela = tabela %>% filter(geo == input_geo, marca == input_marca)
  }
  
  marca_selecionada = tabela[input_linha_tabela,]$marca
  produto = tabela[input_linha_tabela,]$prod
  
  share_prod_geos = dados %>% 
    group_by(geo, mes) %>% 
    mutate(total_vendas = sum(vendas_in_hl)) %>% 
    filter(prod == produto) %>% 
    group_by(geo, mes) %>% 
    summarise(
      share_prod = sum(vendas_in_hl)/mean(total_vendas)*100,
      mes_inicio_prod = first(mes_inicio_prod)
    ) %>% 
    ungroup() %>% 
    filter(mes >= mes_inicio_prod) %>% 
    group_by(geo) %>% 
    arrange(mes) %>% 
    mutate(meses_desde_lancamento = 0:(n_distinct(mes)-1)) #meses desde o lançamento do produto até o mês atual para cada geo.
  
  dados_plot = share_prod_geos %>% 
    filter(geo == input_geo)
  
  plot = highchart() %>% 
    hc_add_series(
      type = "line",
      data = dados_plot,
      hcaes(
        y = share_prod,
        x = meses_desde_lancamento
      ),
      name = paste0("<b style='display: block; font-size: 12px;'>",
                    "Share de Produtos na Geo", str_remove(input_geo, "GEO")),
      showInLegend = TRUE,
      tooltip = list(pointFormat = paste0(
        "<b style='display: block; font-size: 11px;'>",
        "Share de Produtos na Geo",str_remove(input_geo,"GEO"),": <b> {point.share_prod:.4f}%</b><br>"
      )),
      marker = list(symbol = "circle")
    ) %>% 
    
    hc_yAxis(labels = list(format = "{value:.2f}%"),
             lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
             title = list(text = "<style='display: block; font-size: 12px;'>Share de Produtos")) %>% 
    hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
             title = list(text = "<style='display: block; font-size: 12px;'>Meses Desde o Lançamento")) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Share de Inovações ", produto
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          color = "black",
          enabled = TRUE,
          format = "{point.y:.2f}%"
        )
      )
    ) %>% 
    hc_colors(c("#ffc72c", "#363636", "#5B1217")) %>% 
    hc_tooltip(shared = TRUE) %>% 
    hc_legend(
      itemStyle = list(fontSize = "12px",
                       fontWeight = "bold",
                       color = "#5e5e5e")
    ) %>% 
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    ) 
  
  return(plot)
  
}

plot_comp_share_before_after = function(dados,lista_inovacoes_mercado, input_geo, input_marca, input_inovacao,input_linha_tabela){
  
  if(input_inovacao == 'mercado'){
    tabela = lista_inovacoes_mercado %>% filter(geo == input_geo)
  } else if(input_inovacao == 'vendas'){
    tabela = lista_inovacoes_vendas %>% filter(geo == input_geo)
  }
  
  lista_inovacoes_mercado = lista_inovacoes_mercado %>% ungroup()
  
  if(input_marca == "TODAS AS MARCAS"){
    tabela = tabela
  }else{
    tabela = tabela %>% filter(marca == input_marca)
  }
  
  marca_selecionada = tabela[input_linha_tabela,]$marca
  produto = tabela[input_linha_tabela,]$prod
  
  share_marca_geo = dados %>% 
    filter(geo == input_geo) %>% 
    group_by(mes) %>% 
    mutate(total_vendas = sum(vendas_in_hl)) %>% 
    filter(marca == marca_selecionada) %>% 
    group_by(mes) %>% 
    summarise(
      share_marca = sum(vendas_in_hl)/mean(total_vendas)*100,
      n_prod = n_distinct(prod)
    ) %>% 
    ungroup()
  
  data_lancamento = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada, prod == produto) %>% 
    pull(mes_inicio_prod) %>% 
    unique() %>% 
    as.yearmon()
  
  mes_maturidade = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada, prod == produto) %>% 
    filter(mes >= data_lancamento + 0.5) %>% 
    arrange(mes) %>% 
    summarise(mes_maturidade = first(mes)) %>% 
    pull(mes_maturidade) %>% 
    as.yearmon()
  
  ultimo_mes = dados %>% 
    arrange(mes) %>% 
    filter(geo == input_geo, marca == marca_selecionada, prod == produto, mes == last(mes)) %>% 
    pull(mes) %>% 
    as.yearmon()
  
  tempo_obs = dados %>% 
    filter(geo == input_geo, marca == marca_selecionada, prod == produto, mes >= mes_inicio_prod) %>% 
    summarise(n = n_distinct(mes)) %>% 
    pull(n) -1
  
  legenda_maturidade = ifelse(ultimo_mes < mes_maturidade,
                              "Produto Não Atingiu 6 Meses",
                              paste0(mes_maturidade))
  
  dados_plot = share_marca_geo %>% 
    filter(mes %in% c(data_lancamento, mes_maturidade)) %>% 
    arrange(mes) %>% 
    mutate(
      dif = share_marca - lag(share_marca, 1),
      periodo = case_when(mes == data_lancamento ~ "Mês de Lançamento",
                          mes == mes_maturidade ~ "6 Meses Após o Lançamento")
    )
  
  pct_dif = dados_plot %>% filter(!is.na(dif)) %>% pull(dif)
  
  plot = dados_plot %>% 
    hchart(
      "column",
      hcaes(
        y = share_marca,
        x = periodo
      )
    ) %>% 
    hc_yAxis(
      title = list(text = "<style='display: block; font-size: 12px;'>Share da Marca"),
      lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
      max = max(dados_plot$share_marca)+1,
      labels = list(format = "{value}%")
    ) %>% 
    hc_xAxis(
      title = list(text = ""), 
      lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
      plotBands = list(
        list(
          from = 0.5,
          to = 0.5,
          color = "#F7F3F3",
          label = list(text = paste0(
            "<style='display: block; font-size: 12px;'>Delta Share:<b>", pct_dif[1]%>% round(2),"%</b><br/>"
          )
        )
      )
    ),
  labels = list(style = list(fontSize = "12px"))
    ) %>% 
    hc_tooltip(
      pointFormat = paste0(
        "<style='display: block; font-size: 11px;'>Share da Marca na Geo: <b>{point.y:.2f}%</b><br>",
        "<style='display: block; font-size: 11px;'>Número de Produtos: <b>{point.n_prod}</b><br>"
      )
    ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Share da Marca ",marca_selecionada, " por Período - ",input_geo,"</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_subtitle(
      text = paste0(
        "<b style='display: block; font-size: 12px;'>",
        "Produto Referência: ",produto, "</b>"
      ),
      align = "center",
      style = list(useHTML = TRUE)
    )%>% 
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    ) %>%
    hc_colors(c("#ffc72c","#5B1217")) %>%
    hc_caption(text = paste0("<style='display: block; font-size: 11px;'>Lançamento do Produto: ", data_lancamento, " <br>",
                             "<style='display: block; font-size: 11px;'>6 Meses após o Lançamento: ", mes_maturidade, " <br>",
                             "<style='display: block; font-size: 11px;'>Meses Desde o Lançamento: ",tempo_obs," months<br>")
    ) %>% 
    hc_plotOptions(column = list(
      dataLabels = list(
        enabled = TRUE, 
        format = '{y:.2f}%',
        style = list(fontSize = '14px')), 
      pointPadding = 0.05,
      colorByPoint = TRUE, showInLegend = FALSE))
  
  
  
  return(plot)
}

table_plot_geo = function(dados, lista_inovacoes_mercado, input_geo, input_marca, input_linha_tabela, input_inovacao){
  
  if(input_inovacao == 'mercado'){
    is_launch = 'is_launch_prod'
    qnt_inovacao = 'num_inovacao'
  }else if(input_inovacao == 'vendas'){
    is_launch = 'is_launch_prod_sales'
    qnt_inovacao = 'num_inovacao_vendas'
  }
  
  if(input_inovacao == 'mercado'){
    tabela = lista_inovacoes_mercado
  } else if(input_inovacao == 'vendas'){
    tabela = lista_inovacoes_vendas
  }
  
  
  lista_inovacoes_mercado = lista_inovacoes_mercado %>% ungroup()
  
  if(input_marca == "TODAS AS MARCAS"){
    tabela = tabela %>% filter(geo == input_geo)
  }else{
    tabela = tabela %>% filter(geo == input_geo, marca = input_marca)
  }
  
  marca_selecionada = tabela[input_linha_tabela,]$marca
  produto = tabela[input_linha_tabela,]$prod
  
  if(input_inovacao == 'mercado'){
    tempo_entre_inovacao = lista_inovacoes_mercado %>% 
      filter(marca == marca_selecionada, prod == produto)
  } else if(input_inovacao == 'vendas'){
    tempo_entre_inovacoes = lista_inovacoes_vendas %>% 
      filter(marca == marca_selecionada, prod == produto) %>% 
      distinct(geo, tempo_entre_inovacao)
  }
  
  share_prod_geo = dados %>% 
    group_by(geo, mes) %>% 
    mutate(total_vendas = sum(vendas_in_hl)) %>% 
    filter(prod == produto) %>% 
    group_by(geo,mes) %>% 
    summarise(
      share_prod = sum(vendas_in_hl)/mean(total_vendas)*100
    )
  
  data_lancamento = dados %>% 
    filter(marca == marca_selecionada, prod == produto) %>% 
    group_by(geo) %>% 
    distinct(geo, data_lancamento = mes_inicio_prod)
  
  mes_maximo = share_prod_geo %>% 
    left_join(data_lancamento, by = "geo") %>% 
    filter(mes >= data_lancamento) %>% 
    group_by(geo) %>% 
    filter(share_prod == max(share_prod)) %>% 
    ungroup() %>% 
    select(geo, mes, share_prod)
  
  share_mean_prod = share_prod_geo %>% 
    left_join(data_lancamento, by = "geo") %>% 
    filter(mes >= data_lancamento) %>% 
    group_by(geo) %>% 
    summarise(share_mean = mean(share_prod))
  
  share_mean_brand = dados %>% 
    group_by(geo, mes) %>% 
    mutate(vendas_total = sum(vendas_in_hl)) %>% 
    filter(marca == marca_selecionada) %>% 
    group_by(geo) %>% 
    summarise(share_mean_brand = mean(share_tt))
  
  dados_table = data_lancamento %>% 
    left_join(mes_maximo, by = "geo") %>% 
    mutate(
      number_of_months = (year(mes)- year(data_lancamento)) * 12+
        month(mes) - month(data_lancamento)
    ) %>% 
    left_join(share_mean_prod, by = "geo") %>% 
    left_join(share_mean_brand, by = "geo") %>% 
    left_join(tempo_entre_inovacao, by = "geo") %>% 
    filter(!is.na(tempo_entre_inovacao)) %>% 
    mutate(
      data_lancamento = as.character(data_lancamento),
      mes = as.character(mes),
      share_prod = paste0(round(share_prod, 3), "%"),
      number_of_months = paste0(round(number_of_months,0), " months"),
      share_mean = paste0(round(share_mean, 3), "%"),
      share_mean_brand = paste0(round(share_mean_brand,3), "%"),
      produto = produto
    ) %>% 
    rename(
      "Região" = "geo",
      "Data de Lançamento" = "data_lancamento",
      "Mês Share Maximo" = "mes",
      "Share Máximo" = "share_prod",
      "Tempo Para alcançar o Share Máximo" = "number_of_months",
      "Share Médio por Produto" = "share_mean",
      "Share Médio por Marca" = "share_mean_brand",
      "Tempo Entre Inovações" = "tempo_entre_inovacao"
    )
  return(dados_table)
  
}

plot_maturity_inovations = function(dados, lista_inovacoes_mercado, input_geo, input_marca, input_linha_tabela, input_inovacao){
  
  if(input_inovacao == 'mercado'){
    tabela = lista_inovacoes_mercado %>% filter(geo == input_geo)
  } else if(input_inovacao == 'vendas'){
    tabela = lista_inovacoes_vendas %>% filter(geo == input_geo)
  }
  
  dados_plot = dados %>% 
    filter(geo == input_geo) %>% 
    group_by(mes) %>% 
    mutate(
      vendas_total = sum(vendas_in_hl)
    ) %>% 
    filter(prod %in% tabela$prod, mes >= mes_inicio_prod) %>% 
    group_by(geo, mes, marca, prod) %>% 
    summarise(
      share_tt = sum(vendas_in_hl)/first(vendas_total)*100
    ) %>% 
    group_by(prod) %>% 
    arrange(mes) %>% 
    mutate(
      meses_desde_lancamento = 0:(n_distinct(mes)-1)
    )
  
  lista_inovacoes_mercado = lista_inovacoes_mercado %>% ungroup()
  
  if(input_marca == "TODAS AS MARCAS"){
    dados_plot = dados_plot
  }else{
    dados_plot = dados_plot %>% filter(marca == input_marca)
  }
  
  plot = dados_plot %>% 
    hchart(
      "line",
      hcaes(
        y = share_tt,
        x = meses_desde_lancamento,
        group = prod),
      marker = FALSE
    ) %>% 
    hc_yAxis(
      lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
      title = list(text = "<style='display: block; font-size: 12px;'>Share de Produtos"),
      max = max(dados_plot$share_tt),
      labels = list(format = "{value:.1f}%")
    ) %>% 
    hc_xAxis(
      title = list(text = "<style='display: block; font-size: 12px;'>Meses Desde o Lançamento"),
      lineColor = "#f7f7f7", gridLineColor = "#f4f4f4"
    ) %>% 
    hc_tooltip(
      pointFormat = paste0(
        "<style='display: block; font-size: 11px;'>Produto: <b>{point.prod}</b><br>",
        "<style='display: block; font-size: 11px;'>Share do Produto na {point.geo}: <b>{point.y:.3f}%</b><br>"
      )
    ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Share de Produtos por Inovações na ",input_geo,"</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_legend(
      title = list(text = "<style='display: block; font-size: 12px;'>Inovações de Produtos"),
      style = list(useHTML = TRUE),
      align = "right",
      layout = "vertical",  # Configura a legenda para ficar na vertical
      verticalAlign = 'top',
      padding = 40,
      itemMarginTop = 1,
      itemMarginBottom = 1,
      itemStyle = list(
        fontSize = '12px',
        fontWeight = 'bold',
        color = '#5e5e5e'
      )
    ) %>% 
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    ) %>% 
    hc_colors(c("#5B1217", "#ffc72c","#3493ad","#b66bc5", "#3C9E3A","#74c69d","#f70217","#8475cd","#fa4407", "#ab0c04", "#872EAB")
    )
  
  return(plot)
}

plot_share_cycle_time_all_innovations_in_brand = function(dados, lista_inovacoes_mercado, input_geo, input_marca, btn_input_visao, input_inovacao ){
  if(input_inovacao == "mercado"){
    tabela = lista_inovacoes_mercado %>% 
      mutate(link = paste0(geo, prod))
  }else if(input_inovacao == "vendas"){
    tabela = lista_inovacoes_vendas %>% 
      mutate(link = paste0(geo, prod))
  }
  
  
  share_prod_marca = dados %>% 
    group_by(geo, mes, marca) %>% 
    mutate(vendas_total = sum(vendas_in_hl),
           link = paste0(geo, prod)) %>% 
    filter(link %in% tabela$link) %>% 
    select(-link) %>% 
    group_by(geo, mes, marca, prod) %>% 
    summarise(
      share_tt = sum(vendas_in_hl)/first(vendas_total)*100,
      data_lancamento = first(mes_inicio_prod),
      faixa_tamanho = first(faixa_tamanho)
    ) %>% 
    ungroup()
  
  mes_maturidade_corrigido = share_prod_marca %>% 
    group_by(geo, prod) %>% 
    filter(mes >= data_lancamento + 0.5) %>% 
    arrange(mes) %>% 
    group_by(geo, prod) %>% 
    summarise(
      mes_maturidade = first(mes)
      ) %>% 
    ungroup()
  
  share_prod_marca = share_prod_marca %>% 
    left_join(mes_maturidade_corrigido, by = c("geo", "prod"))
  
  delta_share = share_prod_marca %>% 
    group_by(geo, prod) %>% 
    filter(mes %in% c(data_lancamento, mes_maturidade)) %>% 
    arrange(mes) %>% 
    mutate(
      delta = share_tt - lag(share_tt, 1),#retorna o valor anterior (anterior em uma posição) da coluna share_tt
      data_lancamento = as.character(data_lancamento),
      mes_maturidade = as.character(mes_maturidade),
      faixa_tamanho
    ) %>% 
    ungroup() %>% 
    filter(!is.na(delta)) %>% 
    distinct(geo, prod, delta, data_lancamento, mes_maturidade, faixa_tamanho)
  
  dados_plot = delta_share %>% 
    left_join(tabela %>% select(-link), by =c("geo", "prod")) %>% 
    mutate(tempo_entre_inovacao = parse_number(tempo_entre_inovacao))#transformar o conteúdo da coluna em números
  
  if(input_geo == "TODAS AS GEOS"){
    dados_plot = dados_plot
  }else{
    dados_plot = dados_plot %>% filter(geo == input_geo)
  }
  
  if(input_marca == "TODAS AS MARCAS"){
    dados_plot = dados_plot
  }else{
    dados_plot = dados_plot %>% filter(marca == input_marca)
  }
  
  if(btn_input_visao == "Tamanho"){
    
    plot = dados_plot %>% 
      hchart(
        "scatter",
        hcaes(
          y = delta,
          x = tempo_entre_inovacao,
          group = faixa_tamanho,
          prod = prod,
          geo = geo
        )
      ) %>% 
      hc_yAxis(
        title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                   "Delta Share Produto")),
        lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
        labels = list(format = "{value:.2f}%"),
        min = min(dados_plot$delta), max = max(dados_plot$delta)
      ) %>% 
      hc_xAxis(
        title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                   "Ciclo de Inovações")),
        linecolor = "#f7f7f7", gridLineColor = "#f4f4f4"
      ) %>% 
      hc_tooltip(
        pointFormat = paste0(
          "<style='display: block; font-size: 11px;'>Produto: <b>{point.prod}</b><br>",
          "<style='display: block; font-size: 11px;'>Delta Share Produto em {point.marca}: <b>{point.y:.2f}%</b><br>",
          "<style='display: block; font-size: 11px;'>Ciclo de Inovações: <b>{point.x:.0f} months</b><br>",
          "<style='display: block; font-size: 11px;'>Mês de Lançamento: <b>{point.data_lancamento} </b><br>",
          "<style='display: block; font-size: 11px;'>Mês da Maturidade: <b>{point.mes_maturidade}</b><br>"
        )
      ) %>% 
      hc_title(
        text = paste0(
          "<b style='display: block; font-size: 15px;'>",
          "Delta Share do Produto para Todas as Inovações por Faixa de Tamanho - ",input_geo,"</b>"
        ),
        margin = 20,
        align = "center",
        style = list(useHTML = TRUE)
      ) %>% 
      hc_legend(
        title = list(text = "<style='display: block; font-size: 12px;'>Faixa de Tamanho"),
        align = "center",
        itemStyle = list(
          fontSize = '12px',
          fontWeight = 'bold',
          color = '#5e5e5e',
          padding = 3)
      ) %>% 
        hc_colors(
          c("#5B1217", "#ffc72c", "gray","#3493ad","#b66bc5", "#3C9E3A","#74c69d","#f70217","#8475cd","#fa4407", "#ab0c04", "#872EAB")
          ) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = list('downloadCSV', 'downloadSVG')
          )
        )
      )
  }
  
  if(btn_input_visao == "Marca"){
    
    plot = dados_plot %>% 
      hchart(
        "scatter",
        hcaes(
          y = delta,
          x = tempo_entre_inovacao,
          group = marca,
          prod = prod,
          geo = geo
        )
      ) %>% 
      hc_yAxis(
        title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                   "Delta Share Produto")),
        lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
        labels = list(format = "{value:.2f}%"),
        min = min(dados_plot$delta), max = max(dados_plot$delta)
      ) %>% 
      hc_xAxis(
        title = list(text = paste0("<style='display: block; font-size: 12px;'>",
                                   "Ciclo de Inovações")),
        linecolor = "#f7f7f7", gridLineColor = "#f4f4f4"
      ) %>% 
      hc_tooltip(
        pointFormat = paste0(
          "<style='display: block; font-size: 11px;'>Produto: <b>{point.prod}</b><br>",
          "<style='display: block; font-size: 11px;'>Delta Share Produto em {point.marca}: <b>{point.y:.2f}%</b><br>",
          "<style='display: block; font-size: 11px;'>Ciclo de Inovações: <b>{point.x:.0f} months</b><br>",
          "<style='display: block; font-size: 11px;'>Mês de Lançamento: <b>{point.data_lancamento} </b><br>",
          "<style='display: block; font-size: 11px;'>Mês da Maturidade: <b>{point.mes_maturidade}</b><br>"
        )
      ) %>% 
      hc_title(
        text = paste0(
          "<b style='display: block; font-size: 15px;'>",
          "Delta Share do Produto para Todas as Inovações por Marca - ",input_geo,"</b>"
        ),
        margin = 20,
        align = "center",
        style = list(useHTML = TRUE)
      ) %>% 
      hc_legend(
        title = list(text = "<style='display: block; font-size: 12px;'>Marca"),
        align = "center",
        itemStyle = list(
          fontSize = '12px',
          fontWeight = 'bold',
          color = '#5e5e5e',
          padding = 3)
      ) %>% 
      hc_colors(
        c("#5B1217", "#ffc72c", "gray","#3493ad","#b66bc5", "#3C9E3A","#74c69d","#f70217","#8475cd","#fa4407", "#ab0c04", "#872EAB")
      ) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = list('downloadCSV', 'downloadSVG')
          )
        )
      )
  }
  
  return(plot)
 }