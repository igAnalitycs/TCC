# Funcoes ======================================================================

share_vendas_inovacao_mes_geo = function(dados, input_geo, input_marca, input_inovacao){
  
  
  if(input_inovacao == 'mercado'){
    is_launch = 'is_launch_prod'
    qnt_inovacao = 'num_inovacao'
  }else if(input_inovacao == 'vendas'){
    is_launch = 'is_launch_prod_sales'
    qnt_inovacao = 'num_inovacao_vendas'
  }
  
  dados = dados %>% ungroup()
  
  if(input_marca == "TODAS AS MARCAS"){
    
    tag_prod_inovations = dados %>% 
      filter(geo == input_geo, !!sym(is_launch) == 1) %>% 
      pull(tag_prod) %>% 
      unique()
    
    share_geo = dados %>% 
      filter(geo == input_geo) %>%
      group_by(mes) %>% 
      mutate(vendas_total = sum(vendas_in_hl)) %>% 
      ungroup() %>% 
      filter(is_ambev == 1) %>% 
      mutate(agrup = case_when(tag_prod %in% tag_prod_inovations ~ "Inovações por Geo",
                                       TRUE ~ "Produtos Tradicionais por Geo")) %>% 
      group_by(mes, agrup) %>% 
      summarise(indice_preco = sum(vendas_in_hl)/mean(vendas_total)*100) %>% 
      ungroup()
    
    vendas_mes_geo = dados %>% 
      filter(geo == input_geo, is_ambev == 1) %>% 
      group_by(mes) %>% 
      summarise(
        vendas_in_hl = sum(vendas_in_hl),
        n_prod = n_distinct(tag_prod),
        inovacao = sum(!!sym(qnt_inovacao))
      ) %>% 
      ungroup() %>% 
      mutate(inovation_dif0 = ifelse(inovacao==0, NA_integer_, inovacao))
    
    tabela = share_geo %>% 
      left_join(vendas_mes_geo, by = "mes")
  }
  
  else if(input_marca != "TODAS AS MARCAS"){
    
    tag_prod_inovations = dados %>% 
      filter(geo == input_geo, marca == input_marca, !!sym(is_launch) == 1) %>%
      arrange(mes) %>%
      pull(tag_prod) %>% 
      unique()
    
    share_marca_geo = dados %>% 
      filter(geo == input_geo) %>%
      group_by(mes) %>% 
      mutate(vendas_total = sum(vendas_in_hl)) %>% 
      ungroup() %>% 
      filter(marca == input_marca) %>% 
      mutate(agrup = case_when(tag_prod %in% tag_prod_inovations ~ "Inovações por Geo",
                               TRUE ~ "Produtos Tradicionais por Geo")) %>% 
      group_by(mes, agrup) %>% 
      summarise(indice_preco = sum(vendas_in_hl)/mean(vendas_total)*100) %>% 
      ungroup()
    
    vendas_mes_geo = dados %>% 
      filter(geo == input_geo, marca == input_marca) %>% 
      group_by(mes) %>% 
      summarise(
        vendas_in_hl = sum(vendas_in_hl),
        n_prod = n_distinct(tag_prod),
        inovacao = sum(!!sym(qnt_inovacao)),
      ) %>% 
      ungroup() %>% 
      mutate(inovation_dif0 = ifelse(inovacao==0, NA_integer_, inovacao))
    
    tabela = share_marca_geo %>% 
      left_join(vendas_mes_geo, by = "mes")
  }
  return(tabela)
}

lista_inovacoes_lancamento = function(dados, input_geo, input_marca, input_inovacao){
  
  vendas_total = dados %>% 
    mutate(vendas_total = sum(vendas_in_hl))
  
  indice_preco = vendas_total %>% 
    summarise(sum(vendas_in_hl)/mean(vendas_total)*100)
  
  if(input_inovacao == 'mercado'){
    is_launch = 'is_launch_prod'
    qnt_inovacao = 'num_inovacao'
  }else if(input_inovacao == 'vendas'){
    is_launch = 'is_launch_prod_sales'
    qnt_inovacao = 'num_inovacao_vendas'
  }
  
  if(input_marca == "TODAS AS MARCAS"){
    
    inovacoes = dados %>% 
      filter(geo == input_geo, is_ambev == 1, !!sym(qnt_inovacao) == 1) %>% 
      distinct(mes_inicio_prod, prod, marca, mes_inicio_marca, faixa_tamanho) %>% 
      arrange(mes_inicio_prod)
    
    tamanho_prod = dados %>% 
      filter(geo == input_geo, is_ambev == 1) %>% 
      group_by(marca, mes_inicio_prod) %>% 
      distinct(faixa_tamanho_ant = faixa_tamanho, prod_ant = prod) %>% 
      ungroup()
    
    inovacao_tamanho = list()
    for(i in 1:length(inovacoes$prod)){
      marca_inovacao = inovacoes[i,]$marca
      data_inovacao = inovacoes[i,]$mes_inicio_prod
      tamanho_inovacao = inovacoes[i,]$faixa_tamanho
      produto = inovacoes[i,]$prod
      
      tamanho_outros_prod = tamanho_prod %>% 
        filter(marca == marca_inovacao, mes_inicio_prod < data_inovacao) %>% 
        distinct()
      inovacao_tamanho[[i]] = ifelse (tamanho_inovacao %in% tamanho_outros_prod$faixa_tamanho_ant, 0, 1)
    }
    
    inovacoes$inovacao_tamanho = unlist(inovacao_tamanho)
  }
  
  else{
    inovacoes = dados %>% 
      filter(geo == input_geo) %>% 
      group_by(mes) %>% 
      mutate(
        share_total_mes = sum(indice_preco)
      ) %>% 
      ungroup() %>% 
      filter(geo == input_geo, marca == input_marca) %>% 
      filter(!!sym(qnt_inovacao) == 1) %>% 
      arrange(mes_inicio_prod) %>% 
      distinct(mes_inicio_prod, prod, marca, mes_inicio_marca, faixa_tamanho)
    
    tamanho_prod = dados %>% 
      filter(geo == input_geo, marca == input_marca) %>% 
      group_by(mes_inicio_prod) %>% 
      distinct(faixa_tamanho_ant = faixa_tamanho, prod_ant = prod)
    
    inovacao_tamanho = list()
    for(i in 1:length(inovacoes$prod)){
      marca_inovacao = inovacoes[i,]$marca
      data_inovacao = inovacoes[i,]$mes_inicio_prod
      tamanho_inovacao = inovacoes[i,]$faixa_tamanho
      produto = inovacoes[i,]$prod
      
      tamanho_outros_prod = tamanho_prod %>% 
        filter(mes_inicio_prod < data_inovacao) %>% 
        distinct()
      
      inovacao_tamanho[[i]] = ifelse(tamanho_inovacao %in% tamanho_outros_prod$faixa_tamanho_ant, 0, 1)
    }
    
    inovacoes$inovacao_tamanho = unlist(inovacao_tamanho)
    
  }
  
  dados_table = inovacoes %>% 
    group_by(marca) %>% 
    arrange(mes_inicio_prod) %>% 
    mutate(
      primeira_inovacao = ifelse(mes_inicio_prod == first(mes_inicio_prod),1,0),
      tempo_entre_inovacao = case_when(
        primeira_inovacao == 1 ~ 
          (year(mes_inicio_prod) - year(mes_inicio_marca))*12 + month(mes_inicio_prod) - month(mes_inicio_marca),
        primeira_inovacao == 0 ~
          (year(mes_inicio_prod) - year(lag(mes_inicio_prod, 1)))*12 + month(mes_inicio_prod) - month(lag(mes_inicio_prod, 1))
      ),
      tempo_entre_inovacao = paste0(round(tempo_entre_inovacao, 0), " months")
    ) %>% 
    ungroup() %>% 
    mutate(
      inovacao_tamanho = ifelse(inovacao_tamanho == 1, "Yes", "No")
    ) %>% 
    select(-c(mes_inicio_marca, primeira_inovacao, faixa_tamanho)) %>% 
    distinct()
  
  dados_table = dados_table %>% 
    group_by(marca, mes_inicio_prod) %>% 
    mutate(tempo_entre_inovacao = first(tempo_entre_inovacao)) %>% 
    ungroup()
  
  return(dados_table)
  
}



