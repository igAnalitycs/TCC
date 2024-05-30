inicio_ui = function(id, lista_produtos) {
  ns <- NS(id)
  
  tabItem(
    tabName = id, 
   fluidRow(
     column(
       width = 3,
       selectInput(
         inputId = ns("input_marca"),
         label = "Selecione uma Marca:",
         choices = c("TODAS AS MARCAS", sort(unique(lista_produtos$marca))),
         width = "100%"
         )
       ),
     column(
       width = 3,
       selectInput(
         inputId = ns("input_geo"),
         label = "Selecione uma Geo:",
         choices = sort(unique(lista_produtos$geo)),
         selected = "GEO SP",
         width = "100%"
         )
       )
     ),
   fluidRow(
     box(
       width = 12,
       title = "INOVAÇÕES EM TODOS OS CANAIS",
       column(
         width = 12,
         highchartOutput(ns("vendas_mes_marca_geo"))
         ),
       column(
         width = 12,
         style = "margin-top: 25px; margin-bottom: 10px;",
         DTOutput(ns("table_inovations"))
         ),
       column(
         width = 12,
         style = "margin-top: 25px; margin-bottom: 10px",
         highchartOutput(ns("grafico_volume_produto"))
         ),
       column(
         width = 6,
         style = "float: rigth;",
         radioButtons(
           inputId = ns("btn_nivel_inovacao"),
           label = "Apenas Inovações",
           choices = c("Sim", "Não"),
           selected = "Não",
           inline = TRUE
           )
         ),
       column(
         width = 12,
         style = "margin-top: 25px;",
         highchartOutput(ns("grafico_share_prod"))
         ),
       column(
         width = 6,
         style = "margin-top: 25px;",
         highchartOutput(ns("grafico_comp_share_geo"))
         ),
       column(
         width = 6,
         style = "margin-top: 25px;",
         highchartOutput(ns("grafico_antes_depois"))
         ),
       column(
         width = 12,
         style = "margin-top: 25px; margin-bottom: 10px;",
         DTOutput(ns("tabela_prod_geo"))
       ),
       column(
         width = 12,
         style = "margin-top: 25px; margin-bottom: 10px;",
         highchartOutput(ns("grafico_maturity_inno"))
       ),
       column(
         width = 10,
         style = "float: rigth;",
         radioButtons(
           inputId = ns("btn_input_visao"),
           label = "Selecionar Grupo",
           choices = c(
             "Tamanho",
             "Marca"),
           selected = "Tamanho",
           inline = TRUE
         )
       ),
       column(
         width = 12,
         style = "margin-top: 25px; margin-bottom: 10px;",
         highchartOutput(ns("grafico_share_inno_prod"))
       )
       )
     )
   )
}
