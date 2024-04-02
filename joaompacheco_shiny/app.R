#bibliotecas

library(shiny)
library(tidyverse)
library(ggplot2)


#importando base
df <- read.csv("apple_quality.csv")

#funcao
limpar_dataframe <- function(dataframe){
  
  #removendo Na
  df <- df[complete.cases(df), ]
  
  #limpando última linha
  df <- df[-nrow(df), ]
  
  #trocando Acidity para numerico
  df$Acidity <- as.numeric(df$Acidity)
  
  #setando rowname como A_id
  rownames(df) <- df$A_id
  df <- df[,-1]
  
  return(df)
}


df <- limpar_dataframe(df)

df <- df %>%
  select(-8)





#aplicação do shiny
ui <- fluidPage(
  titlePanel("Shiny dashboard projeto de disciplina"),
  
  sidebarLayout(
    sidebarPanel(
      # selecionar qual a variavel do df2
      selectInput("variavel", "Selecione uma variável:", choices = colnames(df)),
      
      # selecionar as cores
      selectInput("cor", "Selecione a cor da linha:", 
                  choices = c("Azul" = "#0bb4ff", "Vermelho" = "#e60049", "Verde" = "#00bfa0")),
      
      # limites para o eixo X
      numericInput("limite_x_min", "Limite inferior do eixo X:", value = 0),
      numericInput("limite_x_max", "Limite superior do eixo X:", value = 10),
      
      # limites para o eixo Y
      numericInput("limite_y_min", "Limite inferior do eixo Y:", value = -10),
      numericInput("limite_y_max", "Limite superior do eixo Y:", value = 10)
    ),
    
    mainPanel(
      plotOutput("grafico")
    )
  )
)


server <- function(input, output, session) {
  
  output$grafico <- renderPlot({
    # filtrando a base de dados para a variável selecionada
    df2 <- df[, input$variavel, drop = FALSE]
    
    # criando o gráfico em linha com a base filtrada
    ggplot(df2, aes(x = seq_along(.data[[input$variavel]]), y = .data[[input$variavel]])) +
      geom_line(color = input$cor) +
      labs(x = input$variavel, y = "contagem") +
      xlim(input$limite_x_min, input$limite_x_max) +
      ylim(input$limite_y_min, input$limite_y_max)
  })
}

shinyApp(ui = ui, server = server)
