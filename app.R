# VITH - Visualização Interativa de testes de hipótese
versao <- 1.1
# Pedro Diniz Guglielmeli

#- - - - - - - - - - - - - - - - - - - -
# Esse Shiny App utiliza a licença GPLv3
#- - - - - - - - - - - - - - - - - - - -

# Github: https://github.com/paulk2jonas/VITH

if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(shiny)
library(ggplot2)

verm.esc <- "#913A3C" # Zona de rejeição
verm.cla <- "#DE6F71" # Hipótese
amarelo <- "#DFDD4C" # p-value
azul.cla <- "#59ACDE" # Beta
azul.esc <- "#417391" # Poder

ui <- fluidPage(
  
  titlePanel("Visualização interativa de testes de hipótese"),
  
  plotOutput(outputId = "plot"),
  
  fluidRow(
    
    column(
      5,
      # Média da hipótese nula
      sliderInput(inputId = "media.0", label = "Média hipotética da população", min = -5, max = 5, value = 0, step = .1, width = "100%"),
      
      # Média da hipótese alternativa
      sliderInput(inputId = "media.1", label = "Média da população", min = -5, max = 5, value = 3, step = .1, width = "100%"),
      
      # Desvio padrão
      sliderInput(inputId = "sd", label = "Desvio padrão", value = 1, min = .5, max = 2, step = .1, width = "100%")
    ),
    
    column(
      5,
      
      # Nível de significância
      sliderInput(inputId = "alfa", label = "Nível de significância", min = 0, max = 1, value = .05, step = .01, width = "100%"),
      
      # Tipo de teste
      selectInput(inputId = "tipo.teste", label = "Natureza do teste", choices = c("À esquerda", "À direita", "Bilateral"), selected = "À direita", width = "100%"),
      
      # Insere o valor da estatística de teste
      conditionalPanel(condition = "input.p_ligado == true", numericInput(inputId = "estat.teste", label = "Estatística de teste", value = 2, min = -10, max = 10, step = .01, width = "100%"))     
    ),
    
    column(
      2,
      
      wellPanel(
        ### Seleciona os gráficos
        ## Hipótese nula
        p(strong("Hipótese nula")),
        # Preenchimento
        checkboxInput(inputId = "p.h.nula", label = "Preenchimento", value = TRUE),
        # Curva
        checkboxInput(inputId = "c.h.nula", label = "Curva", value = TRUE),
        ## Hipótese alternativa
        p(strong("Hipótese alternativa")),
        # Preenchimento
        checkboxInput(inputId = "p.h.alt", label = "Preenchimento", value = TRUE),
        # Curva
        checkboxInput(inputId = "c.h.alt", label = "Curva", value = TRUE),
        
        br(),
        
        # Libera a utilização do p-value
        checkboxInput(inputId = "p_ligado", label = "Inserir estatística de teste")
      )
      
    )
    
  ),
  
  hr(),
  
  fluidRow(
    column(
      4,
      HTML('<a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.pt-br.html"><img alt="Licença GNU GLPv3" style="border-width:0" src="https://www.gnu.org/graphics/gplv3-with-text-84x42.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Visualização interativa de testes de hipótese</span> de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Pedro Diniz Guglielmeli</span> está licenciado com uma Licença <a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.pt-br.html">GNU GLPv3</a>.')
    ),
    column(
      8,
      p("Feito por Pedro Diniz Guglielmeli", align = "right"),
      p(paste("Versão ", versao), align = "right"),
      p(a(href = "https://github.com/paulk2jonas/VITH", "Código fonte"), align = "right")
    )
  )
  
)

server <- function(input, output) {
  
  # Cria os valores para o eixo x
  eixo.x <- data.frame(x = c(-7, 7))
  
  # Altera o alfa para a montagem do gráfico
  alfa.1 <- reactive({
    if (input$tipo.teste == "À esquerda") {
      input$alfa
    } else if (input$tipo.teste == "À direita") {
      1 - input$alfa
    } else if (input$tipo.teste == "Bilateral") {
      input$alfa / 2
    }
  })
  alfa.2 <- reactive(1 - input$alfa / 2)
  
  # Cria o ponto crítico do teste
  ponto.critico <- reactive(qnorm(p = alfa.1(), mean = input$media.0, sd = input$sd))
  ponto.critico.2 <- reactive(qnorm(p = alfa.2(), mean = input$media.0, sd = input$sd))
  
  ## Cria os limites a serem usados pelo gráfico
  # Limites para poder e área de rejeição em testes unilaterais, ou poder e área de rejeição à esquerda em testes bilaterais
  pr <- reactive({
    if (input$alfa == 0) {
      c(NA, NA)
    } else if (input$alfa == 1) {
      if (input$tipo.teste == "Bilateral") {
        c(-7, ponto.critico())
      } else {
        c(-7, 7)
      }
    } else {
      if (input$tipo.teste == "À esquerda") {
        c(-7, ponto.critico())
      } else if (input$tipo.teste == "À direita") {
        c(ponto.critico(), 7)
      } else if (input$tipo.teste == "Bilateral") {
        c(-7, ponto.critico())
      }
    }
  })
  # Limites para poder e área de rejeição à direita em testes bilaterais
  pr.2 <- reactive({
    if (input$tipo.teste == "À esquerda") {
      c(NA, NA)
    } else if (input$tipo.teste == "À direita") {
      c(NA, NA)
    } else if (input$tipo.teste == "Bilateral") {
      c(ponto.critico.2(), 7)
    }
  })
  # Limites para área beta e restante da hipótese nula
  bh <- reactive({
    if (input$alfa == 0) {
      c(-7, 7)
    } else if (input$alfa == 1) {
      c(NA, NA)
    } else {
      if (input$tipo.teste == "À esquerda") {
        c(ponto.critico(), 7)
      } else if (input$tipo.teste == "À direita") {
        c(-7, ponto.critico())
      } else if (input$tipo.teste == "Bilateral") {
        c(ponto.critico(), ponto.critico.2())
      }
    }
  })
  
  ## Cria o gráfico da hipótese alternativa
  # Cria a área de poder
  poder <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.1, sd = input$sd), geom = "area", xlim = pr(), fill = azul.esc, alpha = .3))
  poder.2 <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.1, sd = input$sd), geom = "area", xlim = pr.2(), fill = azul.esc, alpha = .3))
  # Cria a área beta
  area.beta <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.1, sd = input$sd), geom = "area", xlim = bh(), fill = azul.cla, alpha = .3))
  # Cria o outline
  curva.alt <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.1, sd = input$sd), size = .75, col = azul.esc))
  
  # Cria o gráfico da hipótese nula
  # Cria a área de 1 - alfa
  area.nula <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.0, sd = input$sd), geom = "area", xlim = bh(), fill = verm.cla, alpha = .3))
  # Cria a área de rejeição
  area.alfa <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.0, sd = input$sd), geom = "area", xlim = pr(), fill = verm.esc, alpha = .3))
  area.alfa.2 <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.0, sd = input$sd), geom = "area", xlim = pr.2(), fill = verm.esc , alpha = .3))
  # Cria o outline
  curva.nula <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.0, sd = input$sd), size = .75, col = verm.esc))

  # Cria a camada do ponto crítico
  pcritico <- reactive(geom_point(aes(x = ponto.critico(), y = 0), shape = 21, fill = verm.esc, col = "white", size = 4, stroke = 2))
  pcritico.2 <- reactive(geom_point(aes(x = ponto.critico.2(), y = 0), shape = 21, fill = verm.esc, col = "white", size = 4, stroke = 2))
  
  # Cria a área do p-value
  xlim.p <- reactive({
    if (input$tipo.teste == "À esquerda") {
      c(-7, input$estat.teste)
    } else if (input$tipo.teste == "À direita") {
      c(input$estat.teste, 7)
    } else if (input$tipo.teste == "Bilateral") {
      x.1 <- input$media.0 - abs(abs(input$media.0) - abs(input$estat.teste))
      c(-7, x.1)
    }
  })
  xlim.p.2 <- reactive({
    if (input$tipo.teste == "Bilateral") {
      x.2 <- input$media.0 + abs(abs(input$media.0) - abs(input$estat.teste))
      c(x.2, 7)
    } else {
      c(NA, NA)
    }
  })
  area.p <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.0, sd = input$sd), geom = "area", xlim = xlim.p(), fill = amarelo, alpha = .5))
  area.p.2 <- reactive(stat_function(fun = dnorm, args = list(mean = input$media.0, sd = input$sd), geom = "area", xlim = xlim.p.2(), fill = amarelo, alpha = .5))

  # Cria a camada da estatística de teste padronizada
  pteste <- reactive(geom_point(aes(x = input$estat.teste, y = 0), shape = 21, fill = amarelo, col = "white", size = 4, stroke = 2))
  
  # Cria as "linhas críticas"
  linha.crítica <- reactive(geom_vline(aes(xintercept = ponto.critico())))
  linha.crítica.2 <- reactive(geom_vline(aes(xintercept = ponto.critico.2())))
  
  ## Dados relevantes
  # Calcula os dados
  alfa.t <- reactive(input$alfa)
  beta.t <- reactive({
    if (input$alfa == 0) {
      1
    } else if (input$alfa == 1) {
      0
    } else {
      if (input $tipo.teste == "À esquerda") {
        pnorm(pr()[2], mean = input$media.1, sd = input$sd, lower.tail = FALSE)
      } else if (input$tipo.teste == "À direita") {
        pnorm(pr()[1], mean = input$media.1, sd = input$sd)
      } else if (input$tipo.teste == "Bilateral") {
        pnorm(pr()[2], mean = input$media.1, sd = input$sd) + pnorm(pr.2()[1], mean = input$media.1, sd = input$sd, lower.tail = FALSE)
      }
    }
  })
  poder.t <- reactive(1 - beta.t())
  p.value.t <- reactive({
    if (input $tipo.teste == "À esquerda") {
      pnorm(xlim.p()[2], mean = input$media.0, sd = input$sd)
    } else if (input$tipo.teste == "À direita") {
      pnorm(xlim.p()[1], mean = input$media.0, sd = input$sd, lower.tail = FALSE)
    } else if (input$tipo.teste == "Bilateral") {
      pnorm(xlim.p()[2], mean = input$media.0, sd = input$sd) + pnorm(xlim.p.2()[1], mean = input$media.0, sd = input$sd, lower.tail = FALSE)
    }
  })
  # Cria as camadas dos dados
  alfa.t.g <- reactive(annotate(geom = "text", x = -7, y = .75, label = bquote(alpha == .(alfa.t())), hjust = 0, size = 7))
  beta.t.g <- reactive(annotate(geom = "text", x = -7, y = .68, label = bquote(beta == .(beta.t())), hjust = 0, size = 7))
  poder.t.g <- reactive(annotate(geom = "text", x = -7, y = .61, label = bquote(Poder == .(poder.t())), hjust = 0, size = 7))
  p.value.t.g <- reactive(annotate(geom = "text", x = -7, y = .54, label = bquote(Valor~p == .(p.value.t())), hjust = 0, size = 7))
  
  # Plotagem de saída
  output$plot <- renderPlot({
    
    myplot <- ggplot(data = eixo.x, aes(x = x))
    
    # Plota as "linhas críticas"
    if (input$tipo.teste == "Bilateral") {
      myplot <- myplot + linha.crítica() + linha.crítica.2()
    } else {
      myplot <- myplot + linha.crítica()
    }
    
    # Plota a área da hipótese alternativa
    if (input$p.h.alt == TRUE) {
      myplot <- myplot + poder() + poder.2() + area.beta()
    }
    
    # Plota a área da hipótese nula
    if (input$p.h.nula == TRUE) {
      myplot <- myplot + area.nula() + area.alfa() + area.alfa.2()
    }
    
    # Plota a área do p-value
    if (input$p_ligado == TRUE) {
      myplot <- myplot + area.p() + area.p.2()
    }
    
    ## Plota as curvas de distribuição
    # Curva da distribuição alternativa
    if (input$c.h.alt == TRUE) {
      myplot <- myplot + curva.alt()
    }
    # Curva da distribuição nula
    if (input$c.h.nula == TRUE) {
      myplot <- myplot + curva.nula()
    }

    # Plota o ponto da estatística de teste
    if (input$p_ligado) myplot <- myplot + pteste()

    # Plota os pontos críticos
    if (input$tipo.teste == "Bilateral") {
      myplot <- myplot + pcritico() + pcritico.2()
    } else {
      myplot <- myplot + pcritico()
    }
    
    # Plota os dados relevantes
    if (input$p_ligado == TRUE) {
      myplot <- myplot + alfa.t.g() + beta.t.g() + poder.t.g() + p.value.t.g()
    } else {
      myplot <- myplot + alfa.t.g() + beta.t.g() + poder.t.g()
    }
    
    # Plota o gráfico final
    myplot + 
      coord_cartesian(xlim = c(-7, 7), ylim = c(0, .8)) + 
      theme_void() + 
      theme(panel.background = element_rect(size = .5), plot.margin = unit(c(0, 5, 15, 5), "pt"))
    
  })
  
}

shinyApp(ui = ui, server = server)
