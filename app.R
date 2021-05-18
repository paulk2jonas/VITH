# VITH - Visualiza��o Interativa de testes de hip�tese
# VERS�O 1.0
# Pedro Diniz Guglielmeli

#- - - - - - - - - - - - - - - - - - - -
# Esse Shiny App utiliza a licen�a GPLv3
#- - - - - - - - - - - - - - - - - - - -

if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(shiny)
library(ggplot2)

verm.esc <- "#913A3C" # Zona de rejei��o
verm.cla <- "#DE6F71" # Hip�tese
amarelo <- "#DFDD4C" # p-value
azul.cla <- "#59ACDE" # Beta
azul.esc <- "#417391" # Poder

ui <- fluidPage(
  
  titlePanel("Visualiza��o interativa de testes de hip�tese"),
  
  plotOutput(outputId = "plot"),
  
  fluidRow(
    
    column(
      5,
      # M�dia da hip�tese nula
      sliderInput(inputId = "m�dia.0", label = "M�dia hipot�tica da popula��o", min = -5, max = 5, value = 0, step = .1, width = "100%"),
      
      # M�dia da hip�tese alternativa
      sliderInput(inputId = "m�dia.1", label = "M�dia da popula��o", min = -5, max = 5, value = 3, step = .1, width = "100%"),
      
      # Desvio padr�o
      sliderInput(inputId = "sd", label = "Desvio padr�o", value = 1, min = .5, max = 2, step = .1, width = "100%")
    ),
    
    column(
      5,
      
      # N�vel de confian�a
      sliderInput(inputId = "alfa", label = "N�vel de signific�ncia", min = 0, max = 1, value = .05, step = .01, width = "100%"),
      
      # Tipo de teste
      selectInput(inputId = "tipo.teste", label = "Natureza do teste", choices = c("� esquerda", "� direita", "Bilateral"), selected = "� direita", width = "100%"),
      
      # Insere o valor da estat�stica de teste
      conditionalPanel(condition = "input.p_ligado == true", numericInput(inputId = "estat.teste", label = "Estat�stica de teste", value = 2, min = -10, max = 10, step = .01, width = "100%"))     
    ),
    
    column(
      2,
      
      wellPanel(
        ### Seleciona os gr�ficos
        ## Hip�tese nula
        p(strong("Hip�tese nula")),
        # Preenchimento
        checkboxInput(inputId = "p.h.nula", label = "Preenchimento", value = TRUE),
        # Curva
        checkboxInput(inputId = "c.h.nula", label = "Curva", value = TRUE),
        ## Hip�tese alternativa
        p(strong("Hip�tese alternativa")),
        # Preenchimento
        checkboxInput(inputId = "p.h.alt", label = "Preenchimento", value = TRUE),
        # Curva
        checkboxInput(inputId = "c.h.alt", label = "Curva", value = TRUE),
        
        br(),
        
        # Libera a utiliza��o do p-value
        checkboxInput(inputId = "p_ligado", label = "Inserir estat�stica de teste")
      )
      
    )
    
  ),
  
  hr(),
  
  fluidRow(
    column(
      4,
      HTML('<a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.pt-br.html"><img alt="Licen�a GNU GLPv3" style="border-width:0" src="https://www.gnu.org/graphics/gplv3-with-text-84x42.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Visualiza��o interativa de testes de hip�tese</span> de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Pedro Diniz Guglielmeli</span> est� licenciado com uma Licen�a <a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.pt-br.html">GNU GLPv3</a>.')
    ),
    column(
      8,
      p("Feito por Pedro Diniz Guglielmeli", align = "right"),
      p("Vers�o 1.0", align = "right"),
      p(a(href = "https://github.com/paulk2jonas/VITH", "C�digo fonte"), align = "right")
    )
  )
  
)

server <- function(input, output) {
  
  # Cria os valores para o eixo x
  eixo.x <- data.frame(x = c(-7, 7))
  
  # Altera o alfa para a montagem do gr�fico
  alfa.1 <- reactive({
    if (input$tipo.teste == "� esquerda") {
      input$alfa
    } else if (input$tipo.teste == "� direita") {
      1 - input$alfa
    } else if (input$tipo.teste == "Bilateral") {
      input$alfa / 2
    }
  })
  alfa.2 <- reactive(1 - input$alfa / 2)
  
  # Cria o ponto cr�tico do teste
  ponto.cr�tico <- reactive(qnorm(p = alfa.1(), mean = input$m�dia.0, sd = input$sd))
  ponto.cr�tico.2 <- reactive(qnorm(p = alfa.2(), mean = input$m�dia.0, sd = input$sd))
  
  ## Cria os limites a serem usados pelo gr�fico
  # Limites para poder e �rea de rejei��o em testes unilaterais, ou poder e �rea de rejei��o � esquerda em testes bilaterais
  pr <- reactive({
    if (input$alfa == 0) {
      c(NA, NA)
    } else if (input$alfa == 1) {
      if (input$tipo.teste == "Bilateral") {
        c(-7, ponto.cr�tico())
      } else {
        c(-7, 7)
      }
    } else {
      if (input$tipo.teste == "� esquerda") {
        c(-7, ponto.cr�tico())
      } else if (input$tipo.teste == "� direita") {
        c(ponto.cr�tico(), 7)
      } else if (input$tipo.teste == "Bilateral") {
        c(-7, ponto.cr�tico())
      }
    }
  })
  # Limites para poder e �rea de rejei��o � direita em testes bilaterais
  pr.2 <- reactive({
    if (input$tipo.teste == "� esquerda") {
      c(NA, NA)
    } else if (input$tipo.teste == "� direita") {
      c(NA, NA)
    } else if (input$tipo.teste == "Bilateral") {
      c(ponto.cr�tico.2(), 7)
    }
  })
  # Limites para �rea beta e restante da hip�tese nula
  bh <- reactive({
    if (input$alfa == 0) {
      c(-7, 7)
    } else if (input$alfa == 1) {
      c(NA, NA)
    } else {
      if (input$tipo.teste == "� esquerda") {
        c(ponto.cr�tico(), 7)
      } else if (input$tipo.teste == "� direita") {
        c(-7, ponto.cr�tico())
      } else if (input$tipo.teste == "Bilateral") {
        c(ponto.cr�tico(), ponto.cr�tico.2())
      }
    }
  })
  
  ## Cria o gr�fico da hip�tese alternativa
  # Cria a �rea de poder
  poder <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.1, sd = input$sd), geom = "area", xlim = pr(), fill = azul.esc, alpha = .3))
  poder.2 <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.1, sd = input$sd), geom = "area", xlim = pr.2(), fill = azul.esc, alpha = .3))
  # Cria a �rea beta
  area.beta <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.1, sd = input$sd), geom = "area", xlim = bh(), fill = azul.cla, alpha = .3))
  # Cria o outline
  curva.alt <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.1, sd = input$sd), size = .75, col = azul.esc))
  
  # Cria o gr�digo da hip�tese nula
  # Cria a �rea de 1 - alfa
  area.nula <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.0, sd = input$sd), geom = "area", xlim = bh(), fill = verm.cla, alpha = .3))
  # Cria a �rea de rejei��o
  area.alfa <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.0, sd = input$sd), geom = "area", xlim = pr(), fill = verm.esc, alpha = .3))
  area.alfa.2 <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.0, sd = input$sd), geom = "area", xlim = pr.2(), fill = verm.esc , alpha = .3))
  # Cria o outline
  curva.nula <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.0, sd = input$sd), size = .75, col = verm.esc))
  
  # Cria a �rea do p-value
  xlim.p <- reactive({
    if (input$tipo.teste == "� esquerda") {
      c(-7, input$estat.teste)
    } else if (input$tipo.teste == "� direita") {
      c(input$estat.teste, 7)
    } else if (input$tipo.teste == "Bilateral") {
      x.1 <- input$m�dia.0 - abs(abs(input$m�dia.0) - abs(input$estat.teste))
      c(-7, x.1)
    }
  })
  xlim.p.2 <- reactive({
    if (input$tipo.teste == "Bilateral") {
      x.2 <- input$m�dia.0 + abs(abs(input$m�dia.0) - abs(input$estat.teste))
      c(x.2, 7)
    } else {
      c(NA, NA)
    }
  })
  area.p <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.0, sd = input$sd), geom = "area", xlim = xlim.p(), fill = amarelo, alpha = .5))
  area.p.2 <- reactive(stat_function(fun = dnorm, args = list(mean = input$m�dia.0, sd = input$sd), geom = "area", xlim = xlim.p.2(), fill = amarelo, alpha = .5))
  
  # Cria as "linhas cr�ticas"
  linha.cr�tica <- reactive(geom_vline(aes(xintercept = ponto.cr�tico())))
  linha.cr�tica.2 <- reactive(geom_vline(aes(xintercept = ponto.cr�tico.2())))
  
  ## Dados relevantes
  # Calcula os dados
  alfa.t <- reactive(input$alfa)
  beta.t <- reactive({
    if (input$alfa == 0) {
      1
    } else if (input$alfa == 1) {
      0
    } else {
      if (input $tipo.teste == "� esquerda") {
        pnorm(pr()[2], mean = input$m�dia.1, sd = input$sd, lower.tail = FALSE)
      } else if (input$tipo.teste == "� direita") {
        pnorm(pr()[1], mean = input$m�dia.1, sd = input$sd)
      } else if (input$tipo.teste == "Bilateral") {
        pnorm(pr()[2], mean = input$m�dia.1, sd = input$sd) + pnorm(pr.2()[1], mean = input$m�dia.1, sd = input$sd, lower.tail = FALSE)
      }
    }
  })
  poder.t <- reactive(1 - beta.t())
  p.value.t <- reactive({
    if (input $tipo.teste == "� esquerda") {
      pnorm(xlim.p()[2], mean = input$m�dia.0, sd = input$sd)
    } else if (input$tipo.teste == "� direita") {
      pnorm(xlim.p()[1], mean = input$m�dia.0, sd = input$sd, lower.tail = FALSE)
    } else if (input$tipo.teste == "Bilateral") {
      pnorm(xlim.p()[2], mean = input$m�dia.0, sd = input$sd) + pnorm(xlim.p.2()[1], mean = input$m�dia.0, sd = input$sd, lower.tail = FALSE)
    }
  })
  # Cria as camadas dos dados
  alfa.t.g <- reactive(annotate(geom = "text", x = -7, y = .75, label = bquote(alpha == .(alfa.t())), hjust = 0, size = 7))
  beta.t.g <- reactive(annotate(geom = "text", x = -7, y = .68, label = bquote(beta == .(beta.t())), hjust = 0, size = 7))
  poder.t.g <- reactive(annotate(geom = "text", x = -7, y = .61, label = bquote(Poder == .(poder.t())), hjust = 0, size = 7))
  p.value.t.g <- reactive(annotate(geom = "text", x = -7, y = .54, label = bquote(Valor~p == .(p.value.t())), hjust = 0, size = 7))
  
  # Plotagem de sa�da
  output$plot <- renderPlot({
    
    myplot <- ggplot(data = eixo.x, aes(x = x))
    
    # Plota a �rea da hip�tese alternativa
    if (input$p.h.alt == TRUE) {
      myplot <- myplot + poder() + poder.2() + area.beta()
    }
    
    # Plota a �rea da hip�tese nula
    if (input$p.h.nula == TRUE) {
      myplot <- myplot + area.nula() + area.alfa() + area.alfa.2()
    }
    
    # Plota a �rea do p-value
    if (input$p_ligado == TRUE) {
      myplot <- myplot + area.p() + area.p.2()
    }
    
    ## Plota as curvas de distribui��o
    # Curva da distribui��o alternativa
    if (input$c.h.alt == TRUE) {
      myplot <- myplot + curva.alt()
    }
    # Curva da distribui��o nula
    if (input$c.h.nula == TRUE) {
      myplot <- myplot + curva.nula()
    }
    
    # Plota as "linhas cr�ticas"
    if (input$tipo.teste == "Bilateral") {
      myplot <- myplot + linha.cr�tica() + linha.cr�tica.2()
    } else {
      myplot <- myplot + linha.cr�tica()
    }
    
    # Plota os dados relevantes
    if (input$p_ligado == TRUE) {
      myplot <- myplot + alfa.t.g() + beta.t.g() + poder.t.g() + p.value.t.g()
    } else {
      myplot <- myplot + alfa.t.g() + beta.t.g() + poder.t.g()
    }
    
    # Plota o gr�fico final
    myplot + 
      coord_cartesian(xlim = c(-7, 7), ylim = c(0, .8)) + 
      theme_void() + 
      theme(panel.background = element_rect(size = .5), plot.margin = unit(c(0, 5, 15, 5), "pt"))
    
  })
  
}

shinyApp(ui = ui, server = server)