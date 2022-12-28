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

verm_escuro <- "#913A3C" # Zona de rejeição
verm_claro <- "#DE6F71" # Hipótese
amarelo <- "#DFDD4C" # p-value
azul_claro <- "#59ACDE" # Beta
azul_escuro <- "#417391" # Poder

ui <- fluidPage(

  titlePanel("Visualização interativa de testes de hipótese"),

  plotOutput(outputId = "plot"),

  fluidRow(

    column(
      5,
      # Média da hipótese nula
      sliderInput(
        inputId = "media_0",
        label = "Média hipotética da população",
        min = -5,
        max = 5,
        value = 0,
        step = .1,
        width = "100%"
      ),

      # Média da hipótese alternativa
      sliderInput(
        inputId = "media_1",
        label = "Média da população",
        min = -5,
        max = 5,
        value = 3,
        step = .1,
        width = "100%"
      ),

      # Desvio padrão
      sliderInput(
        inputId = "sd",
        label = "Desvio padrão",
        value = 1,
        min = .5,
        max = 2,
        step = .1,
        width = "100%"
      )
    ),

    column(
      5,

      # Nível de significância
      sliderInput(
        inputId = "alfa",
        label = "Nível de significância",
        min = 0,
        max = 1,
        value = .05,
        step = .01,
        width = "100%"
      ),

      # Tipo de teste
      selectInput(
        inputId = "tipo_teste",
        label = "Natureza do teste",
        choices = c("À esquerda", "À direita", "Bilateral"),
        selected = "À direita",
        width = "100%"
      ),

      # Insere o valor da estatística de teste
      conditionalPanel(
        condition = "input_p_ligado == true",
        numericInput(
          inputId = "estat_teste",
          label = "Estatística de teste",
          value = 2,
          min = -10,
          max = 10,
          step = .01,
          width = "100%"
        )
      )
    ),

    column(
      2,

      wellPanel(
        ### Seleciona os gráficos
        ## Hipótese nula
        p(strong("Hipótese nula")),
        # Preenchimento
        checkboxInput(
          inputId = "p_h_nula",
          label = "Preenchimento",
          value = TRUE
        ),
        # Curva
        checkboxInput(
          inputId = "c_h_nula",
          label = "Curva",
          value = TRUE
        ),
        ## Hipótese alternativa
        p(strong("Hipótese alternativa")),
        # Preenchimento
        checkboxInput(
          inputId = "p_h_alt",
          label = "Preenchimento",
          value = TRUE
        ),
        # Curva
        checkboxInput(
          inputId = "c_h_alt",
          label = "Curva",
          value = TRUE
        ),

        br(),

        # Libera a utilização do p-value
        checkboxInput(
          inputId = "p_ligado",
          label = "Inserir estatística de teste"
        )
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
      p(
        a(
          href = "https://github.com/paulk2jonas/VITH",
          "Código fonte"
          ),
        align = "right"
      )
    )
  )

)

server <- function(input, output) {

  # Cria os valores para o eixo x
  eixo_x <- data.frame(x = c(-7, 7))

  # Altera o alfa para a montagem do gráfico
  alfa_1 <- reactive({
    if (input$tipo_teste == "À esquerda") {
      input$alfa
    } else if (input$tipo_teste == "À direita") {
      1 - input$alfa
    } else if (input$tipo_teste == "Bilateral") {
      input$alfa / 2
    }
  })
  alfa_2 <- reactive(1 - input$alfa / 2)

  # Cria o ponto crítico do teste
  ponto_critico <- reactive(
    qnorm(
      p = alfa_1(),
      mean = input$media_0,
      sd = input$sd
    )
  )
  ponto_critico_2 <- reactive(
    qnorm(
      p = alfa_2(),
      mean = input$media_0,
      sd = input$sd
    )
  )

  ## Cria os limites a serem usados pelo gráfico
  # Limites para poder e área de rejeição em testes unilaterais,
  # ou poder e área de rejeição à esquerda em testes bilaterais
  pr <- reactive({
    if (input$alfa == 0) {
      c(NA, NA)
    } else if (input$alfa == 1) {
      if (input$tipo_teste == "Bilateral") {
        c(-7, ponto_critico())
      } else {
        c(-7, 7)
      }
    } else {
      if (input$tipo_teste == "À esquerda") {
        c(-7, ponto_critico())
      } else if (input$tipo_teste == "À direita") {
        c(ponto_critico(), 7)
      } else if (input$tipo_teste == "Bilateral") {
        c(-7, ponto_critico())
      }
    }
  })
  # Limites para poder e área de rejeição à direita em testes bilaterais
  pr_2 <- reactive({
    if (input$tipo_teste == "À esquerda") {
      c(NA, NA)
    } else if (input$tipo_teste == "À direita") {
      c(NA, NA)
    } else if (input$tipo_teste == "Bilateral") {
      c(ponto_critico_2(), 7)
    }
  })
  # Limites para área beta e restante da hipótese nula
  bh <- reactive({
    if (input$alfa == 0) {
      c(-7, 7)
    } else if (input$alfa == 1) {
      c(NA, NA)
    } else {
      if (input$tipo_teste == "À esquerda") {
        c(ponto_critico(), 7)
      } else if (input$tipo_teste == "À direita") {
        c(-7, ponto_critico())
      } else if (input$tipo_teste == "Bilateral") {
        c(ponto_critico(), ponto_critico_2())
      }
    }
  })

  ## Cria o gráfico da hipótese alternativa
  # Cria a área de poder
  poder <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_1,
        sd = input$sd
      ),
      geom = "area",
      xlim = pr(),
      fill = azul_escuro,
      alpha = .3
    )
  )
  poder_2 <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_1,
        sd = input$sd
      ),
      geom = "area",
      xlim = pr_2(),
      fill = azul_escuro,
      alpha = .3
    )
  )
  # Cria a área beta
  area_beta <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_1,
        sd = input$sd
      ),
      geom = "area",
      xlim = bh(),
      fill = azul_claro,
      alpha = .3
    )
  )
  # Cria o outline
  curva_alt <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_1,
        sd = input$sd
      ),
      size = .75,
      col = azul_escuro
    )
  )

  # Cria o gráfico da hipótese nula
  # Cria a área de 1 - alfa
  area_nula <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_0,
        sd = input$sd
      ),
      geom = "area",
      xlim = bh(),
      fill = verm_claro,
      alpha = .3
    )
  )
  # Cria a área de rejeição
  area_alfa <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_0,
        sd = input$sd
      ),
      geom = "area",
      xlim = pr(),
      fill = verm_escuro,
      alpha = .3
    )
  )
  area_alfa_2 <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_0,
        sd = input$sd
        ),
        geom = "area",
        xlim = pr_2(),
        fill = verm_escuro,
        alpha = .3
      )
    )
  # Cria o outline
  curva_nula <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_0,
        sd = input$sd
      ),
      size = .75,
      col = verm_escuro
    )
  )

  # Cria a camada do ponto crítico
  pcritico <- reactive(
    geom_point(
      aes(
        x = ponto_critico(),
        y = 0
      ),
      shape = 21,
      fill = verm_escuro,
      col = "white",
      size = 4,
      stroke = 2
    )
  )
  pcritico_2 <- reactive(
    geom_point(
      aes(
        x = ponto_critico_2(),
        y = 0
      ),
      shape = 21,
      fill = verm_escuro,
      col = "white",
      size = 4,
      stroke = 2
    )
  )

  # Cria a área do p-value
  xlim_p <- reactive({
    if (input$tipo_teste == "À esquerda") {
      c(-7, input$estat_teste)
    } else if (input$tipo_teste == "À direita") {
      c(input$estat_teste, 7)
    } else if (input$tipo_teste == "Bilateral") {
      x_1 <- input$media_0 - abs(abs(input$media_0) - abs(input$estat_teste))
      c(-7, x_1)
    }
  })
  xlim_p_2 <- reactive({
    if (input$tipo_teste == "Bilateral") {
      x_2 <- input$media_0 + abs(abs(input$media_0) - abs(input$estat_teste))
      c(x_2, 7)
    } else {
      c(NA, NA)
    }
  })
  area_p <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_0,
        sd = input$sd
      ),
      geom = "area",
      xlim = xlim_p(),
      fill = amarelo,
      alpha = .5
    )
  )
  area_p_2 <- reactive(
    stat_function(
      fun = dnorm,
      args = list(
        mean = input$media_0,
        sd = input$sd
      ),
      geom = "area",
      xlim = xlim_p_2(),
      fill = amarelo,
      alpha = .5
    )
  )

  # Cria a camada da estatística de teste padronizada
  pteste <- reactive(
    geom_point(
      aes(
        x = input$estat_teste,
        y = 0
      ),
      shape = 21,
      fill = amarelo,
      col = "white",
      size = 4,
      stroke = 2
    )
  )

  # Cria as "linhas críticas"
  linha_critica <- reactive(geom_vline(aes(xintercept = ponto_critico())))
  linha_critica_2 <- reactive(geom_vline(aes(xintercept = ponto_critico_2())))

  ## Dados relevantes
  # Calcula os dados
  alfa_t <- reactive(input$alfa)
  beta_t <- reactive({
    if (input$alfa == 0) {
      1
    } else if (input$alfa == 1) {
      0
    } else {
      if (input $tipo_teste == "À esquerda") {
        pnorm(
          pr()[2],
          mean = input$media_1,
          sd = input$sd,
          lower.tail = FALSE
        )
      } else if (input$tipo_teste == "À direita") {
        pnorm(
          pr()[1],
          mean = input$media_1,
          sd = input$sd
        )
      } else if (input$tipo_teste == "Bilateral") {
        pnorm(
          pr()[2],
          mean = input$media_1,
          sd = input$sd
        ) + pnorm(  # ! Have to fix this crap
          pr_2()[1],
          mean = input$media_1,
          sd = input$sd,
          lower.tail = FALSE
        )
      }
    }
  })
  poder_t <- reactive(1 - beta_t())
  p_value_t <- reactive({
    if (input $tipo_teste == "À esquerda") {
      pnorm(
        xlim_p()[2],
        mean = input$media_0,
        sd = input$sd
      )
    } else if (input$tipo_teste == "À direita") {
      pnorm(
        xlim_p()[1],
        mean = input$media_0,
        sd = input$sd,
        lower.tail = FALSE
      )
    } else if (input$tipo_teste == "Bilateral") {
      pnorm(
        xlim_p()[2],
        mean = input$media_0,
        sd = input$sd
      ) + pnorm(  # ! Fix this crap too
        xlim_p_2()[1],
        mean = input$media_0,
        sd = input$sd,
        lower.tail = FALSE
      )
    }
  })
  # Cria as camadas dos dados
  alfa_t_g <- reactive(
    annotate(
      geom = "text",
      x = -7,
      y = .75,
      label = bquote(
        alpha == .(alfa_t())  # ? Why do I need this??
      ),
      hjust = 0,
      size = 7
    )
  )
  beta_t_g <- reactive(
    annotate(
      geom = "text",
      x = -7,
      y = .68,
      label = bquote(
        beta == .(beta_t())
      ),
      hjust = 0,
      size = 7
    )
  )
  poder_t_g <- reactive(
    annotate(
      geom = "text",
      x = -7,
      y = .61,
      label = bquote(
        Poder == .(poder_t())
      ),
      hjust = 0,
      size = 7
    )
  )
  p_value_t_g <- reactive(
    annotate(
      geom = "text",
      x = -7,
      y = .54,
      label = bquote(
        Valor ~ p == .(p_value_t())
      ),
      hjust = 0,
      size = 7
    )
  )

  # Plotagem de saída
  output$plot <- renderPlot({

    myplot <- ggplot(data = eixo_x, aes(x = .data[["x"]]))

    # Plota as "linhas críticas"
    if (input$tipo_teste == "Bilateral") {
      myplot <- myplot + linha_critica() + linha_critica_2()
    } else {
      myplot <- myplot + linha_critica()
    }

    # Plota a área da hipótese alternativa
    if (input$p_h_alt == TRUE) {
      myplot <- myplot + poder() + poder_2() + area_beta()
    }

    # Plota a área da hipótese nula
    if (input$p_h_nula == TRUE) {
      myplot <- myplot + area_nula() + area_alfa() + area_alfa_2()
    }

    # Plota a área do p-value
    if (input$p_ligado == TRUE) {
      myplot <- myplot + area_p() + area_p_2()
    }

    ## Plota as curvas de distribuição
    # Curva da distribuição alternativa
    if (input$c_h_alt == TRUE) {
      myplot <- myplot + curva_alt()
    }
    # Curva da distribuição nula
    if (input$c_h_nula == TRUE) {
      myplot <- myplot + curva_nula()
    }

    # Plota o ponto da estatística de teste
    if (input$p_ligado) myplot <- myplot + pteste()

    # Plota os pontos críticos
    if (input$tipo_teste == "Bilateral") {
      myplot <- myplot + pcritico() + pcritico_2()
    } else {
      myplot <- myplot + pcritico()
    }

    # Plota os dados relevantes
    if (input$p_ligado == TRUE) {
      myplot <- myplot + alfa_t_g() + beta_t_g() + poder_t_g() + p_value_t_g()
    } else {
      myplot <- myplot + alfa_t_g() + beta_t_g() + poder_t_g()
    }

    # Plota o gráfico final
    myplot +
      coord_cartesian(xlim = c(-7, 7), ylim = c(0, .8)) +
      theme_void() +
      theme(
        panel.background = element_rect(size = .5),
        plot.margin = unit(c(0, 5, 15, 5), "pt")
      )

  })

}

shinyApp(ui = ui, server = server)
