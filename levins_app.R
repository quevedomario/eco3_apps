
library(shiny)
library(ggplot2)

# rsconnect::deployApp(
#   appDir = ".",
#   appPrimaryDoc = "levins_app.R"
# )

options(ggplot2.use_ragg = FALSE)

# UI ####
ui <- fluidPage(
  titlePanel("Dinámica de metapoblaciones - Levins y modificaciones"),
  
  sidebarLayout(
    sidebarPanel(
      # numericInput("m", "Tasa de colonización de parches (m)", 
      # #              value = 0.3, min = 0, step = 0.01),
      # numericInput("e", "Tasa de extinción de parches (e)", 
      #              value = 0.15, min = 0, step = 0.01),
      
      sliderInput(
        "m",
        "Tasa de colonización de parches (m)",
        min = 0,
        max = 1,
        value = 0.3,
        step = 0.01
      ),
      
      sliderInput(
        "e",
        "Tasa de extinción de parches (e)",
        min = 0,
        max = 1,
        value = 0.2,
        step = 0.01
      ),
      
      actionButton("reset", "Reiniciar parametros"),
      
      hr(),
      
      h4("Interpretación del modelo"),
      p("Esta aplicación ilustra los principios de la dinámica básica de metapoblaciones 
        según el modelo de Levins, y las modificaciones conocidas como 'Efecto Rescate' y 
        'Lluvia de Propágulos'."),
      p("Intuitivamente, el incremento de la tasa de colonization (m) tiende a aumentar la 
        proporción de parches ocupados; por otro lado el incremento de la extinción local (e)
        tiende a reducir esa proporción."),
      p("En algunos casos esperamos un equilibrio entre colonización y extinción, en la 
      intersección de las curvas."),
      
      hr(),
      
      tags$div(
        # style = "font-size: 1.2em; color: #244;",
        tags$h4("Cómo interpretar los gráficos"),
        tags$ul(
          tags$li("En la fila superior, a la izquierda, simulamos el modelo de Levins, donde las 
          marcas rojas muestran el punto de equilibrio entre colonización y extinción."),
          tags$li("En la fila superior, a la derecha, simulamos la modificación por la cual 
          la extinción local de parches es ralentizada por la ocupación de los parches ('Efecto Rescate')."),
          tags$li("En la fila inferior, a la izquierda, simulamos la modificación por la cual  
          el aporte de propágulos puede ser alto independientemente de la presencia de 
          parches ocupables ('Lluvia de Propágulos'). Esta versión es compatible con la noción 'Isla - Continente'
                  de Biogeografía de Islas"), 
          tags$li("En la fila inferior, a la derecha, simulamos ambas modificaciones del modelo
          básico de Levins, 'Efecto Rescate' y 'Lluvia de Propágulos'.")
        )
      ),
      
      hr(),
      tags$strong("Interpretación de la combinación actual de m y e"),
      textOutput("narrative")
      
      ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("plot_equilibrium"),
               downloadButton("download_plot_equilibrium", "Descargar figura")),
        column(6, plotOutput("plot_rescue"),
               downloadButton("download_plot_rescue", "Descargar figura"))
      ),
      fluidRow(
        column(6, plotOutput("plot_rain"),
               downloadButton("download_plot_rain", "Descargar figura")),
        column(6, plotOutput("plot_rescue_rain"),
               downloadButton("download_plot_rescue_rain", "Descargar figura"))
      )
 
      # selectInput(
      #   "plot_format",
      #   "Export format",
      #   choices = c("png", "pdf"),
      #   # selected = "png"
      # )
      
    )
  )
)

# SERVER ####
server <- function(input, output, session) {
### DATAFRAME ####  
  levins_data <- reactive({
    req(input$m >= 0, input$e >= 0)
    p <- seq(0, 1, 0.05)
    p_star_levins <- 1 - (input$e / input$m)
    p_star_rain <- input$m / (input$e + input$m)
    p_star_rescue_rain <- input$m / input$e
   
    data.frame(
      p = p,
      col_unlimit = input$m * p,
      col_limit   = input$m * p * (1 - p),
      col_rain    = input$m * (1 - p),
      ext_unlimit = input$e * p,
      ext_rescue  = input$e * p * (1 - p),
      p_star_levins = p_star_levins,
      p_star_rain = p_star_rain,
      p_star_rescue_rain
      )
  })

#### RESET PARAMETERS ####
  observeEvent(input$reset, {
    updateSliderInput(session, "m", value = 0.3)
    updateSliderInput(session, "e", value = 0.2)
  })
  
### ggplots ####
  # plots now within functions to make them exportable 

  plot_equilibrium <- function(df) {
    ggplot(levins_data(), aes(x = p)) +
      
      geom_point(
        aes(x = p_star_levins, y = input$e * p_star_levins),
        size = 4,
        color = "red"
      )+ 
      
      geom_vline(
        xintercept = levins_data()$p_star_levin[1],
        linetype = "dashed",
        color = "red"
      ) +
      
      geom_line(aes(y = col_limit), linewidth = 2, color = "blue") +
      geom_line(aes(y = ext_unlimit), linewidth = 2, color = "darkgreen") +
      scale_y_continuous(
        limits = c(0, max(
          levins_data()$col_unlimit,
          levins_data()$ext_unlimit
        )),
        name = "colonización",
        sec.axis = sec_axis(~ ., name = "extinción")
      ) +
      labs(
        x = "proporción de parches ocupados",
        title = expression("Modelo:  dp/dt = mp(1-p) - ep")) +
      theme_light() +
      theme(
        axis.text.x = element_text(hjust = 1, size = 16),
        axis.title.x = element_text(size = 16),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(size = 16, color = "blue"),
        axis.title.y.left = element_text(size = 16, color = "blue"),
        axis.line.y.right = element_line(color = "darkgreen"),
        axis.text.y.right = element_text(size = 16, color = "darkgreen"),
        axis.title.y.right = element_text(size = 16, color = "darkgreen"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  }
  
  plot_rescue <- function(df) {
    ggplot(levins_data(), aes(x = p)) +
      geom_line(aes(y = col_limit), linewidth = 2, color = "blue") +
      geom_line(aes(y = ext_rescue), linewidth = 2, color = "darkgreen") +
      scale_y_continuous(
        limits = c(0, max(
          levins_data()$col_unlimit,
          levins_data()$ext_unlimit
        )),
        name = "colonización",
        sec.axis = sec_axis(~ ., name = "extinción con rescate")
      ) +
      labs(x = "proporción de parches ocupados",
           title = expression("Modelo:  dp/dt = mp(1-p) - ep(1-p)")) +
      theme_light() +
      theme(
        axis.text.x = element_text(hjust = 1, size = 16),
        axis.title.x = element_text(size = 16),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(size = 16, color = "blue"),
        axis.title.y.left = element_text(size = 16, color = "blue"),
        axis.line.y.right = element_line(color = "darkgreen"),
        axis.text.y.right = element_text(size = 16, color = "darkgreen"),
        axis.title.y.right = element_text(size = 16, color = "darkgreen"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  }
  
  plot_rain <- function(df) {
    ggplot(levins_data(), aes(x = p)) +
      
      geom_point(
        aes(x = p_star_rain, y = input$e * p_star_rain),
        size = 4,
        color = "red"
      )+

      geom_vline(
        xintercept = levins_data()$p_star_rain[1],
        linetype = "dashed",
        color = "red"
      ) +
      
      geom_line(aes(y = col_rain), linewidth = 2, color = "blue") +
      geom_line(aes(y = ext_unlimit), linewidth = 2, color = "darkgreen") +
      scale_y_continuous(
        limits = c(0, max(
          levins_data()$col_unlimit,
          levins_data()$ext_unlimit
        )),
        name = "colonización - lluvia de propágulos",
        sec.axis = sec_axis(~ ., name = "extinción")
      ) +
      labs(x = "proporción de parches ocupados",
           title = expression("Modelo:  dp/dt = m(1-p) - ep")) +
      theme_light() +
      theme(
        axis.text.x = element_text(hjust = 1, size = 16),
        axis.title.x = element_text(size = 16),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(size = 16, color = "blue"),
        axis.title.y.left = element_text(size = 16, color = "blue"),
        axis.line.y.right = element_line(color = "darkgreen"),
        axis.text.y.right = element_text(size = 16, color = "darkgreen"),
        axis.title.y.right = element_text(size = 16, color = "darkgreen"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  }
  
   plot_rescue_rain <- function(df) {
    ggplot(levins_data(), aes(x = p)) +
      
      # geom_point(
      #   aes(x = p_star_rescue_rain, y = input$e * p_star_rescue_rain),
      #   size = 4,
      #   color = "red"
      # )+
      # 
      # geom_vline(
      #   xintercept = levins_data()$p_star_rescue_rain[1],
      #   linetype = "dashed",
      #   color = "red"
      # ) +
      
      geom_line(aes(y = col_rain), linewidth = 2, color = "blue") +
      geom_line(aes(y = ext_rescue), linewidth = 2, color = "darkgreen") +
      scale_y_continuous(
        limits = c(0, max(
          levins_data()$col_unlimit,
          levins_data()$ext_unlimit
        )),
        name = "colonización - lluvia de propágulos",
        sec.axis = sec_axis(~ ., name = "extinción con rescate")
      ) +
      labs(x = "proporción de parches ocupados",
           title = expression("Modelo:  dp/dt = m(1-p) - ep(1-p)")) +
      theme_light() +
      theme(
        axis.text.x = element_text(hjust = 1, size = 16),
        axis.title.x = element_text(size = 16),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(size = 16, color = "blue"),
        axis.title.y.left = element_text(size = 16, color = "blue"),
        axis.line.y.right = element_line(color = "darkgreen"),
        axis.text.y.right = element_text(size = 16, color = "darkgreen"),
        axis.title.y.right = element_text(size = 16, color = "darkgreen"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  }
  
  ### Render plots that can be saved later ####
  
  output$plot_equilibrium <- renderPlot({
    plot_equilibrium(levins_data())
  })
  
  output$plot_rescue <- renderPlot({
    plot_rescue(levins_data())
  })
  
  output$plot_rain <- renderPlot({
    plot_rain(levins_data())
  })
  
  output$plot_rescue_rain <- renderPlot({
    plot_rescue_rain(levins_data())
  })
  
  ### Download handlers ####
  
  output$download_plot_unlimited <- downloadHandler(
    
    filename = function() {
      paste0("levins_unlimited_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_unlimited(levins_data()),
        # device = input$plot_format,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  )
  
  output$download_plot_equilibrium <- downloadHandler(
    
    filename = function() {
      paste0("levins_equilibrium_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_equilibrium(levins_data()),
        # device = input$plot_format,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  )
  
  output$download_plot_rescue <- downloadHandler(
    
    filename = function() {
      paste0("levins_rescue_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_rescue(levins_data()),
        # device = input$plot_format,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  )
  
  output$download_plot_rain <- downloadHandler(
    
    filename = function() {
      paste0("levins_rain_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_rain(levins_data()),
        # device = input$plot_format,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  )

### Narrative - Reactive text ####
  output$narrative <- renderText({
    
    if (input$m > input$e) {
      "La tasa de colonización supera a la de extinción. La metapoblación tiende a persistir."
    } else if (input$m < input$e) {
      "La tasa de extinción supera a la de colonización. Disminuye la ocupación; 
      solo puede persistir una metapoblación con aporte ilimitado de propágulos externos."
    } else {
      "Si la tasa de colonización es idéntica a la de extinción solo 'Lluvia de propágulos' (panel inferior derecho)
      predice estabilidad."
    }
    
  })
  
    
### END OF SERVER BLOCK ####
}

# APP ####
shinyApp(ui, server)