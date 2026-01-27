
library(shiny)

fluidPage(
  
  titlePanel(
    fluidRow(
      column(2),
      column(8, h1("Aplicativo Web - Simulación Actuarial", align = "center")),
      column(2)
    )
  ),
  
  navbarPage(
    title = "Simulación de Seguros",
    
    tabPanel("Distribuciones del Modelo",
             
             h2("Modelo Probabilístico del Seguro", align = "center"),
             p("Esta sección muestra cómo se comportan el número de siniestros y el costo de cada siniestro usando simulación."),
             
             hr(),
             
             fluidRow(
               
               column(6,
                      
                      h3("Frecuencia de Siniestros"),
                      
                      helpText("Modela cuántos siniestros pueden ocurrir en un período."),
                      
                      selectInput("freq_dist",
                                  "Distribución:",
                                  choices = c("Poisson")),
                      
                      numericInput("lambda_freq",
                                   "Promedio de siniestros (λ):",
                                   value = 3,
                                   min = 0.01),
                      
                      numericInput("n_freq_sim",
                                   "Número de simulaciones:",
                                   value = 1000,
                                   min = 100),
                      
                      actionButton("sim_freq",
                                   "Simular Frecuencia",
                                   icon = icon("chart-bar")),
                      
                      hr(),
                      
                      plotOutput("freq_plot"),
                      verbatimTextOutput("freq_summary")
               ),
               
               column(6,
                      
                      h3("Severidad de los Siniestros"),
                      
                      helpText("Modela el costo económico de cada siniestro."),
                      
                      selectInput("sev_dist",
                                  "Distribución:",
                                  choices = c("Exponencial")),
                      
                      numericInput("lambda_sev",
                                   "Intensidad del costo (λ):",
                                   value = 0.05,
                                   min = 0.0001),
                      
                      numericInput("n_sev_sim",
                                   "Número de simulaciones:",
                                   value = 1000,
                                   min = 100),
                      
                      actionButton("sim_sev",
                                   "Simular Severidad",
                                   icon = icon("dollar-sign")),
                      
                      hr(),
                      
                      plotOutput("sev_plot"),
                      verbatimTextOutput("sev_summary")
               )
             )
    )
  )
)
