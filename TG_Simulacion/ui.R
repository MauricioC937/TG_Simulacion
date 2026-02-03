
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
                                  choices = c("Poisson", "Binomial", "Binomial Negativa")),

                      conditionalPanel(
                      condition = "input.freq_dist == 'Poisson'",
                      numericInput("lambda_freq",
                                   "Promedio de siniestros (λ):",
                                   value = 3,
                                   min = 0.01),
                      ),

                      conditionalPanel(
                        condition = "input.freq_dist == 'Binomial'",
                        numericInput("n_bin",
                                     "n (ensayos):",
                                     value = 10,
                                     min = 1),
                        numericInput("p_bin",
                                     "p (probabilidad de éxito(siniestro)):",
                                     value = 0.3,
                                     min = 0.01,
                                     max = 0.99)
                      ),

                      conditionalPanel(
                        condition = "input.freq_dist == 'Binomial Negativa'",
                        numericInput("r_nb",
                                     "r (número de éxitos (siniestros)):",
                                     value = 3,
                                     min = 1),
                        numericInput("p_nb",
                                     "p (probabilidad de éxito):",
                                     value = 0.4,
                                     min = 0.01,
                                     max = 0.99)
                      ),
                      
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

               #Para Severidad
               column(6,
                      
                      h3("Severidad de los Siniestros"),
                      
                      helpText("Modela el costo económico de cada siniestro."),
                      
                      selectInput("sev_dist",
                                  "Distribución:",
                                  choices = c("Exponencial","Gamma", "Log-Normal", "Weibull")),

                      conditionalPanel(
                      condition = "input.sev_dist == 'Exponencial'",
                      numericInput("lambda_sev",
                                   "Intensidad del costo (λ):",
                                   value = 0.05,
                                   min = 0.0001),
                      ),
                      conditionalPanel(
                        condition = "input.sev_dist == 'Gamma'",
                        numericInput("alpha_g",
                                     "α (parámetro de forma):",
                                     value = 2,
                                     min = 0.1),
                        numericInput("lambda_g",
                                     "λ (parámetro de escala):",
                                     value = 0.05,
                                     min = 0.0001)
                      ),
                      conditionalPanel(
                        condition = "input.sev_dist == 'Log-Normal'",
                        numericInput("mu_ln",
                                     "μ (media log)",
                                     value = 3),
                        numericInput("sigma_ln",
                                     "σ (desviación log)",
                                     value = 0.5,
                                     min = 0.01)
                      ),
                      conditionalPanel(
                        condition = "input.sev_dist == 'Weibull'",
                        numericInput("beta_w",
                                     "β (parámetro de forma):",
                                     value = 2,
                                     min = 0.1),
                        numericInput("lambda_w",
                                     "λ (parámetro de escala):",
                                     value = 100,
                                     min = 0.01)
                      ),
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
    ),
    tabPanel("Pérdida Agregada",
             
             h2("Estimación del Costo Total Esperado", align = "center"),
             p("Simulación de la pérdida agregada y cálculo de primas."),
             
             hr(),
             
             fluidRow(
               # COLUMNA DE CONTROLES
               column(4,
                      wellPanel( 
                        h4("Configuración de Simulación"),
                        numericInput("n_sim_total",
                                     "Número de simulaciones:",
                                     value = 10000,
                                     min = 10),
                        
                        hr(),
                        
                        h4("Parámetros Financieros"),
                        helpText("Necesarios para calcular la Prima Comercial."),
                        
                        
                        numericInput("margen", 
                                     "Margen de Ganancia (%):", 
                                     value = 20, min = 0),
                        
                        numericInput("impuestos", 
                                     "Tasa de Impuestos (%):", 
                                     value = 12, min = 0),
                        
                        hr(),
                        h4("Condiciones de la Póliza"),
                        helpText("Aplica deducibles y límites por evento."),
                        numericInput("deducible", "Deducible por evento:", value = 0, min = 0),
                        numericInput("limite", "Límite por evento (0 = Sin límite):", value = 0, min = 0),
                        actionButton("sim_total",
                                     "Simular y Calcular",
                                     class = "btn-primary",
                                     icon = icon("calculator"),
                                     width = "100%")
                      )
               ),
               
               # COLUMNA DE RESULTADOS
               column(8,
                      h4("Distribución de Pérdidas"),
                      plotOutput("agg_plot"),
                      plotOutput("agg_cdf_plot"),
                      br(),
                      
                      tabsetPanel(
                        # Resumen estadístico básico
                        tabPanel("Resumen Estadístico", 
                                 verbatimTextOutput("agg_summary")
                        ),
                        
                        # Primas
                        tabPanel("Cálculo de Primas", 
                                 verbatimTextOutput("primas_summary")
                        ),
                        #Intervalos de confianza
                        tabPanel("Intervalos de Confianza", 
                                 verbatimTextOutput("intervalos_confianza")
                        )
                      )
               )
             )
    )
  )
)

