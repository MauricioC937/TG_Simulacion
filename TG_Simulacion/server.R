library(shiny)

x_poisson <- function(lambda, nval){
  res <- numeric(nval)
  for(j in 1:nval){
    i <- 0
    Fx <- p <- exp(-lambda)
    U <- runif(1)
    while(U > Fx){
      p <- (lambda * p) / (i + 1)
      Fx <- Fx + p
      i <- i + 1
    }
    res[j] <- i
  }
  return(res)
}

x_exponencial <- function(lambda, nval){
  res <- numeric(nval)
  for(i in 1:nval){
    res[i] <- -log(runif(1)) / lambda
  }
  return(res)
}

freq <- function(x){
  cat("• En promedio ocurren", round(mean(x), 2), "siniestros.\n\n")
  cat("• Lo más habitual es observar alrededor de", round(median(x)), "siniestros.\n\n")
  cat("• En su mayoría el número de siniestros se encuentra entre",
      quantile(x, 0.1), "y", quantile(x, 0.9), ".\n\n")
  cat("• En escenarios poco frecuentes, el número de siniestros puede llegar hasta",
      max(x), ".\n")
}

sev <- function(x){
  cat("RESULTADOS DE LA SIMULACIÓN\n\n")
  cat("• El costo promedio de un siniestro es de", round(mean(x), 2), ".\n\n")
  cat("• Un costo típico del siniestro es cercano a", round(median(x), 2), ".\n\n")
  cat("• En su mayoría el costo del siniestro se encuentra entre",
      round(quantile(x, 0.1), 2), "y", round(quantile(x, 0.9), 2), ".\n\n")
  cat("• En casos extremos, el costo del siniestro puede alcanzar valores cercanos a",
      round(max(x), 2), ".\n")
}

function(input, output, session) {
  
  frecuencia <- eventReactive(input$sim_freq, {
    x_poisson(input$lambda_freq, input$n_freq_sim)
  })
  
  output$freq_plot <- renderPlot({
    hist(frecuencia(),
         col = "lightblue",
         border = "black",
         main = "Distribución del Número de Siniestros",
         xlab = "Número de siniestros")
  })
  
  output$freq_summary <- renderPrint({
    freq(frecuencia())
  })
  
  severidad <- eventReactive(input$sim_sev, {
    x_exponencial(input$lambda_sev, input$n_sev_sim)
  })
  
  output$sev_plot <- renderPlot({
    hist(severidad(),
         col = "lightcyan",
         border = "black",
         main = "Distribución del Costo del Siniestro",
         xlab = "Costo del siniestro")
  })
  
  output$sev_summary <- renderPrint({
    sev(severidad())
  })
  
}
