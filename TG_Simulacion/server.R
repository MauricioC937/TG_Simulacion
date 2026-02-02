library(shiny)
# Poisson
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
# Binomial
x_binomial <- function(n, p, nval){
  res <- numeric(nval)
  for(j in 1:nval){
    c <- p/(1-p)
    i <- 0
    Fx <- pr <- (1-p)^n
    U <- runif(1)
    while(U > Fx){
      pr <- c*((n-i)/(i+1))*pr
      Fx <- Fx + pr
      i <- i+1
    }
    res[j] <- i
  }
  return(res)
}
# Binomial Negativa
x_binom_negativa <- function(r,p,nval){
  res <- numeric(nval)
  for(j in 1:nval){
    sum_geom <- 0
    for(i in 1:r){
      U <- runif(1)
      geom <- floor(log(U)/log(1-p)) + 1
      sum_geom <- sum_geom + geom
    }
    res[j] <- sum_geom
  }
  return(res)
}
# Para Severidad
# Exponencial
x_exponencial <- function(lambda, nval){
  res <- numeric(nval)
  for(i in 1:nval){
    res[i] <- -log(runif(1)) / lambda
  }
  return(res)
}
# Gamma
x_gamma <- function(alpha, lambda, nval){
  return(rgamma(nval, shape = alpha, rate = lambda))
}
#Log-Normal
x_lognormal <- function(mu, sigma, nval){
  res <- numeric(nval)
  for(i in 1:nval){
    Z <- rnorm(1, mean = mu, sd = sigma)
    res[i] <- exp(Z)
  }
  return(res)
}
#weibull
x_weibull <- function(beta, lambda, nval){
  res <- numeric(nval)
  for(i in 1:nval){
    U <- runif(1)
    res[i] <- lambda * (-log(1 - U))^(1/beta)
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
  cat("• El costo promedio de un siniestro es de", round(mean(x), 2), ".\n\n")
  cat("• Un costo típico del siniestro es cercano a", round(median(x), 2), ".\n\n")
  cat("• En su mayoría el costo del siniestro se encuentra entre",
      round(quantile(x, 0.1), 2), "y", round(quantile(x, 0.9), 2), ".\n\n")
  cat("• En casos extremos, el costo del siniestro puede alcanzar valores cercanos a",
      round(max(x), 2), ".\n")
}

function(input, output, session) {
  #Frecuencia
  frecuencia <- eventReactive(input$sim_freq, {
    switch(input$freq_dist,
          "Poisson" =x_poisson(input$lambda_freq, input$n_freq_sim),
          "Binomial" = x_binomial(input$n_bin, input$p_bin, input$n_freq_sim),
          "Binomial Negativa" = x_binom_negativa(input$r_nb, input$p_nb, input$n_freq_sim))
  })
  
  output$freq_plot <- renderPlot({
    hist(frecuencia(),
         col = "#23C23C",
         border = "black",
         main = "Distribución del Número de Siniestros",
         xlab = "Número de siniestros")
  })
  
  output$freq_summary <- renderPrint({
    freq(frecuencia())
  })
  #Severidad
  severidad <- eventReactive(input$sim_sev, {
    switch(input$sev_dist,
          "Exponencial" = x_exponencial(input$lambda_exp, input$n_sev_sim),
          "Gamma" = x_gamma(input$alpha_g, input$lambda_g, input$n_sev_sim),
          "Log-Normal" = x_lognormal(input$mu_ln, input$sigma_ln, input$n_sev_sim),
          "Weibull" = x_weibull(input$beta_w, input$lambda_w, input$n_sev_sim))
  })
  
  output$sev_plot <- renderPlot({
    hist(severidad(),
         col = "#26A4C7",
         border = "black",
         main = "Distribución del Costo del Siniestro",
         xlab = "Costo del siniestro")
  })
  
  output$sev_summary <- renderPrint({
    sev(severidad())
  })
  # Pérdida Agregada
  perdida_agregada <- eventReactive(input$sim_total, {
    
    M <- input$n_sim_total
    S <- numeric(M)
    
    for (k in 1:M) {
      
      Nk <- switch(input$freq_dist,
                   "Poisson" = x_poisson(input$lambda_freq, 1),
                   "Binomial" = x_binomial(input$n_bin, input$p_bin, 1),
                   "Binomial Negativa" = x_binom_negativa(input$r_nb, input$p_nb, 1))
      
      if (Nk == 0) {
        S[k] <- 0
      } else {
        Xk <- switch(input$sev_dist,
                     "Exponencial" = x_exponencial(input$lambda_exp, Nk),
                     "Gamma" = x_gamma(input$alpha_g, input$lambda_g, Nk),
                     "Log-Normal" = x_lognormal(input$mu_ln, input$sigma_ln, Nk),
                     "Weibull" = x_weibull(input$beta_w, input$lambda_w, Nk))
        S[k] <- sum(Xk)
      }
    }
    return(S)
  })
  
  output$agg_plot <- renderPlot({
    hist(perdida_agregada(), col = "#9C1600", border = "black",
         main = "Distribución de la Pérdida Agregada",
         xlab = "Costo total")
  })
  
  output$agg_summary <- renderPrint({
    S <- perdida_agregada()
    cat("• El Costo Total de siniestros es de:", round(mean(S), 2), "\n\n")
    cat("• Un costo típico del siniestro es cercano a:", round(median(S), 2), "\n\n")
    cat("• En su mayoría el costo del siniestro se encuentra entre",
        round(quantile(S, 0.1), 2), "y",round(quantile(S, 0.9), 2), "\n")
    cat("• En escenarios poco frecuentes, el Costo total de siniestros puede llegar hasta:",
        max(S), ".\n")
  })
}


