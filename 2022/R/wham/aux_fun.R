post_input = function(input, base_input) {


  # NAA information
  input$par$log_NAA = as.matrix(base_input$par$log_NAA)
  input$map$log_N1_pars = base_input$map$log_N1_pars
  input$map$log_NAA_sigma = base_input$map$log_NAA_sigma
  # F information
  input$par$F_devs = base_input$par$F_devs
  input$par$log_F1 = base_input$par$log_F1
  # Q information
  input$par$logit_q = base_input$par$logit_q
  input$par$q_re = base_input$par$q_re
  input$map$q_repars = base_input$map$q_repars
  input$par$q_repars = base_input$par$q_repars
  input$data$use_q_prior = base_input$data$use_q_prior
  input$data$logit_q_prior_sigma = base_input$data$logit_q_prior_sigma
  input$par$q_prior_re = base_input$par$q_prior_re
  # data agg index sigma
  input$data$agg_index_sigma = base_input$data$agg_index_sigma
  # Ecov
  input$par$Ecov_re = base_input$par$Ecov_re
  # Selectivity
  input$data$selpars_lower[,13:16] = base_input$data$selpars_lower[,13:16]
  input$data$selpars_upper[,13:16] = base_input$data$selpars_upper[,13:16]
  input$par$logit_selpars[,1:16] = base_input$par$logit_selpars
  input$par$selpars_re[1:104] = base_input$par$selpars_re[1:104]
  input$map$selpars_re = factor(c(1:104, rep(NA, 104)))
  input$map$sel_repars = base_input$map$sel_repars

  return(input)
}