get_aging_error_matrix = function(obs_age, sd) {
  
  out_matrix = matrix(NA, ncol = length(obs_age), nrow = length(obs_age))
  
  for(i in seq_along(obs_age)) {
    
    for(j in seq_along(obs_age)) {
     
      # if(i > 1) {
      #   if((j == 1) | (j == length(obs_age))) {
      #     out_matrix[j,i] = 1 - pnorm(q = (j-obs_age[i])/sd[i])
      #   }
      #   if((j > 1) & (j < length(obs_age))) {
      #     out_matrix[j,i] = pnorm(q = (j+1-obs_age[i])/sd[i]) - pnorm(q = (j-obs_age[i])/sd[i])
      #   }
      #} else {
        if(j == length(obs_age)) {
          out_matrix[j,i] = 1 - pnorm(q = (j-obs_age[i])/sd[i])
        } else {
          out_matrix[j,i] = pnorm(q = (j+1-obs_age[i])/sd[i]) - pnorm(q = (j-obs_age[i])/sd[i])
        }
      #}
      
    }
    
  }
  
  return(out_matrix)
  
}


