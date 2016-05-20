create_population <- function() {
  print(">>>> Creating population")
  
  id <- c(1:n_people)
  sex <- c(rep(0, n_people/2), rep(1, n_people/2))
  d_base <- rnorm(n_people/4, 0.0, var_base)
  d_base <- c(d_base, 0-d_base, d_base, 0-d_base)
  return(data.frame(id, sex, d_base))
}