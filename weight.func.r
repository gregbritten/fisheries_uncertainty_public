weighted.var <- function(x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }

  sum(w * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)
}


weighted.sd <- function(x, w, na.rm = TRUE) sqrt(weighted.var(x, w, na.rm = TRUE))


weighted_mean <- function(x, w, ..., na.rm = FALSE){
  
  if(na.rm){
    
    df_omit <- na.omit(data.frame(x, w))
    
    return(weighted.mean(df_omit$x, df_omit$w, ...))
    
  } 
  
  weighted.mean(x, w, ...)
}