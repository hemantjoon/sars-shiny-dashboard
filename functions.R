
format_bignum = function(n){
  case_when(
    n >= 1e12 ~ paste(round(n/1e12, digits = 2), 'Tn'),
    n >= 1e9 ~ paste(round(n/1e9, digits = 2), 'Bn'),
    n >= 1e6 ~ paste(round(n/1e6, digits = 2), 'M'),
    n >= 1e3 ~ paste(round(n/1e3, digits = 2), 'K'),
    TRUE ~ as.character(n))
}