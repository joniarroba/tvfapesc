#' Title
#'
#' @return
#' @export
#'
#' @examples
ler_municipios <- function(){
  mu <- import(file = "extdata/TabMunicipios.csv", encoding = "Latin-1")
  mu <- clean_names(mu)
  mu <- mu %>% mutate(codigo_municipio_completo = as.character(codigo_municipio_completo))
  mu <- mu %>% mutate(codigo_municipio_completo = gsub(x = codigo_municipio_completo, pattern = '.{1}$', replacement = ''))

  macro_d_para <- read_csv("extdata/macro_regioes/Macroreg-DPARA.csv")
  macro_d_para <- macro_d_para %>% select(-municipio)

  macro_d_para <- macro_d_para %>% rename(codigo_municipio_completo = codigo)
  macro_d_para <- macro_d_para %>% mutate(codigo_municipio_completo = as.character(codigo_municipio_completo))

  #macro_d_para <- macro_d_para %>% mutate(codigo_municipio_completo = gsub(x = codigo_municipio_completo, pattern = '.{1}$', replacement = ''))

  mu <- mu %>% left_join(macro_d_para, by = "codigo_municipio_completo") %>% filter(uf == 42)

}
