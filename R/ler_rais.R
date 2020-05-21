#' Leitura e limpeza da rais
#'
#' @return
#' @export
#'
#' @examples
ler_rais <- function() {
  # Leitura da RAIS
  df <- import(file = "extdata/RAIS_SC_18.csv",  encoding = "Latin-1")
  df <- clean_names(df)
  df <- df %>% mutate(municipio = as.character(municipio)) %>% rename(codigo_municipio_completo = municipio)
  df <- df %>% mutate_all(as.character)
  #mantendo somente funcionarios ativos
  df <- df %>% filter(motivo_desligamento == 0 )


  # Flags -------------------------------------------------------------------

  df <- df %>% mutate(g_risco = if_else(idade > 60, 1, 0))

  # Replacements ------------------------------------------------------------


  df <- df %>% mutate(tempo_emprego = str_replace(tempo_emprego, pattern = ",", "."))
  df <- df %>% mutate(tempo_emprego = as.numeric(tempo_emprego))

  df <- df %>% mutate(vl_salario_contratual = str_replace(vl_salario_contratual, ",", "."))
  df <- df %>% mutate(vl_salario_contratual = as.numeric(vl_salario_contratual))

  df <- df %>% mutate(vl_remun_media_nom = str_replace(vl_remun_media_nom, ",", "."))
  df <- df %>% mutate(vl_remun_media_nom = as.numeric(vl_remun_media_nom))

  df <- df %>% mutate(vl_remun_media_sm = str_replace(vl_remun_media_sm, ",", "."))
  df <- df %>% mutate(vl_remun_media_sm = as.numeric(vl_remun_media_sm))

  df <- df %>% mutate(idade = as.numeric(idade))
  df <- df %>% mutate(qtd_hora_contr = as.numeric(qtd_hora_contr))

  return(df)

}
