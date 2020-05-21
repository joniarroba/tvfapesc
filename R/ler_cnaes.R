ler_cnaes <- function(){
  d <- read_delim("extdata/cnae/cnaes_fecha_abre.csv",
                      ";", escape_double = FALSE, trim_ws = TRUE)
  d <- clean_names(d)
  d <- d %>% rename(cnae_2_0_classe = codigo_cnae)
  d <- d %>% mutate(cnae_2_0_classe = substr(x = cnae_2_0_classe, start = 1, stop = 5))

  d <- d %>% distinct(cnae_2_0_classe, .keep_all = TRUE)


  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)

  # Decreto 1
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(ate_29_03 = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_ate_29_03 = peso_impacto_status)

  # Decreto 2

  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(x30_03_a_01_04  = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_x30_03_a_01_04 = peso_impacto_status)

  # Decreto 3

  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(x02_04_a_05_04  = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_x02_04_a_05_04 = peso_impacto_status)

  # Decreto 4

  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(x06_04_a_07_04   = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_x06_04_a_07_04  = peso_impacto_status)


  # Decreto 5

  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(x08_04_a_12_04   = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_x08_04_a_12_04  = peso_impacto_status)


  # Decreto 6

  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(x13_04_a_20_04 = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_x13_04_a_20_04 = peso_impacto_status)


  # Decreto 7

  pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                     grouping_mark = "."), trim_ws = TRUE)
  pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(x21_04_a = status_atividade)

  d <- d %>% left_join(pesos_impacto_atividades)
  d <- d %>% rename(peso_x21_04_a = peso_impacto_status)


  d <- d %>% rowwise() %>%
    mutate(peso_impacto_status = mean(c(peso_ate_29_03,
                                        peso_x30_03_a_01_04,
                                        peso_x02_04_a_05_04,
                                        peso_x06_04_a_07_04,
                                        peso_x08_04_a_12_04,
                                        peso_x13_04_a_20_04,
                                        peso_x21_04_a),  na.rm = T))


  #gera a media dos pesos de todas as semanas

  #pesos_impacto_atividades$peso_impacto_status <- 0


  return(d)

}


