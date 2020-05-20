library(rio)
library(tidyverse)
library(inspectdf)
library(janitor)

#2

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

# t <- df %>% select(vl_salario_contratual,vl_remun_media_nom, vl_remun_media_sm, vl_rem_janeiro_cc,
#                    vl_rem_fevereiro_cc, vl_rem_marco_cc, vl_rem_abril_cc, vl_rem_maio_cc, vl_rem_junho_cc, vl_rem_julho_cc, vl_rem_agosto_cc,
#                    vl_rem_setembro_cc, vl_rem_outubro_cc, vl_rem_novembro_cc, vl_remun_dezembro_nom, motivo_desligamento)


# Organização dos municípios ----------------------------------------------

mu <- import(file = "extdata/TabMunicipios.csv", encoding = "Latin-1")
mu <- clean_names(mu)
mu <- mu %>% mutate(codigo_municipio_completo = as.character(codigo_municipio_completo))
mu <- mu %>% mutate(codigo_municipio_completo = gsub(x = codigo_municipio_completo, pattern = '.{1}$', replacement = ''))
glimpse(mu)

macro_d_para <- read_csv("extdata/macro_regioes/Macroreg-DPARA.csv")
macro_d_para <- macro_d_para %>% select(-municipio)
glimpse(macro_d_para)
macro_d_para <- macro_d_para %>% rename(codigo_municipio_completo = codigo)
macro_d_para <- macro_d_para %>% mutate(codigo_municipio_completo = as.character(codigo_municipio_completo))
glimpse(macro_d_para)
#macro_d_para <- macro_d_para %>% mutate(codigo_municipio_completo = gsub(x = codigo_municipio_completo, pattern = '.{1}$', replacement = ''))

mu <- mu %>% left_join(macro_d_para, by = "codigo_municipio_completo") %>% filter(uf == 42)
glimpse(mu)

# CNAES -------------------------------------------------------------------

cnaes <- read_delim("extdata/cnae/cnaes_fecha_abre.csv",
                               ";", escape_double = FALSE, trim_ws = TRUE)
cnaes <- clean_names(cnaes)
names(cnaes)
cnaes <- cnaes %>% rename(cnae_2_0_classe = codigo_cnae)
cnaes <- cnaes %>% mutate(cnae_2_0_classe = substr(x = cnae_2_0_classe, start = 1, stop = 5))

cnaes <- cnaes %>% distinct(cnae_2_0_classe, .keep_all = TRUE)

pesos_impacto_atividades <- read_delim("extdata/cnae/pesos_impacto_atividades.csv",
                                       ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                   grouping_mark = "."), trim_ws = TRUE)

pesos_impacto_atividades <- pesos_impacto_atividades %>% rename(ate_29_03 = status_atividade)

#pesos_impacto_atividades$peso_impacto_status <- 1

cnaes <- cnaes %>% left_join(pesos_impacto_atividades)
glimpse(cnaes)



# Integrando as bases -----------------------------------------------------

combinado <- df %>% left_join(mu)
glimpse(combinado)

combinado <- combinado %>% left_join(cnaes)
glimpse(combinado)

#retirada de NAs

combinado$peso_impacto_status <- combinado$peso_impacto_status %>% replace_na(replace = mean(combinado$peso_impacto_status, na.rm = T))

glimpse(combinado)


# Número de atingidos até 29 --------------------------------------------------------------

#Impacto financeiro percentual por município
result <- combinado %>%
  group_by(municipio) %>%
  summarise(nome_municipio = last(nome_municipio),
            macrorregiao = last(macrorregiao),
            n_individos_afetados = n(),
            media_salarios = mean(vl_salario_contratual),
            soma_salarios = sum(vl_salario_contratual),
            media_impacto = mean(peso_impacto_status),
            financeiro_impactado =  n_individos_afetados * media_salarios * media_impacto,
            percentual_impacto = financeiro_impactado / soma_salarios * 100
            )

#Caso quiser saber qual foi o impacto por setor
#%>%
#spread(key = ate_29_03, value = n_ind_por_media_salario_peso_impacto, fill = 0) %>%
#ungroup()


# Impacto percentual por macro região -------------------------------------

macro_regiao <- result %>%
  group_by(macrorregiao) %>%
  summarise(n_municipios= n(),
            impacto_financeiro_social = mean(percentual_impacto))





#Financeiro de cada municipio
total_fin_mun <- combinado %>%
  group_by(municipio) %>%
  summarise(total_bruto = sum(vl_salario_contratual))


#total financeiro com os pesos de cada município
result2 <- result %>%
  group_by(municipio) %>%
    summarise(nome_municipio = last(nome_municipio),
              macrorregiao = last(macrorregiao),
    across(`Autorizada, com restrição a serviços presenciais.` : `<NA>` , .fns = sum),
    total_afetado = sum(`Autorizada, com restrição a serviços presenciais.`)) %>%
    left_join(total_fin_mun) %>%
    mutate(impacto_fin =  total_afetado / total_bruto * 100)


#export(result2, file = "extdata/macro_regioes/impacto_por_cidades.csv")

#table(is.na(result2$nome_municipio))

#
# # Grupo de risco  - desenvolvendo
#
# produto <- combinado %>%
#   group_by(municipio, cnae_2_0_classe) %>%
#   summarise(m_idade = round(mean(idade), digits = 0),
#             n_individuos = n(),
#             n_individuos_g_risco_cnae = sum(g_risco),
#             percentual_g_risco_cnae = mean(g_risco)*100,
#             qtd_hora_contr = mean(qtd_hora_contr),
#             tempo_emprego = mean(tempo_emprego)/12,
#             media_vl_salario_contratual = median(vl_salario_contratual))
#
#
# percentual_g_risco_mun =  n_individuos_g_risco_cnae / n_total_g_risco_no_mun * 100)
#
#
# nome_municipio, descricao_do_cnae
# %>%
#   summarise(m_idade = round(mean(idade), digits = 0),
#             n_individuos = n(),
#             n_individuos_g_risco_cnae = sum(g_risco),
#             percentual_g_risco_cnae = mean(g_risco)*100,
#             percentual_g_risco_mun =  n_individuos_g_risco_cnae / n_total_g_risco_no_mun * 100,
#             qtd_hora_contr = mean(qtd_hora_contr),
#             tempo_emprego = mean(tempo_emprego)/12,
#             media_vl_salario_contratual = median(vl_salario_contratual)) %>%
#   ungroup() %>%
#   group_by(municipio)
#
#
#






