library(rio)
library(tidyverse)
library(inspectdf)
library(janitor)


#Leitura do DF
df <- ler_rais()

#Leitura dos muncípios
mu <- ler_municipios()

# CNAES -------------------------------------------------------------------

cnaes <- ler_cnaes()


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
            financeiro_impactado =  n_individos_afetados * media_salarios * media_impacto
            #percentual_impacto = financeiro_impactado / soma_salarios * 100
            )

export(result, file = "extdata/restultados_gutai/impactoMuniciopios_primeira_semana.xlsx")

#Caso quiser saber qual foi o impacto por setor
#%>%
#spread(key = ate_29_03, value = n_ind_por_media_salario_peso_impacto, fill = 0) %>%
#ungroup()


# Impacto percentual por macro região -------------------------------------

macro_regiao <- result %>%
  group_by(macrorregiao) %>%
  summarise(n_municipios= n(),
            impactoMacroRegional = mean(media_impacto) *100)

export(macro_regiao, file = "extdata/restultados_gutai/macroregiões.xlsx")





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






