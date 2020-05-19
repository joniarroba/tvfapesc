library(rio)
library(tidyverse)
library(inspectdf)
library(janitor)
library(sparklyr)


# Leitura da RAIS
df <- import(file = "extdata/RAIS_SC_18.csv",  encoding = "Latin-1")
df <- clean_names(df)
df <- df %>% mutate(municipio = as.character(municipio)) %>% rename(codigo_municipio_completo = municipio)
df <- df %>% mutate_all(as.character) 
#mantendo somente funcionarios ativos
df <- df %>% filter(motivo_desligamento == 0 )



# Tipos de variaveis ------------------------------------------------------

col_numericas <- c("faixa_etaria",
                   "faixa_hora_contrat",
                   "faixa_remun_dezem_sm", 
                   "faixa_remun_media_sm", 
                   "faixa_tempo_emprego", 
                   "escolaridade_apos_2005",
                   "qtd_hora_contr", 
                   "idade", 
                   "qtd_dias_afastamento",
                   "vl_remun_dezembro_nom",
                   "vl_remun_dezembro_sm",
                   "vl_remun_media_nom",
                   "vl_remun_media_sm",
                   "tempo_emprego", 
                   "vl_rem_janeiro_cc" ,    
                   "vl_rem_fevereiro_cc",
                   "vl_rem_marco_cc",
                   "vl_rem_abril_cc" ,  
                   "vl_rem_maio_cc" ,  
                   "vl_rem_junho_cc" ,  
                   "vl_rem_julho_cc",       
                   "vl_rem_agosto_cc",       
                   "vl_rem_setembro_cc",     
                   "vl_rem_outubro_cc",     
                   "vl_rem_novembro_cc"  
)

cal_fatores <- c("causa_afastamento_1", 
                 "causa_afastamento_2", 
                 "causa_afastamento_3", 
                 "motivo_desligamento", 
                 "cbo_ocupacao_2002", 
                 "cnae_2_0_classe", 
                 "cnae_95_classe",
                 "vinculo_ativo_31_12", 
                 "ind_cei_vinculado", 
                 "ind_simples", 
                 "mes_admissao", 
                 "mes_desligamento", 
                 "nacionalidade", 
                 "natureza_juridica", 
                 "ind_portador_defic", 
                 "raca_cor", 
                 "regioes_adm_df", 
                 "cnae_2_0_subclasse", 
                 "sexo_trabalhador", 
                 "tamanho_estabelecimento", 
                 "tipo_admissao", 
                 "tipo_estab", 
                 "tipo_vinculo", 
                 "ibge_subsetor"
)










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



# CNAES -------------------------------------------------------------------

cnaes <- read_delim("extdata/cnae/cnaes_fecha_abre.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
cnaes <- clean_names(cnaes) 
names(cnaes)
cnaes <- cnaes %>% rename(cnae_2_0_classe = codigo_cnae)
cnaes <- cnaes %>% mutate(cnae_2_0_classe = substr(x = cnae_2_0_classe, start = 1, stop = 5))

cnaes <- cnaes %>% distinct(cnae_2_0_classe, .keep_all = TRUE) 




# Integrando as bases -----------------------------------------------------

combinado <- df %>% left_join(mu)
combinado <- combinado %>% left_join(cnaes)


# Produto -----------------------------------------------------------------

percentual_g_risco_atividade <- combinado %>% 
  group_by(municipio) %>% 
  summarise(n_total_g_risco_no_mun = sum(g_risco), 
            n_individuos_municipio = n(), 
            percentual_g_risco_municipio = mean(g_risco)*100)
  
percentual_ativ_afetadas <- combinado %>% 
  group_by(ate_29_03) %>% 
  summarise(n_individos_afetados = n(), 
            soma_salarios = sum(vl_salario_contratual),
            media_salarios = mean(vl_salario_contratual))

export(percentual_ativ_afetadas, file = "extdata/Rais/pesos_atividades.csv")

  
  tally() 

  


produto <- combinado %>% 
  group_by(, cnae_2_0_classe) %>% 
  summarise(m_idade = round(mean(idade), digits = 0), 
            n_individuos = n(),
            n_individuos_g_risco_cnae = sum(g_risco),
            percentual_g_risco_cnae = mean(g_risco)*100,  
            qtd_hora_contr = mean(qtd_hora_contr),
            tempo_emprego = mean(tempo_emprego)/12, 
            media_vl_salario_contratual = median(vl_salario_contratual))


percentual_g_risco_mun =  n_individuos_g_risco_cnae / n_total_g_risco_no_mun * 100)


nome_municipio, descricao_do_cnae
%>% 
  summarise(m_idade = round(mean(idade), digits = 0), 
            n_individuos = n(),
            n_individuos_g_risco_cnae = sum(g_risco),
            percentual_g_risco_cnae = mean(g_risco)*100, 
            percentual_g_risco_mun =  n_individuos_g_risco_cnae / n_total_g_risco_no_mun * 100,
            qtd_hora_contr = mean(qtd_hora_contr),
            tempo_emprego = mean(tempo_emprego)/12, 
            media_vl_salario_contratual = median(vl_salario_contratual)) %>% 
  ungroup() %>% 
  group_by(municipio) 
  




# mercado casas -----------------------------------------------------------

mercado <- combinado %>% filter(vl_salario_contratual < 7000) %>%
  filter(microrregiao_geografica == 16) %>% 
  group_by(nome_municipio) %>% 
  summarise(m_idade = round(mean(idade), digits = 0), 
            contagem = n(),
            percentual_g_risco = mean(g_risco), 
            qtd_hora_contr = mean(qtd_hora_contr),
            tempo_emprego = mean(tempo_emprego)/12, 
            media_vl_salario_contratual = mean(vl_salario_contratual))

export(mercado,file = "mercado_abaixo_7mil_mircroregiao_fpolis.xlsx")

glimpse(combinado)

t <- inspect_cat(combinado)

summary(combinado)

cardiologia <- combinado %>% filter(cbo_ocupacao_2002 == 225120) %>% 
  group_by(nome_municipio) %>% 
  summarise(m_idade = round(mean(idade), digits = 0), 
            contagem = n(),
            percentual_g_risco = mean(g_risco), 
            qtd_hora_contr = mean(qtd_hora_contr),
            tempo_emprego = mean(tempo_emprego)/12, 
            media_vl_salario_contratual = mean(vl_salario_contratual))



