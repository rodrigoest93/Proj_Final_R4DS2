#### pacotes utilizados ####
library(dplyr)
library(forcats)
library(purr)
library(tidyr)
library(ggplot2)
library(stringr)

#### importando base de dados ####
base_credito <- readRDS("data/credito.rds")

#### estrutura das variáveis ####
str(base_credito)
skimr::skim(base_credito) %>%  View()


#### tratamento de missing ####

# renda
base_credito %>%
  filter(!is.na(renda)) %>%
    select(renda) %>%
      arrange(renda) %>%
        View()

# ativos
base_credito %>%
  filter(!is.na(ativos)) %>%
    select(ativos) %>%
      arrange(ativos) %>%
        View()

base_credito %>%
  filter(is.na(ativos)) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = ~mean(.x, na.rm = TRUE)
    )
  )

base_credito %>%
  filter(ativos == 0 | is.na(ativos)) %>%
    select(dividas) %>%
      View()



# dividas
base_credito %>%
  filter(!is.na(dividas)) %>%
    select(dividas) %>%
      arrange(dividas) %>%
      View()

base_credito %>%
  filter(is.na(dividas)) %>%
    # select(ativos) %>%
      count(status)



# suspeitas e afirmacoes:
# 1) quem não tem renda = NA, pode transformar pra 0
# 2) ativos (NA) <> ativos (0)
# 3) dividas (NA) <> dividas (0)

# 4) todos os ativos (NA ou 0) tem divida (0 ou NA)
# 5) todas as dividas (NA) tem ativos (NA)

# 6) quem tem ativos (0 ou NA) não tem dividas

# 7) pode ter uma ação de falta de informacão pra quem tem ativo, dividas e renda (NA)
# 8) dividas (NA) pode seguir a mediana ou media das dividas por serem em sua satatus ruim (no modelo)


#### transformacao ####
base_credito <- base_credito %>%
  mutate(
    renda = str_replace_na(renda, replacement = 0)
  )

#### verificar correlacao das variaveis numericas ####
base_credito %>%
  select(where(is.numeric)) %>%
    cor(use = "pairwise.complete.obs") %>%
      ggcorrplot::ggcorrplot(
       method = "circle",
       type = "lower",
       colors = c("tomato","white","green"),
       lab = TRUE,
       lab_size = 2.5
      )

#### verificar outras variaveis ####
base_credito %>%
  group_by(status) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = mean, na.rm = TRUE
      )
    ) %>%
        pivot_longer(
          cols = everything(),
          names_to = "Variáveis",
          values_to = ""
        )  %>%
        View()

