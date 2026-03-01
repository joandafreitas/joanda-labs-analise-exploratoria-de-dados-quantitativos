# Pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1. Calcular taxa de sucesso (eleitas / candidatas)
dados_resumo <- Tabela_cand_e_eleitas_por_partido %>%
  mutate(taxa_sucesso = ifelse(`qtd de cand` > 0, `qtd de eleitas` / `qtd de cand`, NA)) %>%
  arrange(desc(taxa_sucesso)) %>%
  select(partido, região, `qtd de cand`, `qtd de eleitas`, taxa_sucesso)

# 4. Gráfico 2: Taxa de sucesso eleitoral feminina por partido
dados_validos <- dados_resumo %>%
  group_by(partido) %>%
summarise(taxa_sucesso = sum(`qtd de eleitas`) / sum(`qtd de cand`)) %>%
  filter(!is.na(taxa_sucesso))

ggplot(dados_validos, aes(x = reorder(partido, taxa_sucesso), y = taxa_sucesso)) +
  geom_col(fill = "gray40", width = 0.9) +
  geom_text(aes(label = ifelse(taxa_sucesso > 0, percent(taxa_sucesso, accuracy = 0.2), "0%")),
            hjust = -0.3, size = 3) +  # posicionado fora da barra à direita
  coord_flip() +
  labs(title = "",
       x = "Partidos Políticos",
       y = "Taxa de Sucesso (%)",
       caption = "") +
  theme_minimal(base_family = "sans") +
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.caption = element_text(hjust = 1, face = "italic", size = 9)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 5), expand = expansion(mult = c(0, 0.3)))