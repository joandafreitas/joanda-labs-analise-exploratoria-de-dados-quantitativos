library(dplyr)
library(ggplot2)
library(stringr)

# 1. Filtrar e calcular a razão por município — somente Borborema
razao_borborema <- database_candidatos_candidatas %>%
  filter(str_to_lower(reg) == "borborema") %>%
  group_by(municipio) %>%
  summarise(
    total_cand_fem = sum(cand_fem, na.rm = TRUE),
    total_vagas = sum(t_eleitos_e_eleitas, na.rm = TRUE),
    candidata_vaga = round(total_cand_fem / total_vagas, 2),
    .groups = "drop"
  ) %>%
  mutate(municipio = str_to_title(municipio))

# 2. Selecionar os 10 menores e 10 maiores municípios com grupo
menores <- razao_borborema %>%
  arrange(candidata_vaga) %>%
  slice_head(n = 10) %>%
  mutate(grupo = "Menores razões")

maiores <- razao_borborema %>%
  arrange(desc(candidata_vaga)) %>%
  slice_head(n = 10) %>%
  mutate(grupo = "Maiores razões")

extremos_borborema <- bind_rows(menores, maiores)

# 3. Gráfico de linhas com legenda para Borborema
ggplot(extremos_borborema, aes(x = reorder(municipio, candidata_vaga), y = candidata_vaga, group = grupo, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = candidata_vaga), vjust = -0.8, size = 3.2, color = "black") +
  scale_color_manual(values = c("Menores razões" = "#D95F02", "Maiores razões" = "#1B9E77")) +
  labs(
    title = "",
    x = "Município",
    y = "Candidatas por vaga",
    color = "Grupo"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.title = element_text(face = "bold")
  )
