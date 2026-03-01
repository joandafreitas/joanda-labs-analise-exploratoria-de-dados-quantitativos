# Carregar pacotes
library(dplyr)
library(writexl)
library(ggplot2)
library(forcats)

# 1. Agrupar por região e reeleição
reeleicao_por_regiao <- database_candidatas %>%
  group_by(reg, reel) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(reg, desc(n)) %>%
  mutate(reg = factor(reg, levels = c("agreste", "borborema", "mata", "sertão")))

# 2. Exportar para Excel (opcional)
write_xlsx(
  list(ReeleicaoPorRegiao = reeleicao_por_regiao),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\reeleicao_regiao.xlsx"
)

# 3. Gráfico de barras horizontais com números
ggplot(reeleicao_por_regiao, aes(x = reg, y = n, fill = reel)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.8),
    hjust = -0.2,
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "",
    x = "Região",
    y = "Número de candidatas",
    fill = "Reeleição"
  ) +
  scale_fill_grey(start = 0.6, end = 0.3) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  ) +
  expand_limits(y = max(reeleicao_por_regiao$n) * 1.15)
