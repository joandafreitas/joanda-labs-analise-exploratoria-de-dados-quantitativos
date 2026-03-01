library(dplyr)
library(ggplot2)
library(scales)

# Somar o total de candidaturas masculinas e femininas na base inteira
totais <- database_candidatos_candidatas %>%
  summarise(
    total_masc = sum(cand_masc, na.rm = TRUE),
    total_fem = sum(cand_fem, na.rm = TRUE)
  )

# Criar data frame para gráfico com percentual
dados <- data.frame(
  genero = factor(c("homens", "mulheres"), levels = c("homens", "mulheres")),
  quantidade = c(totais$total_masc, totais$total_fem)
) %>%
  mutate(
    percentual = quantidade / sum(quantidade)
  )

# Criar título da legenda com o total geral
titulo_legenda <- paste0(
  "Gênero\n(Total: ", format(sum(dados$quantidade), big.mark = ".", decimal.mark = ","), ")"
)

# Plotar gráfico
ggplot(dados, aes(x = genero, y = quantidade, fill = genero)) +
  geom_bar(stat = "identity", width = 0.45) +
  geom_text(
    aes(
      label = paste0(
        format(quantidade, big.mark = ".", decimal.mark = ","),
        " (",
        percent(percentual, accuracy = 0.1, decimal.mark = ","),
        ")"
      )
    ),
    vjust = -0.5,  # coloca acima da barra
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("homens" = "grey40", "mulheres" = "grey60"),
    labels = c("Homens", "Mulheres"),
    name = titulo_legenda
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "",
    x = "Gênero da candidatura",
    y = "Número de candidaturas"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.3),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
