library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library(ggimage)
library(ggrepel)



# Lê o arquivo Excel
dados <- read_excel("MBDAF_ed7_M4_Atividade_Individual.xlsx")

#Obter dimensões do campo
max_pos_x <- max(dados$pos_x, na.rm = TRUE)
max_pos_y <- max(dados$pos_y, na.rm = TRUE)

print(max_pos_x)
print(max_pos_y)

# Converter as colunas x e y para numéricas
dados <- dados %>%
  mutate(
    pos_x = as.numeric(pos_x),
    pos_y = as.numeric(pos_y)
  )


#criar campo com dimensões adequadas aos dados que temos
pitch_custom <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.3,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

########## Exercício 1 #########

# Filtrar os dados do Paredes
dados_paredes <- dados %>% filter(code == "8. Paredes")

# Ações defensivas de interesse
acoes_desejadas <- c(
  "Interceptions",
  "Challenges (won)",
  "Tackles (Successful actions)",
  "Picking-ups",
  "Challenges (lost)",
  "Tackles (Unsuccessful actions)",
  "Fouls"
)

# Filtrar apenas ações defensivas
dados_defensivos_paredes <- dados_paredes %>% filter(Action %in% acoes_desejadas)

# Criar uma nova coluna para classificar as ações
dados_defensivos_paredes <- dados_defensivos_paredes %>%
  mutate(
    Categoria = case_when(
      Action == "Fouls" ~ "Falta",
      Action == "Interceptions" ~ "Intercepção",
      Action %in% c("Challenges (won)", "Tackles (Successful actions)", "Picking-ups") ~ "Ação Defensiva Bem-sucedida",
      Action %in% c("Challenges (lost)", "Tackles (Unsuccessful actions)") ~ "Ação Defensiva Mal-sucedida"
    )
  )

# Calcular valores totais e percentagens
total_pickups <- sum(dados_defensivos_paredes$Action == "Picking-ups", na.rm = TRUE)
tackles_ganhos <- sum(dados_defensivos_paredes$Action == "Tackles (Successful actions)", na.rm = TRUE)
tackles_totais <- tackles_ganhos + sum(dados_defensivos_paredes$Action == "Tackles (Unsuccessful actions)", na.rm = TRUE)
challenges_ganhos <- sum(dados_defensivos_paredes$Action == "Challenges (won)", na.rm = TRUE)
challenges_totais <- challenges_ganhos + sum(dados_defensivos_paredes$Action == "Challenges (lost)", na.rm = TRUE)

percent_tackles <- ifelse(tackles_totais > 0, round((tackles_ganhos / tackles_totais) * 100, 2), 0)
percent_challenges <- ifelse(challenges_totais > 0, round((challenges_ganhos / challenges_totais) * 100, 2), 0)

# Calcular a convex hull das ações bem-sucedidas
bem_sucedidas <- dados_defensivos_paredes %>%
  filter(Categoria == "Ação Defensiva Bem-sucedida")
hull_indices <- chull(bem_sucedidas$pos_x, bem_sucedidas$pos_y)
hull_points <- bem_sucedidas[hull_indices, ]

# Criar o campo de futebol com convex hull 
ggplot() +
  annotate_pitch(
    dimensions = pitch_custom,
    colour = "#021e40", linewidth = 0.8, limits = FALSE,fill = "lightblue") + 
  theme_pitch() +
  # Adicionar convex hull 
  geom_polygon(
    data = hull_points,
    aes(x = pos_x, y = pos_y),
    fill = "#021e40", alpha = 0.2, colour = "#021e40", linetype = "dashed"
  ) +
  geom_point(
    data = dados_defensivos_paredes,
    aes(x = pos_x, y = pos_y, color = Categoria),
    size = 3, alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Falta" = "yellow",
      "Intercepção" = "#021e40",
      "Ação Defensiva Bem-sucedida" = "darkgreen",
      "Ação Defensiva Mal-sucedida" = "red"
    ),
    name = " "
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  # Adicionar a legenda personalizada abaixo do campo
  annotate(
    "text",
    x = 50, y = -20,
    label = paste0(
      "Pick-ups: ", total_pickups, " | ",
      "Desarmes bem-sucedidos: ", tackles_ganhos, "/", tackles_totais, 
      " (", percent_tackles, "%) | ",
      "Duelos Ganhos: ", challenges_ganhos, "/", challenges_totais, 
      " (", percent_challenges, "%)"
    ),
    hjust = 0.5, size = 3.9, color = "#021e40"
  ) +
  labs(
    title = "Leandro Paredes: Território Defensivo",
    subtitle = "Ações defensivas jogo Man City VS PSG",
    x = NULL,
    y = NULL
  ) + 
  theme(
    panel.grid = element_blank(),  # Remover linhas do painel
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#021e40"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#021e40"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "lightblue", color = NA),  # Fundo do gráfico
    panel.background = element_rect(fill = "lightblue", color = NA), # Fundo do painel
    legend.background = element_rect(fill = "lightblue", color = NA), # Fundo da legenda
    legend.text = element_text(color = "#021e40")  # Texto da legenda branco
  )



########## Exercício 2 #########

# Filtrar os dados do Rodri
dados_rodri <- dados %>% filter(code == "16. Rodri")

# ---- MAPA DE CALOR ---- #
ggplot() +
  # Heatmap
  stat_density_2d(
    data = dados_rodri,
    aes(x = pos_x, y = pos_y, fill = ..density..),
    geom = "raster", contour = FALSE, alpha = 0.8
  ) +
  #escala de cores
  scale_fill_gradientn(
    colours = c("lightblue", "green", "yellow", "orange", "red", "darkred"),
    guide = "none"
  ) +
  annotate_pitch(
    dimensions = pitch_custom,
    colour = "#021e40", linewidth = 0.8, fill = NA, limits = FALSE) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  # Pontos das ações do Rodri
  geom_point(
    data = dados_rodri,
    aes(x = pos_x, y = pos_y),
    size = 2, alpha = 0.7, colour = "#021e40"
  ) +
  labs(
    title = "Rodri: Mapa de Calor",
    subtitle = "Zonas de ação no jogo Man City VS PSG",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() + 
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#021e40"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#021e40"),
    plot.background = element_rect(fill = "lightblue", color = NA),  # Fundo azul escuro
    panel.background = element_rect(fill = "lightblue", color = NA),

  )

########## Exercício 3 #########

# Filtrar os dados do Messi e Sterling
dados_messi <- dados %>% filter(code == "30. Messi")

dados_sterling <- dados %>% filter(code == "7. Sterling")

# Filtrar dados de drible de Messi e Sterling
dados_messi_dribbles <- dados_messi %>%
  filter(Action %in% c("Dribbles (Successful actions)", "Dribbles (Unsuccessful actions)"))

dados_sterling_dribbles <- dados_sterling %>%
  filter(Action %in% c("Dribbles (Successful actions)", "Dribbles (Unsuccessful actions)"))

#Calcular valores totais e percentagens
messi_total <- nrow(dados_messi_dribbles)
messi_sucess <- nrow(dados_messi_dribbles %>% filter(Action == "Dribbles (Successful actions)"))
messi_percent <- round((messi_sucess / messi_total) * 100, 2)

sterling_total <- nrow(dados_sterling_dribbles)
sterling_sucess <- nrow(dados_sterling_dribbles %>% filter(Action == "Dribbles (Successful actions)"))
sterling_percent <- round((sterling_sucess / sterling_total) * 100, 2)

# Adicionar uma coluna para identificar os jogadores
dados_messi_dribbles <- dados_messi_dribbles %>% mutate(Player = "Messi")
dados_sterling_dribbles <- dados_sterling_dribbles %>% mutate(Player = "Sterling")

# Combinar os dados de ambos os jogadores
dados_dribbles <- bind_rows(dados_messi_dribbles, dados_sterling_dribbles)

library(patchwork)  # Para combinar os gráficos

# Criar o gráfico de barras 
grafico_barras <- ggplot(data = data.frame(
  Player = c("Messi", "Sterling"),
  Percent = c(messi_percent, sterling_percent)
), aes(x = Player, y = Percent, fill = Player)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, width = 0.6) +
  scale_fill_manual(values = c("Messi" = "#021e40", "Sterling" = "lightblue")) +
  labs(
    x = NULL,
    y = "% de Dribles bem sucedidos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#021e40"),
    axis.title.y = element_text(color = "#021e40", size = 12),
    axis.text = element_text(color = "#021e40"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# Criar o campo com dribles representados
campo <- ggplot() +
  annotate_pitch(
    dimensions = pitch_custom,
    colour = "#021e40", linewidth = 0.8, fill = "lightblue", limits = FALSE) +
  geom_point(
    data = dados_dribbles,
    aes(x = pos_x, y = pos_y, color = Action, shape = Player),
    size = 3, alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Dribbles (Successful actions)" = "darkgreen",
      "Dribbles (Unsuccessful actions)" = "red"
    ),
    labels = c(
      "Dribbles (Successful actions)" = "Dribles Bem-sucedidos",
      "Dribbles (Unsuccessful actions)" = "Dribles Mal-sucedidos"
    ),
    name = "Ação"
  ) +
  scale_shape_manual(
    values = c("Messi" = 15, "Sterling" = 17),
    name = "Jogador"
  ) +
  # Adicionar a legenda personalizada abaixo do campo
  annotate(
    "text",
    x = 50, y = -20,
    label = paste0(
      "Dribles com sucesso: Messi ", messi_sucess, "/", messi_total, 
      " | Sterling ", sterling_sucess,"/", sterling_total
    ),
    hjust = 0.5, size = 3.9, color = "#021e40"
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  labs(
    #title = "Eficácia de Drible de Messi e Sterling",
    #subtitle = "Comparação de dribles certos e errados no jogo Man City VS PSG",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#021e40"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#021e40"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightblue", color = NA),
    legend.text = element_text(color = "#021e40")
  )

#Combinar os dois gráficos

final_plot <- campo + grafico_barras + 
  plot_layout(widths = c(2, 1)) + 
  plot_annotation(
    title = "Análise de Dribles de Messi e Sterling",
    subtitle = "Comparação de dribles certos e errados no jogo Man City VS PSG",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#021e40"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#021e40"),
      plot.background = element_rect(fill = "lightblue", color = NA),
      panel.background = element_rect(fill = "lightblue", color = NA),
    )
  )

# Exibir o gráfico final
final_plot

###### Exercício 4 #####

#Filtrar os jogadores titulares
jogadores_titulaes <- c(
  "31. Ederson",
  "27. Cancelo",
  "3. R. Dias",
  "5. Stones",
  "2. Walker",
  "11. Zinchenko",
  "16. Rodri",
  "8. Gundogan",
  "7. Sterling",
  "20. B. Silva",
  "26. Mahrez",
  "1. K. Navas",
  "25. N. Tavares",
  "3. Kimpembe",
  "5. Marquinhos",
  "2. Hakimi",
  "27. Gueye",
  "8. Paredes",
  "21. A. Herrera",
  "10. Neymar",
  "7. Mbappe",
  "30. Messi"
  )


dados_titulares <- dados %>% filter(code %in% jogadores_titulaes)


dados_titulares_medio <- dados_titulares %>%
  group_by(Team, code) %>%
  summarise(
    pos_x_medio = mean(pos_x, na.rm = TRUE),
    pos_y_medio = mean(pos_y, na.rm = TRUE)
  )

# Criar as colunas number e name
dados_titulares_medio <- dados_titulares_medio %>%
  mutate(
    number = as.numeric(sub("\\..*", "", code)),  # Extrai os dígitos antes do ponto
    name = sub(".*\\.\\s*", "", code)            # Extrai o texto após o ponto
  )


# Adicionar o Convex Hull por equipa
convex_hull_data <- dados_titulares_medio %>%
  group_by(Team) %>%
  slice(chull(pos_x_medio, pos_y_medio))


cores_equipas <- c("Manchester City" = "#780434", "PSG" = "#1B2838")

team1 = "Manchester City"
team2 = "PSG"

# Gráfico para Manchester City
grafico_team1 <- ggplot() +
  annotate_pitch(dimensions = pitch_custom, colour = "#1B2838", linewidth = 0.8, fill = "lightblue", limits = FALSE) +
  geom_polygon(
    data = convex_hull_data %>% filter(Team == team1),
    aes(x = pos_x_medio, y = pos_y_medio),
    fill = cores_equipas[team1], alpha = 0.2, colour = cores_equipas[team1], linetype = "dashed"
  ) +
  geom_point(
    data = dados_titulares_medio %>% filter(Team == team1),
    aes(x = pos_x_medio, y = pos_y_medio),
    size = 5, colour = cores_equipas[team1]
  ) +
  geom_label_repel(
    data = dados_titulares_medio %>% filter(Team == team1),
    aes(x = pos_x_medio, y = pos_y_medio, label = name),
    fill = "white", color = "black", size = 3, label.size = 0.3,
    nudge_y = 3,  # Pequeno deslocamento para evitar sobreposição
    force = 2,    # Aumenta a força da separação
    max.overlaps = Inf
  ) + annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  labs(title = team1, x = NULL, y = NULL) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA)
  )

# Gráfico PSG
grafico_team2 <- ggplot() +
  annotate_pitch(dimensions = pitch_custom, colour = "#1B2838", linewidth = 0.8, fill = "lightblue", limits = FALSE) +
  geom_polygon(
    data = convex_hull_data %>% filter(Team == team2),
    aes(x = pos_x_medio, y = pos_y_medio),
    fill = cores_equipas[team2], alpha = 0.2, colour = cores_equipas[team2], linetype = "dashed"
  ) +
  geom_point(
    data = dados_titulares_medio %>% filter(Team == team2),
    aes(x = pos_x_medio, y = pos_y_medio),
    size = 5, colour = cores_equipas[team2]
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  geom_label_repel(
    data = dados_titulares_medio %>% filter(Team == team2),
    aes(x = pos_x_medio, y = pos_y_medio, label = name),
    fill = "white", color = "black", size = 3, label.size = 0.3,
    nudge_y = 3, 
    force = 2,
    max.overlaps = Inf
  ) +
  labs(title = team2, x = NULL, y = NULL) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA)
  )

# Combinar os dois gráficos lado a lado
grafico_team1 + grafico_team2 + plot_layout(ncol = 2) +
  plot_annotation(
    title = "Posicionamento Médio dos Jogadores Titulares",
    subtitle = "Ocupação média do espaço das duas equipas",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#1B2838"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1B2838"),
      plot.background = element_rect(fill = "lightblue", color = NA),
      panel.background = element_rect(fill = "lightblue", color = NA)
    )
  )

##### Exercício 5 #####


#Organizar dados dos passes por equipa
dados_passes <- dados %>% filter(Action %in% c("Passes accurate", "Passes (inaccurate)"))

dados_passes_city <- dados_passes %>% filter(Team %in% 'Manchester City')

dados_passes_psg <- dados_passes %>% filter(Team %in% 'PSG')

# Percentagem geral e número total de passes corretos por equipa
team1_stats <- dados_passes_city %>%
  summarise(
    total_passes = n(),
    accurate_passes = sum(Action == "Passes accurate"),
    accuracy = round((accurate_passes / total_passes) * 100, 2)
  )

team2_stats <- dados_passes_psg %>%
  summarise(
    total_passes = n(),
    accurate_passes = sum(Action == "Passes accurate"),
    accuracy = round((accurate_passes / total_passes) * 100, 2)
  )


# Gráfico para Manchester City
grafico_team1 <- ggplot() +
  annotate_pitch(
    dimensions = pitch_custom,
    colour = "#1B2838", linewidth = 0.8, fill = "lightblue", limits = FALSE) +
  geom_point(
    data = dados_passes_city,
    aes(x = pos_x, y = pos_y, colour = Action),
    size = 2, alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Passes accurate" = "darkgreen",
      "Passes (inaccurate)" = "red"
    ),
    name = "Tipo de Passe"
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  labs(
    title = "Manchester City",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightblue", color = NA),
    legend.text = element_text(color = "#1B2838")
  )

# Gráfico para o PSG
grafico_team2 <- ggplot() +
  annotate_pitch(
    dimensions = pitch_custom,
    colour = "#1B2838", linewidth = 0.8, fill = "lightblue", limits = FALSE) +
  geom_point(
    data = dados_passes_psg,
    aes(x = pos_x, y = pos_y, colour = Action),
    size = 2, alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Passes accurate" = "darkgreen",
      "Passes (inaccurate)" = "red"
    ),
    name = "Tipo de Passe"
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  labs(
    title = "PSG",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightblue", color = NA),
    legend.text = element_text(color = "#1B2838")
  )

# Juntar os gráficos de cada equipa
mapas_passes <- (
  (grafico_team1 + theme(legend.position = "none")) + (grafico_team2 + theme(legend.position = "none")) +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Mapa de Passes",
      subtitle = "Passes completos e errados das duas equipas",
      caption = paste0(
        "Manchester City: ", team1_stats$accurate_passes, "/", team1_stats$total_passes, 
        " (", team1_stats$accuracy, "%) passes completos\n",
        "PSG: ", team2_stats$accurate_passes, "/", team2_stats$total_passes, 
        " (", team2_stats$accuracy, "%) passes completos"
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#1B2838"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1B2838"),
        plot.caption = element_text(face="bold",size = 13, hjust = 0.5, color = "#1B2838", margin = margin(t = 10)),
        plot.background = element_rect(fill = "lightblue", color = NA),
        panel.background = element_rect(fill = "lightblue", color = NA)
      )
    )
) 

mapas_passes


# Calcular os top 5 passers de cada equipa
top_passes_city <- dados_passes_city %>%
  group_by(code) %>%
  summarise(total_passes = n(), accurate_passes = sum(Action == "Passes accurate")) %>%
  mutate(accuracy = round((accurate_passes / total_passes) * 100, 2)) %>%
  arrange(desc(total_passes)) %>%
  slice(1:5)

top_passes_psg <- dados_passes_psg %>%
  group_by(code) %>%
  summarise(total_passes = n(), accurate_passes = sum(Action == "Passes accurate")) %>%
  mutate(accuracy = round((accurate_passes / total_passes) * 100, 2)) %>%
  arrange(desc(total_passes)) %>%
  slice(1:5)



# Gráfico de barras para top passadores
top_passers_city <- ggplot(top_passes_city, aes(x = reorder(code, total_passes), y = total_passes, fill = accuracy)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(accuracy, "%")), vjust = -0.5, size = 3.5, color = "black") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "% de Passes completos") +
  labs(
    x = NULL,
    y = "Total de Passes"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    axis.title.y = element_text(color = "#1B2838"),
    axis.text = element_text(color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA)
  )

top_passers_psg <- ggplot(top_passes_psg, aes(x = reorder(code, total_passes), y = total_passes, fill = accuracy)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(accuracy, "%")), vjust = -0.5, size = 3.5, color = "black") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "% de Passes completos") +
  labs(
    x = NULL,
    y = "Total de Passes"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    axis.title.y = element_text(color = "#1B2838"),
    axis.text = element_text(color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA)
  )



# Juntar os dois gráficos
top_passadores <- (
  (top_passers_city + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())) +(top_passers_psg + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())) +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Top Passadores das Equipas",
      subtitle = "Jogadores com mais passes de cada equipa",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#1B2838"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1B2838"),
        plot.background = element_rect(fill = "lightblue", color = NA),
        panel.background = element_rect(fill = "lightblue", color = NA)
      )
    )
)

top_passadores


##### Exercício 6 #####

# Calcular a soma acumulada de xG para cada equipa por minuto
dados_xg <- dados %>%
  filter(!is.na(xG)) %>%
  mutate(
    start = as.numeric(start),         # Converter start para numérico
    minuto = floor(start / 60)         # Converter para minutos
  ) %>%
  group_by(Team, minuto) %>%           # Agrupar por equipa e minuto
  summarise(
    xG_minuto = sum(xG),               # Somar xG por minuto
    .groups = "drop"
  ) %>%
  arrange(Team, minuto) %>%
  group_by(Team) %>%
  mutate(
    xG_acumulado = cumsum(xG_minuto),  # Calcular o acumulado de xG
    xG_total = max(xG_acumulado)       # Calcular o total de xG
  )

golos <- dados %>%
  filter(Action == "Goals") %>%  # Filtrar apenas as ações de golos
  mutate(minuto = floor(as.numeric(start) / 60)) %>%  # Arredondar 'start' para minutos
  select(Team, minuto, code)



grafico_xg <- ggplot(dados_xg, aes(x = minuto, y = xG_acumulado, group = Team, color = Team)) +
  geom_step(size = 1.2) +  # Linhas separadas para cada equipa
  
  # Adicionar imagem da bola nos minutos dos golos
  geom_image(
    data = golos %>% left_join(dados_xg, by = c("Team", "minuto")),  # Juntar informações de xG acumulado por minuto
    aes(x = minuto, y = xG_acumulado + 0.1, image = "ball.png"),  # Adicionar imagem acima da linha
    size = 0.05, inherit.aes = FALSE
  ) +
  
  # Adicionar etiquetas com o nome do jogador que marcou o golo
  geom_text(
    data = golos %>% left_join(dados_xg, by = c("Team", "minuto")),  # Juntar xG acumulado com os golos
    aes(x = minuto, y = xG_acumulado + 0.3, label = code),  # Adicionar nome do jogador ao lado da bola
    color = "black", fontface = "bold", size = 4, inherit.aes = FALSE, hjust = 0.5
  ) +
  
  # Adicionar etiquetas do total de xG ao final das linhas
  geom_text(
    data = dados_xg %>% group_by(Team) %>% summarise(
      x = max(minuto),
      y = max(xG_acumulado),
      label = paste0("Total xG: ", round(max(xG_acumulado), 2))
    ),
    aes(x = x, y = y + 0.2, label = label),  # Ajuste vertical com `y + 0.2`
    hjust = 0.5,  # Centralizar horizontalmente
    vjust = -1,  # Ajustar a posição vertical acima da linha
    size = 4.5,  # Reduzir o tamanho do texto
    inherit.aes = FALSE
  ) +
  
  scale_color_manual(values = c("Manchester City" = "#780434", "PSG" = "#021e40")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(dados_xg$xG_acumulado) + 0.7)) +  # Expandir limite superior do gráfico
  
  labs(
    title = "Evolução do xG Durante o Jogo",
    x = "Minutos",
    y = "xG Acumulado",
    color = "Equipa"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#021e40"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#021e40"),
    axis.title = element_text(size = 14, color = "#021e40"),
    axis.text = element_text(size = 12, color = "#021e40"),
    axis.line = element_line(color = "#021e40", size = 0.8),  
    axis.ticks = element_line(color = "#021e40", size = 0.8),
    legend.position = "bottom",
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

# Exibir o gráfico 
grafico_xg

# Filtrar apenas os eventos com xG > 0
dados_xg_filtrado <- dados %>%
  filter(xG > 0)  

# Calcular o xG total por equipa
xg_totais <- dados_xg_filtrado %>%
  group_by(Team) %>%
  summarise(xG_total = sum(xG))


# Criar gráfico para CIty
grafico_team1 <- ggplot() +
  annotate_pitch(dimensions = pitch_custom, colour = "#1B2838", linewidth = 0.8, fill = NA, limits = FALSE) +
  geom_point(
    data = dados_xg_filtrado %>% filter(Team == unique(dados_xg_filtrado$Team)[1]),
    aes(x = pos_x, y = pos_y, size = xG),
    color = "#780434", alpha = 0.8
  ) +
  scale_size_continuous(range = c(2, 10), name = "xG") + # Tamanho das bolas baseado no xG
  labs(
    title = unique(dados_xg_filtrado$Team)[1],
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "none"
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 50, y = -10, # Ajustar para ficar abaixo do gráfico
    label = paste0("xG Total: ", round(xg_totais$xG_total[xg_totais$Team == unique(dados_xg_filtrado$Team)[1]], 2)),
    size = 5, fontface = "bold", color = "#1B2838"
  )


# Criar gráfico para PSG
grafico_team2 <- ggplot() +
  annotate_pitch(dimensions = pitch_custom, colour = "#1B2838", linewidth = 0.8, fill = NA, limits = FALSE) +
  geom_point(
    data = dados_xg_filtrado %>% filter(Team == unique(dados_xg_filtrado$Team)[2]),
    aes(x = pos_x, y = pos_y, size = xG),
    color = "#1B2838", alpha = 0.8
  ) +
  scale_size_continuous(range = c(2, 10), name = "xG") + # Tamanho das bolas baseado no xG
  labs(
    title = unique(dados_xg_filtrado$Team)[2],
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1B2838"),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "none"
  ) +
  annotate(
    "segment",
    x = 0, xend = 20,  
    y = -10, yend = -10,  
    arrow = arrow(length = unit(0.3, "cm")), 
    colour = "#021e40", linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 50, y = -10, # Ajustar para ficar abaixo do gráfico
    label = paste0("xG Total: ", round(xg_totais$xG_total[xg_totais$Team == unique(dados_xg_filtrado$Team)[2]], 2)),
    size = 5, fontface = "bold", color = "#1B2838"
  )

# Combinar os dois gráficos lado a lado
xg_plot <- grafico_team1 + grafico_team2 + plot_layout(ncol = 2) +
  plot_annotation(
    title = "Mapa de xG por Equipa",
    subtitle = "Cada bola representa um evento com xG. Quanto maior a bola, maior o xG",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#1B2838"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1B2838"),
      plot.background = element_rect(fill = "lightblue", color = NA),
      panel.background = element_rect(fill = "lightblue", color = NA)
    )
  )

# Exibir o gráfico
xg_plot