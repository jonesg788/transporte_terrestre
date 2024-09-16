# Autor: Glorianna Alfonzo Jones
# Fecha de creación: 4/09/2024

------
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(showtext)
library(readxl)

# Cargar la fuente Montserrat desde Google Fonts
font_add_google("Montserrat", "montserrat")
showtext_auto()

# Leer el archivo Excel
data <- read_excel("viajes_asientos_pasajeros.xlsx")


# Agrupar los datos por año y mes, sumando la cantidad de pasajeros
data_grouped <- data %>%
  mutate(tiempo = as.Date(tiempo)) %>%
  group_by(YearMonth = format(tiempo, "%Y-%m")) %>%
  summarise(Total_pasajeros = sum(pasajeros, na.rm = TRUE))

# Convertir YearMonth a fecha para facilitar el gráfico
data_grouped$YearMonth <- as.Date(paste0(data_grouped$YearMonth, "-01"))

# Filtrar los datos para el rango de fechas entre 2021 y 2024
data_filtered <- data_grouped %>%
  filter(YearMonth >= as.Date("2021-01-01") & YearMonth <= as.Date("2024-12-31"))

# Crear el gráfico de la cantidad de pasajeros por mes
plot <- ggplot(data_filtered, aes(x = YearMonth, y = Total_pasajeros)) +
  geom_line(color = "#242C4F", size = 1) +
  geom_point(color = "#46658B", size = 0.5) +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed") + # Agrega una línea de tendencia suavizada
  labs(title = "Cantidad de Pasajeros por Mes (2021-2024)",
       x = "Fecha",
       y = "Pasajeros") +
  theme_minimal(base_family = "montserrat") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(size = 12)
  )

# Mostrar el gráfico
print(plot)

# Guardar el gráfico como un archivo PNG
ggsave("grafico_pasajeros_por_mes_2021_2024.png", plot = plot, width = 10, height = 6, dpi = 300)
