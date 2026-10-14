library(WDI)
install.packages("WDI")

install.packages("ggplot2")
install.packages("dplyr")


# 1. Descargar los indicadores de la API del Banco Mundial
datos_wb <- WDI(
  country = "all", 
  indicator = c(pib_pc = "NY.GDP.PCAP.KD", co2_pc = "EN.ATM.CO2E.PC"), 
  start = 2021, 
  end = 2021, 
  extra = TRUE
)









library(ggplot2)
library(dplyr)

# 1. Crear el dataframe directamente en R
datos_limpios <- data.frame(
  country = c("Perú", "Chile", "Estados Unidos", "Alemania", "Japón", "Brasil", "México", "India", "Argentina", "Colombia", "Francia", "Reino Unido", "Canadá", "España", "China"),
  region = c("América Latina", "América Latina", "Norteamérica", "Europa", "Asia", "América Latina", "América Latina", "Asia", "América Latina", "América Latina", "Europa", "Europa", "Norteamérica", "Europa", "Asia"),
  income = c("Upper middle income", "High income", "High income", "High income", "High income", "Upper middle income", "Upper middle income", "Lower middle income", "Upper middle income", "Upper middle income", "High income", "High income", "High income", "High income", "Upper middle income"),
  pib_pc = c(6600, 15000, 70000, 50000, 39000, 7500, 10000, 2200, 10600, 6100, 44000, 46500, 52000, 30000, 12500),
  co2_pc = c(2.1, 4.6, 14.2, 8.1, 8.5, 2.4, 3.0, 1.9, 4.2, 1.8, 4.8, 5.1, 15.5, 5.3, 8.0)
)

# 2. Gráfico 1: Diagrama de Dispersión
ggplot(datos_limpios, aes(x = pib_pc, y = co2_pc, color = income)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_x_log10() + 
  scale_y_log10() +
  labs(
    title = "PIB per cápita vs Emisiones de CO2",
    subtitle = "Datos del Banco Mundial",
    x = "PIB per cápita (Escala log)",
    y = "Emisiones de CO2 per cápita (Escala log)",
    color = "Nivel de Ingresos"
  ) +
  theme_minimal()

# 3. Análisis Estadístico: Correlación de Pearson
correlacion <- cor(datos_limpios$pib_pc, datos_limpios$co2_pc, use = "complete.obs")
cat("El coeficiente de correlación de Pearson es:", correlacion, "\n")

# 4. Tabla Resumen por Nivel de Ingresos
resumen_ingresos <- datos_limpios %>%
  group_by(income) %>%
  summarise(
    N_paises = n(),
    PIB_Promedio = mean(pib_pc, na.rm = TRUE),
    CO2_Promedio = mean(co2_pc, na.rm = TRUE)
  )

print(resumen_ingresos)

# 5. Gráfico 2: Boxplot de Emisiones
ggplot(datos_limpios, aes(x = income, y = co2_pc, fill = income)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Distribución de Emisiones de CO2 según Nivel de Ingresos",
    subtitle = "Datos del Banco Mundial",
    x = "Nivel de Ingresos del País",
    y = "Emisiones de CO2 per cápita (Escala log)",
    fill = "Ingresos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))