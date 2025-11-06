################################################################################
#                                Bolivia PPT                                   # 
################################################################################

library(forcats)
library(dplyr)
library(haven)
library(ggplot2)
library(gganimate)
library(dplyr)
library(tibble)
library(purrr)
library(grid)    
library(gifski)
library(av)
library(showtext)
library(sysfonts)
library(sf)
library(readxl)
library(dplyr)
library(tidyr)
library(ragg)
library(ggrepel)
library(readr)

# --------------
# 0) Cargar datos
# --------------
font_add_google("Noto Sans", "noto")  
showtext_auto()    
showtext_opts(dpi = 300)   # que showtext use el mismo dpi de exportación

# Rutas
data                <- "/Users/florenciaruiz/BID 2/IDB-Data"
main                <- "/Users/florenciaruiz/BID 2/presentacionesIDB/Bolivia" 
data2               <- file.path(main, "Data")
data_processed_path <- file.path(data, "Data_Processed")
plots_path          <-  file.path(main, "Plots")

# Data completa
ind_all        <- read_dta(file.path(data_processed_path, "df_final.dta"))
ind_desag      <- read_dta(file.path(data_processed_path, "desag_data.dta"))
unique(ind_all$iso3c)

  # Filtrar solo Bolivia
ind_bolivia    <- ind_all   %>% filter(iso3c == "BOL") %>% arrange(year) 
ind_bol_desag  <- ind_desag %>% filter(iso3c == "BOL") %>% arrange(year) 

# Desastres naturales
desastres <- read_excel(file.path(data2, "desastres.xlsx"))
taxes     <-  read_excel(file.path(data2, "taxes.xlsx"))

# Datos del pdf data-bol (armo el data frame a mano)
data_bol <- data.frame(
  year          = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027),
  ca_pctgdp     = c(-5.6, -5.1, -4.3, -3.3, 0.0, 3.9, 2.1, -2.5, -3.1, -2.8, -3, -3),
  pov_3         = c(7.2, 6.4, 4.7, 3.1, 4.2, 3.1, 3.3, 2.8, 3.0, 3.9, 4.8, 5.7),
  pov_420       = c(10.2, 9.3, 7.9, 5.8, 6.6, 5.5, 5.7, 5.1, 5.5, 6.5, 7.5, 8.6),
  pov_830       = c(23.7, 23.1, 21.3, 17.8, 19.5, 17.1, 18.5, 16.5, 17.2, 18.4, 19.9, 21.1),
  infla_average = c(3.6, 2.8, 2.3, 1.8, 0.9, 0.7, 1.7, 2.6, 5.1, 30.0, 35, 40),
  gdp_growth    = c(4.3, 4.2, 4.2, 2.2, -8.7, 6.1, 3.6, 3.1, 0.7, -0.5, -1.1, -1.5)
  #gdp_g_pc_r    = c(2.7, 2.7, 2.7, 0.8, -9.9, 5.0, 2.4, 1.7, -0.6, -1.8)
)

# Proyecciones de pobreza
pov_proy <- read_csv(file.path(data2, "poverty_proy.csv")) %>% 
  select(-1) %>% 
  rename(country = 1) %>% 
  filter(!is.na(country))

  # paso a formato long
pov_proy <- pov_proy %>% 
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "pov_rate"
  ) %>%
  mutate(
    year = as.integer(year)
  )

# proyecciones de pobreza para difernetes lineas
pov_proy_l <- read_csv(file.path(data2, "poverty_proy_lineas.csv")) 

  # Creo la columna de indicador
     # las primeras 4 filas son el indicador Upper middle-income poverty rate ($6.85 in 2017 PPP)
    # las segundas 4 filas son el indicador Lower middle-income poverty rate ($3.65 in 2017 PPP)
    # las ultimas 4 filas son el indicador  International poverty rate ($2.15 in 2017 PPP)
pov_proy_l <-  pov_proy_l %>% 
  mutate(
    indicator = case_when(
      row_number() <= 5 ~ "Upper middle-income poverty rate ($6.85 in 2017 PPP)",
      row_number() >= 7  & row_number() <= 11 ~ "Lower middle-income poverty rate ($3.65 in 2017 PPP)",
      row_number() >= 13 & row_number() <= 17 ~ "International poverty rate ($2.15 in 2017 PPP)"
    )) %>% 
  select(-1) %>% 
  rename(country = 1) %>% 
  filter(!is.na(country)) %>% 
  select(country, indicator, everything()) 

  # paso a formato long las columnas de años (todas menos country e indicator)
pov_proy_l <- pov_proy_l %>% 
  pivot_longer(
    cols = c(-country, -indicator),
    names_to = "year",
    values_to = "pov_rate"
  ) %>%
  mutate(
    year = as.integer(year),
    indicator_lab = recode(as.character(indicator),
                      "International poverty rate ($2.15 in 2017 PPP)"       = "International poverty\n rate ($2.15 in 2017 PPP)",   
                      "Lower middle-income poverty rate ($3.65 in 2017 PPP)" = "Lower middle-income poverty\n rate ($3.65 in 2017 PPP)",
                      "Upper middle-income poverty rate ($6.85 in 2017 PPP)" = "Upper middle-income poverty\n rate ($6.85 in 2017 PPP)"
    )
  ) %>% 
  arrange(country, indicator, year)

# Data gini de WB con proyecciones
gini <- read_xlsx(file.path(data2, "data_gini.xlsx"))

  # paso a formato long
gini <- gini %>% 
  select(-Indicator) %>% 
  pivot_longer(
    cols = c(-Country),
    names_to = "year",
    values_to = "gini"
  ) %>%
  mutate(
    year = parse_number(year)
  )

# data de trabajo y sectores
labor_sectors <- read_xlsx(file.path(data2, "Data2.xlsx"))

  # paso a formato long
labor_sectors <- labor_sectors %>% 
  rename(indicator = `Labor Status`) %>% 
  pivot_longer(
    cols = c(-Country, -indicator),
    names_to = "year",
    values_to = "value"
  ) %>% 
  mutate(
    year = parse_number(year)
  )

# Data de pbi con poryecciones
gdp <- read_csv(file.path(data2, "gdp_real_andinos.csv")) %>% 
  select(-1) %>% 
  rename(country = 1) %>% 
  filter(!is.na(country))

  # paso a formato long
gdp <- gdp %>%
  pivot_longer(
    cols = -c(country),
    names_to = "year",
    values_to = "gdp_pc_real"
  ) %>%
  mutate(
    year = parse_number(year),
    indicator = "GDP per capita at Market Price, Volume, Millions 2015 real USD"
  )

# Data de crecimiento de PBI real

gdp_growth <- read_xlsx(file.path(data2, "gdp_pc_growth.xlsx")) 

  # paso a formato long
# -----------------------------------
# 1) Reducing Poverty and Inequality
# -----------------------------------

## 1.1) Pobreza extrema, pobreza, vulnerabilidad y clase media
{
  # Data para gráfico de pobreza
  df <- ind_bolivia %>% 
    select(year, idb_pobreza31, idb_pobreza, idb_vulnerable, idb_middle) %>% 
    mutate(across(-year, ~ .x * 100)) %>% 
    drop_na() %>%
    pivot_longer(-year, names_to = "band", values_to = "value") %>%
    mutate(
      band = factor(band,
                    levels = c("idb_pobreza31","idb_pobreza", "idb_vulnerable", "idb_middle"),
                    labels = c("Extreme poverty (≤$3.1)", "Poverty (>$3.1 & ≤$5)",
                               "Vulnerability (>$5 & ≤$12.4)", "Middle-Income (>$12.4 & ≤$62.0)")),
      # etiqueta en 2 líneas
      band_lab = recode(as.character(band),
                        "Extreme poverty (≤$3.1)"         = "Extreme poverty\n(\u2264$3.1)",   # sin espacio
                        "Poverty (>$3.1 & ≤$5)"           = "Poverty\n(>$3.1 & \u2264$5)",
                        "Vulnerability (>$5 & ≤$12.4)"    = "Vulnerability\n(>$5 & \u2264$12.4)",
                        "Middle-Income (>$12.4 & ≤$62.0)" = "Middle-Income\n(>$12.4 & \u2264$62.0)"
      )
    ) %>%
    arrange(year, band)
  
  # Df para poner etiquetas al final
  df_lab <-  df %>% 
    filter(year == max(year))
  
  # Paleta
  pal <- c("Extreme poverty (≤$3.1)" = "#9AA3A8",   
           "Poverty (>$3.1 & ≤$5)" = "#0A2043",
           "Vulnerability (>$5 & ≤$12.4)" = "#34a0a4",
           "Middle-Income (>$12.4 & ≤$62.0)" = "#1e6091")
  
  # Esoacio a la derecha
  x_pad <- diff(range(df$year)) * 0.10
  x_lab <- max(df$year) + x_pad*0.07  # mismo offset para todas
  
  # Gráfico
  ggplot(df, aes(x = year, y = value, fill = band, group = band)) +
    geom_area(position = position_stack(reverse = TRUE), alpha = 0.8, color = NA) +
    scale_fill_manual(values = pal) +
    # etiqueta
    geom_text(
      data = df_lab,
      aes(x = x_lab, y = value, label = band_lab, color = band, group = band),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      hjust = 0, vjust = 0.5,
      lineheight = 0.85, size = 3.5, fontface = "bold",
      family = "noto", show.legend = FALSE, inherit.aes = FALSE
    )+
    scale_color_manual(values = pal) +
    guides(fill = "none", color = "none") +
    labs(x = "Year", y = "% of people") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                       expand = expansion(mult = 0)) +
    scale_x_continuous(limits = c(min(df$year),
                                  max(df$year) + x_pad),
                       #breaks = seq(2000, 2020, 1),
                       expand = expansion(mult = c(0.01, 0.01))) +
    theme_minimal(base_size = 13) +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(10, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "pobreza.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
  
}

## 1.2) Pobreza urbana y rural
{
  # Data para gráfico de pobreza urbana y rural
  df <- bind_rows(
    ind_bol_desag %>% filter(area == "urban", sex== "Total", quintile == "Total") %>% transmute(year, value = pobreza_under5, series = "Urban"),
    ind_bol_desag %>% filter(area == "rural", sex== "Total", quintile == "Total") %>% transmute(year, value = pobreza_under5, series = "Rural")
  ) %>% 
    mutate(value = value*100) %>% 
    filter(!is.na(value))
  
  # Valor inicial
  start_lab <- df %>% group_by(series) %>% slice_min(year, with_ties = FALSE) %>% ungroup() %>%
    mutate(lbl = sprintf("%.0f", value))
  
  # Valor final
  end_lab <- df %>% group_by(series) %>% slice_max(year, with_ties = FALSE) %>% ungroup() %>%
    mutate(lbl = sprintf("%.0f", value))
  
  # Paleta de colores
  pal <- c("Urban" = "#34a0a4",
           "Rural" = "#0A2043")
  
  min_x  <- min(df$year)            
  max_x  <- max(df$year)
  x_pad <- diff(range(df$year)) * 0.1
  
  # Grafico
  ggplot(df, aes(year, value, color = series, group = series)) +
    geom_line(linewidth = 1.2) +
    #geom_point(size = 2) +
    
    # valor final
    geom_text(data = end_lab, inherit.aes = FALSE,
              aes(x = year, y = value, label = sprintf("%.0f", value), color = series),
              hjust = -0.2, vjust = 0, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab, inherit.aes = FALSE,
              aes(x = year, y = value, label = sprintf("%.0f", value), color = series),
              vjust = 1.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # Nombre de la serie
    geom_text(data = end_lab, aes(label = series, color = series),
              hjust = 0.4, vjust = 0, nudge_x = x_pad,
              size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_color_manual(values = pal) +
    guides(color = "none") +  
    labs(x = "Year", y = "% of people in poverty ($5 in 2021 PPP)") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                       expand = expansion(mult = 0)) +
    scale_x_continuous(limits = c(min_x, max_x + x_pad),
                       expand = expansion(mult = c(0.02, 0.02))) +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 30, 10, 10)) +
    coord_cartesian(clip = "off")
  
  ggsave(
    filename = file.path(plots_path, "pobreza_urb_ru.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
}

## 1.4) Pobreza comprando con paises
{
  # Data para gráfico de pobreza
  df <- pov_proy_l %>% 
    filter(
      # Linea de pobreza internacional
      indicator == "International poverty rate ($2.15 in 2017 PPP)",
    )
  
  pal <- c("Bolivia"   = "#34a0a4",
           "Colombia"  = "grey75",
           "Ecuador"   = "grey75",
           "Peru"      = "grey75"
           #"Paraguay"  = "grey75"
           )
  
  # valor incial
  start_lab <- df %>% 
    filter(country == "Bolivia", !is.na(pov_rate)) %>%
    slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", pov_rate))
  
  # valor final
  end_lab <- df %>% 
    filter(country == "Bolivia") %>%
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", pov_rate))
  
  # Nombres de series
  series_lab <- df %>% 
    group_by(country) %>%
    slice_max(year, with_ties = FALSE)
  
  # Grafico
  min_x  <- min(df$year, na.rm = TRUE)            
  max_x  <- max(df$year, na.rm = TRUE)
  (min_y  <- min(df$pov_rate, na.rm = TRUE))
  (max_y  <- max(df$pov_rate, na.rm = TRUE))
  
  ggplot() +
    # líneas solidas para valores observados
    geom_line(df %>% filter(year <= 2025),
              mapping = aes(x= year, y= pov_rate, color = country, group = country), 
              linewidth = 1.2)+
    
    # Lineas punteadas para proyecciones
    geom_line(df %>% filter(year > 2024), linetype = "dotted", linewidth = 1.2,
              mapping = aes(x= year, y= pov_rate, color = country, group = country)) +
    
    # valor final
    geom_text(data = end_lab, aes(x= year, y= pov_rate,
                                  label = lbl, color = country), fontface = "bold",
              hjust = -0.8, vjust = 2, size = 3, show.legend = FALSE) +
    
    # valor inicial
    geom_text(data = start_lab, aes(x= year, y= pov_rate, label = lbl, color = country),
              vjust = 0.5, hjust = 1.2, size = 3, fontface = "bold", show.legend = FALSE) +
    
    # Etiquetas a las series
    geom_text(data = series_lab,
              aes(x= year, y= pov_rate, label = country, color = country),
              vjust = 0.5, hjust = -0.1,
              size = 3, fontface = "bold", show.legend = FALSE) +
    
    scale_color_manual(values = pal) +
    guides(color = "none") + 
    labs(x = "Year", y = "% of people in poverty ($2.15 in 2017 PPP)") +
    scale_y_continuous(limits = c(0, 16), 
                       breaks = seq(0, 16, 2),
                       #expand = expansion(mult = c(0.02, 0))
                       ) +
    scale_x_continuous(limits = c(2010, 2027), 
                       breaks = seq(2010, 2027, 3),
                       #expand = expansion(mult = c(0.02, 0.02))
                       ) +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 13),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(25, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "pov_comp_int.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
}

## 1.5) Pobreza con diferentes lineas del WB
{
  # DF para bolivia
  df <- pov_proy_l %>% 
    filter(country == "Bolivia")
  
  # Valor inicial
  start_lab <- df %>% group_by(indicator) %>% filter(!is.na(pov_rate)) %>% slice_min(year, with_ties = FALSE) %>% ungroup() %>%
    mutate(lbl = sprintf("%.0f", pov_rate))
  
  # Valor final
  end_lab <- df %>% 
    group_by(indicator) %>% 
    filter(year<=2025) %>% 
    slice_max(year, with_ties = FALSE) %>% 
    ungroup() %>%
    mutate(lbl = sprintf("%.0f", pov_rate))
  
  # X fijo para las etiquetas
  min_x  <- min(df$year, na.rm = TRUE)            
  #max_x  <- max(df$year, na.rm = TRUE)
  max_x <- 2025
  x_pad <- diff(range(df$year)) * 0.10
  x_lab <- max_x + 0.8 * x_pad   # el 70% del padding derecho
  end_lab <- end_lab %>% mutate(x_lab = x_lab)
  
  # Paleta de colores
  pal <- c("International poverty rate ($2.15 in 2017 PPP)" = "#34a0a4" ,
           "Lower middle-income poverty rate ($3.65 in 2017 PPP)" = "#0A2043",
           "Upper middle-income poverty rate ($6.85 in 2017 PPP)" = "#1e6091")
  
  min(df$pov_rate, na.rm = TRUE)  
  max(df$pov_rate, na.rm = TRUE)  
  
  # Grafico
  ggplot() +
    # Linea sólida para valores observados
    geom_line(data = df %>% filter(year <= 2025), linewidth = 1.2, 
              aes(year, pov_rate, color = indicator, group = indicator)) +
    
    # Linea punteada para valores proyectados
    #geom_line(data = df %>% filter(year > 2024), linetype = "dashed", linewidth = 1.2,
    #          aes(year, pov_rate, color = indicator, group = indicator), show.legend = FALSE) +
    
    # Puntos
    geom_point(data = df %>% filter(year <= 2025),
               aes(year, pov_rate, color = indicator, group = indicator), show.legend = FALSE, size = 1.4) +
    
    # valor final
    geom_text(data = end_lab,
              aes(x = year, y = pov_rate, label = sprintf("%.0f", pov_rate), color = indicator),
              hjust = -0.8, vjust = 0.1, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab,
              aes(x = year, y = pov_rate, label = sprintf("%.0f", pov_rate), color = indicator),
              vjust = 1.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # Nombre de la serie
    geom_text(
      data = end_lab,
      aes(x = x_lab, y = pov_rate, label = indicator_lab, color = indicator),
      hjust = 0, vjust = 0.5,
      lineheight = 0.8, size = 4, fontface = "bold", show.legend = FALSE
    )+
  
    scale_color_manual(values = pal) +
    guides(color = "none") +  
    labs(x = "Year", y = "% of people in poverty") +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5),
                       expand = expansion(mult = 0)) +
    scale_x_continuous(limits = c(min_x, 2025 + x_pad),
                       expand = expansion(mult = c(0.02, 0.02))
                       ) +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid = element_blank(),
          #panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 160, 10, 10)) +
    coord_cartesian(clip = "off")
  
  ggsave(
    filename = file.path(plots_path, "pobreza_ppp2.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
  
}

## 1.6) Pobreza con forcasts
{
  # df para gráfico de pobreza con forecast
  df <- rbind(
    data_bol %>% transmute(year, value = pov_3, series = "$3.00 in 2021 PPP"),
    data_bol %>% transmute(year, value = pov_420, series = "$4.20 in 2021 PPP"),
    data_bol %>% transmute(year, value = pov_830, series = "$8.30 in 2021 PPP")
  )
  
  # Valor inicial
  start_lab <- df %>% group_by(series) %>% slice_min(year, with_ties = FALSE) %>% ungroup() %>%
    mutate(lbl = sprintf("%.0f", value))
  
  # Valor final
  end_lab <- df %>% group_by(series) %>% slice_max(year, with_ties = FALSE) %>% ungroup() %>%
    mutate(lbl = sprintf("%.0f", value))
  
  # Paleta de colores
  pal <- c("$3.00 in 2021 PPP" = "#34a0a4",
           "$4.20 in 2021 PPP" = "#0A2043",
           "$8.30 in 2021 PPP" = "#1e6091")
  
  min_x  <- min(df$year)            
  max_x  <- max(df$year)
  x_pad <- diff(range(df$year)) * 0.1
  
  # Grafico
  ggplot() +
    # Linea sólida para valores observados
    geom_line(data = df %>% filter(year <= 2025), linewidth = 1.2, 
              aes(year, value, color = series, group = series)) +
    
    # Linea punteada para valores proyectados
    geom_line(data = df %>% filter(year > 2024), linetype = "dashed", linewidth = 1.2,
              aes(year, value, color = series, group = series), show.legend = FALSE) +
    
    # Puntos
    geom_point(data = df, aes(year, value, color = series, group = series), show.legend = FALSE, size = 1.4) +
    
    # valor final
    geom_text(data = end_lab,
              aes(x = year, y = value, label = sprintf("%.0f", value), color = series),
              hjust = -0.8, vjust = 0.1, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab,
              aes(x = year, y = value, label = sprintf("%.0f", value), color = series),
              vjust = 1.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # Nombre de la serie
    geom_text(data = end_lab, aes(x = year, y = value, label = series, color = series),
              hjust = 0.1, vjust = 0.1, nudge_x = x_pad,
              size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_color_manual(values = pal) +
    guides(color = "none") +  
    labs(x = "Year", y = "% of people in poverty") +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5),
                       expand = expansion(mult = 0)) +
    scale_x_continuous(limits = c(min_x, max_x + x_pad),
                       expand = expansion(mult = c(0.02, 0.02))) +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 85, 10, 10)) +
    coord_cartesian(clip = "off")
  
  ggsave(
    filename = file.path(plots_path, "pobreza_ppp.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
}

## 1.7) Indice de Gini
{
  # Data para el indice de gini
  df <- ind_bolivia %>% 
    select(year, idb_gini, idb_ginihh, idb_ginihh_PHC) %>% 
    filter(year>=2000) %>% 
    filter(!is.na(idb_gini) | !is.na(idb_ginihh) | !is.na(idb_ginihh_PHC))
  
  # valor incial
  start_lab <- df %>% slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.2f", idb_ginihh))
  
  # valor final
  end_lab <- df %>% slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.2f", idb_ginihh))
  
  # Gráfico de lineas
  ggplot() +
    geom_line(data = df, aes(x = year, y = idb_ginihh),
              color = "#34a0a4", linewidth = 1.2) +
    geom_point(data = df, aes(x = year, y = idb_ginihh),
               color = "#34a0a4", size = 1.5) +
    geom_text(data = end_lab, inherit.aes = FALSE,
              aes(x = year, y = idb_ginihh, label = lbl),
              color = "#34a0a4", fontface = "bold", 
              hjust = -0.2, vjust = 0.5, size = 4, show.legend = FALSE) +
    geom_text(data = start_lab, inherit.aes = FALSE,
              aes(x = year, y = idb_ginihh, label = lbl),
              color = "#34a0a4",fontface = "bold",hjust = 0.9,
              vjust = -0.5, size = 4, show.legend = FALSE)+
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                       expand = expansion((mult = c(0.05, 0.07)))) +
    scale_x_continuous(breaks = seq(2000, 2023, 2),
                       expand = expansion((mult = c(0.09, 0.09)))) +
    guides(color = "none") +  
    labs(x = "Year", y = "Gini Index (0-1)") +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 40, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gini.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
}

## 1.8) Gini comparando con paises
{
  # Data para gráfico de PIB per cápita
  df <- ind_all %>% 
    filter(
      # Países de la ragion grupo Andino sin venezuela
      iso3c == "BOL" | 
        iso3c == "LCN" 
      #iso3c == "COL" | 
      #iso3c == "ECU" | 
      #iso3c == "PER" |
      #iso3c == "PRY"
    ) %>% 
    select(year, iso3c, idb_ginihh) %>%
    arrange(iso3c, year) %>% 
    drop_na() %>% 
    mutate(
      pais = case_when(
        iso3c == "BOL" ~ "Bolivia",
        iso3c == "LCN" ~ "Latin America & Caribbean"
        #iso3c == "COL" ~ "Colombia",
        #iso3c == "ECU" ~ "Ecuador",
        #iso3c == "PER" ~ "Peru",
        #iso3c == "PRY" ~ "Paraguay"
      ),
      pais_lab = recode(as.character(pais),
                        "Bolivia"             = "Bolivia",
                        "Latin America & Caribbean" = "Latin America\n& Caribbean")
    ) %>% 
    arrange(iso3c, year)
  
  pal <- c("Bolivia"   = "#34a0a4",
           "Latin America & Caribbean" = "grey75"
           #"Colombia"  = "grey75",
           #"Ecuador"   = "grey75",
           #"Peru"      = "grey75",
           #"Paraguay"  = "grey75"
  )
  
  # valor incial
  start_lab <- df %>% 
    #filter(iso3c == "BOL") %>%
    filter(year>=2005) %>% 
    group_by(pais) %>%
    slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.2f", idb_ginihh))
  
  # valor final
  end_lab <- df %>% 
    #filter(iso3c == "BOL") %>%
    group_by(pais) %>%
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.2f", idb_ginihh))
  
  # Nombres de series
  series_lab <- df %>% 
    group_by(pais) %>%
    slice_max(year, with_ties = FALSE)
  
  # Grafico
  min_x  <- min(df$year)            
  max_x  <- max(df$year)
  (min_y  <- min(df$idb_ginihh))
  (max_y  <- max(df$idb_ginihh))
  
  ggplot() +
    # líneas solidas
    geom_line(df, mapping = aes(x= year, y= idb_ginihh, color = pais, group = pais), 
              linewidth = 1.2)+
    
    # valor final
    geom_text(data = end_lab, aes(x= year, y= idb_ginihh,
                                  label = lbl, color = pais), fontface = "bold",
              hjust = 0.6, vjust = 2, size = 4, show.legend = FALSE) +
    
    # valor inicial
    geom_text(data = start_lab, aes(x= year, y= idb_ginihh, label = lbl, color = pais),
              vjust = -0.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # Etiquetas a las series
    geom_text(data = series_lab,
              aes(x= year, y= idb_ginihh, label = pais_lab, color = pais),
              vjust = -0.6, 
              size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_color_manual(values = pal) +
    guides(color = "none") + 
    labs(x = "Year", y = "Gini Index (0-1)") +
    scale_y_continuous(limits = c(0.4, 0.7), 
                       breaks = seq(0.4, 0.7, 0.1),
                       labels = comma,  
                       expand = expansion(mult = c(0.02, 0))) +
    scale_x_continuous(limits = c(2005, 2023), 
                       breaks = seq(2005, 2023, 2),
                       #expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 13),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(25, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gini_comp.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
}

## 1.9) Gini con proyecciones y comparando paises
{
  # df para gini con paises andinos y laram
  df <- gini %>% 
    filter(Country == "Bolivia" | Country == "Colombia" | Country == "Ecuador" | 
             Country == "Peru" 
             #Country == "Latin America and the Caribbean"
             ) %>% 
    mutate(
      country_lab = recode(as.character(Country),
                             "Bolivia"       = "Bolivia",   
                             "Colombia" = "Colombia",
                             "Ecuador" = "Ecuador",
                             "Peru" = "Peru"
                             #"Latin America and the Caribbean" = "Latin America and\n the Caribbean"
      )
    ) %>% 
    filter(!is.na(gini)) 
 
  pal <- c("Bolivia"   = "#34a0a4",
           #"Latin America and the Caribbean" = "grey75",
           "Colombia"  = "grey75",
           "Ecuador"   = "grey75",
           "Peru"      = "grey75"
           #"Paraguay"  = "grey75"
  )
  
  # valor incial
  start_lab <- df %>% 
    filter(Country == "Bolivia") %>%
    group_by(Country) %>%
    slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", gini))
  
  # valor final
  end_lab <- df %>% 
    filter(Country == "Bolivia") %>%
    group_by(Country) %>%
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", gini))
  
  # Nombres de series
  series_lab <- df %>% 
    group_by(Country) %>%
    slice_max(year, with_ties = FALSE)
  
  # Grafico
  min_x  <- min(df$year, na.rm = TRUE)            
  max_x  <- max(df$year, na.rm = TRUE)
  (min_y  <- min(df$gini, na.rm = TRUE))
  (max_y  <- max(df$gini, na.rm = TRUE))
  
  ggplot() +
    # líneas solidas para valores observados
    geom_line(df %>% filter(year <= 2025), 
                , mapping = aes(x= year, y= gini, color = Country, group = Country), 
              linewidth = 1.2)+
    # lineas punteadas para proyecciones
    geom_line(df %>% filter(year > 2024), linetype = "dotted", linewidth = 1.2,
              mapping = aes(x= year, y= gini, color = Country, group = Country)) +
    
    # valor final
    geom_text(data = end_lab, aes(x= year, y= gini,
                                  label = lbl, color = Country), fontface = "bold",
              hjust = 0.6, vjust = 2, size = 4, show.legend = FALSE) +
    
    # valor inicial
    geom_text(data = start_lab, aes(x= year, y= gini, label = lbl, color = Country),
              vjust = -0.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # Etiquetas a las series
    geom_text(data = series_lab,
              aes(x= year, y= gini, label = country_lab, color = Country),
              vjust = -0.6, lineheight = 0.8,
              size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_color_manual(values = pal) +
    guides(color = "none") + 
    labs(x = "Year", y = "Gini Index (0-100)") +
    scale_y_continuous(limits = c(30, 60), 
                       breaks = seq(30, 60, 5),
                       labels = comma,  
                       expand = expansion(mult = c(0.02, 0))) +
    scale_x_continuous(
                       #limits = c(2005, 2023), 
                       breaks = seq(2022, 2027, 1),
                       #expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 13),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(25, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gini_forcast.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
  
}


# -----------------------
# 2) Building Resilience
# -----------------------

## 2.1) Desastres naturales
{
  
  # data en formato largo
  df <- desastres %>%
    select(-ObjectId, -Country, -ISO2, -ISO3, -Unit, -Source, -`CTS Name`, -`CTS Code`, -`CTS Full Descriptor`) %>%
    pivot_longer(
      cols = -Indicator,
      names_to = "year",
      values_to = "count"
    ) %>%
    mutate(
      # año numerico
      year   = parse_number(year),
      # categoría corta: lo que viene después de los ": "
      hazard = str_trim(sub(".*:\\s*", "", Indicator))
    ) %>%
    # saco la categoria TOTAL
    filter(hazard != "Total", hazard != "TOTAL") %>%
    # NAs a 0 (por si faltan años en alguna categoría)
    mutate(count = replace_na(as.numeric(count), 0))
  
  # ordenar niveles de hazard
  haz_levels <- c("Wildfire", "Flood", "Drought", "Storm", "Landslide","Extreme temperature")
  df <- df %>% mutate(hazard = factor(hazard, levels = haz_levels))
  
  # paleta de colores
  pal <- c(
    "Flood"               = "#1e6091" ,
    "Extreme temperature" = "#89c2d9",
    "Storm"               = "#00b4d8",
    "Wildfire"            = "#0A2043",
    "Drought"             = "#34a0a4",
    "Landslide"           = "#9ceaef"
  )
  
  # Grafico
  ggplot(df, aes(x = year, y = count, fill = hazard)) +
    geom_col(width = 0.93, color = NA) +
    scale_fill_manual(values = pal) +
    scale_y_continuous(breaks = pretty(df$count, n = 6)) +
    scale_x_continuous(breaks = pretty(df$year, n = 10)) +
    labs(x = NULL, y = "Count", fill = NULL) +
    theme_classic() +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      legend.text = element_text(size = 9),
      legend.key.size = unit(9, "pt"),
      legend.spacing.x = unit(20, "pt"),
      axis.line.y = element_blank(),
      panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
      plot.margin = margin(10, 20, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "desastres.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
}

## 2.2) Impuesto ambiental
{
  # Data en formato largo
  df <- taxes %>%
    filter(Unit=="Percent of GDP") %>% 
    select(-ObjectId, -Country, -ISO2, -ISO3, -`CTS Code`, -`CTS Name`, -`CTS Full Descriptor`, -Unit, -Source) %>%
    pivot_longer(
      cols = -Indicator,
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(
      # año numerico
      year   = parse_number(year),
      # nombre limpio de la categoría
      tax_type = case_when(
        Indicator == "Taxes on Pollution"                                ~ "Pollution",
        Indicator == "Taxes on Transport (excluding fuel for transport)" ~ "Transport (excluding fuel for transport)",
        Indicator == "Taxes on Energy (including fuel for transport)"    ~ "Energy (including fuel for transport)",
        Indicator == "Taxes on Resources"                                ~ "Resources",
        TRUE ~ NA_character_
      )) %>%
    filter(!is.na(tax_type)) %>%
    # NAs a 0 (por si faltan años en alguna categoría)
    mutate(value = replace_na(as.numeric(value), 0))
  
  # ordenar niveles de tax_type
  type_levels <- c("Pollution","Resources", "Transport (excluding fuel for transport)", "Energy (including fuel for transport)")
  df <- df %>% mutate(tax_type = factor(tax_type, levels = type_levels))
  
  # paleta de colores
  pal <- c(
    "Pollution"                                = "#00b4d8" ,
    "Resources"                                = "#0A2043",
    "Transport (excluding fuel for transport)" = "#34a0a4",
    "Energy (including fuel for transport)"    = "#1e6091"
  )
  
  # Grafico
  ggplot(df, aes(x = year, y = value, fill = tax_type)) +
    geom_col(width = 0.93, color = NA) +
    scale_fill_manual(values = pal) +
    scale_y_continuous(breaks = pretty(df$value, n = 6)) +
    scale_x_continuous(breaks = pretty(df$year, n = 10)) +
    labs(x = NULL, y = "% of GDP", fill = NULL) +
    theme_classic() +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      legend.text = element_text(size = 9),
      legend.key.size = unit(9, "pt"),
      legend.spacing.x = unit(20, "pt"),
      axis.line.y = element_blank(),
      panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
      plot.margin = margin(10, 20, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "env_tax.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
  
}

## 2.3) Inflacion
{
  # Data para gráfico de inflación
  df <- ind_bolivia %>% 
    select(year, idb_PCPIPCH) %>% 
    drop_na() %>% 
    mutate(idb_PCPIPCH = idb_PCPIPCH*100) %>% 
    filter(year >=1990)
  
  # valor incial
  start_lab <- df %>% slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", idb_PCPIPCH))
  
  # valor final
  end_lab <- df %>% slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", idb_PCPIPCH))
  
  # Gráfico de lineas
  ggplot() +
    geom_line(data = df, aes(x = year, y = idb_PCPIPCH),
              color = "#34a0a4", linewidth = 1.2) +
    geom_text(data = end_lab, inherit.aes = FALSE,
              aes(x = year, y = idb_PCPIPCH, label = lbl),
              color = "#34a0a4", fontface = "bold", 
              hjust = -0.2, vjust = 0.5, size = 4, show.legend = FALSE) +
    geom_text(data = start_lab, inherit.aes = FALSE,
              aes(x = year, y = idb_PCPIPCH, label = lbl),
              color = "#34a0a4",fontface = "bold",hjust = 0.6,
              vjust = 1.8, size = 4, show.legend = FALSE)+
    scale_y_continuous(expand = expansion((mult = c(0.05, 0.07)))) +
    scale_x_continuous(expand = expansion((mult = c(0.09, 0.09)))) +
    guides(color = "none") +  
    labs(x = "Year", y = "Inflation (annual average %)") +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 10, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "inflacion.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
}

## 2.5) Inflacion con forcast
{
  # df para gráfico de inflación con forecast
  df <- data_bol %>%
    select(year, infla_average)
  
  # Valor inicial
  start_lab <- df %>% slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", infla_average))
  
  # Valor final
  end_lab <- df %>% 
    filter(year <= 2025) %>% 
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", infla_average))
  
  # Gráfico
  ggplot() +
    # Linea sólida para valores observados
    geom_line(data = df %>% filter(year <= 2025), linewidth = 1.2, 
              aes(year, infla_average), color = "#34a0a4") +
    
    # Linea punteada para valores proyectados
    #geom_line(data = df %>% filter(year > 2024), linetype = "dashed", linewidth = 1.2,
    #          aes(year, infla_average), color = "#34a0a4", show.legend = FALSE) +
    
    # Puntos
    geom_point(data = df %>% filter(year <= 2025),
               aes(year, infla_average), color = "#34a0a4", show.legend = FALSE, size = 1.4) +
    
    # valor final
    geom_text(data = end_lab,
              aes(x = year, y = infla_average, label = lbl),  color = "#34a0a4",
              hjust = -0.5, vjust = 0.1, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab,  color = "#34a0a4",
              aes(x = year, y = infla_average, label = lbl),
              vjust = 1.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 5),
                       expand = expansion((mult = c(0.05, 0.07)))) +
    scale_x_continuous(limits = c(min(df$year)-0.5, 2025+0.5),
                       breaks = seq(2015, 2025, 1),
                       #expand = expansion((mult = c(0.09, 0.09)))
                       ) +
    guides(color = "none") +
    labs(x = "Year", y = "Inflation (annual average %)") +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid = element_blank(),
          #panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 10, 10, 10))
    
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "inflacion2.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
  
}

# --------------
# 3) Fostering Growth
# --------------

## 3.1) PIB per cápita
{
  # Data para gráfico de PIB per cápita
  df <- ind_all %>% 
    filter(
      # Países de la ragion grupo Andino sin venezuela
      iso3c == "BOL" | iso3c == "COL" | iso3c == "ECU" | iso3c == "PER"
    ) %>% 
    select(year, iso3c, idb_NGDPRPPPPC) %>%
    arrange(iso3c, year) %>% 
    drop_na() %>% 
    mutate(
      pais = case_when(
        iso3c == "BOL" ~ "Bolivia",
        iso3c == "COL" ~ "Colombia",
        iso3c == "ECU" ~ "Ecuador",
        iso3c == "PER" ~ "Peru")
    )
  
  pal <- c("Bolivia"   = "#34a0a4",
           "Colombia"  = "grey75",
           "Ecuador"   = "grey75",
           "Peru"      = "grey75")
  
  # valor incial
  start_lab <- df %>% 
    filter(iso3c == "BOL") %>%
    slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.0f", idb_NGDPRPPPPC))
  
  # valor final
  end_lab <- df %>% 
    filter(iso3c == "BOL") %>%
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.0f", idb_NGDPRPPPPC))
  
  # Nombres de series
  series_lab <- df %>% 
    group_by(pais) %>%
    slice_max(year, with_ties = FALSE)
  
  # Grafico
  min_x  <- min(df$year)            
  max_x  <- max(df$year)
  (min_y  <- min(df$idb_NGDPRPPPPC))
  (max_y  <- max(df$idb_NGDPRPPPPC))

  ggplot() +
    # líneas solidas para valores observados
    geom_line(df %>% filter(year <= 2025), 
              mapping = aes(x= year, y= idb_NGDPRPPPPC, color = pais, group = pais), 
              linewidth = 1.2)+
          
    # líneas punteadas para valores proyectados
    geom_line(data = df %>% filter(year >= 2025),
              aes(x= year, y= idb_NGDPRPPPPC, color = pais, group = pais),
              linetype = "dotted", linewidth = 1.15, show.legend = FALSE) +
    
    # valor final
    geom_text(data = end_lab, aes(x= year, y= idb_NGDPRPPPPC,
                                  label = lbl, color = pais), fontface = "bold",
              hjust = 0.6, vjust = 2, size = 4, show.legend = FALSE) +
    
    # valor inicial
    geom_text(data = start_lab, aes(x= year, y= idb_NGDPRPPPPC, label = lbl, color = pais),
              vjust = -0.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # Etiquetas a las series
    geom_text(data = series_lab,
              aes(x= year, y= idb_NGDPRPPPPC, label = pais, color = pais),
              vjust = -0.5, 
              size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_color_manual(values = pal) +
    guides(color = "none") + 
    labs(x = "Year", y = "GDP per capita (constant US$)") +
    scale_y_continuous(limits = c(min_y, max_y), 
                       breaks = seq(0, max_y, 2000),
                       labels = comma,  
                       expand = expansion(mult = c(0.02, 0))) +
    scale_x_continuous(limits = c(2000, 2027), 
                       breaks = seq(2000, 2026, 5),
                      #expand = expansion(mult = c(0.02, 0.02))
                      ) +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 13),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(25, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gdp.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
  
}

## 3.2) Trade balance
{
  # Data para gráfico de balanza comercial
  df <- ind_bolivia %>% 
    select(year, idb_trade_bal_1) %>% 
    drop_na()
  
  # valor incial
  start_lab <- df %>% slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.2f", idb_trade_bal_1))
  
  # valor final
  end_lab <- df %>% slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.2f", idb_trade_bal_1))
  
  # Gráfico de lineas
  ggplot() +
    geom_line(data = df, aes(x = year, y = idb_trade_bal_1),
              color = "#34a0a4", linewidth = 1.2) +
    # linea punteada en el cero
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_text(data = end_lab, inherit.aes = FALSE,
              aes(x = year, y = idb_trade_bal_1, label = lbl),
              color = "#34a0a4", fontface = "bold", 
              hjust = -0.2, vjust = 0.5, size = 4, show.legend = FALSE) +
    geom_text(data = start_lab, inherit.aes = FALSE,
              aes(x = year, y = idb_trade_bal_1, label = lbl),
              color = "#34a0a4",fontface = "bold",hjust = 1.2,
              vjust = 0.4, size = 4, show.legend = FALSE)+
    #scale_y_continuous(expand = expansion((mult = c(0.05, 0.07)))) +
    scale_x_continuous(breaks = seq(1995, 2025, 5),
                       limits = c(min(df$year)-2, max(df$year)+2)
                       ) +
    guides(color = "none") +  
    labs(x = "Year", y = "Trade Balance (% of GDP)") +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 10, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "balance.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
  
}

## 3.3) Trade balance con forcasts
{
  # df para graáfico de balanza comercial con forecast
  df <- data_bol %>%
    select(year, ca_pctgdp)
  
  # punto inicial
  start_lab <- df %>% slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", ca_pctgdp))
  
  # punto final
  end_lab <- df %>% filter(year <= 2025) %>% slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", ca_pctgdp))
  
  # Gráfico
  ggplot() +
    # Linea sólida para valores observados
    geom_line(data = df %>% filter(year <= 2025), linewidth = 1.2, 
              aes(year, ca_pctgdp), color = "#34a0a4") +
    
    # Linea punteada para valores proyectados
    #geom_line(data = df %>% filter(year > 2024), linetype = "dashed", linewidth = 1.2,
    #          aes(year, ca_pctgdp), color = "#34a0a4", show.legend = FALSE) +
    
    # Puntos
    geom_point(data = df %>% filter(year <= 2025), aes(year, ca_pctgdp), color = "#34a0a4", show.legend = FALSE, size = 1.4) +
    
    # linea punteada en el cero
    #geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
    
    # valor final
    geom_text(data = end_lab ,
              aes(x = year, y = ca_pctgdp, label = lbl),  color = "#34a0a4",
              hjust = -0.8, vjust = 0.1, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab,  color = "#34a0a4",
              aes(x = year, y = ca_pctgdp, label = lbl),
              vjust = 1.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_y_continuous(limits = c(-7, 5), breaks = seq(-6, 5, 2)) +
    scale_x_continuous(limits = c(min(df$year)-0.5, 2025+0.5),
                       breaks = seq(2000, 2025, 1)
                       #expand = expansion((mult = c(0.09, 0.02)))
                       ) +
    guides(color = "none") +
    labs(x = "Year", y = "Current account balance (% of GDP) ") +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          #panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 10, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "balance2.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
  
  }

## 3.5) Crecimiento del PBI
{
  # Data para gráfico de crecimiento del PBI
  df <- data_bol %>% 
    select(year, gdp_growth)
  
  # punto inicial
  start_lab <- df %>% slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", gdp_growth))
  
  # punto final
  end_lab <- df %>% 
    filter(year <= 2025) %>% 
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.1f", gdp_growth))
  
  # Gráfico
  ggplot() +
    # Linea sólida para valores observados
    geom_line(data = df %>% filter(year <= 2025), linewidth = 1.2, 
              aes(year, gdp_growth), color = "#34a0a4") +
    
    # Linea punteada para valores proyectados
    #geom_line(data = df %>% filter(year > 2024), linetype = "dashed", linewidth = 1.2,
    #          aes(year, gdp_growth), color = "#34a0a4", show.legend = FALSE) +
    
    # Puntos
    geom_point(data = df, aes(year, gdp_growth), color = "#34a0a4", show.legend = FALSE, size = 1.4) +
    
    # linea punteada en el cero
    #geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
    
    # valor final
    geom_text(data = end_lab,
              aes(x = year, y = gdp_growth, label = lbl),  color = "#34a0a4",
              hjust = -0.8, vjust = 0.1, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab,  color = "#34a0a4",
              aes(x = year, y = gdp_growth, label = lbl),
              vjust = 1.8, size = 4, fontface = "bold", show.legend = FALSE) +
    
    scale_y_continuous(limits = c(min(df$gdp_growth), max(df$gdp_growth)), 
                       breaks = seq(-8, 8, 2)) +
    scale_x_continuous(limits = c(min(df$year)-0.5, 2025+0.5),
                       breaks = seq(2000, 2025, 1),
                       expand = expansion((mult = c(0.09, 0.09)))) +
    guides(color = "none") +
    labs(x = "Year", y = "GDP growth (annual %)") +
    theme_classic() +
    theme(axis.text = element_text(color = "black", size = 13),
          axis.title = element_text(color = "black", size = 14),
          axis.line.y = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(10, 10, 10, 10))
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gdp_growth2.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
  
}

## 3.6) PBI real con forcasts
{
  pal <- c("Bolivia"   = "#34a0a4",
           "Colombia"  = "grey75",
           "Ecuador"   = "grey75",
           "Peru"      = "grey75")
  
  # valor incial
  start_lab <- gdp %>% 
    filter(country == "Bolivia") %>%
    slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.0f", gdp_pc_real))
  
  # valor final
  end_lab <- gdp %>% 
    filter(country == "Bolivia") %>%
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.0f", gdp_pc_real))
  
  # Nombres de series
  series_lab <- gdp %>% 
    group_by(country) %>%
    slice_max(year, with_ties = FALSE)
  
  # Grafico
  min_x  <- min(gdp$year)            
  max_x  <- max(gdp$year)
  (min_y  <- min(gdp$gdp_pc_real))
  (max_y  <- max(gdp$gdp_pc_real))

  ggplot() +
    # lineas solidas para valores observados
    geom_line(data = gdp %>% filter(year <= 2025),
              aes(year, gdp_pc_real, color = country, group = country), linewidth = 1.2) +
    # lineas punteadas para valores proyectados
    geom_line(data = gdp %>% filter(year >= 2025),
              aes(year, gdp_pc_real, color = country, group = country),
              linetype = "dotted", linewidth = 1.15, show.legend = FALSE) +
    # etiquetas
    geom_text_repel(
      data = series_lab,
      aes(x = year, y = gdp_pc_real, label = country, color = country),
      nudge_x = 0.9,                
      direction = "y",                 
      hjust = 0,                       
      box.padding = 0.15, point.padding = 0.1,
      segment.color = NA, segment.size = 0.3,
      min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE,
      fontface = "bold"
    ) +
    # ejes y tema
    scale_x_continuous(limits = c(2000, 2028),
                       breaks = seq(2000, 2027, 3),
                       expand = expansion(mult = c(0.02, 0.1))
                       ) +
    scale_color_manual(values = pal) + guides(color = "none") +
    labs(x = "Year", y = "GDP per capita (Millions 2015 real USD)") +
    theme_classic()+
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 13),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(25, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gdp_comp.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
  
}

## 3.7) Crecimiento del PBI real con forcasts
{
  pal <- c("Bolivia"   = "#34a0a4",
           "Colombia"  = "grey75",
           "Ecuador"   = "grey75",
           "Peru"      = "grey75")
  
  # valor incial
  start_lab <- gdp_growth %>% 
    filter(country == "Bolivia") %>%
    slice_min(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.0f", val))
  
  # valor final
  end_lab <- gdp_growth %>% 
    filter(country == "Bolivia") %>%
    slice_max(year, with_ties = FALSE) %>%
    mutate(lbl = sprintf("%.0f", val))
  
  # Nombres de series
  series_lab <- gdp_growth %>% 
    group_by(country) %>%
    slice_max(year, with_ties = FALSE)
  
  # Grafico
  min_x  <- min(gdp_growth$year)            
  max_x  <- max(gdp_growth$year)
  (min_y  <- min(gdp_growth$val))
  (max_y  <- max(gdp_growth$val))
  
  ggplot() +
    # lineas solidas para valores observados
    geom_line(data = gdp_growth %>% filter(year <= 2024),
              aes(year, val, color = country, group = country), linewidth = 1.1) +
    # lineas punteadas para valores proyectados
    geom_line(data = gdp_growth %>% filter(year >= 2024),
              aes(year, val, color = country, group = country),
              linetype = "dotted", linewidth = 1, show.legend = FALSE) +
    # linea en el cero 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey85") +
    
    # valor final
    geom_text(data = end_lab, inherit.aes = FALSE,
              aes(x = year, y = val, label = sprintf("%.0f", val), color = country),
              hjust = -0.5, vjust = 0.4, size = 4, show.legend = FALSE, fontface = "bold") +
    
    # valor inicial
    geom_text(data = start_lab, inherit.aes = FALSE,
              aes(x = year, y = val, label = sprintf("%.0f", val), color = country),
              hjust = 1.2, vjust = 0.35, size = 4, fontface = "bold", show.legend = FALSE) +
    
    # nombre de la serie
    geom_text_repel(
      data = series_lab,
      aes(x = year, y = val, label = country, color = country),
      nudge_x = 0.9,                
      direction = "y",                 
      hjust = 0,                       
      box.padding = 0.15, point.padding = 0.1,
      segment.color = NA, segment.size = 0.3,
      min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE,
      fontface = "bold"
    ) +
    # ejes y tema
    scale_x_continuous(limits = c(2016, 2025.9),
                       breaks = seq(2016, 2025, 3),
                       expand = expansion(mult = c(0.02, 0.1))
    ) +
    scale_color_manual(values = pal) + guides(color = "none") +
    labs(x = "Year", y = "GDP per capita growth (annual %, real)") +
    theme_classic()+
    theme(axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 13),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
          plot.margin = margin(25, 45, 10, 10)) +
    coord_cartesian(clip = "off")
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "gdp_g_comp.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  ) 
}

## 3.8) Crecimiento de sectores
{
  # df para bolivia, indicadores de sectores
  df <- labor_sectors %>%
    filter(
      Country == "Bolivia",
      indicator %in% c("Economic sector: Agriculture",
                       "Economic sector: Industry",
                       "Economic sector: Services")
    ) %>%
    transmute(
      year,
      indicator = recode(indicator,
                         "Economic sector: Agriculture" = "Agriculture",
                         "Economic sector: Industry"    = "Industry",
                         "Economic sector: Services"    = "Services"
      ),
      value = value * 100
    )
  
  # completar grilla año × sector
  years_all <- seq(min(df$year, na.rm=TRUE), max(df$year, na.rm=TRUE), by = 1)
  df <- df %>%
    complete(year = years_all, indicator)
  
  # interpolar por sector
  df_interp <- df %>%
    group_by(indicator) %>%
    arrange(year) %>%
    mutate(value = na.approx(value, x = year, na.rm = FALSE, rule = 2)) %>% # lineal
    ungroup()
  
  # re-escalar a 100 por año si corresponde
  df_scaled <- df_interp %>%
    group_by(year) %>%
    mutate(total = sum(value, na.rm = TRUE),
           value = ifelse(!is.na(total) & total > 0, value * (100/total), value)) %>%
    select(-total) %>%
    ungroup()
  
  # etiquetas al final (usa el último año no-NA)
  df_lab <- df_scaled %>%
    filter(!is.na(value)) %>%
    group_by(indicator) %>%
    slice_max(year, with_ties = FALSE) %>%
    ungroup()
  
  # paleta, padding, etc. (tu mismo código)
  pal <- c("Agriculture"="#0A2043","Industry"="#34a0a4","Services"="#1e6091")
  x_pad <- diff(range(df_scaled$year)) * 0.10
  x_lab <- max(df_scaled$year) + x_pad*0.07
  
  ggplot(df_scaled, aes(year, value, fill = indicator, group = indicator)) +
    geom_area(position = position_stack(reverse = TRUE), alpha = 0.8, color = NA) +
    scale_fill_manual(values = pal) +
    geom_text(
      data = df_lab,
      aes(x = x_lab, y = value, label = indicator, color = indicator),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      hjust = 0, vjust = 0.5, lineheight = 0.85, size = 3.5, fontface = "bold",
      show.legend = FALSE, inherit.aes = FALSE
    ) +
    scale_color_manual(values = pal) + guides(fill="none", color="none") +
    labs(x = "Year", y = "% of GDP") +
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10), expand = expansion(mult=0)) +
    scale_x_continuous(limits = c(min(df_scaled$year), max(df_scaled$year) + x_pad),
                       expand = expansion(mult = c(0.01,0.01))) +
    theme_classic(base_size = 14) +
    theme(axis.text = element_text(color="black", size=13),
          axis.title = element_text(color="black", size=14),
          axis.line.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(10,45,10,10)) +
    coord_cartesian(clip = "off")
  
  
  # Guardo el gráfico
  ggsave(
    filename = file.path(plots_path, "pobreza.png"),
    device   = ragg::agg_png,    
    width    = 8,                  
    height   = 4.5,               
    dpi      = 300
  )
  
}


