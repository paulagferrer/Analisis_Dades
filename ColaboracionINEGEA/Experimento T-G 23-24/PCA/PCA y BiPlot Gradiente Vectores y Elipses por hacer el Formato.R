# ---------------------------------------------------------
# Librerías necesarias
# ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(factoextra)
library(ggnewscale)

# ---------------------------------------------------------
# Llamada al Excel
# ---------------------------------------------------------
datosRepetidas <- Excel_Completo_Combinaciones_2024

# ---------------------------------------------------------
# Convertir factores
# ---------------------------------------------------------
datos <- datosRepetidas %>%
  mutate(across(c(Variedad, Clon, Portainjerto, Genotipo, Fecha, Replica, Planta),
                as.factor))

# ---------------------------------------------------------
# Variables fisiológicas
# ---------------------------------------------------------
variables_fisio <- c("AN", "gs", "E", "WUEi", "Ymd", "Ypd",
                     "Kroot", "Kplant", "DifY", "Hojas", "Afoliar")
# -------------------------------------------------------------------
# 16 COLORES Y 16 SÍMBOLOS
# -------------------------------------------------------------------
colores_16 <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
  "#bcbd22", "#17becf", "#393b79", "#637939",
  "#8c6d31", "#843c39", "#7b4173", "#31a354"
)

simbolos_16 <- c(
  16, 17, 15, 3, 7, 8, 18, 4,
  0, 1, 2, 5, 6, 9, 10, 12
)
# ---------------------------------------------------------
# Fechas
# ---------------------------------------------------------
fechas <- c("WW", "WS1", "WS2", "R1", "R2")

# ---------------------------------------------------------
# Factores para hacer PCA por separado
# ---------------------------------------------------------
factores <- c("Genotipo", "Variedad", "Portainjerto", "Clon")

# ---------------------------------------------------------
# Funciones generales
# ---------------------------------------------------------

# ---------------------------------------------------------
# FUNCIONES CORREGIDAS usando 16 colores y símbolos
# ---------------------------------------------------------
hacer_pca_plot <- function(pca_res, df, color_factor, fecha_actual, factor_actual) {
  
  p <- fviz_pca_ind(
    pca_res,
    geom.ind = "point",
    
    # SOLO LE PASAMOS EL FACTOR, NO LOS SÍMBOLOS
    col.ind = df[[color_factor]],
    shape.ind = df[[color_factor]],
    
    palette = colores_16,
    addEllipses = TRUE,
    ellipse.level = 0.95,
    pointsize = 4,
    title = paste("PCA -", factor_actual, " - Fecha:", fecha_actual),
    legend.title = factor_actual
  )
  
  # AQUÍ AÑADIMOS LOS 16 SÍMBOLOS
  p <- p + scale_shape_manual(values = simbolos_16)
  
  # DEVOLVEMOS EL GRÁFICO COMPLETO
  p + theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
}

# -------------------------------------------------------
# NUEVA función hacer_biplot_plot() - biplot construido manualmente
# -------------------------------------------------------
hacer_biplot_plot <- function(pca_res, df, color_factor, fecha_actual, factor_actual) {
  
  # 1) Scores de individuos
  ind_scores <- as.data.frame(pca_res$x[, 1:2])
  colnames(ind_scores) <- c("PC1", "PC2")
  ind_scores[[color_factor]] <- df[[color_factor]]
  
  # 2) Loadings
  loadings <- as.data.frame(pca_res$rotation[, 1:2])
  colnames(loadings) <- c("u1", "u2")
  loadings$Variable <- rownames(loadings)
  
  # 3) Contribuciones
  contrib <- (pca_res$rotation[,1]^2 + pca_res$rotation[,2]^2)
  contrib_norm <- (contrib - min(contrib)) / (max(contrib) - min(contrib))
  loadings$contrib_norm <- contrib_norm
  
  # 4) Escalar vectores
  pc1_rng <- diff(range(ind_scores$PC1))
  pc2_rng <- diff(range(ind_scores$PC2))
  load1_rng <- diff(range(loadings$u1))
  load2_rng <- diff(range(loadings$u2))
  
  if (load1_rng == 0) load1_rng <- 1
  if (load2_rng == 0) load2_rng <- 1
  
  arrow_mult <- min((pc1_rng/load1_rng)*0.6, (pc2_rng/load2_rng)*0.6)
  loadings$xend <- loadings$u1 * arrow_mult
  loadings$yend <- loadings$u2 * arrow_mult
  
  # 5) Paletas individuos
  niveles <- levels(factor(df[[color_factor]]))
  if (is.null(niveles)) niveles <- unique(as.character(df[[color_factor]]))
  nlev <- length(niveles)
  cols_use <- rep(colores_16, length.out=nlev)
  names(cols_use) <- niveles
  shapes_use <- rep(simbolos_16, length.out=nlev)
  names(shapes_use) <- niveles
  
  # 6) Paleta gradiente para variables
  grad_palette <- c("#1f77b4", "#ffbf00", "#d62728")
  
  # 7) Construcción del biplot
  p <- ggplot() +
    #Individuos
    geom_point(data = ind_scores,
               aes(x = PC1, y = PC2,
                   color = .data[[color_factor]],
                   shape = .data[[color_factor]]),
               size = 3) +
    scale_color_manual(values = cols_use, name = factor_actual) +
    scale_shape_manual(values = shapes_use, name = factor_actual) +
    
    # Elipses
    stat_ellipse(data = ind_scores,
                 aes(x=PC1, y=PC2, group=.data[[color_factor]],
                     color=.data[[color_factor]]),
                 linetype = 2, level=0.95, show.legend=FALSE) +
    
    # --- AQUÍ reseteamos la escala de color ---
    ggnewscale::new_scale_color()
  
    # Flechas con gradiente continuo
  p <- p +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = xend, yend = yend,
                     color = contrib_norm),
                 arrow = arrow(length = unit(0.25, "cm")),
                 size = 1.1, alpha = 0.85) +
    
    # Etiquetas de variables
    geom_text(data = loadings,
              aes(x = xend * 1.07, y = yend * 1.07, label = Variable,
                  color = contrib_norm),
              size = 4, fontface = "bold") +
    
    # Barra de leyenda continua
    scale_color_gradientn(
      colors = c("#1f77b4", "#ffbf00", "#d62728"),
      limits = c(0,1),
      name = "Contribución"
    ) +
    
    theme_bw(base_size = 14) +
    labs(title=paste("Biplot PCA -", factor_actual, " - Fecha:", fecha_actual),
         x=paste0("PC1 (", round(100*(pca_res$sdev[1]^2)/sum(pca_res$sdev^2),1), "%)"),
         y=paste0("PC2 (", round(100*(pca_res$sdev[2]^2)/sum(pca_res$sdev^2),1), "%)")) +
    theme(
      plot.title = element_text(hjust=0.5, face="bold"),
      legend.position="right"
    )
  
  return(p)
}


# ---------------------------------------------------------
# Bucle principal
# ---------------------------------------------------------

for (factor_actual in factores) {
  
  # Crear carpeta para este factor
  carpeta <- paste0("Graficos", factor_actual, "_PCAyBiplot")
  dir.create(carpeta, showWarnings = FALSE)
  
  cat("\nCreando gráficos para factor:", factor_actual, "\n")
  
  for (f in fechas) {
    
    df_f <- datos %>% filter(Fecha == f)
    
    if (nrow(df_f) <= 2) {
      message(paste("?????? No hay suficientes datos para la fecha:", f))
      next
    }
    
    # Selección numérica
    df_num <- df_f %>% select(all_of(variables_fisio))
    
    # PCA
    pca_res <- prcomp(df_num, scale. = TRUE)
    
    # -------------------------------
    # PCA plot
    # -------------------------------
    p_pca <- hacer_pca_plot(pca_res, df_f, factor_actual, f, factor_actual)
    
    ggsave(
      filename = paste0(carpeta, "/PCA_", factor_actual, "_", f, ".png"),
      plot = p_pca,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    # -------------------------------
    # Biplot
    # -------------------------------
    p_biplot <- hacer_biplot_plot(pca_res, df_f, factor_actual, f, factor_actual)
    
    ggsave(
      filename = paste0(carpeta, "/Biplot_", factor_actual, "_", f, ".png"),
      plot = p_biplot,
      width = 10,
      height = 8,
      dpi = 300
    )
    
  }
}

cat("\n\n??? Todos los PCA y Biplots han sido generados correctamente.\n")

