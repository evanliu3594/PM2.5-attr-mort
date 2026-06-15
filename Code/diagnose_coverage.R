# Diagnose spatial overlap between Pop / Conc / Grid_info
# Output: raster plot showing which grids have population and concentration data
library(tidyverse)
library(stars)
library(sf)

source('./Code/model.R')
source('./Code/data.R')

set_Model('NCD+LRI')
read_files(
  Grids = './Data/GRID_information_instance_220628.xlsx',
  Pop = './Data/GridPop_instance_231220.xlsx',
  Conc_real = './Data/GridPM25_instance_231220.xlsx',
  Conc_cf = './Data/GridPM25_cf_instance_260325.xlsx',
  MortRate = "./Data/GBD_mortality_instance_231220.xlsx",
  AgeGroup = './Data/GBD_agestructure_instance_231220.xlsx',
  dgt_grid = 2
)

at <- names(Conc_real)[3]

# ── Build grid coverage map ──
# Each grid cell gets a status code:
#   3 = has both Pop and Conc
#   2 = has Pop only
#   1 = has Conc only
#   0 = in Grid_info only (no data)

grid_x <- Grid_info |> select(x, y)
pop_x <- getPop(at) |> select(x, y) |> mutate(has_pop = 1L)
conc_x <- getConc_real(at) |> select(x, y) |> mutate(has_conc = 1L)

coverage <- grid_x |>
  left_join(pop_x, by = c("x", "y")) |>
  left_join(conc_x, by = c("x", "y")) |>
  replace_na(list(has_pop = 0L, has_conc = 0L)) |>
  mutate(
    status = case_when(
      has_pop == 1 & has_conc == 1 ~ "Both",
      has_pop == 1 & has_conc == 0 ~ "Pop only",
      has_pop == 0 & has_conc == 1 ~ "Conc only",
      TRUE ~ "Neither"
    ) |>
      factor(levels = c("Both", "Pop only", "Conc only", "Neither")),
    x = as.numeric(x),
    y = as.numeric(y)
  )

# ── Convert to stars raster ──
# Grids are regularly spaced; infer resolution
x_vals <- sort(unique(coverage$x))
y_vals <- sort(unique(coverage$y))
dx <- min(diff(x_vals))
dy <- min(diff(y_vals))

r <- st_as_stars(coverage, dims = c("x", "y"), xy = c("x", "y"))

# ── Plot ──
cols <- c(
  "Both"     = "#0072B2",
  "Pop only" = "#E69F00",
  "Conc only" = "#56B4E9",
  "Neither"  = "#000000"
)

ggplot() +
  geom_stars(data = r, mapping = aes(x = x, y = y, fill = status)) +
  scale_fill_manual(values = cols, name = NULL) +
  coord_equal() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Grid coverage: Population × Concentration")

# ── Summary ──
cat("\nCoverage summary:\n")
coverage |> count(status) |> mutate(pct = round(n / sum(n) * 100, 1)) |> print()
