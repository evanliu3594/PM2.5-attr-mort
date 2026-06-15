# Diagnose why certain countries have no PWRR
library(tidyverse)
source('./Code/model.R')
source('./Code/data.R')
source('./Code/mortality.R')

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
domain <- "Country"

# Replicate PWRR chain
RR_all <- .RR_std_tbl |>
  pivot_wider(names_from = CI, values_from = RR) |>
  rename(RR = MEAN, RR_UP = UP, RR_LOW = LOW)

rr_for_pwr <- RR_all |> select(concentration, endpoint, agegroup, RR)

# Find which countries are in Grid_info but NOT in PWRR result
Grids <- Grid_info
Conc_r <- getConc_real(at)
pop <- getPop(at)

PWRR_pre <- Grids |>
  left_join(Conc_r, by = c("x", "y")) |>
  left_join(pop, by = c("x", "y")) |>
  left_join(rr_for_pwr, by = "concentration")

PWRR_data <- PWRR_pre |> na.omit()

PWRR <- PWRR_data |>
  group_by(Country) |>
  summarise(PWRR = weighted.mean(RR, Pop, na.rm = TRUE), .groups = "drop")

all_countries <- unique(Grids$Country)
pwr_countries <- unique(PWRR$Country)
missing <- setdiff(all_countries, pwr_countries)

cat("Countries in Grid_info:", length(all_countries), "\n")
cat("Countries with PWRR:   ", length(pwr_countries), "\n")
cat("Missing from PWRR:     ", length(missing), "\n")
cat("Missing countries:", str_c(head(missing, 10), collapse = ", "), if (length(missing) > 10) " ..." else "", "\n\n")

# For each missing country, diagnose why
for (cc in head(missing, 5)) {
  cat("──", cc, "──\n")
  g <- Grids |> filter(Country == cc)
  cat("  Grids in Grid_info:", nrow(g), "\n")

  # Has conc?
  g_conc <- g |> inner_join(Conc_r, by = c("x", "y"))
  cat("  With concentration: ", nrow(g_conc), " (", round(nrow(g_conc)/nrow(g)*100, 1), "%)\n", sep = "")

  # Has pop?
  g_pop <- g |> inner_join(pop, by = c("x", "y"))
  cat("  With population:    ", nrow(g_pop), " (", round(nrow(g_pop)/nrow(g)*100, 1), "%)\n", sep = "")

  # Has RR match?
  if (nrow(g_conc) > 0 && nrow(g_pop) > 0) {
    g_full <- g |>
      inner_join(Conc_r, by = c("x", "y")) |>
      inner_join(pop, by = c("x", "y")) |>
      left_join(rr_for_pwr, by = "concentration")
    n_ok <- nrow(g_full |> filter(!is.na(RR)))
    cat("  Valid after RR join:", n_ok, "/", nrow(g_full), "\n")
    if (n_ok == 0) {
      conc_vals <- g_full$concentration |> unique() |> head(5)
      cat("  Sample concentrations:", str_c(conc_vals, collapse = ", "), "\n")
      rr_range <- range(as.numeric(RR_all$concentration), na.rm = TRUE)
      cat("  RR lookup range: [", rr_range[1], ", ", rr_range[2], "]\n", sep = "")
    }
  }

  # After PWRR_pre na.omit
  g_pwrr <- PWRR_pre |> filter(Country == cc) |> na.omit()
  cat("  Rows after na.omit:  ", nrow(g_pwrr), "\n\n", sep = "")
}
