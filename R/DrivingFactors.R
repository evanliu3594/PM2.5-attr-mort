#' Decomposition of attributed deaths
#'
#' @param serie   # Calc Steps for Each Series:
#' 1. PG-PA-EXP-ORF
#' 2. PG-PA-ORF-EXP
#' 3. PG-EXP-PA-ORF
#' 4. PG-EXP-ORF-PA
#' 5. PG-ORF-PA-EXP
#' 6. PG-ORF-EXP-PA
#' 7. PA-PG-EXP-ORF
#' 8. PA-PG-ORF-EXP
#' 9. PA-EXP-PG-ORF
#' 10. PA-EXP-ORF-PG
#' 11. PA-ORF-PG-EXP
#' 12. PA-ORF-EXP-PG
#' 13. EXP-PG-PA-ORF
#' 14. EXP-PG-ORF-PA
#' 15. EXP-PA-PG-ORF
#' 16. EXP-PA-ORF-PG
#' 17. EXP-ORF-PA-PG
#' 18. EXP-ORF-PG-PA
#' 19. ORF-PG-PA-EXP
#' 20. ORF-PG-EXP-PA
#' 21. ORF-PA-PG-EXP
#' 22. ORF-PA-EXP-PG
#' 23. ORF-EXP-PG-PA
#' 24. ORF-EXP-PA-PG
#' @param G refers to `field` in `Mortality()` 
#' @param D_r refers to `dose_real` in `Mortality()` 
#' @param D_c refers to `dose_cf` in `Mortality()` 
#' @param P refers to `pop` in `Mortality()` 
#' @param A refers to `age` in `Mortality()` 
#' @param M refers to `mort` in `Mortality()` 
#' @param L refers to `lvl` in `Mortality()`
#' @param key name of key storing the scenario/year information
#' @param from scenario/year when the driving space start
#' @param to scenario/year when the driving space end
#'
#' @return a data.frame of decomposed attributed deaths
#' @export
#'
#' @examples 1:24 %>% map(~ Decomposition(serie = .x, start.y = 'base2015', end.y = 'SSP1-Baseline_2030'))
Decomposition <- function(serie,
                          G, D_r, D_c = NULL,
                          P, A, M, L = NULL,
                          key, from, to) {
  
  serie.step <- expand_grid(
    step1 = c('PA','PG','EXP','ORF'), step2 = c('PA','PG','EXP','ORF'),
    step3 = c('PA','PG','EXP','ORF'), step4 = c('PA','PG','EXP','ORF'),
  ) %>% filter(
    step1 != step2 & step2 != step3 & step3 != step4 & 
    step4 != step1 & step1 != step3 & step2 != step4
  ) %>% slice(serie) %>% unlist
  
  D_c <- if (is.null(D_c)) D_r
  
  Decomp <- list(
    # Mort.Start ----
    Mort_0 = Mortality(
      field = G, 
      dose_cf = get_at(D_c, key, from),
      pop = get_at(p, key, from), 
      age = get_at(A, key, from), 
      dose_real = get_at(D_r, key, from), 
      mort = get_at(M, key, from),
      lvl = L,
      RR = "MEAN"
    ),
    # Mort.1----
    Mort_1 = if (serie.step[1] == 'PG') {
      Mortality(
        field = G,
        dose_cf = get_at(D_c, key, from),
        pop = get_at(P, key, to),
        age = get_at(A, key, from),
        dose_real = get_at(D_r, key, from),
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (serie.step[1] == 'PA') {
      Mortality(
        field = G,
        dose_cf = get_at(D_c, key, from),
        pop = get_at(P, key, from),
        age = get_at(A, key, to),
        dose_real = get_at(D_r, key, from),
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (serie.step[1] == 'EXP') {
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, from), 
        age = get_at(A, key, from), 
        dose_real = get_at(D_r, key, from), 
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (serie.step[1] == 'ORF') {
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, from),
        pop = get_at(p, key, from), 
        age = get_at(A, key, from), 
        dose_real = get_at(D_r, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    },
    # Mort.2----
    Mort_2 = if (all(serie.step[1:2] %in% c('PG','PA'))) {
      # PG  PA
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, from),
        pop = get_at(p, key, to), 
        age = get_at(A, key, to), 
        dose_real = get_at(D_r, key, from), 
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:2] %in% c('PG','EXP'))) {
      #  PG  EXP
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, from), 
        age = get_at(A, key, to), 
        dose_real = get_at(D_r, key, from), 
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:2] %in% c('PG', 'ORF'))) {
      #  PG  ORF
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, from),
        pop = get_at(p, key, to), 
        age = get_at(A, key, from), 
        dose_real = get_at(D_r, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:2] %in% c('PA', 'EXP'))) {
      #  PA  EXP
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, from), 
        age = get_at(A, key, to), 
        dose_real = get_at(D_r, key, from), 
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:2] %in% c('PA', 'ORF'))) {
      #  PA  ORF
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, from),
        pop = get_at(p, key, from), 
        age = get_at(A, key, to), 
        dose_real = get_at(D_r, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:2] %in% c('EXP', 'ORF'))) {
      #  EXP ORF
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, from), 
        age = get_at(A, key, from), 
        dose_real = get_at(D_r, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    },
    # Mort.3----
    Mort_3 = if (all(serie.step[1:3] %in% c('PG', 'PA', 'EXP'))) {
      # PG PA EXP
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, to), 
        age = get_at(A, key, to), 
        dose_real = get_at(D_r, key, from), 
        mort = get_at(M, key, from),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:3] %in% c('PG', 'PA', 'ORF'))) {
      # PG  PA ORF
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, from),
        pop = get_at(p, key, to), 
        age = get_at(A, key, to), 
        dose_real = get_at(D_r, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:3] %in% c('PG', 'EXP', 'ORF'))) {
      #  PG	EXP	ORF
      Mortality(
        field = G, 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, to), 
        age = get_at(A, key, from), 
        dose_real = get_at(D_r, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    } else if (all(serie.step[1:3] %in% c('PA', 'EXP', 'ORF'))) {
      #  PA	EXP	ORF
      Mortality(
        field = G, 
        dose_real = get_at(D_r, key, to), 
        dose_cf = get_at(D_c, key, to),
        pop = get_at(p, key, from), 
        age = get_at(A, key, to), 
        mort = get_at(M, key, to),
        lvl = L,
        RR = "MEAN"
      )
      
    },
    # Mort.End ----
    Mort_4 = Mortality(
      field = G, 
      dose_cf = get_at(D_c, key, to),
      pop = get_at(p, key, to), 
      age = get_at(A, key, to), 
      dose_real = get_at(D_r, key, to), 
      mort = get_at(M, key, to),
      lvl = L,
      RR = "MEAN"
    )
    
  )

  Decomp <- Decomp %>% imap(~{
    .x %>% pivot_longer(
      matches("[05]$"), names_to = "Cause_Age",values_to = 'Mort'
    ) %>% mutate(Step = .y)
  }) %>% pivot_wider(names_from = 'Step', values_from = 'Mort') %>% mutate(
    Start = Mort_0,
    !!serie.step[1] := Mort_1 - Mort_0,
    !!serie.step[2] := Mort_2 - Mort_1,
    !!serie.step[3] := Mort_3 - Mort_2,
    !!serie.step[4] := Mort_4 - Mort_3,
    End = Mort_4,
    .keep = 'unused'
  )
  
  # Print Result ----
  {
    cat(str_c('Drivers Between', start.y, 'and', end.y, ':\n', sep = ' '))
    cat(str_c(serie.step[1], ':\t', sum(Decomp %>% pull(PA) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[2], ':\t', sum(Decomp %>% pull(PG) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[3], ':\t', sum(Decomp %>% pull(EXP) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[4], ':\t', sum(Decomp %>% pull(ORF) %>% sum %>% round)),'\n')
  }
  
  return(Decomp)

}

