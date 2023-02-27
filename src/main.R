library(dplyr)
library(lubridate)
library(magrittr)
library(writexl)

source("src/ufmn.R")
source("src/ufmn_ext.R")

als_patients_diagnosed <- ufmn_clinical %>%
    left_join(ufmn_patients, by = "pid") %>%
    filter(year(fecha_diagnostico) == 2022) %>%
    select(pid, nhc, cip, fecha_diagnostico) %>%
    arrange(fecha_diagnostico)

als_patients_firstvisits <- ufmn_followups %>%
    left_join(ufmn_patients, by = "pid") %>%
    group_by(pid) %>%
    slice_min(fecha_visita, n = 1) %>%
    ungroup() %>%
    filter(year(fecha_visita) == 2022) %>%
    select(pid, nhc, cip, fecha_visita) %>%
    arrange(fecha_visita)

write_xlsx(ame_visits, "ame-visits-2022.xlsx")
write_xlsx(kennedy_visits, "kennedy-visits-2022.xlsx")
write_xlsx(als_patients_diagnosed, "als-diagnosed-2022.xlsx")
write_xlsx(als_patients_firstvisits, "als-firstvisits-2022.xlsx")
