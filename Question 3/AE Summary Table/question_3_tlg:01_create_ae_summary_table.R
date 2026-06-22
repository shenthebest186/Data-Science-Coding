library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(pharmaverseadam)
library(flextable)
library(officer)


#Create log file
log_file <- "Question3_table_Program.log"

sink(log_file, split = TRUE)

cat("Program started:", Sys.time(), "\n")

#Use tryCatch function to catch any error messages
tryCatch(
  {
    
#--------------------------------------------------
# Data
#--------------------------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

teae <- adae %>%
  filter(TRTEMFL == "Y")

#--------------------------------------------------
# Denominators (subjects per treatment)
#--------------------------------------------------
denoms <- adsl %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, ACTARM) %>%
  count(ACTARM, name = "N")

total_n <- adsl %>%
  distinct(USUBJID) %>%
  nrow()

trt_levels <- denoms$ACTARM

#--------------------------------------------------
# Helper function
#--------------------------------------------------
fmt_npct <- function(n, N) {
  pct <- ifelse(N > 0, 100 * n / N, 0)
  sprintf("%d (%.1f%%)", n, pct)
}

#--------------------------------------------------
# First row: Any TEAE
#--------------------------------------------------
any_teae <- teae %>%
  distinct(USUBJID, ACTARM)

teae_row <- map_chr(
  trt_levels,
  \(trt) {
    n <- any_teae %>%
      filter(ACTARM == trt) %>%
      nrow()
    
    N <- denoms %>%
      filter(ACTARM == trt) %>%
      pull(N)
    
    fmt_npct(n, N)
  }
)

total_teae_n <- any_teae %>%
  distinct(USUBJID) %>%
  nrow()

teae_total <- fmt_npct(total_teae_n, total_n)

#--------------------------------------------------
# AESOC frequencies for sorting
#--------------------------------------------------
soc_order <- teae %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC, sort = TRUE, name = "freq")

#--------------------------------------------------
# Build SOC + PT rows
#--------------------------------------------------
rows <- list()

for (soc in soc_order$AESOC) {
  
  # SOC row
  soc_counts <- teae %>%
    filter(AESOC == soc) %>%
    distinct(USUBJID, ACTARM)
  
  soc_row <- tibble(
    Label = soc
  )
  
  for (trt in trt_levels) {
    
    n <- soc_counts %>%
      filter(ACTARM == trt) %>%
      nrow()
    
    N <- denoms %>%
      filter(ACTARM == trt) %>%
      pull(N)
    
    soc_row[[trt]] <- fmt_npct(n, N)
  }
  
  soc_total_n <- soc_counts %>%
    distinct(USUBJID) %>%
    nrow()
  
  soc_row[["Total"]] <- fmt_npct(soc_total_n, total_n)
  
  rows[[length(rows) + 1]] <- soc_row
  
  # PT rows within SOC
  pt_order <- teae %>%
    filter(AESOC == soc) %>%
    distinct(USUBJID, AETERM) %>%
    count(AETERM, sort = TRUE, name = "freq")
  
  for (pt in pt_order$AETERM) {
    
    pt_counts <- teae %>%
      filter(AESOC == soc, AETERM == pt) %>%
      distinct(USUBJID, ACTARM)
    
    pt_row <- tibble(
      Label = paste0("    ", pt)
    )
    
    for (trt in trt_levels) {
      
      n <- pt_counts %>%
        filter(ACTARM == trt) %>%
        nrow()
      
      N <- denoms %>%
        filter(ACTARM == trt) %>%
        pull(N)
      
      pt_row[[trt]] <- fmt_npct(n, N)
    }
    
    pt_total_n <- pt_counts %>%
      distinct(USUBJID) %>%
      nrow()
    
    pt_row[["Total"]] <- fmt_npct(pt_total_n, total_n)
    
    rows[[length(rows) + 1]] <- pt_row
  }
}

body_tbl <- bind_rows(rows)

#--------------------------------------------------
# Final table
#--------------------------------------------------
teae_tbl <- tibble(
  Label = "Treatment Emergent AEs"
)

for (i in seq_along(trt_levels)) {
  teae_tbl[[trt_levels[i]]] <- teae_row[i]
}

teae_tbl[["Total"]] <- teae_total

final_tbl <- bind_rows(teae_tbl, body_tbl)

#--------------------------------------------------
# Column headers with N
#--------------------------------------------------
col_names <- c(
  "Primary System Organ Class\nReported Term for the Adverse Event"
)

for (trt in trt_levels) {
  
  N <- denoms %>%
    filter(ACTARM == trt) %>%
    pull(N)
  
  col_names <- c(col_names,
                 paste0(trt, "\nN=", N))
}

col_names <- c(
  col_names,
  paste0("Total\nN=", total_n)
)

names(final_tbl) <- col_names

#--------------------------------------------------
# Flextable
#--------------------------------------------------
ft <- flextable(final_tbl)

ft <- theme_booktabs(ft)
ft <- fontsize(ft, size = 5)
ft <- width(ft, j = 1, width = 3.5)
ft <- bold(ft, i = 1, bold = TRUE)

# Bold SOC rows
soc_rows <- which(
  !startsWith(final_tbl[[1]], "    ") &
    final_tbl[[1]] != "Treatment Emergent AEs"
)

ft <- bold(ft, i = soc_rows, bold = TRUE)

ft <- autofit(ft)

ft <- align(ft, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "body")

#--------------------------------------------------
# Export PDF
#--------------------------------------------------
doc <- read_docx()

doc <- body_add_par(
  doc,
  "Treatment-Emergent Adverse Events",
  style = "heading 1"
)

doc <- body_add_flextable(doc, ft)

print(doc, target = "TEAE_Summary_Table.docx")


cat("Program completed successfully.\n")
  },

error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  }
)

cat("Program ended:", Sys.time(), "\n")

sink(type = "message")
sink()

