library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

#Create log file
log_file <- "Question2_Program.log"

sink(log_file, split = TRUE)

cat("Program started:", Sys.time(), "\n")

#Use tryCatch function to catch any error messages
tryCatch(
  {

dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae

dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)

#Use SDTM.DM as the basis of ADSL
adsl <- dm %>%
  select(-DOMAIN)

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <=50 ~ "18 -50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    ITTFL = case_when(
      !is.na(ARM) & ARM != "" ~ "Y",
      TRUE ~ "N"
    )
  )

#Create TRTSDTM and TRTSTMF
#Step1 Identify valid dosing records and impute missing times
ex_trt <- ex %>%
  filter(
    !is.na(EXSTDTC),
    nchar(substr(EXSTDTC, 1, 10)) == 10,
    EXDOSE > 0 |
      (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))
  ) %>%
  mutate(
    TRTSTMF = case_when(
      !str_detect(EXSTDTC, "T") ~ "H",
      str_detect(EXSTDTC, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}$") ~ "M",
      str_detect(EXSTDTC, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}$") ~ "",
      TRUE ~ ""
    ),
    EXSTDTC_IMP = case_when(
      !str_detect(EXSTDTC, "T") ~ paste0(EXSTDTC, "T00:00:00"),
      str_detect(EXSTDTC, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}$") ~ paste0(EXSTDTC, ":00:00"),
      str_detect(EXSTDTC, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}$") ~ paste0(EXSTDTC, ":00"),
      TRUE ~ EXSTDTC
    ),
    EXDTM = ymd_hms(EXSTDTC_IMP, quiet = TRUE)
  )

#Step2 Select earliest valid exposure per USUBJID
trtsdtm <- ex_trt %>%
  arrange(USUBJID, EXSTDTC_IMP) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    USUBJID,
    TRTSDTM = EXSTDTC_IMP,
    TRTSTMF = na_if(TRTSTMF, "")
  )

#Step3 Merge to ADSL
adsl <- adsl %>%
  left_join(trtsdtm, by = "USUBJID")


# Create LSTAVLDT
#Step1 Derive last valid vital signs date
vs_last <- vs %>%
  filter(
    !(is.na(VSSTRESN) & (is.na(VSSTRESC) | VSSTRESC == "")),
    nchar(substr(VSDTC, 1, 10)) == 10
  ) %>%
  mutate(VSDT = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(VSDT = max(VSDT), .groups = "drop")

#Step2 Derive last AE onset date
ae_last <- ae %>%
  filter(
    nchar(substr(AESTDTC, 1, 10)) == 10
  ) %>%
  mutate(AEDT = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(AEDT = max(AEDT), .groups = "drop")

#Step3 Derive last disposition date
ds_last <- ds %>%
  filter(
    nchar(substr(DSSTDTC, 1, 10)) == 10
  ) %>%
  mutate(DSDT = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(DSDT = max(DSDT), .groups = "drop")

#Step4 Derive last treatment date by using the ex_trt data
trtedtm <- ex_trt %>%
  arrange(USUBJID, EXDTM) %>%
  group_by(USUBJID) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    USUBJID,
    TRTEDTM = EXDTM,
  )

#Step5 Derive LSTAVLDT
adsl <- adsl %>%
  left_join(vs_last, by = "USUBJID") %>%
  left_join(ae_last, by = "USUBJID") %>%
  left_join(ds_last, by = "USUBJID") %>%
  left_join(trtedtm, by = "USUBJID") %>%
  rowwise() %>%
  mutate(
    LSTAVLDT = max(
      c(VSDT, AEDT, DSDT, TRTEDTM),
      na.rm = TRUE
    )
  ) %>%
  ungroup() 

#Keep the variables we need
adsl <- adsl %>%
  select(
    STUDYID, USUBJID, SUBJID, AGEGR9, AGEGR9N, TRTSDTM, TRTSTMF, ITTFL, LSTAVLDT
  )

#Output ADSL
write.csv(adsl, "ADSL.csv", row.names = FALSE)


cat("Program completed successfully.\n")
  },

error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  }
)

cat("Program ended:", Sys.time(), "\n")

sink(type = "message")
sink()
