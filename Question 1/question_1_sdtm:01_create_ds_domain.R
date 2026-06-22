library(pharmaverseraw)
library(dplyr)
library(sdtm.oak)


#Create log file
log_file <- "Question1_Program.log"

sink(log_file, split = TRUE)

cat("Program started:", Sys.time(), "\n")

#Use tryCatch function to catch any error messages
tryCatch(
  {

#Get the raw data from pharmaverseraw package
ds_raw <- pharmaverseraw::ds_raw

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "disposition"
  )

#Input the study_ct 
study_ct <- read.csv("sdtm_ct.csv")

#Derive all variables in SDTM.DS
ds_raw2 <- ds_raw %>%
  rename(STUDYID=STUDY) %>%
  mutate(DOMAIN = "DS") %>%
  mutate(USUBJID = paste0("01-", PATNUM)) %>%
  mutate(DSTERM = toupper(IT.DSTERM)) %>%
  mutate(DSDECOD = toupper(if_else(is.na(OTHERSP), IT.DSDECOD, OTHERSP))) %>%
  mutate(DSCAT = if_else(IT.DSDECOD == "Randomized", "PROTOCOL MILESTONE", "DISPOSITION EVENT")) %>%
  mutate(
      DSDECOD = toupper(if_else(!is.na(OTHERSP), OTHERSP, DSDECOD)),
      DSTERM = toupper(if_else(!is.na(OTHERSP), OTHERSP, DSTERM))
  ) %>%
  mutate(DSCAT = if_else(!is.na(OTHERSP), "OTHER EVENT", DSCAT)) %>%
  mutate(DSTERM = toupper(if_else(is.na(OTHERSP), IT.DSTERM, DSTERM))) %>%
  mutate(DSSTDTC = format(as.Date(IT.DSSTDAT, format = "%m-%d-%Y"), "%Y-%m-%d")) %>%
  mutate(DSDTC_pre = format(as.Date(DSDTCOL, format = "%m-%d-%Y"), "%Y-%m-%d")) %>%
  mutate(
    DSDTC = if_else(
    !is.na(DSTMCOL),
    paste0(format(as.Date(DSDTC_pre), "%Y-%m-%d"),"T",DSTMCOL),
    DSDTC_pre
    )
  ) %>%
  mutate(VISIT = toupper(INSTANCE)) %>%
  mutate(
    VISITNUM = case_when(
      VISIT == "SCREENING 1" ~ 1.0,
      VISIT == "BASELINE" ~ 3.0,
      VISIT == "WEEK 2"   ~ 4.0,
      VISIT == "WEEK 4"   ~ 5.0,
      VISIT == "WEEK 6"   ~ 7.0,
      VISIT == "WEEK 8"   ~ 8.0,
      VISIT == "WEEK 12"   ~ 9.0,
      VISIT == "WEEK 16"   ~ 10.0,
      VISIT == "WEEK 20"   ~ 11.0,
      VISIT == "WEEK 24"   ~ 12.0,
      VISIT == "WEEK 26"   ~ 13.0,
      VISIT == "RETRIEVAL"   ~ 201.0,
      VISIT == "Unscheduled 1.1"   ~ 1.1,
      VISIT == "Unscheduled 3.1"   ~ 3.1,
      VISIT == "Unscheduled 4.1"   ~ 4.1,
      VISIT == "Unscheduled 5.1"   ~ 5.1,
      VISIT == "Unscheduled 6.1"   ~ 6.1,
      VISIT == "AMBUL ECG REMOVAL"   ~ 6.0,
      VISIT == "Ambul ECG Placement"   ~ 3.5,
      TRUE ~ NA_real_
    )
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC")
  ) 


#Keep the variables we need
ds_raw3 <- ds_raw2 %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
  )

#Output ADSL
write.csv(ds_raw3, "DS.csv", row.names = FALSE)


cat("Program completed successfully.\n")
  },

error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  }
)

cat("Program ended:", Sys.time(), "\n")

sink(type = "message")
sink()

