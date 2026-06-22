library(ggplot2)
library(dplyr)
library(binom)

#Create log file
log_file <- "Question3_Program.log"

sink(log_file, split = TRUE)

cat("Program started:", Sys.time(), "\n")

#Use tryCatch function to catch any error messages
tryCatch(
  {

#Plot 1
ae_sev <- pharmaverseadam::adae %>%
  filter(!is.na(AESEV), !is.na(ACTARM)) %>%
  count(ACTARM, AESEV)

plot1 <-
  ggplot(ae_sev,
       aes(x = ACTARM,
           y = n,
           fill = AESEV)) +
  geom_col() +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment ARM",
    y = "Count of Adverse Events",
    fill = "AE Severity"
  ) +
  theme_minimal()

#Save the plot as a PNG file
ggsave(
  filename = "AE_Severity_Distribution.png",
  plot = plot1,
  width = 8,
  height = 6,
  dpi = 300
)



#Plot 2
adae_saff <- pharmaverseadam::adae %>%
  filter(SAFFL == "Y")

N <- n_distinct(adae_saff$USUBJID)

ae_summary <- pharmaverseadam::adae %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n") %>%
  slice_max(n, n = 10) %>%
  arrange(n)

ci <- binom.confint(
  ae_summary$n,
  N,
  methods = "exact"
)

ae_summary <- ae_summary %>%
  mutate(
    pct = 100 * n / N,
    lower = 100 * ci$lower,
    upper = 100 * ci$upper
  )

#Use ggplot to generate plot
plot2 <-
ggplot(
  ae_summary,
  aes(
    x = pct,
    y = reorder(AETERM, pct)
  )
) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0.2
  ) +
  geom_point(
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    x = paste0("Percentage of Patients (%)  [N=", N, "]"),
    y = NULL
  ) +
  theme_minimal()

#Save the plot as a PNG file
ggsave(
  filename = "Top 10 Most Frequent Adverse Events.png",
  plot = plot2,
  width = 8,
  height = 6,
  dpi = 300
)



cat("Program completed successfully.\n")
  },

error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  }
)

cat("Program ended:", Sys.time(), "\n")

sink(type = "message")
sink()







