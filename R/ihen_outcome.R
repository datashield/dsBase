library(dplyr)
library(tidyr)
library(ggplot2)

# --- Simplified Web Table 7 (with INMA + EDEN collapsed) ---
bmi_data <- data.frame(
  Cohort = c("RHEA","CHOP","SWS","GECKO","Raine","INMA","EDEN","GENR",
             "ALSPAC","ABCD","NFBC66","NFBC86","BiB","ELFE","DNBC","MoBa"),
  n0_1  = c(974,1668,2942,2738,2303,1910,1760,7230,1420,5669,7379,5141,12959,17795,56821,85079),
  n2_3  = c(684,938,2701,2212,614,1177,1521,6466,1221,4763,5809,4739,6225,10773,0,45673),
  n4_7  = c(887,1092,2166,2309,2088,1634,1278,6572,5682,4754,7268,7110,10539,10192,43164,49728),
  n8_13 = c(334,755,1209,2180,1988,1043,904,5723,9585,3603,7239,4750,5592,3360,44177,33473),
  n14_17= c(NA,NA,NA,NA,1623,NA,NA,NA,7675,NA,7035,5760,NA,NA,6508,NA)
)

# --- Reshape ---
bmi_long <- bmi_data %>%
  pivot_longer(cols = starts_with("n"), names_to = "Age_group", values_to = "n") %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("n0_1", "n2_3", "n4_7", "n8_13", "n14_17"),
                            labels = c("0–1 years", "2–3 years", "4–7 years", "8–13 years", "14–17 years"))) %>%
  drop_na(n)

# --- Order cohorts by total contribution ---
bmi_long <- bmi_long %>%
  group_by(Cohort) %>%
  mutate(total_n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(total_n)

# --- Split into 5 cumulative stages ---
n_cohorts <- n_distinct(bmi_long$Cohort)
stage_breaks <- round(seq(1, n_cohorts, length.out = 5))  # 5 roughly equal steps

# --- Fixed axis limits for identical scaling ---
ymax <- bmi_long %>%
  group_by(Age_group) %>%
  summarise(total = sum(n)) %>%
  summarise(max_total = max(total)) %>%
  pull(max_total)

# --- Loop to create 5 plots ---
for (i in seq_along(stage_breaks)) {
  
  included <- unique(bmi_long$Cohort)[1:stage_breaks[i]]
  plot_data <- bmi_long %>% filter(Cohort %in% included)
  
  p <- ggplot(plot_data, aes(x = Age_group, y = n, fill = Cohort)) +
    geom_bar(stat = "identity", width = 0.7, color = "white") +
    scale_y_continuous(labels = scales::comma, limits = c(0, ymax)) +
    scale_fill_viridis_d(option = "turbo", direction = -1) +
    labs(
      x = "Child age group",
      y = "Number of BMI z-score observations",
      fill = "Cohort"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "right",
      plot.title = element_blank()
    )
  
  ggsave(sprintf("bmi_stacked_stage_%02d.png", i), p, width = 7, height = 5, dpi = 300)
}
