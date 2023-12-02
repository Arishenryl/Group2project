pacbio_summary <- group2dataset %>%
+     filter(Instrument == "PacBio") %>%
+     summarize(
+         mean_error = mean(Error_counts),
+         median_error = median(Error_counts),
+         sd_error = sd(Error_counts)
+     )

illumina_summary <- group2dataset %>%
+     filter(Instrument == "Illumina") %>%
+     summarize(
+         mean_error = mean(Error_counts),
+         median_error = median(Error_counts),
+         sd_error = sd(Error_counts)
+     )
