
ggplot(group2dataset, aes(x = Variant, y = Position, color = Instrument, size = Error_counts)) +
  geom_segment(aes(xend = Variant, yend = 0), alpha = 0.7, size = 1) + 
  geom_point(aes(y = 0), alpha = 0.7, size = 3) +  
  labs(
    title = "Spike Gene Position vs Variant",
    x = "Variant",
    y = "Spike Gene Position",
    color = "Instrument",
    linewidth = "Error Counts"  
  ) + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  facet_wrap(~Instrument)



ggplot(group2dataset, aes(x = Variant, y = Position, color = Instrument, size = Error_counts)) +
  geom_point(alpha = 0.7) +  
  labs(
    title = "Spike Gene Position vs Variant",
    x = "Variant",
    y = "Spike Gene Position",
    color = "Instrument",
    size = "Error Counts"
  ) + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  facet_wrap(~Instrument)

