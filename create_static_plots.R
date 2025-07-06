
### Character Set Barplot

custom_colors <- scales::alpha(brewer.pal(n = 4, name = "BuGn"), alpha = 0.8)

custom_colors <- c(custom_colors[4:2], rep(custom_colors[1], nrow(count_character_sets) - 3))

char_set_barplot <- ggplot(count_character_sets, aes(x = idx, y = Freq, fill = factor(idx))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = custom_colors,
    breaks = c("1", "2", "3"),
    labels = c(
      "1" = "Heterosexual Man, 35-44",
      "2" = "Heterosexual Man, 45-54",
      "3" = "Heterosexual Woman, 35-44"
    )
  ) +
  theme_minimal() +
  labs(y = "Number of Reviewers", x = "Character Set", fill = "Character Set") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.65, 0.65),
    axis.text.x = element_blank(), # Remove x-axis text
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    legend.title = element_text(size = 12), # Increase legend title size
    legend.text = element_text(size = 10) # Increase legend text size
  )

## Create Review Type Histogram
custom_colors <- scales::alpha(brewer.pal(n = 4, name = "BuGn"), alpha = 0.6)

review_type_hist <- ggplot(average_score_per_review, aes(x = score, fill = review_type)) +
  geom_histogram(binwidth = 0.5, position = "identity") +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Average Review Score",
    y = "Count",
    fill = "review_type"
  ) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.60, 0.85),
    legend.title=element_blank(),
    legend.text = element_text(size = 10)
  )


## Create Review Diff Histogram

review_diff_hist <- ggplot(first_last_df, aes(x = score_diff)) +
    geom_histogram(binwidth = 0.5, fill = rgb(0.3, 0.6, 0.5, 0.7)) +
    labs(
      title = "Increase from Initial Reviews",
      x = "Average Score Difference",
      y = "Frequency"
    ) +
    theme_minimal()


## Create Reviews Per Genre Barchart
 review_per_genre_barchart <- ggplot(data = average_score_per_review, aes(
    x = factor(genre),
    fill = factor(review_type)
  )) +
    geom_bar(position = "stack") + # Stacked bar plot
    scale_fill_brewer(palette = "BuGn") +
    labs(
      x = "Genre",
      y = "Number of Reviews",
      fill = "Review Type") + 
    theme_minimal() +
    guides(fill = guide_legend(ncol = 1)) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.25, 0.75),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust=1),
      legend.title=element_blank()
    )

 ## Create Movies Per Genre Barchart
 movies_per_genre_barchart <- ggplot(data = all_movies, aes(x = factor(genre))) +
     geom_bar(fill = rgb(0.3, 0.6, 0.5, 0.7)) +
     theme_minimal() +
     labs(y = "Number of Movies", x = "Genre") +
     theme(axis.text.x = element_text(angle = 45, hjust=1))
