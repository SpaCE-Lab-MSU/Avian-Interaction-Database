
# Generate summary table for generic interaction categories and standardized colors
generate_interaction_table <- function(){
  interaction_categories <- data.frame(
    interaction = c(
      # Trophic (Dark Red)
      "predation",
      # Mobbing (Pink)
      "mobbing",
      # Competition (Dark Blues)
      "competition",
      # Facilitation (Greens)
      "facilitation", "facilitation-mixed flocking", "facilitation-comigration",
      "facilitation-foraging", "communal nesting", "communal roosting",
      # Commensalism (Purples)
      "commensalism", "commensalism-scavenge", "commensalism-chick adoption",
      "commensalism-call mimicry",
      # Parasitism (Oranges/Browns)
      "kleptoparasitism", "brood parasitism", "nest takeover",
      # Amenalism (Black)
      "amensalism",
      # Other (Grays)
      "co-occur", "hybridization", "copulation", "courtship", "play", "accidental killing"
    ),
    category = c(
      # Trophic
      "Trophic",
      # Mobbing
      "Mobbing",
      # Competition
      "Competition",
      # Facilitation
      rep("Facilitation", 6),
      # Commensalism
      rep("Commensalism", 4),
      # Parasitism
      rep("Parasitism", 3),
      # Amenalism
      "Amenalism",
      # N/A
      rep("Other", 6)
    ),
    color = c(
      # Trophic (Reds/Oranges)
      "#800000",
      # Mobbing (Teal)
      "#E0115F",
      # Competition (Dark Blues)
      "#00008B",
      # Facilitation (Greens)
      "#006400", "#228B22", "#32CD32", "#00A86B", "#50C878", "#3CB371",
      # Commensalism (Purples)
      "#4B0082", "#6A5ACD", "#9370DB", "#BA55D3",
      # Parasitism (Oranges/Browns)
      "#D2691E", "#DEB887", "#F4A460",
      # Amenalism (Black)
      "#000000",
      # N/A (Grays)
      "#808080", "#696880", "#373737", "#7C6E7F", "#9897A9", "#787276"
    ),
    category_color = c(
      # Trophic
      "#800000",
      # Mobbing
      "#E0115F",
      # Competition
      "#00008B",
      # Facilitation
      rep("#006400", 6),
      # Commensalism
      rep("#4B0082", 4),
      # Parasitism
      rep("#D2691E", 3),
      # Amenalism
      "#000000",
      # Other
      rep("#808080", 6)
    ),
    stringsAsFactors = FALSE
  )

  # Define category order for plotting
  category_order <- c("Trophic", "Mobbing", "Competition", "Facilitation",
                      "Commensalism", "Parasitism", "Amenalism",
                      "Other")

  interaction_categories$category <- factor(interaction_categories$category,
                                            levels = category_order)
  return(interaction_categories)
}

interaction_categories <- generate_interaction_table()
