library(tidyverse)
library(readxl)

# Load subclass-level metadata.
subclass_df <- read_csv("subclass.csv",
                        col_types = cols(subclass = col_character(), .default = col_double())) %>%
  rename(subclass_original = subclass) %>%
  mutate(subclass = str_remove(subclass_original, "^\\d+\\s+")) %>%
  select(subclass_original, subclass, everything())

# Load dictionary mapping acronyms to full names (https://brainmapportal-live-4cc80a57cd6e400d854-f7fdcae.divio-media.net/filer_public/c8/42/c8421c8e-72d8-4781-8505-8770e88671a7/whole_mouse_brain_acronym_june_2024.xlsx).
dictionary <- read_excel("whole_mouse_brain_acronym_june_2024.xlsx")

# Match and expand acronyms within each subclass string to full descriptive names.
# Steps:
#   1. Split subclass into whitespace-delimited tokens.
#   2. Match each token to dictionary$Acronym.
#   3. Retain matched rows in original token order.
#   4. Collapse multiple matches to a semicolon-delimited descriptor string.
match_acronyms <- function(subclass_string, dict) {
  parts <- str_split(subclass_string, " ")[[1]]
  
  matches <- tibble(part = parts, position = seq_along(parts)) %>%
    left_join(dict, by = c("part" = "Acronym")) %>%
    filter(!is.na(`Full Name`)) %>%         # keep only successful matches
    distinct(`Full Name`, .keep_all = TRUE) # avoid duplicates if acronyms repeat
  
  if (nrow(matches) > 0) {
    str_c(matches$`Full Name`, collapse = "; ")
  } else {
    NA_character_
  }
}

# Apply acronym matching and derive neuron_type classifications.
subclass_df <- subclass_df %>%
  # Add descriptive full-name expansion (semicolon-separated).
  mutate(descriptor = map_chr(subclass, ~ match_acronyms(.x, dictionary))) %>%
  # Extract the final term of descriptor.
  mutate(
    last_term = if_else(
      !is.na(descriptor),
      str_split(descriptor, ";\\s*") %>% map_chr(last),
      NA_character_
    ),
    # neuron_type is extracted from the final term's structure "<type> neuron".
    neuron_type = str_extract(last_term, ".+(?=\\s+neuron)"),
    # Standardize formatting (e.g., "glutamatergic neuron-gabaergic neuron" → "glutamatergic-gabaergic").
    neuron_type = str_replace_all(neuron_type, "\\s+neuron-", "-"),
    # Default to "NN" for non-neuronal or unmatched subclasses.
    neuron_type = if_else(is.na(neuron_type), "NN", neuron_type)
  ) %>%
  select(-last_term)

# Report subclasses with no acronym matches (dictionary incomplete or subclass malformed).
no_matches <- subclass_df %>%
  filter(is.na(descriptor))

if (nrow(no_matches) > 0) {
  cat("\nWarning:",
      nrow(no_matches),
      "subclass(es) had no dictionary matches:\n\n")
  print(no_matches %>% select(subclass))
} else {
  cat("\nAll subclasses had at least one dictionary match\n")
}

# Display the processed subclass_df for inspection.
view(subclass_df)

# Extract unique neuron types excluding the non-neuronal "NN".
# Represents the set of neuron-type groups recognized by dictionary parsing.
neuron_types <- subclass_df %>%
  filter(neuron_type != "NN") %>%
  pull(neuron_type) %>%
  unique() %>%
  sort()

cat("\n", length(neuron_types), "unique neuron types found:\n")
print(neuron_types)

# Export the processed table for downstream use.
write.csv(subclass_df, "brainwide_biccn.csv")
