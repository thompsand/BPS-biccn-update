library(tidyverse)
library(readxl)
library(cli)

# Load cluster-level quantitative data.
cluster_df <- read_csv("cluster.csv",
                       col_types = cols(cluster = col_character(), .default = col_double()))

# Load the annotation table (https://allen-brain-cell-atlas.s3-us-west-2.amazonaws.com/metadata/WMB-taxonomy/20231215/cl.df_CCN202307220.xlsx) mapping each cluster ID to subclass/class identifiers and marker combinations.
glossary <- read_excel("cl.df_CCN202307220.xlsx")

# Merge annotation metadata into cluster_df.
# Add: subclass_id_label, class_id_label, cluster.markers.combo.
cluster_df <- cluster_df %>%
  left_join(
    glossary %>% select(
      cluster_id_label,
      subclass_id_label,
      class_id_label,
      cluster.markers.combo
    ),
    by = c(cluster = "cluster_id_label")
  )

# Identify clusters with no glossary match—indicates missing or inconsistent IDs.
missed_clusters <- cluster_df %>% filter(is.na(subclass_id_label))

if (nrow(missed_clusters) > 0) {
  cli_alert_warning("{nrow(missed_clusters)} cluster(s) had no glossary match.")
  missed_clusters %>% select(cluster) %>% cli::cli_dl()
} else {
  cli_alert_success("All clusters matched successfully to glossary.")
}

# Merge subclass-level metadata from subclass_df (preloaded from the other script).
# subclass_df provides:
#   - subclass_original: raw subclass label
#   - subclass: subclass cleaned of numeric prefix
#   - descriptor: semicolon-delimited full names for all acronyms found
#   - neuron_type: parsed from the final term in descriptor (e.g. "glutamatergic", "GABAergic", "NN")
cluster_df <- cluster_df %>%
  left_join(
    subclass_df %>% select(subclass_original, descriptor, neuron_type),
    by = c(subclass_id_label = "subclass_original")
  )

# Flag subclasses whose subclass_id_label does not exist in subclass_df.
# This indicates upstream mismatch in annotation or ID formatting.
missed_subclasses <- cluster_df %>% filter(is.na(descriptor))

if (nrow(missed_subclasses) > 0) {
  cli_alert_warning("{nrow(missed_subclasses)} subclass(es) had no match in subclass_df.")
  missed_subclasses %>% select(subclass, subclass_id_label) %>% distinct() %>% cli::cli_dl()
} else {
  cli_alert_success("All subclasses matched successfully to subclass_df.")
}

# Partition clusters into broad neuron-type groups using  neuron_type labels.
glut_df     <- cluster_df %>% filter(neuron_type == "glutamatergic")
gaba_df     <- cluster_df %>% filter(neuron_type == "GABAergic")
neuromod_df <- cluster_df %>% filter(!neuron_type %in% c("glutamatergic", "GABAergic", "NN"))
NN_df       <- cluster_df %>% filter(neuron_type == "NN")  # non-neuronal classes

# Subdivide NN_df into biologically relevant non-neuronal groups.
astrocyte_df <- NN_df %>% filter(str_detect(class_id_label, "Astro-Epen"))
oligo_df     <- NN_df %>% filter(str_detect(class_id_label, "OPC-Oligo"))
oec_df       <- NN_df %>% filter(str_detect(class_id_label, "OEC"))
vascular_df  <- NN_df %>% filter(str_detect(class_id_label, "Vascular"))
immune_df    <- NN_df %>% filter(str_detect(class_id_label, "Immune"))

# Summarize the number of clusters in each subset.
cli_h2("Subset creation summary")
cli_dl(
  list(
    "glut_df"      = nrow(glut_df),
    "gaba_df"      = nrow(gaba_df),
    "neuromod_df"  = nrow(neuromod_df),
    "NN_df"        = nrow(NN_df),
    "astrocyte_df" = nrow(astrocyte_df),
    "oligo_df"     = nrow(oligo_df),
    "oec_df"       = nrow(oec_df),
    "vascular_df"  = nrow(vascular_df),
    "immune_df"    = nrow(immune_df)
  )
)

# Retrieve all objects in the environment that end in "_df".
df_names <- ls(pattern = "_df$")

# Create output directory for CSV exports, ignoring warning if it already exists.
output_dir <- "subsets"
dir.create(output_dir, showWarnings = FALSE)

# Write each *_df object to subsets/<name>.csv.
walk(df_names, function(name) {
  df <- get(name)
  short <- sub("_df$", "", name)
  write_csv(df, file.path(output_dir, paste0(short, ".csv")))
})

cli_alert_success("Saved {length(df_names)} subset data frames to '{output_dir}'.")
