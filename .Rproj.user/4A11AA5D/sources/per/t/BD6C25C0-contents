#script for analyses for 
#Drapeau Picard et al. - Nonlethal sampling of wild bees: a scoping review of methods, biases and reporting practices

library(tidyverse)
library(readxl)
library(ggExtra)
library(wordcloud2)
library(ComplexUpset)
library(patchwork)
library(htmlwidgets)
library(webshot2)
library(png)
library(grid)

dataset_raw <- read_excel("~/RStudio/nonlethal_revlitt/nonlethal_monitoring_article_data.xlsx", 
                          na = "NA")

dataset_unique <- 
  dataset_raw %>%
  distinct(ref_num, .keep_all = TRUE) #keep only 1 line per study based on reference name

dataset_unique_methods <- dataset_raw %>% 
  filter(original_data == "1") #keep only studies with original data

#figure 1 - through landscapes and years
years_landscape <- dataset_unique_methods[,c("year", "habitat_class")]

long_years_landscape <- years_landscape %>%
  mutate(
    year = strsplit(as.character(year), ";\\s*") |> map(trimws),
    landscape = strsplit(as.character(habitat_class), ";\\s*") |> map(trimws)) %>%
  rowwise() %>%
  mutate(combinaisons = list(expand.grid(year = year, landscape = landscape))) %>%
  unnest(combinaisons, names_sep = "_") %>%
  select(year = combinaisons_year, landscape = combinaisons_landscape)%>%
  drop_na() %>%            
  mutate(year = as.numeric(trimws(year)))%>%
  drop_na()%>%
  as.data.frame()

landscape_freq <- long_years_landscape %>%
  group_by(year, landscape) %>%
  summarise(frequency = n(), .groups = "drop")%>%
  mutate(year = factor(year, levels = sort(unique(year))))  

landscapeyear_plot <- ggplot(landscape_freq,
       aes(x = factor(year),
           y = frequency,
           fill = landscape)) +
  geom_col(color = "black", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sampling year",
    y = "Number of studies",
    fill = "Landscape"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.2, 0.7),
    legend.title = element_text(face = "bold")
  )

ggsave(
  filename = "fig_1_landscapeyear_plot.png",
  plot = landscapeyear_plot,
  width = 20,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)

#figure 2A - research topics
topic <- dataset_unique %>%
  separate_rows(study_topic, sep = ";\\s*") %>%
  mutate(study_topic = trimws(study_topic)) %>%
  count(study_topic)%>%
  mutate(
    percent = 100 * n / sum(n)
  ) %>%
  arrange(desc(n))

pie(topic$n, labels = topic$study_topic)

topic <- dataset_unique %>%
  separate_rows(study_topic, sep = ";\\s*") %>%
  mutate(study_topic = trimws(study_topic)) %>%
  filter(!is.na(study_topic), study_topic != "") %>%
  count(study_topic, sort = TRUE) %>%
  mutate(
    percent = 100 * n / sum(n),
    label = paste0(n, " (", round(percent, 1), "%)")
  )

topic_plot <- ggplot(topic, aes(x = fct_reorder(study_topic, n), y = n)) +
  geom_col(width = 0.75, fill = "#2C7FB8") +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.35))
  ) +
  labs(
    x = NULL,
    y = "Number of studies"
  ) +
  publication_theme +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    plot.margin = margin(10, 40, 10, 10))


#figure 2B - wordcloud of keywords
keywords <- dataset_unique %>%
  separate_rows(keywords, sep = ";\\s*") %>%
  mutate(keywords = trimws(keywords)) %>%
  mutate(keywords = case_when(
    keywords %in% c("bumblebees", "bumble bee", "bumble bees", "Bombus sp.", "Bombus spp.") ~ "Bombus",
    keywords %in% c("agro-ecosystems") ~ "agroecosystems",
    keywords %in% c("bee") ~ "bees",
    keywords %in% c("pollinator") ~ "pollinators",
    keywords %in% c("DNA barcoding") ~ "barcoding",
    keywords %in% c("agri-environment schemes") ~ "agrienvironment",
    keywords %in% c("acoustics signals") ~ "acoustics",
    keywords %in% c("distance modelling") ~ "distance models",
    keywords %in% c("hoverflies") ~ "Syrphidae",
    keywords %in% c("flower") ~ "flowers",
    keywords %in% c("honey bee", "honey bees", "honeybee") ~ "Apis mellifera",
    keywords %in% c("flower visitation") ~ "flower visit",
    keywords %in% c("occupancy model", "occupancy modeling") ~ "occupancy modelling",
    keywords %in% c("plant-insect interactions") ~ "insect-plant interactions",
    keywords %in% c("microsatellites") ~ "microsatellites",
    keywords %in% c("virtual reality") ~ "virtual reality",
    keywords %in% c("survey") ~ "surveys",
    keywords %in% c("urbanisation") ~ "urbanization",
    keywords %in% c("North American Midwest") ~ "Midwest",
    TRUE ~ keywords)) %>%
  filter(!keywords %in% c("wild bees", "wild bee", "bees", "sampling", "monitoring", "bee monitoring"))%>%
  count(keywords)%>%
  drop_na()

keywords_plot <- keywords %>%
  filter(n > 1) %>%
  mutate(
    freq_scaled = log(n)
  )

wordcloud_plot <- wordcloud2(data=keywords_plot, size = 0.5)

saveWidget(wordcloud_plot, "wordcloud_plot.html", selfcontained = TRUE)

webshot(
  "wordcloud_plot.html",
  "wordcloud_plot.png",
  vwidth = 500,
  vheight = 500
)

img <- png::readPNG("wordcloud_plot.png")

wordcloud_plot <- ggplot() +
  annotation_raster(
    img,
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  ) +
  theme_void()

fig2 <-
  topic_plot +
  wordcloud_plot +
  plot_layout(widths = c(1.5, 1.5)) +
  plot_annotation(tag_levels = "A")

ggsave(
  filename = "fig_2_wordcloud.png",
  plot = fig2,
  width = 25,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)

#figure 3 - sampled taxa
#UpSet plot####
df_study <- dataset_unique %>%
  mutate(
    ref_num = as.character(ref_num),
    target_taxa = str_squish(target_taxa)
  ) %>%
  group_by(ref_num) %>%
  summarise(
    taxa_text = str_c(target_taxa, collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(
    taxa_text = str_to_lower(taxa_text)
  )

df_upset_twolevel <- df_study %>%
  mutate(
    taxa_Bombus_sp = str_detect(
      taxa_text,
      regex("\\bbombus\\s+sp\\.?(?=\\s*(;|$))", ignore_case = TRUE)
    ),
    taxa_Bombus_spp = str_detect(
      taxa_text,
      regex("\\bbombus\\s+spp\\.?(?=\\s*(;|$))", ignore_case = TRUE)    ) |
      str_detect(
        taxa_text,
        regex("\\bbombus\\s+(?!sp\\.?\\b|spp\\.?\\b)[a-z][a-z-]+\\b", ignore_case = TRUE)      ),
    taxa_Apis_mellifera = str_detect(
      taxa_text,
      "\\bapis mellifera\\b"    ),
    taxa_Peponapis = str_detect(
      taxa_text,
      "\\bpeponapis\\b"    ),
    taxa_All_bees = str_detect(
      taxa_text,
      str_c(
        "\\banthophila\\b",
        "\\bapoidea\\b",
        sep = "|"      )    ),
    taxa_Xylocopa = str_detect(
      taxa_text,
      str_c(
        "\\bxylocopa\\b",
        sep = "|"      )    ),
    taxa_Megachile = str_detect(
      taxa_text,
      str_c(
        "\\bmegachile\\b",
        sep = "|"      )    ),
    taxa_Osmia = str_detect(
      taxa_text,
      str_c(
        "\\bosmia\\b",
        sep = "|"      )    ),
    taxa_Melissodes = str_detect(
      taxa_text,
      str_c(
        "\\bmelissodes\\b",
        sep = "|"      )    ),
    taxa_Colletidae = str_detect(
      taxa_text,
      str_c(
        "\\bcolletes\\b",
        "\\bcolletidae\\b",
        sep = "|"      )    ),
    taxa_Euglossa = str_detect(
      taxa_text,
      str_c(
        "\\beuglossa\\b",
        sep = "|"      )    ),
    taxa_Andrenidae = str_detect(
      taxa_text,
      str_c(
        "\\bandrenidae\\b",
        "\\bandrena\\b",
        sep = "|"      )    ),
    taxa_Agapostemon = str_detect(
      taxa_text,
      str_c(
        "\\bagapostemon\\b",
        sep = "|"      )    ),
    taxa_Nomadinae = str_detect(
      taxa_text,
      str_c(
        "\\bnomadinae\\b",
        sep = "|"      )    ),
    taxa_Lasioglossum = str_detect(
      taxa_text,
      str_c(
        "\\blasioglossum\\b",
        sep = "|"      )    ),
    taxa_Augochloropsis = str_detect(
      taxa_text,
      str_c(
        "\\baugochloropsis\\b",
        sep = "|"      )    ),
    taxa_Halictidae_spp = str_detect(
      taxa_text,
      str_c(
        "\\bgreen sweat bees\\b",
        "\\bgrey sweat bees\\b",
        "\\bhalictidae\\b",
        sep = "|"      )    ),
    taxa_Other_Apidae = str_detect(
      taxa_text,
      str_c(
        "\\bapidae\\b",
        "\\bcentris\\b",
        "\\beucera\\b",
        sep = "|"
      )
    ),
    taxa_Syrphidae = str_detect(
      taxa_text,
      "\\bsyrphidae\\b"  ),
    taxa_Other_Diptera = str_detect(
      taxa_text,
      "\\bdiptera\\b|\\bculicidae\\b|\\bbombyliidae\\b|\\basilidae\\b"  ),
    taxa_Lepidoptera = str_detect(
      taxa_text,
      "\\blepidoptera\\b|\\brhopalocera\\b" ),
    taxa_Coleoptera = str_detect(
      taxa_text,
      "\\bcoleoptera\\b"),
    taxa_Wasps = str_detect(
      taxa_text,
      str_c(
        "\\bwasps\\b",
        "\\bvespinae\\b",
        "\\bsphecidae\\b",
        "\\baculeate\\b",
        sep = "|"  )),
    taxa_Hymenoptera = str_detect(
      taxa_text,
      str_c(
        "\\bhymenoptera\\b",
        "\\bcavity-nesting hymenopterans\\b",
        sep = "|"  )),
    taxa_Hemiptera = str_detect(
      taxa_text,
      "\\bhemiptera\\b|\\bheteroptera\\b"
    ),
    taxa_Other_taxa = str_detect(
      taxa_text,
      str_c(
        "\\bodonata\\b",
        "\\bformicidae\\b",
        sep = "|" ) ),
    morpho_Bees_unspecified = str_detect(
      taxa_text,
      str_c(
        "\\bwild bees\\b",
        "\\bother wild bees\\b",
        "\\bother bees\\b",
        "\\bsolitary bees\\b",
        "\\blarge solitary bees\\b",
        "\\bsmall bees\\b",
        sep = "|" ) ),
    morpho_Size_colour_bees = str_detect(
      taxa_text,
      str_c(
        "\\blarge black bee\\b",
        "\\bsmall black bee\\b",
        "\\blarge striped bee\\b",
        "\\bsmall striped bee\\b",
        "\\bgreen bee\\b",
        sep = "|"   )  ),
    morpho_Pollinators_broad = str_detect(
      taxa_text,
      str_c(
      "\\bpollinators\\b",
      "\\binsecta\\b",
      "\\banimalia\\b",
      "\\barthropoda\\b",
      "\\bseveral orders\\b",
      sep = "|")),
    morpho_Flying_insects = str_detect(
      taxa_text,
      "\\bflying insects\\b"),
    morpho_Other_unknown = str_detect(
      taxa_text,
      "(^|;)\\s*(other|unknown|other visitors|other non-bee insects)\\s*(;|$)"
    )
  )

sets_taxa <- names(df_upset_twolevel) %>%
  str_subset("^taxa_") %>%
  setdiff("taxa_text")

sets_morpho <- names(df_upset_twolevel) %>%
  str_subset("^morpho_")

sets_all <- c(sets_taxa, sets_morpho)

set_counts <- df_upset_twolevel %>%
  summarise(across(all_of(sets_all), sum)) %>%
  pivot_longer(
    everything(),
    names_to = "group",
    values_to = "n_studies")

sets_all_plot <- set_counts %>%
  filter(n_studies > 0) %>%
  pull(group)


sets_taxa_plot <- sets_all_plot[str_detect(sets_all_plot, "^taxa_")]
sets_morpho_plot <- sets_all_plot[str_detect(sets_all_plot, "^morpho_")]

df_upset_twolevel %>%
  summarise(across(all_of(sets_taxa_plot), sum)) %>%
  pivot_longer(
    everything(),
    names_to = "group",
    values_to = "n_studies"
  ) %>%
  arrange(desc(n_studies))

p_twolevel <- upset(
  df_upset_twolevel,
  intersect = sets_all_plot,
  name = NULL,
  min_size = 1,
  width_ratio = 0.25,
  height_ratio = 3,
  queries = c(
    lapply(
      sets_taxa_plot,
      function(x) upset_query(
        set = x,
        fill = "#0000FF",
        color = "#0000FF"
      )
    ),
    lapply(
      sets_morpho_plot,
      function(x) upset_query(
        set = x,
        fill = "#FF0000",
        color = "#FF0000"
      )
    )
  ),
  base_annotations = list(
    "Intersection size" = intersection_size(counts = FALSE)
  ),
  set_sizes = upset_set_size() +
    labs(x = NULL),
  themes = upset_modify_themes(
    list(
      intersections_matrix = theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
      ),
      `Intersection size` = theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        theme_classic()
      )
    )
  )
)&
  theme(
    text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

p_twolevel

ggsave(
  filename = "figure_3_upset.png",
  plot = p_twolevel,
  width = 20,
  height = 20,
  units = "cm",
  dpi = 600,
  bg = "white"
)

#####

#figure 4 - sampled vs analyzed groups
n_classes <- dataset_unique_methods%>%
  select(n_apriori_classes, n_class_analyses)%>%
  filter(!is.na(n_apriori_classes))%>%
  separate_rows(n_class_analyses, sep = ";\\s*") %>%
  mutate(n_class_analyses = trimws(n_class_analyses)) %>%
  mutate(
    n_apriori_classes = as.numeric(n_apriori_classes),
    n_class_analyses  = as.numeric(n_class_analyses),
    difference = n_apriori_classes - n_class_analyses)

fit <- lm(n_class_analyses ~ n_apriori_classes, data = n_classes)
sum_fit <- summary(fit)

r2  <- sum_fit$r.squared
p   <- coef(sum_fit)[2, "Pr(>|t|)"]
n   <- nobs(fit)

p_scatter <- ggplot(
  n_classes,
  aes(n_apriori_classes, n_class_analyses)) +
  geom_point(
    size = 2.5,
    alpha = 0.7,
    shape = 21,
    fill = "black",
    color = "black",
    stroke = 0.3) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 0.8,
    fill = "grey80") +
  scale_x_continuous(
    breaks = seq(0, 12, by = 2),
    limits = c(0, 12),
    minor_breaks = NULL,
    expand = c(0.02, 0.02)) +
  scale_y_continuous(
    breaks = seq(0, 9, by = 2),
    limits = c(0, 9),
    minor_breaks = NULL,
    expand = c(0.02, 0.02)) +
  labs(
    x = "Number of sampled groups",
    y = "Number of analyzed groups") +
  publication_theme +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 12),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5)
  )

ggMarginal(
  p_scatter,
  type = "boxplot",
  margins = "both",
  size = 5,
  outlier.colour = NA,
  fill = "white",
  color = "black"
)

n_classes %>%
  summarise(
    mean_sampled = mean(n_apriori_classes, na.rm = TRUE),
    sd_sampled   = sd(n_apriori_classes, na.rm = TRUE),
    mean_analyzed = mean(n_class_analyses, na.rm = TRUE),
    sd_analyzed   = sd(n_class_analyses, na.rm = TRUE)
  )

#figure 5 - tools and protocols
tool_protocol <- dataset_unique_methods %>%
  separate_rows(nonlethal_protocol, sep = ";\\s*") %>%
  mutate(nonlethal_protocol = trimws(nonlethal_protocol)) %>%
  separate_rows(nonlethal_tool, sep = ";\\s*") %>%
  mutate(nonlethal_tool = trimws(nonlethal_tool)) %>%
  filter(!is.na(nonlethal_protocol))%>%
  filter(nonlethal_protocol != "transects")%>%
  filter(!str_detect(nonlethal_protocol, "survey-point|extraction|mark"))%>%
  mutate(
    protocol_group = if_else(
    grepl("transects", nonlethal_protocol, ignore.case = TRUE),
    "Transects",
    "Other"),
    nonlethal_tool = recode(
      nonlethal_tool, "photo camera" = "photo cameras"
    ),
    nonlethal_tool = recode(
      nonlethal_tool, "3D video cameras" = "video cameras"
    ),
    nonlethal_tool = recode(
      nonlethal_tool, "net" = "catch and release"
    ),
    protocol_group = factor(
      protocol_group,
      levels = c("Transects", "Other")
  )) %>%
  count(nonlethal_protocol, nonlethal_tool)%>%
  group_by(nonlethal_protocol) %>%
  mutate(
    protocol_n = sum(n),
    protocol_label = paste0(nonlethal_protocol, " (n = ", protocol_n, ")")
  ) %>%
  ungroup() %>%
  mutate(
    protocol_label = forcats::fct_reorder(protocol_label, protocol_n)
  )

protocol_tool <- ggplot(tool_protocol,
       aes(x = protocol_label,
           y = n,
           fill = nonlethal_tool)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Protocol",
    y = "Number of studies",
    fill = "Nonlethal tool"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(
  filename = "figure_4_protocol_tool.png",
  plot = protocol_tool,
  width = 20,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)

#figure 6 - time vs distance-standardized protocol
protocol <- dataset_raw%>% 
  filter(original_data == "1")

protocol_time <- c("timed walks", "point-counts", "timed distance transects")

time_box <- protocol %>%
  select(ref_name, nonlethal_tool, standardization, nonlethal_protocol,
         nonlethal_sample_duration, total_observation_time) %>%
  distinct(ref_name, nonlethal_protocol, .keep_all = TRUE) %>%
  filter(nonlethal_protocol %in% protocol_time,
         ref_name != "Stothut et al. 2023") %>%
  mutate(
    sample = str_split(nonlethal_sample_duration, ";\\s*"),
    total  = str_split(total_observation_time,  ";\\s*")
  ) %>%
  unnest_longer(sample, indices_to = "set") %>%
  mutate(total = map2(total, set, ~ .x[.y])) %>%
  unnest(total) %>%
  mutate(
    sample = as.numeric(str_trim(sample)),
    total  = as.numeric(str_trim(total))) %>%
  pivot_longer(c(sample, total), names_to = "time", values_to = "value") %>%
  mutate(
    time = recode(
      time,
      sample = "Sample duration",
      total  = "Total time"
  )) %>%
  filter(!is.na(value))

distance_box <- protocol %>%
  select(
    ref_name, standardization, nonlethal_tool, nonlethal_protocol,
    nonlethal_sample_distance, total_observation_distance
  ) %>%
  filter(nonlethal_protocol %in% c("distance transects", "timed distance transects")) %>%
  distinct(ref_name, nonlethal_protocol, .keep_all = TRUE) %>%
  mutate(
    sample = str_split(nonlethal_sample_distance, ";\\s*"),
    total  = str_split(total_observation_distance, ";\\s*")
  ) %>%
  unnest_longer(sample, indices_to = "set") %>%
  mutate(total = map2(total, set, ~ .x[.y])) %>%
  unnest(total) %>%
  mutate(
    sample = as.numeric(str_trim(sample)),
    total  = as.numeric(str_trim(total))
  ) %>%
  pivot_longer(
    c(sample, total),
    names_to = "distance",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    distance = recode(
      distance,
      sample = "Sample distance",
      total  = "Total distance"
    ),
    distance = factor(
      distance,
      levels = c("Sample distance", "Total distance")
    )
  ) %>%
  filter(!str_detect(ref_name, "Chacon|Wood|Boone et al.")) %>%
  filter(!is.na(value))

time_distance_palette <- c(
  "Sample duration" = "#0072B2",
  "Total time" = "#D55E00",
  "Sample distance" = "#0072B2",
  "Total distance" = "#D55E00"
)

publication_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 13),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

time_violin <-
  ggplot(time_box, aes(x = nonlethal_protocol, y = value, fill = time)) +
  geom_violin(
    position = position_dodge(width = 0.8),
    alpha = 0.65,
    colour = "black",
    linewidth = 0.25,
    trim = FALSE
  ) +
  scale_fill_manual(values = time_distance_palette) +
  coord_cartesian(ylim = c(0, 350)) +
  labs(
    x = "Time-based protocols",
    y = "Survey length (minutes)",
    fill = NULL
  ) +
  publication_theme +
  theme(
        legend.position = c(0.2, 0.9),
        legend.background = element_rect(fill = "transparent"))

distance_violin <-
  ggplot(distance_box, aes(x = distance, y = value, fill = distance)) +
  geom_violin(
    alpha = 0.65,
    colour = "black",
    linewidth = 0.25,
    trim = FALSE
  ) +
  scale_fill_manual(values = time_distance_palette) +
  coord_cartesian(ylim = c(0, 400)) +
  labs(
    x = "Distance-based protocols",
    y = "Survey length (meters)",
    fill = NULL
  ) +
  publication_theme +
  theme(
    legend.position = "none"
  )

time_violin + distance_violin +
  plot_layout(ncol = 2, widths = c(1.5, 1)) +
  plot_annotation(tag_levels = "A")

combined_violin <-
  time_violin + distance_violin +
  plot_layout(ncol = 2, widths = c(1.5, 1)) +
  plot_annotation(tag_levels = "A")

ggsave(
  filename = "figure_5_time_distance_violin.png",
  plot = combined_violin,
  width = 25,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)
