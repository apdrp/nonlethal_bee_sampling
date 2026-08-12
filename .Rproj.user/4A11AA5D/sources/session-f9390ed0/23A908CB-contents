#script for analyses for 
#Drapeau Picard et al. 2025 - Nonlethal methods for wild bee sampling : a systematic review 

library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)
library(hrbrthemes)
library(treemap)
library(patchwork)
library(gridExtra)
library(ggExtra)
library(wordcloud2)


dataset_raw <- read_excel("nonlethal_monitoring_article_data.xlsx")

dataset_unique <- dataset_raw[!duplicated(dataset_raw$ref_num),] #keep only 1 line per study based on reference name

dataset_unique_methods <- dataset_unique %>% 
  filter(original_data == "1") #keep only studies with original data


#WHEN did the sampling take place?
#sampling months
sampling_months <- nonlethal_rawdata[,"months"]

sep_months <- separate(sampling_months, months, into = paste0("month_", 1:12), sep = ";")

long_month <- pivot_longer(data      = sep_months, 
                     cols      = paste0("month_", 1:12),
                     names_to  = , 
                     values_to = )

month_list <- long_month[,"value"]
month_list <- na.omit(month_list) 
month_vec <- trimws(month_list$value)

df <- data.frame(month_vec = factor(month_vec, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

ggplot(df, aes(x = month_vec)) +
  geom_bar() +
  labs(title = "Frequency of each sampling month (n=51)", x = "Month", y = "Frequency") +
  theme_minimal()

#year
years <- dataset_unique[,c("year", "habitat_class1")] #get "year" column
years <- separate(years, year, into = paste0("year_", 1:8), sep = ";") #separate multiple values

long_years <- pivot_longer(years,
                           cols = paste0("year_", 1:8))

years_df <- long_years %>%
  select(-1) %>%
  na.omit() %>%
  mutate(value = as.numeric(trimws(value))) %>%
  pull(value) %>%
  data.frame(long_years = .)

years_freq <- years_df %>%
  count(long_years)


ggplot(years_df,
       aes(x=long_years,
       y=))+
  geom_histogram(stat="count")+
  theme_minimal()+
  labs(x ='Sampling year', y='Number of studies', title = 'Histogram of years during which nonlethal sampling took place')


#WHERE did the sampling take place?
##country
country_freq <- dataset_unique %>%
  count(country, name = "n", sort = TRUE) %>%
  mutate(country = if_else(n == 1, "others", country)) %>%  # pays à freq=1 -> "autre"
  group_by(country)%>%
  summarise(n = sum(n), .groups = "drop")%>%
  arrange(desc(n))%>%
  arrange(n == "7", desc(n), country)

ggplot(country_freq, 
       aes(x = reorder(country, n), 
           y=n)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of studies per country", x = "Country", y = "Number of studies") +
  theme_minimal()

pie(country_freq$n, labels = country_freq$country)

##landscape
landscape <- dataset_unique[,"habitat_class1"] #get "habitat" column


long_hab <- habitat %>%
  separate(habitat_class1, into = paste0("hab_", 1:4), sep = ";") %>%
  pivot_longer(cols = starts_with("hab_"), values_to = "hab_vec") %>%
  filter(!is.na(hab_vec)) %>%
  mutate(hab_vec = trimws(hab_vec))

df <- data.frame(hab_vec = factor(long_hab$hab_vec, levels = c("natural", "seminatural", "agricultural", "suburban", "urban")))

ggplot(df, aes(x = hab_vec)) +
  geom_bar() +
    labs(title = "Frequency of each sampled lanscape (n=53)", x = "Month", y = "Frequency") +
  theme_minimal()

##landscape through time
years_landscape <- dataset_unique_methods[,c("year", "habitat_class1")]

long_years_landscape <- years_landscape %>%
  mutate(
    year = strsplit(as.character(year), ";\\s*") |> map(trimws),
    landscape = strsplit(as.character(habitat_class1), ";\\s*") |> map(trimws)) %>%
    rowwise() %>%
  mutate(combinaisons = list(expand.grid(year = year, landscape = landscape))) %>%
  unnest(combinaisons, names_sep = "_") %>%
  select(year = combinaisons_year, landscape = combinaisons_landscape)%>%
  drop_na() %>%            
  mutate(year = as.numeric(trimws(year)))%>%
  as.data.frame()

landscape_freq <- long_years_landscape %>%
  group_by(year, landscape) %>%
  summarise(frequency = n(), .groups = "drop")%>%
  mutate(year = factor(year, levels = sort(unique(year))))  


ggplot(landscape_freq, aes(x = factor(year), y = frequency, fill = landscape)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.3) +
  scale_fill_manual(
    values = c(
      "urban" = "black",
      "natural" = "darkgrey",
      "seminatural" = "lightgrey",
      "agricultural" = "white"))+
  labs(
    title = "Number of studies per landscape through time (n=82)",
    x = "Sampling year",
    y = "Number of studies",
    fill = "Lanscape"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###fig 1
ggplot(landscape_freq, aes(x = factor(year), y = frequency)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.3) +
  scale_fill_manual(
    values = c(
      "urban" = "black",
      "natural" = "darkgrey",
      "seminatural" = "lightgrey",
      "agricultural" = "white"))+
  labs(
    title = "Number of studies per landscape through time (n=82)",
    x = "Sampling year",
    y = "Number of studies",
    fill = "Lanscape"
  ) +
  ylim(0, 8)+
  facet_grid(landscape ~. )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#figure 2
#WHY? study topic
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
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    x = NULL,
    y = "Number of studies"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    plot.margin = margin(10, 40, 10, 10)
  )

#keywords - wordcloud?
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

keywords_plot <- keywords_plot %>%
  filter(n > 1) %>%
  mutate(
    freq_scaled = log(n)
  )

wordcloud2(data=keywords_plot, size = 0.5)

#who?
observers <-  dataset_unique_methods[,c("observer_quality", "observer_quantity")]

observers <- observers  %>%
    separate_rows(observer_quantity, sep = ";\\s*") %>%  # sépare les valeurs sur les virgules
  mutate(observer_quantity = trimws(observer_quantity)) %>%   # enlève les espaces autour
  filter(!is.na(observer_quantity),                    # enlève les NA
         observer_quantity != "",                      # enlève les chaînes vides
         tolower(observer_quantity) != "unknown") %>%
  mutate(observer_quantity = as.numeric(trimws(observer_quantity)))

mean(observers$observer_quantity[-29]) #mean after removing Roy et al. 2016 (outlier)
sd(observers$observer_quantity[-29]) #sd after removing Roy et al. 2016 (outlier)

observers <- observers  %>%
  separate_rows(observer_quality, sep = ";\\s*") %>%  # sépare les valeurs
  mutate(observer_quantity = trimws(observer_quantity)) %>%   # enlève les espaces autour
  filter(!is.na(observer_quantity),                    # enlève les NA
         observer_quantity != "",                      # enlève les chaînes vides
         tolower(observer_quantity) != "unknown")

observers_freq <- observers %>%
  count(observer_quality)

ggplot(observers_freq, aes(x="observer_quality", y=n, fill=observer_quality)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#what?

##what kind of data?
data <- dataset_unique_methods %>%
  separate_rows(measurementType, sep = ";\\s*") %>%
  mutate(measurementType = trimws(measurementType))

data_freq <- data %>%
  count(measurementType) %>%
  mutate(percent = 100 * n / sum(n)) 

ggplot(data_freq, aes(x="measurementType", y=n, fill=measurementType)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)

##what is the taxonomic resolution? 
taxo_res_apriori <- dataset_unique_methods %>%
  filter(!is.na(dataset_unique_methods$n_apriori_classes))%>%
  count(taxonomic_resolution)%>%
  arrange(desc(n))

taxo_res_noapriori <- dataset_unique_methods %>%
  filter(is.na(dataset_unique_methods$n_apriori_classes))%>%
  count(taxonomic_resolution)%>%
  arrange(desc(n))

#taxa
all_taxa <- dataset_unique_methods%>%
  separate_rows(target_taxa, sep = ";\\s*") %>%
  mutate(target_taxa = trimws(target_taxa))

taxa_topic <- all_taxa %>%
  separate_rows(study_topic, sep = ";\\s*") %>%
  mutate(study_topic = trimws(study_topic))

##apriori classes vs no predefined classes
dataset_unique_methods <- dataset_unique_methods %>%
  mutate(a_priori = factor(is.na(dataset_unique_methods$n_apriori_classes), "no", "yes"))

non_hymeno <- c(
  "Diptera", "Coleoptera", "Animalia", "Hemiptera", "Insecta", "Lepidoptera",
  "Rhopalocera", "Syrphidae", "flying insects", "several orders", "Asilidae",
  "Bombyliidae", "Culicidae", "Odonata", "other", "other non-bee insects",
  "other visitors"
)


taxa_class <- all_taxa %>%
  select(target_taxa, n_apriori_classes) %>%
  mutate(a_priori = factor(ifelse(is.na(n_apriori_classes), "no", "yes")))%>%
  mutate(target_taxa = case_when(
    target_taxa %in% c("Bombus sp.", "Bombus spp.") ~ "Bombus",
    TRUE ~ target_taxa)) %>%
  count(a_priori, target_taxa) %>%
  group_by(a_priori) 

taxa_class$a_priori <- as.character(taxa_class$a_priori)
taxa_class$a_priori[taxa_class$target_taxa %in% c("Diptera", "Coleoptera", "Animalia", "Hemiptera", "Insecta", "Lepidoptera", "Rhopalocera", "Syrphidae", "flying insects", "several orders", "Asilidae", "Bombyliidae", "Culicidae", "Odonata", "other", "other non-bee insects", "other visitors")] <- "ynon-Hymeno"


taxa_class <- all_taxa %>%
  select(target_taxa, n_apriori_classes) %>%
  mutate(
    a_priori = factor(if_else(is.na(n_apriori_classes), "no", "yes")),
    target_taxa = case_when(
      target_taxa %in% c("Bombus sp.", "Bombus spp.") ~ "Bombus",
      TRUE ~ target_taxa
    ),
    taxa_group = if_else(target_taxa %in% non_hymeno, "non-Hymeno", "Hymeno")
  ) %>%
  count(a_priori, taxa_group, target_taxa, name = "n")


# treemap
treemap(taxa_class,
        index=c("a_priori","target_taxa"),
        vSize="n",
        type="index") 


tree1 <- treemap(
  taxa_class[taxa_class$a_priori %in% c("no", "yes"), ],
  index = c("a_priori", "target_taxa"),
  vSize = "n",
  type = "index",
  fontsize.labels = c(0, 12)
)

tree2 <- treemap(
  taxa_class[taxa_class$taxa_group %in% c("non-Hymeno", "Hymeno"), ],
  index = c("taxa_group", "target_taxa"),
  vSize = "n",
  type = "index",
  fontsize.labels = c(0, 12)
  
)

tree3 <- treemap(
  taxa_class,
  index = c("a_priori", "taxa_group", "target_taxa"),
  vSize = "n",
  type = "index",
  fontsize.labels = c(10, 10, 10)
)


ggplot(taxa_class, aes(y=n, x=reorder(target_taxa, n))) + 
  geom_bar(stat="identity")+
  facet_wrap(~ a_priori, scales = "free")+
  labs(title = "Top 5  target_taxa per a priori or not",
       x = "Target Taxa", y = "Number of occurrence") +
  coord_flip()

a_priori <- dataset_unique_methods%>%
  filter(!is.na(dataset_unique_methods$n_apriori_classes))%>%
  separate_rows(target_taxa, sep = ";\\s*") %>%
  mutate(target_taxa = trimws(target_taxa)) %>%
  count(target_taxa)


ggplot(a_priori, aes(x = target_taxa, y =reorder(target_taxa, -n))) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ a_priori, scales = "free")+
  labs(x = "Taxon", y = "Fréquence", title = "Fréquence des taxons (a priori)") +
  theme_minimal()

ggplot(a_priori, aes(x=target_taxa_std) ) +
  geom_histogram( aes(x = target_taxa_std), fill="#69b3a2" ) +
  #geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  #geom_histogram( aes(x = var2, y = -..density..), fill= "#404080") +
  #geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  labs(x = "Taxon", y = "Fréquence", title = "Fréquence des taxons (a priori)")

a_priori_other <- a_priori %>%
  mutate(target_taxa = ifelse(n == 1, "others", target_taxa)) %>%
  group_by(target_taxa) %>%
  summarise(n = sum(n), .groups = "drop")%>%
  arrange(desc(n))

ggplot(a_priori_other, aes(x="target_taxa", y=n, fill=target_taxa)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

no_a_priori <- dataset_unique_methods[is.na(dataset_unique_methods$n_apriori_classes), ]


#correlation between n_apriori classes and n classes used for analyses
n_classes <- dataset_unique_methods%>%
  filter(!is.na(n_apriori_classes))%>%
  separate_rows(n_class_analyses, sep = ";\\s*") %>%
  mutate(n_class_analyses = trimws(n_class_analyses)) %>%
  mutate(
    n_apriori_classes = as.numeric(n_apriori_classes),
    n_class_analyses  = as.numeric(n_class_analyses))

p_scatter <- ggplot(n_classes, aes(n_apriori_classes, n_class_analyses)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 9, by = 2), limits = c(0, 9), minor_breaks = NULL) +
  labs(x = "Number of sampled classes", y = "Number of analyzed classes") +
  theme_minimal() +
  theme(
    axis.text   = element_text(size = 14),
    axis.title  = element_text(size = 16)
  )

ggMarginal(
  p_scatter,
  type = "boxplot",      
  margins = "both",      
  size = 5,              
  outlier.colour = NA)

mean(n_classes$n_apriori_classes, na.rm = TRUE)
mean(n_classes$n_class_analyses, na.rm = TRUE)

##How?
#standardization
std <- as.data.frame(table(dataset_raw$standardization))

ggplot(std, 
       aes(x = reorder(Var1, Freq), 
           y=Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "How", x = "Standardization", y = "Number of studies")+ 
  coord_flip() +
  theme_minimal()

#tool

palette_daltonien <- c(
  "#E69F00",  
  "#56B4E9",  
  "#009E73",  
  "#F0E442",  
  "#0072B2",  
  "#D55E00",  
  "#CC79A7",  
  "#999999",  
  "#000000",  
  "#E0E0E0",  
)

tool_topic <- dataset_unique_methods %>%
  separate_rows(study_topic, sep = ";\\s*") %>%
  mutate(study_topic = trimws(study_topic)) %>%
  separate_rows(nonlethal_tool, sep = ";\\s*") %>%
  mutate(nonlethal_tool = trimws(nonlethal_tool)) %>%
  #filter(study_topic %in% c("habitat management", "pollination ecology", "methodology"))%>% #extract main topics
  mutate(nonlethal_tool = case_when(
    nonlethal_tool %in% c("3D video cameras", "photo cameras", "video cameras") ~ "camera traps",
    nonlethal_tool %in% c("DNA barcoding", "eDNA") ~ "DNA",
    TRUE ~ nonlethal_tool
  )) %>%
  count(study_topic, nonlethal_tool)

ggplot(tool_topic, aes(fill=nonlethal_tool, y=n, x=study_topic)) + 
  geom_bar(position="fill", stat="identity")+ 
  coord_flip() +
  labs(title = 'Tools used for the three main study topics',
       x = 'Topic',
       y = 'Frequency')+
  labs(fill = "Nonlethal tool")+
  scale_fill_manual(values = palette_daltonien) +
  theme_minimal()


#protocol
protocol <- dataset_raw%>% 
  filter(original_data == "1")

dataset_unique_methods[,c("ref_name", "standardization", "nonlethal_protocol", "nonlethal_tool", "nonlethal_sample_duration",	"total_observation_time", "nonlethal_sample_distance",	"total_observation_distance")]

protocol_nature <- protocol[,c("ref_name", "nonlethal_protocol")]

protocol_freq <- protocol_nature %>%
  separate(nonlethal_protocol, c("protocol1", "protocol2", "protocol3"), ";") %>%
  pivot_longer(!ref_name, names_to = "x", values_to = "protocol") %>%
  mutate(protocol = trimws(protocol)) %>%
  count(protocol) %>%
  #filter(n > 5)%>%
  drop_na()

ggplot(protocol_freq, aes(x = reorder(protocol, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "How", x = "Most used protocols", y = "Number of studies") + 
  coord_flip() +
  theme_minimal()

#bopxplots time
time1 <- protocol[,c("ref_name", "nonlethal_tool", "standardization", "nonlethal_protocol", "nonlethal_sample_duration", "total_observation_time")] %>%
  distinct(ref_name, nonlethal_protocol, .keep_all = TRUE) %>%
  separate(`nonlethal_sample_duration`, c("sample1", "sample2", "sample3", "sample4", "sample5", "sample6"), ";") %>%
  separate(`total_observation_time`, c("total1", "total2", "total3", "total4", "total5", "total6"), ";") %>%
  separate(`standardization`, c("std1", "std2", "std3"), ";") %>%
    pivot_longer(cols = starts_with("sample") | starts_with("total"),
               names_to = c(".value", "set"),
               names_prefix = "",
               names_pattern = "(.*)(\\d+)"       
  )

time_num  <- time1 %>%
  filter(nonlethal_protocol %in% c("timed walks", "point-counts", "timed distance transects")) %>%
  filter(!ref_name %in% "Stothut et al. 2023")

time_box <- time_num %>%
  pivot_longer(cols = c("sample", "total"),
               names_to = "time")

time_box$value <- as.numeric(time_box$value)
time_box <- time_box %>%
  filter(!(is.na(value)))

ggplot(time_box, aes(x=nonlethal_protocol, y=value, fill=time))+
  geom_boxplot(outlier.shape = )+
  labs(title = "How", x = "Time-based protocols", y = "Survey length (minutes)") + 
  scale_y_continuous(limits = c(0, 200)) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,
               position=position_dodge(0.8)) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.8),  # Position relative dans le graphique (x, y)
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )


time_box %>%
  group_by(time) %>%
  summarise(
    n = sum(!is.na(value)),
    moyenne = mean(value, na.rm = TRUE),
    ecart_type = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

time_box %>%
  group_by(time, nonlethal_protocol) %>%
  summarise(
    moyenne = mean(value, na.rm = TRUE),
    ecart_type = sd(value, na.rm = TRUE)
  )

# Grouped violin plot
time_violin <-
  ggplot(time_box, aes(fill=time, y=value, x=nonlethal_protocol)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent", colour = "transparent") +
  theme_minimal()  +
  xlab("Time-based protocols") +
  ylab("Survey length (minutes)") +
  ylim(0,175)+
  theme(
    legend.position = c(0.5, 0.98),    # superposé en haut du graphique
    legend.justification = "top",
    legend.direction = "horizontal",   # éléments sur une seule ligne
    legend.box = "horizontal",
    legend.title = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 1)      # forcer une seule ligne
  )


#boxplots distance
distance1 <- protocol[,c("ref_name", "standardization", "nonlethal_tool", "nonlethal_protocol", "nonlethal_sample_distance", "total_observation_distance")] %>%
  filter(nonlethal_protocol %in% c("distance transects", "timed distance transects"))%>%
  distinct(ref_name, nonlethal_protocol, .keep_all = TRUE) %>%
  separate(`nonlethal_sample_distance`, c("sample1", "sample2", "sample3", "sample4", "sample5", "sample6"), ";") %>%
  separate(`total_observation_distance`, c("total1", "total2", "total3", "total4", "total5", "total6"), ";") %>%
  separate(`standardization`, c("std1", "std2", "std3"), ";") %>%
  pivot_longer(cols = starts_with("sample") | starts_with("total"),
               names_to = c(".value", "set"),
               names_prefix = "",
               names_pattern = "(.*)(\\d+)"
  )


distance_box <- distance1 %>%
  pivot_longer(cols = c("sample", "total"), names_to = "distance", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  filter(!str_detect(ref_name, "Chacon|Wood|Boone et al.")) #remove outliers

  
ggplot(distance_box, aes(x=distance, y=value))+
  geom_boxplot(outlier.shape =, aes(fill=distance), show.legend = FALSE )+
  labs(title = "How", x = "Distance-based protocols", y = "Survey length (meters)") + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,
               position=position_dodge(0.8)) +
  scale_y_continuous(limits = c(0, 250)) + 
  theme(legend.position = "none") +
  theme_minimal()


distance_box %>%
  group_by(distance) %>%
  summarise(
    moyenne = mean(value, na.rm = TRUE),
    ecart_type = sd(value, na.rm = TRUE)
  )



# distance violin plot
distance_violin <-
  ggplot(distance_box, aes(fill=distance, y=value, x=distance)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent", colour = "transparent") +
  theme_minimal()  + 
  theme(legend.position = "none")+
  xlab("Distance-based protocols") +
  ylab("Survey length (meters)") +
  ylim(0,250)

grid.arrange(time_violin, distance_violin, ncol=2)

##tools VS protocols
palette_daltonien <- c(
  "#E69F00",  
  "#56B4E9",  
  "#009E73",  
  "#F0E442",  
  "#0072B2",  
  "#D55E00",  
  "#CC79A7",  
  "#999999",  
  "#000000",  
  "#E0E0E0",  
)

transects <- c(
  "Diptera", "Coleoptera", "Animalia", "Hemiptera", "Insecta", "Lepidoptera",
  "Rhopalocera", "Syrphidae", "flying insects", "several orders", "Asilidae",
  "Bombyliidae", "Culicidae", "Odonata", "other", "other non-bee insects",
  "other visitors"
)


tool_protocol <- dataset_unique_methods %>%
  separate_rows(nonlethal_protocol, sep = ";\\s*") %>%
  mutate(study_topic = trimws(nonlethal_protocol)) %>%
  separate_rows(nonlethal_tool, sep = ";\\s*") %>%
  mutate(nonlethal_tool = trimws(nonlethal_tool)) %>%
  #mutate(nonlethal_tool = case_when(
   # nonlethal_tool %in% c("3D video cameras", "video cameras") ~ "camera traps",
    #nonlethal_tool %in% c("DNA barcoding", "eDNA") ~ "DNA",
    #TRUE ~ nonlethal_tool
  #)) %>%
  filter(!is.na(nonlethal_protocol))%>%
  filter(!str_detect(nonlethal_protocol, "survey-point|extraction|mark"))%>%
  count(nonlethal_protocol, nonlethal_tool)%>%
  mutate(protocol_group = if_else(
    grepl("transects", nonlethal_protocol, ignore.case = TRUE),
    "transects",
    "other"
  ))

ggplot(tool_protocol, aes(fill=nonlethal_tool, y=n, x=nonlethal_protocol)) + 
  geom_bar(position="fill", stat="identity")+ 
  coord_flip() +
  labs(title = 'Nonlethal tools used for different protocols',
       x = 'Protocol',
       y = 'Relative frequency')+
  labs(fill = "Nonlethal tool")+
  scale_fill_manual(values = palette_daltonien) +
  facet_wrap(~ protocol_group, ncol = 1, scales = "free_y") +
  theme_minimal()




##data validation
data_val <- nonlethal_unique_method[,"data_validation"]

par(mfrow = c(1,2))
