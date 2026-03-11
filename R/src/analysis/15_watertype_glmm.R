# ====================================================
# Scriptnaam: 15_watertype_glmm.R
# Projec: Craywatch
# Datum: 01-12-2025
# Beschrijving: 
# - Laadt verwerkte data uit Script 10 (RDS)
# - Statistiek: GLMM (watertype)
# - Model validatie & Post-hoc
# - Rapportage in Word
# Gebaseerd op: Concepten uit script [voorkomen_lentisch_lotisch] van M. Vermeylen
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

library(lme4)   # Voor GLMM
library(DHARMa)
library(emmeans)
library(car)    # Voor Type III Anova

# --- 1. Data Inlezen (via RDS of 10_watertype_plot.R) ---

# Pad naar de tussenliggende file (deze wordt aangemaakt in script 10)
file_inter_watertype <- file.path(dir_data_intermediate, "cray_long_watertype.rds")
script_source_14     <- "./src/14_watertype_plot.R" # Pas pad aan indien nodig

if (!file.exists(file_inter_watertype)) {
  message("Verwerkte data (RDS) niet gevonden. Script 14 wordt uitgevoerd om deze aan te maken...")
  
  if(file.exists(script_source_14)) {
    source(script_source_14)
  } else {
    stop(paste("Kan Script 14 niet vinden op:", script_source_14))
  }
}

# Laad de data
message("Laden van cray_long dataframe...")
cray_long <- readRDS(file_inter_watertype)


# --- 2. GLMM --- 

# Selecteer de dataset (craywatch (1/0)  
valid_species <- cray_long %>%
  filter(dat.source == "craywatch_data") %>%
  filter(!is.na(presence)) %>%
  group_by(species, water_type) %>%
  summarise(n_vangsten = sum(presence), .groups = "drop") %>%
  group_by(species) %>%
  filter(all(n_vangsten > 0)) %>% 
  pull(species) %>%
  unique()

print("Soorten geschikt voor GLMM (komen voor in beide types):")
print(valid_species)

# Maak model 
model_data <- cray_long %>%
  filter(dat.source == "craywatch_data") %>% 
  filter(!is.na(presence)) %>%
  filter(species %in% valid_species) %>% 
  mutate(
    Waterbody_ID = coalesce(as.character(VHAG), as.character(WVLC), as.character(locID))
  )

# Draai het model
if(nrow(model_data) > 0) {
  message("Start GLMM analyse (Stilstaand vs Stromend) - Cleaned Data...")
  
  glmm_water <- glmer(
    presence ~ water_type * species + (1 | Waterbody_ID),
    family = binomial("logit"),
    data = model_data,
    control = glmerControl(optimizer = "bobyqa")
  )
  
  print(summary(glmm_water))
  
  
} else {
  warning("Te weinig data.")
}

# --- 3. Model validatie en post hoc analyses ---
# --- Model validatie (DHARMa) ---
# simuleren residuen om te checken voor model fit
simulationOutput <- simulateResiduals(fittedModel = glmm_water, plot = TRUE)

# extra test (dispersie / zero-inflation)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

# --- Type III ANOVA ---
# Omdat we een interactie hebben, kijken we naar de Type III Anova voor de globale effecten
print(car::Anova(glmm_water, type = "III"))

# --- C. Post-hoc analyse (Bonferroni) ---
# Is het verschil tussen open en gesloten significant per soort?

# Bereken de geschatte gemiddelden (estimated marginal means)
emm_results <- emmeans(glmm_water, ~ water_type | species)

# paarsgewijze vergelijking met Bonferroni correctie
# type = "response" geeft de kansen weer (probabiliteiten) in plaats van log-odds
pairwise_comp <- pairs(emm_results, adjust = "bonferroni")
conf_int      <- confint(pairwise_comp)

print("Resultaten per soort (open vs gesloten) met Bonferroni correctie:")
print(pairwise_comp)

# Als je de geschatte kansen (probabiliteiten) wilt zien per groep:
print(as.data.frame(emmeans(glmm_water, ~ water_type | species, type = "response")))

# --- 4. Rapportage en export naar word ---

library(officer)      
library(flextable)    
library(broom.mixed)  
library(dplyr)

# --- A. Voorbereiding ---
doc <- read_docx() %>%
  body_add_par("Statistisch rapport: watertype analyse", style = "heading 1") %>%
  body_add_par(paste("Gegenereerd op:", Sys.Date()), style = "Normal")

# --- B. Model Validatie Plot ---
doc <- doc %>% 
  body_add_par("1. Model validatie (DHARMa)", style = "heading 2") %>%
  body_add_par("Figuur 1: Analyse van de residuen.", style = "Image Caption")

temp_plot_file <- tempfile(fileext = ".png")
png(filename = temp_plot_file, width = 1000, height = 500)
plot(simulationOutput)
dev.off()

doc <- doc %>% 
  body_add_img(src = temp_plot_file, width = 6.5, height = 3.5) 

# --- C. Model Resultaten (Tabel) ---
res_model <- broom.mixed::tidy(glmm_water, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  mutate(
    term = dplyr::recode(term, 
                         "(Intercept)" = "Gevlekte (Gesloten)",
                         "water_typeopen" = "Type: Open",
                         "speciesprocambarus acutus" = "Soort: Gestreepte",
                         "speciesprocambarus clarkii" = "Soort: Rode",
                         "water_typeopen:speciesprocambarus acutus" = "Interactie: Open x Gestreepte",
                         "water_typeopen:speciesprocambarus clarkii" = "Interactie: Open x Rode"),
    across(where(is.numeric), ~ round(., 3)),
    p_label = ifelse(p.value < 0.001, "< 0.001", as.character(p.value))
  ) %>%
  select(term, estimate, conf.low, conf.high, statistic, p_label)

ft_model <- flextable(res_model) %>%
  set_header_labels(
    term = "variabele", estimate = "odds Ratio", 
    conf.low = "CI Laag", conf.high = "CI Hoog", 
    statistic = "z-waarde", p_label = "p-waarde"
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header") 

doc <- doc %>%
  body_add_break() %>%
  body_add_par("2. Model Resultaten (GLMM)", style = "heading 2") %>%
  body_add_flextable(ft_model) %>%
  body_add_par("Tabel 1: Fixed effects van het GLMM (odds Ratios).", style = "table title")

# --- D. ANOVA type III ---
df_anova <- broom::tidy(car::Anova(glmm_water, type = "III")) %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    p_label = ifelse(p.value < 0.001, "< 0.001", as.character(p.value))
  )

ft_anova <- flextable(df_anova, col_keys = c("term", "statistic", "df", "p_label")) %>%
  set_header_labels(term = "Effect", statistic = "Chi-sq", df = "Df", p_label = "P-waarde") %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header")

doc <- doc %>%
  body_add_par("3. Globale effecten (ANOVA)", style = "heading 2") %>%
  body_add_flextable(ft_anova) %>%
  body_add_par("Tabel 2: Type III Analysis of Deviance.", style = "table title")

# --- E. Post-hoc (Tabel) ---
raw_contrasts <- summary(pairs(emmeans(glmm_water, ~ water_type | species), adjust="bonf", type="response"))

df_posthoc <- as.data.frame(raw_contrasts) %>%
  mutate(
    species = species_labels_dutch[as.character(species)],
    across(where(is.numeric), ~ round(., 3)),
    p_label = ifelse(p.value < 0.001, "< 0.001", as.character(p.value))
  ) %>%
  select(species, contrast, odds.ratio, SE, z.ratio, p_label)

ft_posthoc <- flextable(df_posthoc) %>%
  set_header_labels(
    species = "soort", contrast = "vergelijking", 
    odds.ratio = "odds Ratio", SE = "std. Error", 
    z.ratio = "z-ratio", p_label = "p-waarde (Bonf.)"
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header")

doc <- doc %>%
  body_add_par("4. Post-hoc vergelijkingen per soort", style = "heading 2") %>%
  body_add_flextable(ft_posthoc) %>%
  body_add_par("Tabel 3: Paarsgewijze vergelijkingen (open vs gesloten).", style = "table title")

# --- F. Opslaan ---
output_word <- file.path(dir_watertype_output, "statistiek_watertype_rapport.docx")
print(doc, target = output_word)

message(paste("Clean word rapport opgeslagen:", output_word))
shell.exec(output_word)