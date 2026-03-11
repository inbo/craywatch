# ============================================================
# Scriptnaam: 10_pca_ordination.R
# Project: Craywatch
# Doel:
# - Imputeer ontbrekende fysico-chemische waardes (missMDA)
# - Voer PCA uit en visualiseer ordination (biplot)
# - Voeg pijlen toe voor fysico-chemische variabelen en soort-vecoren (presence)
#
# Verwachte input:
# - R-object/dataframe `data_fc_cray` beschikbaar in de globale omgeving
#   (alternatief: laad vooraf via readr::read_csv())
# - Verwachte kolommen:
#   - longitude, latitude (voor sf-conversie)
#   - numerieke PCA parameters: Cl, Nt, O2, EC20, T, pH, Secchi, Pt, ZS
#   - binaire soortkolommen: `faxonius limosus`, `procambarus clarkii`
#
# Output:
# - Twee ggplot biplots in de actieve grafische device:
#     - PCA + pijlen + presence kleur voor Faxonius limosus
#     - PCA + pijlen + presence kleur voor Procambarus clarkii
# - (Optioneel: sla plots op met ggsave indien gewenst)
#
# Benodigde packages:
# - vegan, dplyr, ggplot2, tibble, missMDA, mapview, sf, ggrepel
#
# Opmerkingen:
# - Als estim_ncpPCA niet betrouwbaar een ncp kan schatten, wordt ncp=2 gebruikt.
# - Controleer dat numerieke kolommen numeriek zijn vóór PCA.
# - Script is geschreven voor interactieve gebruik; pas paden/opslaan naar behoefte.
# ============================================================

library(vegan)
library(tibble)
library(missMDA)
library(mapview)
library(ggrepel)

# Lees data in 
FC_data <- data_fc_cray

# Visualiseer welke waarnemingen in de gelinkte data zitten
FC_data_sf <- FC_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 

FC_data_sf$faxonius.limosus.status <- factor(
  FC_data_sf$`faxonius limosus`, 
  levels = c(0, 1), 
  labels = c("Afwezig", "Aanwezig")
)

FC_data_sf$procambarus.clarkii.status <- factor(
  FC_data_sf$`procambarus clarkii`, 
  levels = c(0, 1), 
  labels = c("Afwezig", "Aanwezig")
)

# Plot de kaart
mapview(FC_data_sf, zcol = "faxonius.limosus.status", legend = TRUE, homebutton = TRUE)
mapview(FC_data_sf, zcol = "procambarus.clarkii.status", legend = TRUE, homebutton = TRUE)

table(FC_data_sf$faxonius.limosus.status, useNA = "always")
table(FC_data_sf$procambarus.clarkii.status, useNA = "always")

FC_data_sf %>%
  mutate(is_NA = is.na(WVLC)) %>%
  count(is_NA)
FC_data_sf %>%
  mutate(is_NA = is.na(VHAG)) %>%
  count(is_NA)

# # Haal de numerieke kolommen nodig voor PCA uit dataset
PCA_kolommen <- c("Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS")


ncp_estimate <- estim_ncpPCA(FC_data[PCA_kolommen])
n_components <- ncp_estimate$ncp
print(paste("Estimated number of components (ncp):", n_components))

# Omdat de components als 0 werd berekend, de components manueel op 2
pca_imputed_result <- imputePCA(
  FC_data[PCA_kolommen], 
  ncp = 2
)

# Continue with PCA
pca_result <- prcomp(pca_imputed_result$completeObs, center = TRUE, scale. = TRUE)

# Proportion of variance explained
var_exp <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# site scores - van locaties
site_scores <- as.data.frame(scores(pca_result, choices = 1:2, display = "sites"))
colnames(site_scores) <- c("PC1", "PC2") # Hernoem kolommen voor duidelijkheid

# fc scores: de coördinaten van de fysisch-chemische variabelen (voor biplot pijlen)
fc_scores <- as.data.frame(scores(pca_result, choices = 1:2, display = "species"))
colnames(fc_scores) <- c("PC1", "PC2")

# voeg naam van variabele toe
row.names(fc_scores)
fc_scores <- fc_scores %>% 
  add_column(var_eng = row.names(fc_scores))

# Eerst: Check of de rijen overeenkomen (moet waar zijn als je GEEN na.omit() meer gebruikt)
if (nrow(site_scores) != nrow(FC_data)) {
  stop("Aantal rijen komt niet overeen! Controleer data cleaning.")
}

# Voeg plot kleur data toe (faxonius limosus)
ordinated_data <- site_scores %>%
  add_column(`faxonius limosus` = FC_data$`faxonius limosus`)

ordinated_data_clean <- ordinated_data %>%
  mutate(`faxonius limosus` = factor(
    case_when(
      is.na(`faxonius limosus`) ~ "Onbekend",
      `faxonius limosus` == 1 ~ "Aanwezig",
      `faxonius limosus` == 0 ~ "Afwezig"
    ),
    levels = c("Afwezig", "Aanwezig", "Onbekend")
  ))

# Filter de NA's er uit
ordinated_data_clean_no_NA <- ordinated_data_clean %>%
  filter(!`faxonius limosus` == "Onbekend")

# voeg plot kleur toe (Procambarus clarkii)
ordinated_data_PC <- site_scores %>%
  add_column(`procambarus clarkii` = FC_data$`procambarus clarkii`)

ordinated_data_PC_clean <- ordinated_data_PC %>%
  mutate(`procambarus clarkii` = factor(
    case_when(
      is.na(`procambarus clarkii`) ~ "Onbekend",
      `procambarus clarkii` == 1 ~ "Aanwezig",
      `procambarus clarkii` == 0 ~ "Afwezig"
    ),
    levels = c("Afwezig", "Aanwezig", "Onbekend")
  ))

ordinated_data_PC_clean_no_NA <- ordinated_data_PC_clean %>%
  filter(!`procambarus clarkii` == "Onbekend")

ordinated_data_cpue_PC <- site_scores %>%
  add_column(`CPUE_procambarus clarkii` = FC_data$`CPUE_procambarus clarkii`)

# Soortpijl berekenen (faxonius limosus)
# We gebruiken de binaire kolom direct uit FC_data
soort_vector <- FC_data$`faxonius limosus`

# We moeten filteren op NA's in de soort vector en de PC-scores
data_gefilterd <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Soort = soort_vector
) %>%
  filter(!is.na(Soort))

# Handmatige berekening van de correlatie:
soort_corr_pc1 <- cor(data_gefilterd$Soort, data_gefilterd$PC1)
soort_corr_pc2 <- cor(data_gefilterd$Soort, data_gefilterd$PC2)

# Maak een dataframe voor de pijl
soort_scores <- data.frame(
  PC1 = soort_corr_pc1,
  PC2 = soort_corr_pc2,
  var_eng = "gevlekte Am. rivierkreeft" # Duidelijk label voor de pijl
)

# Scale arrows to match site score range
mult_active <- min(
  (max(site_scores$PC1) - min(site_scores$PC1)) / (max(fc_scores$PC1) - min(fc_scores$PC1)),
  (max(site_scores$PC2) - min(site_scores$PC2)) / (max(fc_scores$PC2) - min(fc_scores$PC2))
) * 0.8

fc_scores <- fc_scores %>%
  mutate(PC1 = PC1 * mult_active,
         PC2 = PC2 * mult_active)

# Schaal soortscores
soort_scores <- soort_scores %>%
  mutate(PC1 = PC1 * mult_active,
         PC2 = PC2 * mult_active)

fc_scores_totaal <- bind_rows(fc_scores, soort_scores)

# als je minder variabelen wil plotten met pijlen moet je gewoon de fc_scores tabel reduceren

# Faxonius limosus
plot_pca_lim <- ggplot(ordinated_data_clean_no_NA , aes(x = PC1, y = PC2)) +
  geom_point(aes(color = `faxonius limosus`), size = 3) + # shape = type_observed
  labs(color = "Aanwezigheid gevlekte\nAmerikaanse rivierkreeft") +
  scale_color_manual(values = c("Afwezig" = "lightblue", "Aanwezig" = "darkmagenta"
                                # , "Onbekend" = "gray70"
                                )) + 
  theme_minimal() +
  # Voeg biplot pijlen toe voor de fysisch-chemische variabelen
  geom_segment(data = fc_scores_totaal,
               aes(x = 0, y = 0, xend = PC1 * 1, yend = PC2 * 1), 
               # Vermenigvuldig met een factor voor betere schaal
               arrow = arrow(length = unit(0.2, "cm")), 
               color = ifelse(fc_scores_totaal$var_eng == "gevlekte Am. rivierkreeft", "red", "black"), 
               linetype = "solid") +
  ggrepel::geom_text_repel(data = fc_scores_totaal,
            aes(x = PC1 * 1, y = PC2 * 1, label = var_eng), 
            color = ifelse(fc_scores_totaal$var_eng == "gevlekte Am. rivierkreeft", "red", "black"), 
            size = 4, force = 2,
            max.overlaps = Inf) +
  xlab(paste0("PCA1 (",round(var_exp[1]*100,2),"%)")) +
  ylab(paste0("PCA2 (",round(var_exp[2]*100,2),"%)"))


plot_pca_lim

# Opslaan Limosus
file_out_lim <- file.path(dir_data_output, "fysicochemie", "PCA", "pca_biplot_limosus.png")
ggsave(file_out_lim, plot_pca_lim, width = 10, height = 8, dpi = 300)
message(paste("Opgeslagen:", file_out_lim))

# Soortpijl berekenen (Procambarus clarkii)
# We gebruiken de binaire kolom direct uit FC_data
soort_vector_PC <- FC_data$`procambarus clarkii`

# We moeten filteren op NA's in de soort vector en de PC-scores
data_gefilterd <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Soort = soort_vector_PC
) %>%
  filter(!is.na(Soort))

# Handmatige berekening van de correlatie:
soort_corr_PC_pc1 <- cor(data_gefilterd$Soort, data_gefilterd$PC1)
soort_corr_PC_pc2 <- cor(data_gefilterd$Soort, data_gefilterd$PC2)

# Maak een dataframe voor de pijl
soort_scores_PC <- data.frame(
  PC1 = soort_corr_PC_pc1,
  PC2 = soort_corr_PC_pc2,
  var_eng = "rode Am. rivierkreeft" # Duidelijk label voor de pijl
)

# Schaal soortscores P. clarkii
soort_scores_PC <- soort_scores_PC %>%
  mutate(PC1 = PC1 * mult_active,
         PC2 = PC2 * mult_active)

fc_scores_totaal_PC <- bind_rows(fc_scores, soort_scores_PC)

# Procambarus clarkii
plot_pca_clarkii <- ggplot(ordinated_data_PC_clean_no_NA , aes(x = PC1, y = PC2)) +
  geom_point(aes(color = `procambarus clarkii`), size = 3) + # shape = type_observed
  labs(color = "Aanwezigheid rode\nAmerikaanse rivierkreeft") +
  scale_color_manual(values = c("Afwezig" = "lightblue", "Aanwezig" = "darkmagenta"
                                # , "Onbekend" = "gray70"
                                )) + 
  theme_minimal() +
  # Voeg biplot pijlen toe voor de fysisch-chemische variabelen
  geom_segment(data = fc_scores_totaal_PC,
               aes(x = 0, y = 0, xend = PC1 * 1, yend = PC2 * 1), 
               # Vermenigvuldig met een factor voor betere schaal
               arrow = arrow(length = unit(0.2, "cm")), 
               color = ifelse(fc_scores_totaal_PC$var_eng == "rode Am. rivierkreeft", "red", "black"), 
               linetype = "solid") +
  ggrepel::geom_text_repel(data = fc_scores_totaal_PC,
                           aes(x = PC1 * 1, y = PC2 * 1, label = var_eng), 
                           color = ifelse(fc_scores_totaal_PC$var_eng == "rode Am. rivierkreeft", "red", "black"), 
                           size = 4, force = 3,
                           max.overlaps = Inf) +
  xlab(paste0("PCA1 (",round(var_exp[1]*100,2),"%)")) +
  ylab(paste0("PCA2 (",round(var_exp[2]*100,2),"%)"))


file_out_clarkii <- file.path(dir_data_output, "fysicochemie", "PCA", "pca_biplot_clarkii.png")
ggsave(file_out_clarkii, plot_pca_clarkii, width = 10, height = 8, dpi = 300)
message(paste("Opgeslagen:", file_out_clarkii))

plot_pca_clarkii

# # CPUE Procambarus clarkii
# plot_pca <- ggplot(ordinated_data_cpue_PC , aes(x = PC1, y = PC2)) +
#   stat_ellipse(
#     data = ordinated_data_cpue_PC,
#     aes(color = `CPUE_procambarus clarkii`), # Kleurt de ellips volgens de "Aanwezig" kleur
#     level = 0.95,
#     linetype = "solid",
#     linewidth = 1 
#   ) +
#   geom_point(aes(color = `CPUE_procambarus clarkii`), size = 3) + # shape = type_observed
#   labs(color = "CPUE P. clarkii") +
#   theme_minimal() +
#   ggtitle("PCA") +
#   # Voeg biplot pijlen toe voor de fysisch-chemische variabelen
#   geom_segment(data = fc_scores,
#                aes(x = 0, y = 0, xend = PC1 * 1, yend = PC2 * 1), 
#                # Vermenigvuldig met een factor voor betere schaal
#                arrow = arrow(length = unit(0.2, "cm")), 
#                color = "black", linetype = "solid") +
#   geom_text(data = fc_scores,
#             aes(x = PC1 * 1.05, y = PC2 * 1.05, label = var_eng), 
#             # Plaats labels iets verder
#             color = "black", size = 4, vjust = 0.5, hjust = 0.5)+
#   xlab(paste0("PCA1 (",round(var_exp[1]*100,2),"%)")) +
#   ylab(paste0("PCA2 (",round(var_exp[2]*100,2),"%)"))
# 
# 
# plot_pca


