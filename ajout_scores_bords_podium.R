# ==============================================================================
# SCORES PAR BORD POLITIQUE + PODIUM, pour :
#   - municipales 2026, T1   (csv, une colonne "Nuance liste N" par liste)
#   - présidentielle 2022, T1 (xlsx, pas de nuance : on classe par candidat)
# Mêmes variables que pour 2020 (total_droite / total_centre / total_gauche,
# podium_1 / podium_2 / podium_3), suffixées par l'élection pour ne pas écraser
# celles de 2020 : total_droite_mun1_2026, podium_1_pres1_2022, etc.
# ==============================================================================

# ------------------------------------------------------------------------------
# PARTIE A -- à coller dans le chunk "PREPARE POLITICAL PARTY SCORES", APRÈS le
# inner_join(mun_bv_mrs_top3, mun_bv_mrs, ...) qui clôt le bloc de 2020.
# (Les colonnes ajoutées descendent ensuite seules dans res_eq puis `fin`, car
#  ton select() garde déjà les colonnes contenant "total_" et "podium".)
# ------------------------------------------------------------------------------

# 1. Classification en bords ---------------------------------------------------
# Mêmes regroupements que 2020. Comparaison EXACTE (%in%), ce qui évite que
# "LUD" attrape aussi "LUDI". Ajouts propres aux nuances 2026 (absentes de
# Marseille, sans effet ici, à vérifier si tu étends à d'autres communes) :
#   droite : LUXD, LUDR, LREC   |   centre : LREN, LHOR   |   LDSV non classée
nuances_gauche = c("LEXG", "LCOM", "LFI", "LSOC", "LRDG", "LDVG", "LUG", "LVEC")
nuances_centre = c("LREM", "LMDM", "LUDI", "LUC", "LDVC", "LECO", "LDIV", "LREG", "LGJ",
                   "LREN", "LHOR")
nuances_droite = c("LLR", "LUD", "LDVD", "LDLF", "LRN", "LEXD",
                   "LUXD", "LUDR", "LREC")

classe_nuance = function(x) {
  case_when(x %in% nuances_gauche ~ "gauche",
            x %in% nuances_centre ~ "centre",
            x %in% nuances_droite ~ "droite",
            TRUE ~ NA_character_)
}

# Présidentielle 2022 : pas de nuance dans le fichier, donc on classe les
# candidats. Choix à discuter : Macron et Lassalle -> "centre et divers"
# (cohérent avec LREM / LDIV), Dupont-Aignan -> droite (cohérent avec LDLF).
# Regex sans accents ("M.LENCHON") pour ne pas dépendre de l'encodage.
classe_candidat = function(x) {
  case_when(str_detect(x, "ARTHAUD|ROUSSEL|M.LENCHON|HIDALGO|JADOT|POUTOU") ~ "gauche",
            str_detect(x, "MACRON|LASSALLE")                                ~ "centre",
            str_detect(x, "LE PEN|ZEMMOUR|P.CRESSE|DUPONT-AIGNAN")          ~ "droite",
            TRUE ~ NA_character_)
}

# 2. Lecture -> format long : une ligne par BV x liste (ou candidat) -----------
# Sortie commune : idBV_miomFormat, etiquette (nuance ou nom), pct (% des exprimés)

# Municipales 2026 : on repère les colonnes par leur NOM ("Nuance liste N",
# "% Voix/exprimés N"), pas par leur position.
lire_mun26_long = function(fichier, dpt = "13", cmn = "055") {
  read_delim(here("input_data", fichier), delim = ";",
             col_types = cols(.default = col_character()),
             col_select = c("Code commune", "Code BV",
                            starts_with("Nuance liste "),
                            starts_with("% Voix/exprimés "))) %>% 
    filter(`Code commune` == paste0(dpt, cmn)) %>% 
    mutate(idBV_miomFormat = paste0(`Code commune`, "_", 
                                    str_pad(`Code BV`, width = 4, "left", pad = "0"))) %>% 
    select(-`Code commune`, -`Code BV`) %>% 
    pivot_longer(-idBV_miomFormat,
                 names_to = c(".value", "rang"),
                 names_pattern = "^(Nuance liste|% Voix/exprimés) (\\d+)$") %>% 
    filter(!is.na(`Nuance liste`)) %>%                      # emplacements vides
    transmute(idBV_miomFormat,
              etiquette = `Nuance liste`,
              pct = as.numeric(str_replace(str_remove(`% Voix/exprimés`, "%"), ",", ".")))
}

# Présidentielle 2022 : les blocs candidats 2 à N n'ont pas d'en-tête dans le
# fichier, donc lecture par POSITION, avec col_names = FALSE. Structure :
# 21 colonnes d'identification/résultats globaux, puis 7 colonnes par candidat
# (N°Panneau, Sexe, Nom, Prénom, Voix, % Voix/Ins, % Voix/Exp).
lire_pres_long = function(fichier, dpt = "13", cmn = "055") {
  df = read_xlsx(here("input_data", fichier), col_names = FALSE, skip = 1, 
                 guess_max = 10000)
  
  n_cand = (ncol(df) - 21) / 7
  stopifnot(n_cand == round(n_cand))          # T1 : 12 candidats ; T2 : 2
  
  cles = tibble(
    codeDpt = str_pad(as.character(df[[1]]), 2, "left", "0"),   # Code du département
    codeCmn = str_pad(as.character(df[[5]]), 3, "left", "0"),   # Code de la commune
    codeBV  = str_pad(as.character(df[[7]]), 4, "left", "0")    # Code du b.vote
  ) %>% 
    mutate(idBV_miomFormat = paste0(codeDpt, codeCmn, "_", codeBV))
  garde = which(cles$codeDpt == dpt & cles$codeCmn == cmn)
  
  map_dfr(seq_len(n_cand), function(k) {
    b = 22 + 7 * (k - 1)                       # colonne "N°Panneau" du candidat k
    tibble(idBV_miomFormat = cles$idBV_miomFormat[garde],
           etiquette = toupper(as.character(df[[b + 2]][garde])),   # Nom
           pct = as.numeric(df[[b + 6]][garde]))                    # % Voix/Exp
  })
}

# 3. Agrégation : mêmes variables qu'en 2020 -----------------------------------
agrege_scores = function(long, suffixe) {
  
  # Contrôle : étiquettes non classées (elles ne compteraient dans aucun bord)
  non_classees = long %>% filter(is.na(bord)) %>% distinct(etiquette) %>% pull()
  if (length(non_classees) > 0) 
    message(suffixe, " - étiquettes non classées : ", paste(non_classees, collapse = ", "))
  
  totaux = 
    long %>% 
    group_by(idBV_miomFormat) %>% 
    summarise(total_droite = sum(pct[which(bord == "droite")]),
              total_centre = sum(pct[which(bord == "centre")]),
              total_gauche = sum(pct[which(bord == "gauche")]),
              .groups = "drop")
  
  podium = 
    long %>% 
    arrange(idBV_miomFormat, desc(pct)) %>% 
    group_by(idBV_miomFormat) %>% 
    mutate(rang = row_number()) %>% 
    filter(rang <= 3) %>% 
    ungroup() %>% 
    mutate(podium = factor(paste0("podium_", rang), levels = paste0("podium_", 1:3)),
           pct_et_parti = paste0(round(pct, 1), "% (", etiquette, ")")) %>% 
    pivot_wider(id_cols = idBV_miomFormat, names_from = podium, 
                values_from = pct_et_parti, names_expand = TRUE)
  
  inner_join(totaux, podium, by = "idBV_miomFormat") %>% 
    rename_with(~ paste0(.x, "_", suffixe), -idBV_miomFormat)
}

# 4. Application aux deux élections ---------------------------------------------
mun26_t1_long = 
  lire_mun26_long("municipales-2026-resultats-bv-par-communes-2026-03-20.csv") %>% 
  mutate(bord = classe_nuance(etiquette))

pres22_t1_long = 
  lire_pres_long("presidentielles22_resultats-par-niveau-burvot-t1-france-entiere.xlsx") %>% 
  mutate(bord = classe_candidat(etiquette))

scores_supp = list(
  agrege_scores(mun26_t1_long,  "mun1_2026"),
  agrege_scores(pres22_t1_long, "pres1_2022")
)

# Contrôle : la somme des trois bords doit faire ~100 pour chaque BV
scores_supp %>% 
  walk(~ .x %>% 
         transmute(grand_total = rowSums(across(starts_with("total_")))) %>% 
         pull(grand_total) %>% summary() %>% print())

# 5. Jointure sur mun_bv_mrs (restreint à Marseille, après le bloc de 2020)
mun_bv_mrs = reduce(scores_supp, left_join, by = "idBV_miomFormat", .init = mun_bv_mrs)


# ------------------------------------------------------------------------------
# PARTIE B -- chunk de la carte, après le mutate(fmt_...) sur `fin`
# ------------------------------------------------------------------------------

scores_supp_meta = tribble(
  ~suffixe,      ~libelle,
  "mun1_2026",   "Municipales 2026, T1",
  "pres1_2022",  "Présidentielle 2022, T1"
)

# Totaux formatés ("xx.x %")
for (s in scores_supp_meta$suffixe) {
  for (b in c("droite", "centre", "gauche")) {
    fin[[paste0("fmt_total_", b, "_", s)]] = fn_format(fin[[paste0("total_", b, "_", s)]])
  }
}

# Bloc HTML : un paragraphe par élection (bords + trois premiers)
bloc_scores_supp = 
  pmap(scores_supp_meta, function(suffixe, libelle) {
    paste0("<strong>", libelle, " : voix exprimées</strong><br/>",
           "Gauche : ",           fin[[paste0("fmt_total_gauche_", suffixe)]],
           "   Centre & divers : ", fin[[paste0("fmt_total_centre_", suffixe)]],
           "   Droite : ",          fin[[paste0("fmt_total_droite_", suffixe)]], "<br/>",
           fin[[paste0("podium_1_", suffixe)]], "<br/>",
           fin[[paste0("podium_2_", suffixe)]], "<br/>",
           fin[[paste0("podium_3_", suffixe)]], "<br/><br/>")
  }) %>% 
  reduce(paste0)

# Dans fin$label_html, insérer `bloc_scores_supp,` juste avant le bloc
# "Trois premières listes..." (ou après "Répartition des voix exprimées") :
#
#   "Gauche : ", fin$fmt_total_gauche, ... , "<br/>",
#   "<br/>",
#   bloc_scores_supp,
#   "<strong>Trois premières listes, ...
