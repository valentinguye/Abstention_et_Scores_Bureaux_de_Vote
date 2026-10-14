# ==============================================================================
# AJOUT DE 4 RÉSULTATS ÉLECTORAUX SUPPLÉMENTAIRES (abstention par BV, Marseille)
#   - présidentielle 2022, T1 et T2 (xlsx, format "burvot")
#   - municipales 2026, T1 et T2 (csv, format différent : séparateur ";",
#     pourcentages en texte avec virgule, code commune à 5 chiffres)
# ==============================================================================

# ------------------------------------------------------------------------------
# PARTIE A -- à coller dans le chunk "DATA PREPARATION", juste APRÈS la jointure
# de reg_bv sur mun_bv (le bloc qui crée pct_Abs_in_Ins_avgt1Mun20Reg21).
# ------------------------------------------------------------------------------

# 1. Table de description des élections à ajouter (fichiers dans input_data/)
elections_supp = tribble(
  ~suffixe,       ~libelle,                       ~fichier,
  "pres1_2022",   "T1 présidentielle 2022",       "presidentielles22_resultats-par-niveau-burvot-t1-france-entiere.xlsx",
  "pres2_2022",   "T2 présidentielle 2022",       "presidentielles22_resultats-par-niveau-burvot-t2-france-entiere.xlsx",
  "mun1_2026",    "T1 municipales 2026",          "municipales-2026-resultats-bv-par-communes-2026-03-20.csv",
  "mun2_2026",    "T2 municipales 2026",          "municipales-2026-resultats-bureau-de-vote-2026-03-23-16h15.csv"
)

# 2. Fonction de lecture + préparation d'un fichier de résultats par BV.
#    Elle gère les deux formats rencontrés ici et renvoie, pour Marseille
#    uniquement, deux colonnes : la clé idBV_miomFormat (identique à celle de
#    mun_bv / reg_bv, ex. "13055_0101") et le % d'abstention (numérique).
prep_resultats_bv = function(fichier, suffixe, dpt = "13", cmn = "055") {

  chemin  = here("input_data", fichier)
  est_csv = str_detect(fichier, regex("\\.csv$", ignore_case = TRUE))

  if (est_csv) {
    # --- Format municipales 2026 --------------------------------------------
    # Tout est lu en texte ; "Code commune" contient déjà dpt + commune (13055),
    # et les pourcentages sont du type "47,77%".
    df = read_delim(chemin, delim = ";",
                    col_types = cols(.default = col_character()),
                    col_select = c("Code commune", "Code BV", "% Abstentions"))

    out = 
      df %>% 
      # On filtre sur le CODE commune (le libellé "Marseille" attrape aussi
      # Marseillette, dans l'Aude)
      filter(`Code commune` == paste0(dpt, cmn)) %>% 
      transmute(
        idBV_miomFormat = paste0(`Code commune`, "_", 
                                 str_pad(`Code BV`, width = 4, "left", pad = "0")),
        pct_abs = as.numeric(str_replace(str_remove(`% Abstentions`, "%"), ",", "."))
      )

  } else {
    # --- Format "résultats par niveau burvot" (présidentielle 2022) ----------
    df = read_xlsx(chemin, guess_max = 10000)

    trouve = function(motif) {
      nm = names(df)[str_detect(names(df), regex(motif, ignore_case = TRUE))]
      if (length(nm) == 0) stop("Colonne introuvable dans ", fichier, " : motif ", motif)
      nm[1]
    }
    col_dpt = trouve("^Code du département$")
    col_cmn = trouve("^Code de la commune$")
    col_bv  = trouve("^Code (du )?b\\.? ?vote$")
    col_abs = trouve("^% Abs/Ins$")

    out = 
      df %>% 
      transmute(
        codeDpt = str_pad(as.character(.data[[col_dpt]]), width = 2, "left", pad = "0"),
        codeCmn = str_pad(as.character(.data[[col_cmn]]), width = 3, "left", pad = "0"),
        codeBV_inCommune = str_pad(as.character(.data[[col_bv]]), width = 4, "left", pad = "0"),
        idBV_miomFormat = paste0(codeDpt, codeCmn, "_", codeBV_inCommune),
        pct_abs = as.numeric(.data[[col_abs]])
      ) %>% 
      filter(codeDpt == dpt, codeCmn == cmn) %>% 
      select(idBV_miomFormat, pct_abs)
  }

  # Une seule ligne par BV, sinon la jointure dupliquerait des lignes
  stopifnot(!anyDuplicated(out$idBV_miomFormat))

  names(out)[names(out) == "pct_abs"] = paste0("pct_Abs_in_Ins_", suffixe)
  out
}

# 3. Lecture de tous les fichiers (un peu long : les xlsx font France entière)
liste_supp = map2(elections_supp$fichier, elections_supp$suffixe, prep_resultats_bv)

# 4. Contrôles de couverture AVANT la jointure : BV présents dans le fichier
#    mais absents de la base 2020 (BV créés/renumérotés depuis, ils seront
#    écartés de la carte), et inversement.
cles_mrs_2020 = mun_bv %>% filter(nomCommune == "Marseille") %>% pull(idBV_miomFormat)
walk2(liste_supp, elections_supp$suffixe, function(d, s) {
  cat(s, ":", nrow(d), "BV Marseille |",
      sum(!d$idBV_miomFormat %in% cles_mrs_2020), "absents de la base 2020 |",
      sum(!cles_mrs_2020 %in% d$idBV_miomFormat), "BV 2020 sans résultat\n")
})

# 5. Jointure successive sur mun_bv
mun_bv = reduce(liste_supp, left_join, by = "idBV_miomFormat", .init = mun_bv)

# 6. (Optionnel) Moyenne de l'abstention sur toutes les élections disponibles.
#    À adapter à ce que tu veux représenter : remplace 
#    pct_Abs_in_Ins_avgt1Mun20Reg21 par cette variable, ou ajoute-la.
cols_abs = c("pct_Abs_in_Ins", "pct_Abs_in_Ins_regt1_2021", 
             paste0("pct_Abs_in_Ins_", elections_supp$suffixe))

mun_bv = 
  mun_bv %>% 
  mutate(pct_Abs_in_Ins_avg_toutes = rowMeans(across(all_of(cols_abs)), na.rm = TRUE))


# ------------------------------------------------------------------------------
# NB : pas de modification nécessaire dans la jointure vers `fin`. 
# Le select() existant garde déjà toutes les colonnes contenant "pct" ou "Abs", 
# donc les nouvelles colonnes pct_Abs_in_Ins_<suffixe> passent automatiquement.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# PARTIE B -- chunk de la carte, juste APRÈS la définition de fn_format() 
# et le mutate(fmt_...) sur `fin`.
# ------------------------------------------------------------------------------

# Colonnes formatées pour le popup
for (s in elections_supp$suffixe) {
  fin[[paste0("fmt_pct_Abs_in_Ins_", s)]] = fn_format(fin[[paste0("pct_Abs_in_Ins_", s)]])
}

# Bloc HTML "une ligne par élection supplémentaire"
bloc_abs_supp = 
  pmap(elections_supp, function(suffixe, libelle, fichier) {
    paste0(libelle, " : ", fin[[paste0("fmt_pct_Abs_in_Ins_", suffixe)]], "<br/>")
  }) %>% 
  reduce(paste0)

# Dans la construction de fin$label_html, ajouter `bloc_abs_supp,` juste après 
# la ligne des régionales 2021 :
#
#   "T1 municipales 2020: ", fin$fmt_pct_Abs_in_Ins, "<br/>",
#   "T1 regionales 2021: ", fin$fmt_pct_Abs_in_Ins_regt1_2021, "<br/>",
#   bloc_abs_supp,
#   "<br/>",
