# ==============================================================================
# PART DE GAUCHE PARMI LES NOUVEAUX VOTANTS, ET POTENTIEL : pipeline + deux couches de carte
# Mesures, entre un scrutin de référence et un scrutin nouveau où les voix exprimées augmentent :
#   - part_nouveaux  : part de la gauche parmi les voix exprimées SUPPLÉMENTAIRES (en %)
#   - ecart_nouveaux : part_nouveaux moins la part de gauche du scrutin de référence (points)
#                      > 0 : les nouveaux votants sont plus à gauche que l'électorat déjà mobilisé
#   - potentiel_*    : voix de gauche supplémentaires (nombre, et en points d'inscrits) si les 
#                      abstentionnistes restants votaient comme les nouveaux votants (scénario)
# Parties : A (lecteurs), B (fonction), C (jointure avec les contours), D (carte).
# ==============================================================================


# ==============================================================================
# PARTIE A -- LECTEURS : extraire les voix exprimées (colonne "Exprimés", même nom partout)
# Suppose que les modifications "inscrits/votants" de ajout_votants_pipeline.R sont faites.
# ==============================================================================

# --- lire_xlsx_burvot() -------------------------------------------------------
# A1. APRÈS   c_vot = col_id("^Votants$")   AJOUTER :
  c_exp = col_id("^Exprimés$")
# A2. REMPLACER   types[c(c_abs, c_ins, c_vot)] = "numeric"   PAR :
    types[c(c_abs, c_ins, c_vot, c_exp)] = "numeric"
# A3. DANS la création de `abstention`, REMPLACER   votants  = df[[pos(c_vot)]][garde])   PAR :
                      votants  = df[[pos(c_vot)]][garde],
                      exprimes = df[[pos(c_exp)]][garde])

# --- lire_csv_burvot() --------------------------------------------------------
# A4. APRÈS   c_vot = col_id("^Votants$")   AJOUTER :
  c_exp = col_id("^Exprimés$")
# A5. REMPLACER la création de `abstention` PAR :
  abstention = tibble(idBV_miomFormat = ids, pct_Abs_in_Ins = pct_fr(df[[c_abs]]),
                      inscrits = as.numeric(df[[c_ins]]), votants = as.numeric(df[[c_vot]]),
                      exprimes = as.numeric(df[[c_exp]]))

# --- lire_colonnes_nommees() --------------------------------------------------
# A6. Branche csv : REMPLACER   "% Abstentions", "Inscrits", "Votants",   PAR :
                    col_select = c("Code commune", "Code BV", "% Abstentions", "Inscrits", "Votants", "Exprimés",
# A7. Branche xlsx : REMPLACER   c("Code commune", "Code BV", "% Abstentions", "Inscrits", "Votants") |   PAR :
    utiles  = entetes %in% c("Code commune", "Code BV", "% Abstentions", "Inscrits", "Votants", "Exprimés") |
# A8. REMPLACER la création de `abstention` PAR :
  abstention = df %>% 
    transmute(idBV_miomFormat, pct_Abs_in_Ins = pct_fr(`% Abstentions`),
              inscrits = as.numeric(Inscrits), votants = as.numeric(Votants),
              exprimes = as.numeric(Exprimés))

# --- lire_txt_mun14() ---------------------------------------------------------
# A9. REMPLACER   distinct(idBV_miomFormat, inscrits, votants) %>%   PAR :
    distinct(idBV_miomFormat, inscrits, votants, exprimes) %>% 
# A10. REMPLACER la fin du transmute() de `abstention` PAR :
              pct_Abs_in_Ins = (inscrits - votants) / inscrits * 100,
              inscrits, votants, exprimes)


# ==============================================================================
# PARTIE B -- FONCTION ET JOINTURE : à ajouter dans la section 7 du pipeline, après la création 
# de res_bv_mrs (et après le bloc evolution_votants s'il est déjà intégré)
# ==============================================================================

# Notations (par BV) : E = voix exprimées, p = part de la famille (% des exprimés), 
# I = inscrits, V = votants. Voix de la famille = p x E / 100.
# Identité comptable : p_nouveau x E_nouveau = p_ref x E_ref + p_marginale x (E_nouveau - E_ref)
# => p_marginale = (p_nouveau x E_nouveau - p_ref x E_ref) / (E_nouveau - E_ref)
# Hypothèse : les électeurs du scrutin de référence ont voté pareil au scrutin nouveau ; toute 
# l'évolution est alors portée par les voix exprimées supplémentaires.
part_marginale = function(res, s_new, s_ref, part = "gauche", seuil_hausse = 0.05) {
  p_new = res[[paste0("total_", part, "_", s_new)]];  p_ref = res[[paste0("total_", part, "_", s_ref)]]
  E_new = res[[paste0("exprimes_", s_new)]];          E_ref = res[[paste0("exprimes_", s_ref)]]
  I_new = res[[paste0("inscrits_", s_new)]];          V_new = res[[paste0("votants_", s_new)]]
  
  dE = E_new - E_ref
  p_marg = (p_new * E_new - p_ref * E_ref) / dE
  
  # Le calcul n'a de sens que si les voix exprimées augmentent nettement (ici d'au moins 5 %)
  # et si la part obtenue est un pourcentage plausible (entre 0 et 100)
  statut = case_when(is.na(p_marg)                      ~ NA_character_,
                     dE <= seuil_hausse * E_ref         ~ "non défini",
                     p_marg < 0 | p_marg > 100          ~ "hors bornes",
                     TRUE                               ~ "ok")
  ok = !is.na(statut) & statut == "ok"
  
  # Potentiel : voix supplémentaires pour la famille si les abstentionnistes restants votaient 
  # comme les nouveaux votants (scénario, pas une prévision). Votants -> exprimés : taux E/V du scrutin nouveau.
  potentiel_voix = (I_new - V_new) * (E_new / V_new) * p_marg / 100
  
  tibble(idBV_miomFormat = res$idBV_miomFormat,
         statut                 = statut,
         part_nouveaux          = if_else(ok, p_marg, NA_real_),               # % des voix exprimées supplémentaires
         ecart_nouveaux         = if_else(ok, p_marg - p_ref, NA_real_),       # points, vs électorat du scrutin de référence
         potentiel_voix         = if_else(ok, potentiel_voix, NA_real_),       # voix
         potentiel_pts_inscrits = if_else(ok, 100 * potentiel_voix / I_new, NA_real_))  # points d'inscrits
}

# Couples de scrutins (nouveau, référence) et famille ("gauche", ou "lfi" si disponible aux deux scrutins) : à adapter
paires_marg = tribble(
  ~nouveau,     ~reference,   ~part,
  "leg1_2024",  "leg1_2022",  "gauche"
)

marg_bv = 
  pmap(paires_marg, function(nouveau, reference, part) {
    part_marginale(res_bv_mrs, nouveau, reference, part) %>% 
      rename_with(~ paste0(.x, "_", part, "_", nouveau, "_vs_", reference), -idBV_miomFormat)
  }) %>% 
  reduce(left_join, by = "idBV_miomFormat")

res_bv_mrs = left_join(res_bv_mrs, marg_bv, by = "idBV_miomFormat")
# Variables créées, par exemple : ecart_nouveaux_gauche_leg1_2024_vs_leg1_2022, 
# part_nouveaux_..., statut_..., potentiel_voix_..., potentiel_pts_inscrits_...


# ==============================================================================
# PARTIE C -- JOINTURE AVEC LES CONTOURS : dans le select() des colonnes à conserver pour `fin`, AJOUTER :
#     | contains("statut_") | contains("part_nouveaux") | contains("ecart_nouveaux") | contains("potentiel_")
# ==============================================================================


# ==============================================================================
# PARTIE D -- CARTE : deux couches (6 : nouveaux votants ; 7 : potentiel)
# D1 à placer avec les autres définitions de couches, avant la construction de `m`.
# Utilise pal_cols (palette colRoz), lib(), fn_format() et `fin`, déjà définis dans le chunk.
# ==============================================================================

# Couple de scrutins représenté sur la carte (doit figurer dans paires_marg)
couple_marg = c(nouveau = "leg1_2024", reference = "leg1_2022")
part_marg   = "gauche"
sfx_marg    = paste0(part_marg, "_", couple_marg[["nouveau"]], "_vs_", couple_marg[["reference"]])
statut_marg = fin[[paste0("statut_", sfx_marg)]]
niv_non_interp = "non interprétable (exprimés en hausse < 5 % ou part hors 0-100 %)"
gris = "#BDBDBD"

# Couche 6 : part de gauche parmi les nouveaux votants, comparée à celle de l'électorat de référence.
# Classes centrées sur 0, palette divergente différente des couches 3 et 4 (orange = moins à gauche, violet = plus à gauche).
lab_ecart = c("moins à gauche que l'électorat de référence (< -5 pt)", "comparable (-5 à +5 pt)", 
              "plus à gauche (+5 à +15 pt)", "nettement plus à gauche (+15 à +30 pt)", 
              "beaucoup plus à gauche (> +30 pt)")
ecart_cat = cut(fin[[paste0("ecart_nouveaux_", sfx_marg)]], breaks = c(-Inf, -5, 5, 15, 30, Inf), 
                labels = lab_ecart, include.lowest = TRUE)
ecart_cat = factor(ecart_cat, levels = c(lab_ecart, niv_non_interp))
ecart_cat[!is.na(statut_marg) & statut_marg != "ok"] = niv_non_interp
pal_ecart = colorFactor(palette = c("#F1A340", "#F7F7F7", "#D8DAEB", "#998EC3", "#542788", gris), 
                        domain = ecart_cat, na.color = "transparent")

# Couche 7 : potentiel = points d'inscrits de voix de gauche supplémentaires si les abstentionnistes 
# restants votaient comme les nouveaux votants (quintiles, palette colRoz).
pot = fin[[paste0("potentiel_pts_inscrits_", sfx_marg)]]
br  = unique(quantile(pot, probs = seq(0, 1, 0.2), na.rm = TRUE)); n_pot = length(br) - 1
lab_pot = paste0("Q", seq_len(n_pot), " (", formatC(head(br, -1), format = "f", digits = 1), " – ",
                 formatC(tail(br, -1), format = "f", digits = 1), " pt)")
pot_cat = factor(cut(pot, br, labels = lab_pot, include.lowest = TRUE), levels = c(lab_pot, niv_non_interp))
pot_cat[!is.na(statut_marg) & statut_marg != "ok"] = niv_non_interp
pal_pot = colorFactor(palette = c(pal_cols[seq_len(n_pot)], gris), domain = pot_cat, na.color = "transparent")

# Noms des couches (= libellés des boutons)
grp_ecart = paste0("Nouveaux votants, ", lib(couple_marg[["reference"]]), " → ", lib(couple_marg[["nouveau"]]))
grp_pot   = "Potentiel de la gauche"

# ------------------------------------------------------------------------------
# D2 -- DANS la construction de `m` : AJOUTER, avec les autres add_couche() :
# ------------------------------------------------------------------------------
  add_couche(pal_ecart(ecart_cat),  grp_ecart) %>%
  add_couche(pal_pot(pot_cat),      grp_pot) %>%

# D3 -- AJOUTER, avec les autres addLegend() :
  addLegend(
    pal = pal_ecart, values = ecart_cat, opacity = 0.7, position = "bottomleft",
    title = paste0("Part de gauche parmi les nouveaux votants (", lib(couple_marg[["nouveau"]]), 
                   " vs. ", lib(couple_marg[["reference"]]), "),<br/>comparée à celle de l'électorat de référence :"),
    className = "info legend leg-ecart"
  ) %>%
  addLegend(
    pal = pal_pot, values = pot_cat, opacity = 0.7, position = "bottomleft",
    title = paste0("Voix de gauche supplémentaires si les abstentionnistes restants de ", 
                   lib(couple_marg[["nouveau"]]), "<br/>votaient comme les nouveaux votants (scénario, en points d'inscrits) :"),
    className = "info legend leg-pot"
  ) %>%

# D4 -- DANS addLayersControl() et hideGroup() : AJOUTER grp_ecart et grp_pot
#   baseGroups = c(grp1, grp2, grp_delta, grp_ratio, grp_profil1426, grp_ecart, grp_pot),
#   hideGroup(c(grp2, grp_delta, grp_ratio, grp_profil1426, grp_ecart, grp_pot))

# D5 -- DANS le vecteur `legendes` (script JavaScript des légendes) : AJOUTER
#   setNames("leg-ecart", grp_ecart), setNames("leg-pot", grp_pot)


# ------------------------------------------------------------------------------
# D6 (optionnel) -- DANS label_html : AJOUTER, avant le "</div>" final ou en dernier paste0(), 
# deux lignes dans la fenêtre au clic (valeurs pour les BV interprétables)
# ------------------------------------------------------------------------------
  ,"<br/><strong>Nouveaux votants (", lib(couple_marg[["nouveau"]]), " vs. ", lib(couple_marg[["reference"]]), ") :</strong><br/>",
  ifelse(!is.na(statut_marg) & statut_marg == "ok",
         paste0("Part de gauche estimée : ", fn_format(fin[[paste0("part_nouveaux_", sfx_marg)]]),
                " (électorat de référence : ", fn_format(fin[[paste0("total_", part_marg, "_", couple_marg[["reference"]])]]), ")<br/>"),
         "Non interprétable : voix exprimées en hausse < 5 % ou part hors 0-100 %.<br/>")
