# Variable à cartographier : abstention au T1 des municipales 2020 (scrutin de référence)
field_name = abs_ref
# adresse_field <- "adresseBV"

# ------------------------------------------------------------------------------
# Préparation du html à renvoyer au clique sur la carte
# ------------------------------------------------------------------------------
fn_format = function(var){ifelse(
              is.na(var),
              NA_character_,
              paste0(formatC(var, format = "f", digits = 1, big.mark = " "), " %")
            )}

# Scrutins affichés dans le label
elec_pres   = "pres1_2022"   # présidentielle : abstention + bords + podium
elec_mun    = "mun1_2026"    # municipales : abstention + évolution
elec_compar = "mun1_2014"    # scrutin de comparaison pour l'évolution de l'abstention
lib = function(s) elections$libelle[elections$suffixe == s]

# Évolution en points de pourcentage, avec une flèche (▲ hausse, ▼ baisse)
fmt_evol = function(actuel, ref) {
  delta  = round(actuel - ref, 1)               # le signe se lit sur la valeur arrondie
  fleche = case_when(delta > 0 ~ "▲", delta < 0 ~ "▼", TRUE ~ "=")
  ifelse(is.na(delta),
         "(pas de résultat en 2014)",
         paste0("(", fleche, " ", formatC(abs(delta), format = "f", digits = 1), " pt vs. 2014)"))
}

fin$label_html <- 
  paste0(
    "<div style='
      font-size:20px;
      line-height:1.5;
      padding:3px 8px;
      max-width:700px;
  '>",
  
  "<strong>Abstention</strong><br/>",
  lib(elec_pres), " : ", fn_format(fin[[paste0("pct_Abs_in_Ins_", elec_pres)]]), "<br/>",
  lib(elec_mun),  " : ", fn_format(fin[[paste0("pct_Abs_in_Ins_", elec_mun)]]), " ",
  fmt_evol(fin[[paste0("pct_Abs_in_Ins_", elec_mun)]], 
           fin[[paste0("pct_Abs_in_Ins_", elec_compar)]]), "<br/><br/>",
  
  "<strong>Répartition des voix exprimées et podium des candidats au T1 de la présidentielle 2022 :</strong><br/>",
  "Gauche : ",             fn_format(fin[[paste0("total_gauche_", elec_pres)]]),
  "   Centre & divers : ", fn_format(fin[[paste0("total_centre_", elec_pres)]]),
  "   Droite : ",          fn_format(fin[[paste0("total_droite_", elec_pres)]]), "<br/>",
  fin[[paste0("podium_1_", elec_pres)]], "<br/>",
  fin[[paste0("podium_2_", elec_pres)]], "<br/>",
  fin[[paste0("podium_3_", elec_pres)]], "<br/>"
  )


# ------------------------------------------------------------------------------
# 3. Trois couches commutables : variable, catégories et palette de chacune
# ------------------------------------------------------------------------------

# Palette colRoz (5 couleurs), triée clair -> foncé quel que soit l'ordre renvoyé par le paquet
pal_cols = substr(as.character(paletteer_d("colRoz::v_acanthurus")), 1, 7)   # retire un éventuel canal alpha
lum      = colSums(col2rgb(pal_cols) * c(0.299, 0.587, 0.114))
pal_cols = pal_cols[order(lum, decreasing = TRUE)]

# Nice palettes: 
# colRoz::v_acanthurus
# MexBrewer::Naturaleza
# calecopal::desert - trop militaire

# Quintiles d'une variable (calculés sur les BV cartographiés) + palette associée
make_quintiles = function(vals) {
  breaks  = quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE) %>% unique()  # 6 cut points
  n_class = length(breaks) - 1
  labs = paste0("Q", seq_len(n_class), " (",
                formatC(head(breaks, -1), format = "f", digits = 1), " – ",
                formatC(tail(breaks, -1), format = "f", digits = 1), " %)")
  f = cut(vals, breaks = breaks, labels = labs, include.lowest = TRUE)
  list(f = f,
       pal = colorFactor(palette = pal_cols[seq_len(n_class)], domain = f, na.color = "transparent"))
}

# Couche 1 : abstention au T1 des municipales 2020
q_ref  = make_quintiles(fin[[field_name]])
# Couche 2 : abstention au T1 de la présidentielle 2022 (quintiles propres à ce scrutin)
q_pres = make_quintiles(fin[[paste0("pct_Abs_in_Ins_", elec_pres)]])

# Couche 3 : évolution de l'abstention, T1 municipales 2014 -> 2026, en points de pourcentage.
# La variable est signée (baisse / hausse) : on utilise des classes fixes centrées sur 0 
# et une palette divergente (violet = baisse, orange = hausse, blanc = stable), plutôt 
# que des quintiles dont les bornes ne tomberaient pas sur 0.
fin$delta_abs = fin[[paste0("pct_Abs_in_Ins_", elec_mun)]] - fin[[paste0("pct_Abs_in_Ins_", elec_compar)]]
lab_delta = c("baisse de plus de 10 pt", "baisse de 5 à 10 pt", "baisse de 2 à 5 pt", "stable (± 2 pt)",
              "hausse de 2 à 5 pt", "hausse de 5 à 10 pt", "hausse de plus de 10 pt")
delta_cat = cut(fin$delta_abs, breaks = c(-Inf, -10, -5, -2, 2, 5, 10, Inf), 
                labels = lab_delta, include.lowest = TRUE)
pal_delta = colorFactor(palette = rev(RColorBrewer::brewer.pal(7, "PuOr")), 
                        domain = delta_cat, na.color = "transparent")

# Noms des couches (= libellés des boutons)
grp_ref   = paste0("Abstention, ", lib(election_ref))
grp_pres  = paste0("Abstention, ", lib(elec_pres))
grp_delta = "Évolution de l'abstention, T1 municipales 2014 → 2026"

# ------------------------------------------------------------------------------
# 4. Carte leaflet : une couche de polygones par variable, commutées par addLayersControl()
# ------------------------------------------------------------------------------
# Les trois couches ont la même géométrie et le même popup, seules les couleurs changent.
add_couche = function(map, couleurs, groupe) {
  map %>% 
    addPolygons(
      fillColor = couleurs,
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      popup = ~label_html,
      popupOptions = popupOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        direction = "auto"
      ),
      group = groupe
    )
}

m <- leaflet(fin) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  add_couche(q_ref$pal(q_ref$f),    grp_ref) %>%
  add_couche(q_pres$pal(q_pres$f),  grp_pres) %>%
  add_couche(pal_delta(delta_cat),  grp_delta) %>%
  # Une légende par couche ; la classe CSS "leg-*" sert à les afficher/masquer (cf. onRender ci-dessous)
  addLegend(
    pal = q_ref$pal, values = q_ref$f, opacity = 0.7, position = "bottomleft",
    title = paste0("Abstention, ", lib(election_ref), "<br/>(quintiles de BV)"),
    className = "info legend leg-ref"
  ) %>%
  addLegend(
    pal = q_pres$pal, values = q_pres$f, opacity = 0.7, position = "bottomleft",
    title = paste0("Abstention, ", lib(elec_pres), "<br/>(quintiles de BV)"),
    className = "info legend leg-pres"
  ) %>%
  addLegend(
    pal = pal_delta, values = delta_cat, opacity = 0.7, position = "bottomleft",
    title = "Évolution de l'abstention<br/>T1 municipales 2014 → 2026",
    na.label = "pas de résultat en 2014",
    className = "info legend leg-delta"
  ) %>%
  # Boutons radio : une seule couche affichée à la fois (la première est affichée au départ)
  addLayersControl(
    baseGroups = c(grp_ref, grp_pres, grp_delta),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  ) %>%
  hideGroup(c(grp_pres, grp_delta))

# Les légendes ne suivent pas automatiquement les couches : ce petit script JavaScript 
# (exécuté dans le navigateur, sans serveur) n'affiche que la légende de la couche active.
legendes = c(setNames("leg-ref", grp_ref), setNames("leg-pres", grp_pres), setNames("leg-delta", grp_delta))

js_legendes = sprintf("
function(el, x) {
  var map = this;
  var legendes = %s;
  function afficher(nom) {
    Object.keys(legendes).forEach(function(groupe) {
      el.querySelectorAll('.' + legendes[groupe]).forEach(function(div) {
        div.style.display = (groupe === nom) ? '' : 'none';
      });
    });
  }
  afficher(%s);
  map.on('baselayerchange', function(e) { afficher(e.name); });
}",
  jsonlite::toJSON(as.list(legendes), auto_unbox = TRUE),
  jsonlite::toJSON(grp_ref, auto_unbox = TRUE)
)

m <- m %>% htmlwidgets::onRender(js_legendes)

# 5. Display the map in RStudio viewer
m

# add title
m_titled <- htmlwidgets::prependContent(
  m,
  htmltools::tags$h3("Abstention et résultats électoraux par bureau de vote")
)

# 6. Save it as an interactive HTML file. Has to be named "index" for web publication on github. 
saveWidget(m_titled, file = here("index.html"), selfcontained = TRUE)
