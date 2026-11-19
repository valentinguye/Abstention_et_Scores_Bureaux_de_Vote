# Correspondance des noms et corrections (doc_abstention_et_scores_bv_marseille_propre.Rmd)

## 1. Convention de noms

| Préfixe / forme | Contenu |
|---|---|
| `brk_*` | bornes des classes |
| `lab_*` | libellés des classes |
| `col_*` | couleurs (une par classe) |
| `grp_*` | libellé du bouton d'une couche |
| `couches$<id>` | une liste par couche : `id`, `grp`, `cat` (variable catégorielle), `pal` (palette), `titre`, `na_label` |
| `leg-<id>` | classe CSS de la légende (id avec des tirets) |
| `evol_<variable>_<aabb>` | évolution par BV, scrutin récent moins scrutin ancien, en points (aa = année du scrutin ancien, bb = année du scrutin récent) |
| `couche_*()` | constructeurs de couches |
| `detecte_*()` | repérage d'un parti dans les listes / candidats |

Les 13 couches (`names(couches)`, dans l'ordre des boutons) : `abs_mun26`, `abs_pres22`, `evol_mun1426`,
`evol_mun2026`, `evol_pres1722`, `evol_leg2224`, `profil_pres1722`, `profil_leg2224`, `part_gauche`,
`part_lfi`, `part_rn`, `solde_gauche`, `solde_lfi`.

## 2. Anciens noms -> nouveaux noms

### Objets des couches (chunk de la carte)

| Avant | Après |
|---|---|
| `q1`, `q2` ; `make_quintiles()` | `couches$abs_mun26`, `couches$abs_pres22` ; `couche_quintiles()` |
| `lab_delta`, `delta_cat`, `pal_delta`, `grp_evol1426` | `lab_evol_mun1426`, `col_evol_mun1426`, `couches$evol_mun1426` (`$cat`, `$pal`, `$grp`) |
| `lab_ratio`, `ratio_cat`, `pal_ratio`, `grp_evol2026`, `baisse_moy` | `lab_evol_mun2026`, `col_evol_mun2026`, `couches$evol_mun2026`, `baisse_moy_mun2026` |
| `lab_evol_pres`, `evol_pres_cat`, `pal_evol_pres`, `grp_evol1722` | `lab_evol_pres1722`, `col_evol_pres1722`, `couches$evol_pres1722` |
| `lab_baisse_pts`, `baisse_pts_cat`, `pal_baisse_pts`, `grp_evol2224` | `lab_evol_leg2224`, `col_evol_leg2224`, `couches$evol_leg2224` |
| `niveaux_profil` ; `pal_profil_1722`, `pal_profil_2224` ; `grp_profil_1722`, `grp_profil_2224` | `lab_profil` (chunk des variables dérivées) ; `col_profil` ; `couches$profil_pres1722`, `couches$profil_leg2224` |
| `cp_gauche` ; `couche_part()` (une famille) | `couches$part_gauche`, `part_lfi`, `part_rn` ; `couche_part(part)` |
| `solde_cat`, `pal_solde`, `lab_solde`, `grp_solde` | `couches$solde_gauche`, `couches$solde_lfi` ; `lab_solde`, `brk_solde`, `col_solde` ; `couche_solde(part)` |
| `breaks_part`, `cols_part` | `brk_part`, `col_part` |
| `niv_non_interp`, `gris`, `pal_cols` | `lab_non_interp`, `col_gris`, `col_colroz` |
| classes CSS `leg-ref`, `leg-pres`, `leg-delta`, `leg-ratio`, `leg-profil_2224`... | `leg-abs-mun26`, `leg-abs-pres22`, `leg-evol-mun1426`... (`leg-<id>`) |
| vecteur `legendes` écrit à la main | `setNames(css_couches, grp_couches)` |

### Variables par BV (`res_bv_mrs`, puis `fin`)

| Avant | Après |
|---|---|
| `baisse_abs_1426`, `baisse_abs_1722`, `baisse_abs_2224` (ancien - récent) | `evol_abs_1426`, `evol_abs_1722`, `evol_abs_2224` (récent - ancien : **signe inversé**, négatif = baisse) |
| `delta_abs`, `diff_pct_Abs_2026from2014` | `evol_abs_1426` |
| `baisse_2020_2026` | `-evol_abs_2026` |
| `ratio_baisse` | calculé dans le chunk de la carte à partir de `evol_abs_2026` |
| `baisse_pts_leg` | `-evol_abs_2224` |
| `evol_pts_pres` | `evol_abs_1722` |
| `var_gauche_1722`, `var_gauche_2224` | `evol_gauche_1722`, `evol_gauche_2224` |
| `profil_1722`, `profil_2224` | inchangés (facteurs de niveaux `lab_profil`) |

### Pipeline

| Avant | Après |
|---|---|
| `est_lfi_nuance()`, `est_lfi_candidat()` | `detecte_lfi(format, etiquette, nom)` |
| (absent) | `detecte_rn(format, etiquette)`, variable `total_rn_<scrutin>` |
| `sfx_marg` (variable, redéfinie dans la carte), `sfx_lfi`... | fonction `sfx_marg(part)` définie une fois dans le pipeline |
| `lire_csv_mun26()`... | `lire_colonnes_nommees()` (inchangé depuis la version précédente) |

Supprimés (inutilisés) : `elec_compar`, `elec_leg_ref`, `elec_pres_ref`, `annee_pres_ref`, `annee_leg_ref`, `fmt_evol()`,
`gauche_ref`, `droite_ref`, `niveaux_profil_avecLFI`, `profil_1426`, `var_lfi_1722`, `diff_pct_Abs_2026from2014`.

## 3. Erreurs corrigées

1. **Couches 5 (évolution 2017 -> 2022) et 7 (profils présidentielles) jamais ajoutées à la carte** : définies mais absentes de
   `add_couche()`, `addLegend()`, `addLayersControl()`, `hideGroup()` et de la liste `legendes`.
2. **`pal_profil_1722` construite sur `fin$profil_2224`** (copier-coller).
3. **Balise HTML non fermée** dans le titre de la couche 6 : `<u>baissé<u>` -> `<u>baissé</u>`.
4. **`View(check)`** dans un chunk exécuté au knit (échoue hors session interactive), et `table()` sur des variables
   inexistantes (`profil_1426`, `profil_1722_avecLFI`) : supprimés.
5. **Mesure LFI 2017 -> 2022 des nouvelles voix** (`paires_marg`) : supprimée (pas de nouvelles voix).
6. **Contrôle « somme des bords » du pipeline** : commentaire coupé en pleine phrase et lignes mal indentées nettoyés.
7. **`LFI` dans les législatives** : `total_lfi` valait NA (nuances NUP / UG) ; repéré maintenant par le nom des candidats.
8. **Parts nulles** (famille sans candidat dans la circonscription) : elles produisaient une part « 0 % » trompeuse ; nouveau statut
   `"sans candidat"` (classe grise).
9. **`lire_colonnes_nommees()`, branche xlsx** : en-têtes vides (`NA`) gérés (`replace_na`), indentation corrigée.
10. **Paquets inutilisés** retirés du setup (liste en commentaire) : `xlsx` (dépend de Java), `fixest`, `DescTools`, `rnaturalearth`,
    `ggpubr`, `units`, `scales`, `kableExtra`, `webshot`, `mapvotr`.
11. **Code mort** retiré : lecture commentée des secteurs de Paris, vérifications `length()` sans effet, essais de profils
    commentés, bloc commenté du label.
