library(ggplot2)
library(dplyr)
library(ggiraph)
library(lubridate)

# ── Load SDTM domains ─────────────────────────────────────────────────────────
setwd("C:/Users/user/Desktop/shiny_apps/sdtm app")
dm <- read_xpt("dm.xpt")
ds <- read_xpt("ds.xpt")
se <- read_xpt("se.xpt")
sv <- read_xpt("sv.xpt")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 1 — DM: get reference start date per subject
# ══════════════════════════════════════════════════════════════════════════════
dm_ref <- dm %>%
  select(usubjid = USUBJID, rfstdtc = RFSTDTC) %>%
  filter(!is.na(rfstdtc), rfstdtc != "") %>%
  mutate(rfstdtc = as.Date(rfstdtc))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2 — SE: epochs
# ══════════════════════════════════════════════════════════════════════════════
epochs <- se %>%
  select(
    usubjid    = USUBJID,
    epoch      = ELEMENT,
    sestdtc    = SESTDTC,
    seendtc    = SEENDTC
  ) %>%
  filter(!is.na(sestdtc), sestdtc != "") %>%
  mutate(
    sestdtc = as.Date(sestdtc),
    seendtc = as.Date(seendtc)
  ) %>%
  left_join(dm_ref, by = "usubjid") %>%
  mutate(
    epoch_start = as.numeric(sestdtc - rfstdtc),
    epoch_end   = as.numeric(seendtc - rfstdtc),
    tooltip     = paste0(
      usubjid, " · ", epoch, "\n",
      "Start : ", format(sestdtc, "%d %b %Y"), "\n",
      "End   : ", format(seendtc, "%d %b %Y")
    )
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 3 — SV: visits
# ══════════════════════════════════════════════════════════════════════════════
visits_raw <- sv %>%
  select(
    usubjid  = USUBJID,
    visitnum = VISITNUM,
    visit    = VISIT,
    svstdtc  = SVSTDTC,
    svendtc  = SVENDTC
  ) %>%
  filter(!is.na(svstdtc), svstdtc != "") %>%
  mutate(
    svstdtc  = as.Date(svstdtc),
    svendtc  = as.Date(svendtc),
    # use VISITNUM as label, formatted as V1, V2 etc
    visit_label = paste0("V", visitnum)
  ) %>%
  left_join(dm_ref, by = "usubjid") %>%
  mutate(
    v_start = as.numeric(svstdtc - rfstdtc),
    v_end   = as.numeric(svendtc - rfstdtc)
  )

# ── Detect overlaps ───────────────────────────────────────────────────────────
visits <- visits_raw %>%
  group_by(usubjid) %>%
  arrange(v_start, .by_group = TRUE) %>%
  mutate(
    prev_end   = lag(v_end, default = -1),
    is_overlap = v_start <= prev_end
  ) %>%
  mutate(
    next_overlap = lead(is_overlap, default = FALSE),
    flag_overlap = is_overlap | next_overlap
  ) %>%
  ungroup() %>%
  mutate(
    tooltip = paste0(
      usubjid, " · ", visit_label, " (", visit, ")\n",
      "Start : ", format(svstdtc, "%d %b %Y"), "\n",
      "End   : ", format(svendtc, "%d %b %Y"),
      ifelse(flag_overlap, "\n⚠ Overlap with adjacent visit", "")
    )
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 4 — DS: disposition
# ══════════════════════════════════════════════════════════════════════════════
# Keep only the primary disposition event per subject (DSCAT == "DISPOSITION EVENT"
# and DSSEQ == 1, i.e. the first/primary event)
disposition <- ds %>%
  select(
    usubjid   = USUBJID,
    dsdecod   = DSDECOD,
    dsstdtc   = DSSTDTC,
    dsstdy    = DSSTDY,
    dscat     = DSCAT
  ) %>%
  filter(dscat == "DISPOSITION EVENT") %>%
  group_by(usubjid) %>%
  slice(1) %>%           # one row per subject — primary disposition event
  ungroup() %>%
  mutate(dsstdtc = as.Date(dsstdtc)) %>%
  left_join(dm_ref, by = "usubjid") %>%
  mutate(
    # days from study start — use DSSTDY if available, otherwise calculate
    end_day = case_when(
      !is.na(dsstdy) ~ as.numeric(dsstdy),
      !is.na(dsstdtc) & !is.na(rfstdtc) ~ as.numeric(dsstdtc - rfstdtc),
      TRUE ~ NA_real_
    ),
    completed  = dsdecod == "COMPLETED",
    disp_label = case_when(
      dsdecod == "COMPLETED"                   ~ "Completed",
      dsdecod == "ADVERSE EVENT"               ~ "Discontinued — Adverse Event",
      dsdecod == "SCREEN FAILURE"              ~ "Discontinued — Screen Failure",
      dsdecod == "WITHDRAWAL BY SUBJECT"       ~ "Discontinued — Withdrew Consent",
      dsdecod == "STUDY TERMINATED BY SPONSOR" ~ "Discontinued — Sponsor Decision",
      dsdecod == "LOST TO FOLLOW-UP"           ~ "Discontinued — Lost to Follow-up",
      dsdecod == "DEATH"                       ~ "Discontinued — Death",
      TRUE                                     ~ paste0("Discontinued — ", dsdecod)
    ),
    tooltip = paste0(
      usubjid, "\n",
      disp_label, "\n",
      "Day ", end_day, ": ", format(dsstdtc, "%d %b %Y")
    )
  ) %>%
  filter(!is.na(end_day))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 5 — Milestones from DS (ICF, Randomization etc.)
# ══════════════════════════════════════════════════════════════════════════════
# Pull key milestone events from DS — adjust DSDECOD values to match your data
milestones <- ds %>%
  select(
    usubjid = USUBJID,
    dsdecod = DSDECOD,
    dsstdtc = DSSTDTC,
    dsstdy  = DSSTDY,
    dscat   = DSCAT
  ) %>%
  filter(dsdecod %in% c(
    "INFORMED CONSENT OBTAINED",   # ICF1
    "RANDOMIZED",                  # Randomized
    "COMPLETED"                    # End
  )) %>%
  mutate(dsstdtc = as.Date(dsstdtc)) %>%
  left_join(dm_ref, by = "usubjid") %>%
  mutate(
    m_day = case_when(
      !is.na(dsstdy) ~ as.numeric(dsstdy),
      !is.na(dsstdtc) & !is.na(rfstdtc) ~ as.numeric(dsstdtc - rfstdtc),
      TRUE ~ NA_real_
    ),
    milestone = case_when(
      dsdecod == "INFORMED CONSENT OBTAINED" ~ "ICF1",
      dsdecod == "RANDOMIZED"                ~ "Randomized",
      dsdecod == "COMPLETED"                 ~ "End",
      TRUE                                   ~ dsdecod
    ),
    tooltip = paste0(
      usubjid, " · ", milestone, "\n",
      "Day ", m_day, ": ", format(dsstdtc, "%d %b %Y")
    )
  ) %>%
  filter(!is.na(m_day))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 6 — Subject ordering and factor levels
# ══════════════════════════════════════════════════════════════════════════════
subj_order <- disposition %>%
  arrange(end_day) %>%
  pull(usubjid) %>%
  as.character()

epochs      <- epochs      %>% mutate(usubjid = factor(usubjid, levels = subj_order))
visits      <- visits      %>% mutate(usubjid = factor(usubjid, levels = subj_order))
milestones  <- milestones  %>% mutate(usubjid = factor(usubjid, levels = subj_order))
disposition <- disposition %>% mutate(usubjid = factor(usubjid, levels = subj_order))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 7 — Colour / shape scales
# ══════════════════════════════════════════════════════════════════════════════
# Epoch colours — keyed on ELEMENT values from your SE domain
# Adjust names to match whatever ELEMENT values appear in your data
epoch_colors <- c(
  "Screen"      = "#85B7EB",
  "Placebo"     = "#FCBBC7",
  "Low"         = "#C3FA8F",
  "High_Start"  = "#FAD68F",
  "High_Middle" = "#D4A8F5",
  "High_End"    = "#F5D4A8",
  "Follow_up"   = "#A8F5D4"
)

milestone_shapes <- c(ICF1 = 21, Randomized = 24, End = 23)
milestone_fills  <- c(ICF1 = "#B30B7E", Randomized = "yellow", End = "#534AB7")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 8 — Direct colour vectors (avoids TRUE/FALSE clash in scale_colour_manual)
# ══════════════════════════════════════════════════════════════════════════════
visit_colours <- ifelse(visits$flag_overlap, "#E24B4A", "#2742F5")
disp_colours  <- ifelse(disposition$completed, "#0F6E56", "#A32D2D")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 9 — Build plot
# ══════════════════════════════════════════════════════════════════════════════
# x-axis upper limit: max end day + buffer for disposition labels
x_max <- max(disposition$end_day, na.rm = TRUE) + 80

p <- ggplot() +
  
  # Epoch bars
  geom_segment_interactive(
    data = epochs,
    aes(x = epoch_start, xend = epoch_end,
        y = usubjid, yend = usubjid,
        colour  = epoch,
        tooltip = tooltip,
        data_id = paste(usubjid, epoch)),
    linewidth = 5, lineend = "round"
  ) +
  
  # Normal visit windows
  geom_segment_interactive(
    data = visits %>% filter(!flag_overlap),
    aes(x = v_start, xend = v_end,
        y = usubjid, yend = usubjid,
        tooltip = tooltip,
        data_id = paste(usubjid, visit_label)),
    colour = "#2742F5", linewidth = 3.5, lineend = "round"
  ) +
  
  # Overlapping visit windows
  geom_segment_interactive(
    data = visits %>% filter(flag_overlap),
    aes(x = v_start, xend = v_end,
        y = usubjid, yend = usubjid,
        tooltip = tooltip,
        data_id = paste(usubjid, visit_label)),
    colour = "#E24B4A", linewidth = 5, lineend = "round"
  ) +
  
  # Visit labels above bars
  geom_text(
    data = visits,
    aes(x     = (v_start + v_end) / 2,
        y     = usubjid,
        label = visit_label),
    colour   = visit_colours,
    vjust    = -1.6,
    size     = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Milestone markers
  geom_point_interactive(
    data = milestones,
    aes(x       = m_day,
        y       = usubjid,
        shape   = milestone,
        fill    = milestone,
        tooltip = tooltip,
        data_id = paste(usubjid, milestone)),
    size = 3.2, colour = "white", stroke = 0.7
  ) +
  
  # End-of-bar tick
  geom_point_interactive(
    data = disposition,
    aes(x       = end_day,
        y       = usubjid,
        tooltip = tooltip,
        data_id = paste(usubjid, "end")),
    shape = 124, size = 5, colour = "grey30"
  ) +
  
  # Disposition label after bar
  geom_text_interactive(
    data = disposition,
    aes(x       = end_day + 4,
        y       = usubjid,
        label   = disp_label,
        tooltip = tooltip,
        data_id = paste(usubjid, "end_label")),
    colour = disp_colours, hjust = 0, size = 3.5, show.legend = FALSE
  ) +
  
  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_colour_manual(
    values = epoch_colors,
    breaks = names(epoch_colors),
    name   = "Study Element / Epoch (SE)"
  ) +
  
  scale_fill_manual(
    values = milestone_fills,
    name   = "Disposition Event (DS)"
  ) +
  
  scale_shape_manual(
    values = milestone_shapes,
    name   = "Disposition Event (DS)"
  ) +
  
  scale_x_continuous(
    name   = "Days from first study treatment (RFSTDTC)",
    breaks = seq(
      floor(min(c(epochs$epoch_start, visits$v_start), na.rm=TRUE) / 28) * 28,
      ceiling(x_max / 28) * 28,
      by = 28
    ),
    expand = expansion(mult = c(0.01, 0))
  ) +
  
  scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
  
  coord_cartesian(xlim = c(
    floor(min(c(epochs$epoch_start, visits$v_start), na.rm=TRUE) / 28) * 28,
    x_max
  ), clip = "off") +
  
  labs(
    title    = "Swimmer Plot — Disposition (DS), Study Visits (SV) and Study Elements (SE)",
    subtitle = paste0(
      "Bars = SE epoch   Thin bars = SV visit window   ",
      "Red = visit overlap (data error)\n",
      "\u25CF ICF1   \u25B2 Randomized   \u25C6 End of study"
    ),
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    axis.text.x        = element_text(size = 12),
    axis.text.y        = element_text(size = 10),
    legend.position    = "bottom",
    legend.box         = "vertical",
    legend.title       = element_text(face = "bold", size = 14),
    legend.text        = element_text(size = 14),
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(colour = "grey40", size = 13,
                                      margin = margin(b = 15)),
    plot.margin        = margin(t = 6, r = 5, b = 10, l = 10)
  ) +
  
  guides(
    colour = guide_legend(override.aes = list(linewidth = 4), order = 1),
    fill   = guide_legend(order = 2),
    shape  = guide_legend(order = 2)
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 10 — Render
# ══════════════════════════════════════════════════════════════════════════════
girafe(
  ggobj      = p,
  width_svg  = 16,
  height_svg = max(8, length(subj_order) * 0.5),  # auto-scale height to n subjects
  options    = list(
    opts_tooltip(
      css     = "background:white;border:1px solid #ccc;padding:6px 10px;
                 border-radius:6px;font-size:14px;
                 font-family:sans-serif;white-space:pre;",
      opacity = 0.95
    ),
    opts_hover(css = "opacity:0.75;cursor:crosshair;"),
    opts_sizing(rescale = TRUE)
  )
)
