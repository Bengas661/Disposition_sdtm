library(ggplot2)
library(dplyr)
library(ggiraph)
library(lubridate)
library(haven)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 0 — Load data
# ══════════════════════════════════════════════════════════════════════════════
setwd("C:/Users/user/Desktop/shiny_apps/sdtm app")

dm <- read_xpt("dm.xpt")
ds <- read_xpt("ds.xpt")
se <- read_xpt("se.xpt")
sv <- read_xpt("sv.xpt")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 1 — Baseline anchor: VISITNUM == 3 (BASELINE) from SV
# Day 0 = date of BASELINE visit per subject
# ══════════════════════════════════════════════════════════════════════════════
baseline_ref <- sv %>%
  filter(VISITNUM == 3) %>%
  select(usubjid = USUBJID, baseline_date = SVSTDTC) %>%
  mutate(baseline_date = as.Date(baseline_date))

# Use RFXSTDTC from DM as fallback
dm_ref <- dm %>%
  select(usubjid = USUBJID, rfxstdtc = RFXSTDTC) %>%
  filter(!is.na(rfxstdtc), rfxstdtc != "") %>%
  mutate(rfxstdtc = as.Date(rfxstdtc))

# Merge: prefer baseline_date, fall back to rfxstdtc
day0_ref <- dm_ref %>%
  left_join(baseline_ref, by = "usubjid") %>%
  mutate(day0 = coalesce(baseline_date, rfxstdtc))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2 — SE: epochs
# ══════════════════════════════════════════════════════════════════════════════
epochs <- se %>%
  select(
    usubjid = USUBJID,
    epoch   = ELEMENT,
    sestdtc = SESTDTC,
    seendtc = SEENDTC
  ) %>%
  filter(!is.na(sestdtc), sestdtc != "") %>%
  mutate(
    sestdtc = as.Date(sestdtc),
    seendtc = as.Date(seendtc)
  ) %>%
  left_join(day0_ref, by = "usubjid") %>%
  mutate(
    epoch_start = as.numeric(sestdtc - day0),
    epoch_end   = as.numeric(seendtc - day0),
    tooltip     = paste0(
      usubjid, " · ", epoch, "\n",
      "Start : ", format(sestdtc, "%d %b %Y"),
      " (Day ", as.numeric(sestdtc - day0), ")\n",
      "End   : ", format(seendtc, "%d %b %Y"),
      " (Day ", as.numeric(seendtc - day0), ")"
    )
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 3 — SV: visits
# ══════════════════════════════════════════════════════════════════════════════
visits <- sv %>%
  select(
    usubjid  = USUBJID,
    visitnum = VISITNUM,
    visit    = VISIT,
    svstdtc  = SVSTDTC,
    svendtc  = SVENDTC
  ) %>%
  filter(!is.na(svstdtc), svstdtc != "") %>%
  mutate(
    svstdtc     = as.Date(svstdtc),
    svendtc     = as.Date(ifelse(is.na(svendtc) | svendtc == "",
                                 as.character(svstdtc),
                                 svendtc)),
    visit_label = as.character(visitnum) 
  ) %>%
  left_join(day0_ref, by = "usubjid") %>%
  mutate(
    v_start = as.numeric(svstdtc - day0),
    v_end   = as.numeric(svendtc - day0)
  ) %>%
  group_by(usubjid) %>%
  arrange(visitnum, .by_group = TRUE) %>%
  mutate(
    prev_end     = lag(v_end, default = first(v_start) - 1),
    is_overlap   = v_start <= prev_end,
    next_overlap = lead(is_overlap, default = FALSE),
    flag_overlap = is_overlap | next_overlap
  ) %>%
  ungroup() %>%
  mutate(
    tooltip = paste0(
      usubjid, " · Visit ", visit_label, " (", visit, ")\n",
      "Start : ", format(svstdtc, "%d %b %Y"),
      " (Day ", v_start, ")\n",
      "End   : ", format(svendtc, "%d %b %Y"),
      " (Day ", v_end, ")",
      ifelse(flag_overlap, "\n⚠ Overlap with adjacent visit", "")
    )
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 4 — DS: disposition — ALL rows, no filtering
# ══════════════════════════════════════════════════════════════════════════════
disposition <- ds %>%
  select(
    usubjid = USUBJID,
    dsdecod = DSDECOD,
    dsterm  = DSTERM,
    dsstdtc = DSSTDTC,
    dsstdy  = DSSTDY,
    dscat   = DSCAT,
    dsseq   = DSSEQ
  ) %>%
  filter(dscat=='DISPOSITION EVENT') %>%
  mutate(dsstdtc = as.Date(dsstdtc)) %>%
  left_join(day0_ref, by = "usubjid") %>%
  mutate(
    end_day = case_when(
      !is.na(dsstdy)  ~ as.numeric(dsstdy),
      !is.na(dsstdtc) & !is.na(day0) ~ as.numeric(dsstdtc - day0),
      TRUE ~ NA_real_
    ),
    completed  = dsdecod %in% ("COMPLETED"),
    disp_label = paste0(dsdecod, " (", format(dsstdtc, "%d %b %Y"), ")"),
    tooltip    = paste0(
      usubjid, "\n",
      "Disposition : ", dsdecod, "\n",
      "Date        : ", format(dsstdtc, "%d %b %Y"), "\n",
      "Day         : ", end_day
    )
  ) %>%
  filter(!is.na(end_day))   # only drop rows where day cannot be calculated

# ── Bar ends: last disposition event per subject ──────────────────────────────
bar_ends <- disposition %>%
  group_by(usubjid) %>%
  summarise(
    end_day   = max(end_day, na.rm = TRUE),
    completed = any(completed),
    .groups   = "drop"
  ) %>%
  left_join(
    disposition %>%
      group_by(usubjid) %>%
      slice(which.max(end_day)) %>%
      ungroup() %>%
      select(usubjid, disp_label, tooltip),
    by = "usubjid"
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 5 — DS: milestones
# ══════════════════════════════════════════════════════════════════════════════
milestones <- ds %>%
  select(
    usubjid = USUBJID,
    dsdecod = DSDECOD,
    dsstdtc = DSSTDTC,
    dsstdy  = DSSTDY,
    dscat   = DSCAT
  ) %>%
  filter(dsdecod %in% c(
    "INFORMED CONSENT OBTAINED",
    "RANDOMIZED",
    "COMPLETED",
    "PROTOCOL COMPLETED",
    "SCREEN FAILURE",
    "ADVERSE EVENT",
    "STUDY TERMINATED BY SPONSOR",
    "WITHDRAWAL BY SUBJECT",
    "LOST TO FOLLOW-UP"
  )) %>%
  mutate(dsstdtc = as.Date(dsstdtc)) %>%
  left_join(day0_ref, by = "usubjid") %>%
  mutate(
    m_day = case_when(
      !is.na(dsstdy)  ~ as.numeric(dsstdy),
      !is.na(dsstdtc) & !is.na(day0) ~ as.numeric(dsstdtc - day0),
      TRUE ~ NA_real_
    ),
    milestone = case_when(
      dsdecod == "INFORMED CONSENT OBTAINED"   ~ "ICF",
      dsdecod == "RANDOMIZED"                  ~ "Randomized",
      dsdecod %in% c("COMPLETED",
                     "PROTOCOL COMPLETED")     ~ "End",
      dsdecod == "SCREEN FAILURE"              ~ "Screen Failure",
      dsdecod == "ADVERSE EVENT"               ~ "Adverse Event",
      dsdecod == "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
      dsdecod == "WITHDRAWAL BY SUBJECT"       ~ "Withdrew Consent",
      dsdecod == "LOST TO FOLLOW-UP"           ~ "Lost to Follow-up",
      TRUE                                     ~ dsdecod
    ),
    tooltip = paste0(
      usubjid, " · ", milestone, "\n",
      "Date : ", format(dsstdtc, "%d %b %Y"), "\n",
      "Day  : ", m_day
    )
  ) %>%
  filter(!is.na(m_day))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 6 — Subject ordering (longest bar on top)
# ══════════════════════════════════════════════════════════════════════════════
subj_order <- bar_ends %>%
  arrange(desc(end_day)) %>%
  pull(usubjid) %>%
  as.character()

epochs      <- epochs      %>% mutate(usubjid = factor(usubjid, levels = subj_order))
visits      <- visits      %>% mutate(usubjid = factor(usubjid, levels = subj_order))
milestones  <- milestones  %>% mutate(usubjid = factor(usubjid, levels = subj_order))
disposition <- disposition %>% mutate(usubjid = factor(usubjid, levels = subj_order))
bar_ends    <- bar_ends    %>% mutate(usubjid = factor(usubjid, levels = subj_order))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 7 — Colour / shape scales
# ══════════════════════════════════════════════════════════════════════════════
epoch_levels <- unique(as.character(epochs$epoch))

epoch_palette <- c(
  "#85B7EB", "#FCBBC7", "#C3FA8F", "#FAD68F",
  "#D4A8F5", "#F5D4A8", "#A8F5D4", "#F5A8D4",
  "#A8D4F5", "#D4F5A8"
)

epoch_colors <- setNames(
  epoch_palette[seq_along(epoch_levels)],
  epoch_levels
)

milestone_shapes <- c(
  "ICF"              = 21,
  "Randomized"       = 24,
  "End"              = 23,
  "Screen Failure"   = 4,
  "Adverse Event"    = 25,
  "Sponsor Decision" = 22,
  "Withdrew Consent" = 7,
  "Lost to Follow-up"= 8
)

milestone_fills <- c(
  "ICF"               = "#B30B7E",
  "Randomized"        = "yellow",
  "End"               = "#534AB7",
  "Screen Failure"    = "#E24B4A",
  "Adverse Event"     = "#EF9F27",
  "Sponsor Decision"  = "#888780",
  "Withdrew Consent"  = "#378ADD",
  "Lost to Follow-up" = "#1D9E75"
)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 8 — Direct colour vectors
# ══════════════════════════════════════════════════════════════════════════════
visit_colours <- ifelse(visits$flag_overlap,   "#E24B4A", "#7F77DD")
disp_colours  <- ifelse(bar_ends$completed,    "#0F6E56", "#A32D2D")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 9 — x-axis range
# ══════════════════════════════════════════════════════════════════════════════
x_min <- floor(min(c(epochs$epoch_start, visits$v_start), na.rm = TRUE) / 7) * 7
x_max <- max(bar_ends$end_day, na.rm = TRUE) + 80

# ══════════════════════════════════════════════════════════════════════════════
# STEP 10 — Build plot
# ══════════════════════════════════════════════════════════════════════════════
p <- ggplot() +
  
  # Day 0 reference line
  geom_vline(
    xintercept = 0, linetype = "dashed",
    colour = "grey50", linewidth = 0.5
  ) +
  
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
    colour = "#7F77DD", linewidth = 3.5, lineend = "round"
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
  
  # Visit number labels above bars
  geom_text(
    data = visits,
    aes(x     = (v_start + v_end) / 2,
        y     = usubjid,
        label = visit_label),
    colour      = visit_colours,
    vjust       = -1.6,
    size        = 3,
    fontface    = "bold",
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
  
  # All disposition event markers on the bar
  geom_point_interactive(
    data = disposition,
    aes(x       = end_day,
        y       = usubjid,
        tooltip = tooltip,
        data_id = paste(usubjid, dsseq)),
    shape  = 18,
    size   = 3,
    colour = "#534AB7"
  ) +
  
  # End-of-bar tick (last event per subject)
  geom_point_interactive(
    data  = bar_ends,
    aes(x       = end_day,
        y       = usubjid,
        tooltip = tooltip,
        data_id = paste(usubjid, "end")),
    shape = 124, size = 5, colour = "grey30"
  ) +
  
  # Disposition label after bar (last event per subject)
  geom_text_interactive(
    data = bar_ends,
    aes(x       = end_day + 4,
        y       = usubjid,
        label   = disp_label,
        tooltip = tooltip,
        data_id = paste(usubjid, "end_label")),
    colour      = disp_colours,
    hjust       = 0,
    size        = 3,
    show.legend = FALSE
  ) +
  
  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_colour_manual(
    values = epoch_colors,
    breaks = names(epoch_colors),
    name   = "Study Element (SE)"
  ) +
  
  scale_fill_manual(
    values = milestone_fills,
    name   = "Disposition milestone (DS)"
  ) +
  
  scale_shape_manual(
    values = milestone_shapes,
    name   = "Disposition milestone (DS)"
  ) +
  
  scale_x_continuous(
    name   = "Days from Baseline (VISITNUM = 3, Day 0)",
    breaks = seq(x_min, ceiling(x_max / 28) * 28, by = 28),
    expand = expansion(mult = c(0.01, 0)),
    position='top'
  ) +
  
  scale_y_discrete(limits = rev, expand = expansion(add = c(0.5, 1.2))) +
  
  coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
  
  labs(
    title    = "Swimmer Plot — Disposition (DS), Study Visits (SV) and Study Elements (SE)",
    subtitle = paste0(
      "Dashed line = Day 0 (Baseline)   ",
      "Bars = SE epoch   Thin bars = SV visit window   ",
      "Red bars = visit overlap\n",
      "\u25C6 All DS events   ",
      "Green label = Completed   Red label = Discontinued"
    ),
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    axis.text.x        = element_text(size = 12),
    axis.text.y        = element_text(size = 11),
    legend.position    = "top",
    legend.box         = "vertical",
    legend.margin      = margin(b = 10),
    legend.title       = element_text(face = "bold", size = 12),
    legend.text        = element_text(size = 11),
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(colour = "grey40", size = 11,
                                      margin = margin(b = 15)),
    plot.margin        = margin(t = 6, r = 5, b = 10, l = 10)
  ) +
  
  guides(
    colour = guide_legend(override.aes = list(linewidth = 4), order = 1),
    fill   = guide_legend(order = 2),
    shape  = guide_legend(order = 2)
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 11 — Render
# ══════════════════════════════════════════════════════════════════════════════
girafe(
  ggobj      = p,
  width_svg  = 18,
  height_svg = max(8, length(subj_order) * 0.5),
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