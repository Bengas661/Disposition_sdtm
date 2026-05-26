library(ggplot2)
library(dplyr)

# ── Data ───────────────────────────────────────────────────────────────────────
# All days are relative to each subject's own Day 0 (first study contact)

subjects_df <- data.frame(
  subject_id = paste0("SUBJ-", sprintf("%02d", 1:10)),
  site       = c("Site A","Site A","Site B","Site B","Site C",
                 "Site C","Site A","Site B","Site C","Site A"),
  stringsAsFactors = FALSE
)

epochs <- data.frame(
  subject_id  = c(
    rep("SUBJ-01",4), rep("SUBJ-02",3), rep("SUBJ-03",4),
    rep("SUBJ-04",2), rep("SUBJ-05",4), rep("SUBJ-06",3),
    rep("SUBJ-07",4), rep("SUBJ-08",2), rep("SUBJ-09",3),
    rep("SUBJ-10",4)
  ),
  epoch = c(
    "Screening","Run-in","Treatment","Follow-up",
    "Screening","Run-in","Treatment",
    "Screening","Run-in","Treatment","Follow-up",
    "Screening","Run-in",
    "Screening","Run-in","Treatment","Follow-up",
    "Screening","Run-in","Treatment",
    "Screening","Run-in","Treatment","Follow-up",
    "Screening","Run-in",
    "Screening","Run-in","Treatment",
    "Screening","Run-in","Treatment","Follow-up"
  ),
  # Each subject starts at day 0
  epoch_start = c(
    0,14,28,119,   0,14,28,   0,14,28,121,   0,14,
    0,14,28,119,   0,14,28,   0,14,28,119,   0,14,
    0,14,28,         0,14,28,121
  ),
  epoch_end = c(
    13,27,118,181,  13,27,91,  13,27,120,211,  13,63,
    13,27,118,240,  13,27,134,  13,27,118,270,  13,129,
    13,27,200,       13,27,120,309
  ),
  stringsAsFactors = FALSE
)

visits <- data.frame(
  subject_id = c(
    rep("SUBJ-01",5), rep("SUBJ-02",3), rep("SUBJ-03",5),
    rep("SUBJ-04",2), rep("SUBJ-05",5), rep("SUBJ-06",3),
    rep("SUBJ-07",5), rep("SUBJ-08",2), rep("SUBJ-09",3),
    rep("SUBJ-10",5)
  ),
  visit_label = c(
    "V1","V2","V3","V4","V5",
    "V1","V2","V3",
    "V1","V2","V3","V4","V5",
    "V1","V2",
    "V1","V2","V3","V4","V5",
    "V1","V2","V3",
    "V1","V2","V3","V4","V5",
    "V1","V2",
    "V1","V2","V3",
    "V1","V2","V3","V4","V5"
  ),
  visit_day = c(
    0,28,56,112,181,
    0,28,56,
    0,28,56,112,211,
    0,14,
    0,28,56,112,240,
    0,28,56,
    0,28,56,112,270,
    0,14,
    0,28,56,
    0,28,56,112,309
  ),
  stringsAsFactors = FALSE
)

# ── Disposition milestones (along the bar) ─────────────────────────────────────
milestones <- data.frame(
  subject_id    = c(
    rep("SUBJ-01",4), rep("SUBJ-02",3), rep("SUBJ-03",4),
    rep("SUBJ-04",2), rep("SUBJ-05",4), rep("SUBJ-06",3),
    rep("SUBJ-07",4), rep("SUBJ-08",2), rep("SUBJ-09",3),
    rep("SUBJ-10",4)
  ),
  milestone     = c(
    "ICF1","ICF2","Randomized","Completed",
    "ICF1","ICF2","Randomized",
    "ICF1","ICF2","Randomized","Completed",
    "ICF1","ICF2",
    "ICF1","ICF2","Randomized","Completed",
    "ICF1","ICF2","Randomized",
    "ICF1","ICF2","Randomized","Completed",
    "ICF1","ICF2",
    "ICF1","ICF2","Randomized",
    "ICF1","ICF2","Randomized","Completed"
  ),
  milestone_day = c(
    0,7,28,181,
    0,7,28,
    0,7,28,211,
    0,7,
    0,7,28,240,
    0,7,28,
    0,7,28,270,
    0,7,
    0,7,28,
    0,7,28,309
  ),
  stringsAsFactors = FALSE
)

# ── End-of-study disposition (label printed after bar end) ────────────────────
disposition <- data.frame(
  subject_id   = paste0("SUBJ-", sprintf("%02d", 1:10)),
  disp_label   = c(
    "Completed",
    "Discontinued — Withdrew Consent",
    "Completed",
    "Discontinued — Screen Failure",
    "Completed",
    "Discontinued — Lost to Follow-up",
    "Completed",
    "Discontinued — Screen Failure",
    "Discontinued — Adverse Event",
    "Completed"
  ),
  disp_day     = c(181, 91, 211, 63, 240, 134, 270, 129, 200, 309),
  completed    = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE),
  stringsAsFactors = FALSE
)

# ── Subject order (longest bar on top) ────────────────────────────────────────
subj_order <- disposition %>% arrange(disp_day) %>% pull(subject_id)

epochs      <- epochs      %>% mutate(subject_id = factor(subject_id, levels = subj_order))
visits      <- visits      %>% mutate(subject_id = factor(subject_id, levels = subj_order))
milestones  <- milestones  %>% mutate(subject_id = factor(subject_id, levels = subj_order))
disposition <- disposition %>% mutate(subject_id = factor(subject_id, levels = subj_order))

# ── Colour / shape scales ─────────────────────────────────────────────────────
epoch_colors <- c(
  "Screening"  = "#85B7EB",
  "Run-in"     = "#EF9F27",
  "Treatment"  = "#1D9E75",
  "Follow-up"  = "#7F77DD"
)

milestone_shapes <- c(
  "ICF1"       = 21,   # filled circle
  "ICF2"       = 22,   # filled square
  "Randomized" = 23,   # filled diamond
  "Completed"  = 24    # filled triangle up
)

milestone_fills <- c(
  "ICF1"       = "#378ADD",
  "ICF2"       = "#85B7EB",
  "Randomized" = "#1D9E75",
  "Completed"  = "#534AB7"
)

# ── Plot ──────────────────────────────────────────────────────────────────────
max_day <- max(disposition$disp_day) + 80   # room for end labels

p <- ggplot() +
  
  # Epoch bars
  geom_segment(
    data = epochs,
    aes(x = epoch_start, xend = epoch_end,
        y = subject_id,  yend = subject_id,
        colour = epoch),
    linewidth = 5,
    lineend   = "round"
  ) +
  
  # Visit tick marks
  geom_point(
    data   = visits,
    aes(x  = visit_day, y = subject_id),
    shape  = 124,
    size   = 3.5,
    colour = "grey25"
  ) +
  
  # Visit labels above bar
  geom_text(
    data   = visits,
    aes(x  = visit_day, y = subject_id, label = visit_label),
    vjust  = -1.5,
    size   = 2.6,
    colour = "grey20",
    fontface = "bold"
  ) +
  
  # Milestone markers ON the bar
  geom_point(
    data   = milestones,
    aes(x  = milestone_day, y = subject_id,
        shape = milestone, fill = milestone),
    size   = 3.2,
    colour = "white",
    stroke = 0.8
  ) +
  
  # End-of-bar disposition label
  geom_text(
    data      = disposition,
    aes(x     = disp_day + 5,
        y     = subject_id,
        label = disp_label,
        colour = completed),   # recycle colour aesthetic
    hjust     = 0,
    size      = 2.9,
    fontface  = "plain",
    show.legend = FALSE
  ) +
  
  # Thin vertical line at end of each bar
  geom_point(
    data   = disposition,
    aes(x  = disp_day, y = subject_id),
    shape  = 124,
    size   = 5,
    colour = "grey30"
  ) +
  
  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_colour_manual(
    values = c(
      "Screening" = "#85B7EB","Run-in" = "#EF9F27",
      "Treatment" = "#1D9E75","Follow-up" = "#7F77DD",
      "TRUE"  = "#1D9E75",   # Completed label colour
      "FALSE" = "#E24B4A"    # Discontinued label colour
    ),
    breaks = c("Screening","Run-in","Treatment","Follow-up"),
    name   = "Epoch"
  ) +
  
  scale_fill_manual(
    values = milestone_fills,
    name   = "Disposition milestone"
  ) +
  
  scale_shape_manual(
    values = milestone_shapes,
    name   = "Disposition milestone"
  ) +
  
  scale_x_continuous(
    name   = "Days from subject Day 0",
    breaks = seq(0, 320, by = 28),
    expand = expansion(mult = c(0.01, 0.0))
  ) +
  
  coord_cartesian(xlim = c(0, max_day)) +
  
  labs(
    title    = "Swimmer plot — subject recruitment timeline",
    subtitle = paste0(
      "Bars = study epoch   |  = visit   ",
      "\u25CF = ICF1   \u25A0 = ICF2   \u25C6 = Randomized   \u25B2 = Completed"
    ),
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 10),
    legend.position    = "bottom",
    legend.box         = "vertical",
    legend.title       = element_text(face = "bold", size = 10),
    legend.text        = element_text(size = 9),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey45", size = 9),
    plot.margin        = margin(t = 15, r = 5, b = 10, l = 10)
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(linewidth = 4),
      order = 1
    ),
    fill  = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  )

print(p)