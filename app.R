library(ggplot2)
library(dplyr)
library(ggiraph)
library(lubridate)
library(haven)
library(shiny)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 0 — Load data (once, outside Shiny)
# ══════════════════════════════════════════════════════════════════════════════
setwd("C:/Users/user/Desktop/shiny_apps/sdtm app")

dm <- read_xpt("dm.xpt")
ds <- read_xpt("ds.xpt")
se <- read_xpt("se.xpt")
sv <- read_xpt("sv.xpt")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 1 — Baseline anchor
# ══════════════════════════════════════════════════════════════════════════════
baseline_ref <- sv %>%
  filter(VISITNUM == 3) %>%
  select(usubjid = USUBJID, baseline_date = SVSTDTC) %>%
  mutate(baseline_date = as.Date(baseline_date))

dm_ref <- dm %>%
  select(usubjid = USUBJID, rfxstdtc = RFXSTDTC,
         siteid = SITEID, actarm = ACTARM) %>%
  filter(!is.na(rfxstdtc), rfxstdtc != "") %>%
  mutate(rfxstdtc = as.Date(rfxstdtc))

day0_ref <- dm_ref %>%
  inner_join(baseline_ref, by = "usubjid") %>%
  mutate(day0 = coalesce(baseline_date, rfxstdtc))

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2 — SE: epochs (pre-processed once)
# ══════════════════════════════════════════════════════════════════════════════
epochs_all <- se %>%
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
  inner_join(day0_ref, by = "usubjid") %>%
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
# STEP 3 — SV: visits (pre-processed once)
# ══════════════════════════════════════════════════════════════════════════════
visits_all <- sv %>%
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
  inner_join(day0_ref, by = "usubjid") %>%
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
# STEP 4 — DS: disposition (pre-processed once)
# ══════════════════════════════════════════════════════════════════════════════
disposition_all <- ds %>%
  select(
    usubjid = USUBJID,
    dsdecod = DSDECOD,
    dsterm  = DSTERM,
    dsstdtc = DSSTDTC,
    dsstdy  = DSSTDY,
    dscat   = DSCAT,
    dsseq   = DSSEQ
  ) %>%
  filter(dscat == "DISPOSITION EVENT") %>%
  mutate(dsstdtc = as.Date(dsstdtc)) %>%
  inner_join(day0_ref, by = "usubjid") %>%
  mutate(
    end_day = case_when(
      !is.na(dsstdy)  ~ as.numeric(dsstdy),
      !is.na(dsstdtc) & !is.na(day0) ~ as.numeric(dsstdtc - day0),
      TRUE ~ NA_real_
    ),
    completed  = dsdecod %in% c("COMPLETED", "PROTOCOL COMPLETED"),
    disp_label = paste0(dsdecod, " (", format(dsstdtc, "%d %b %Y"), ")"),
    tooltip    = paste0(
      usubjid, "\n",
      "Disposition : ", dsdecod, "\n",
      "Date        : ", format(dsstdtc, "%d %b %Y"), "\n",
      "Day         : ", end_day
    )
  ) %>%
  filter(!is.na(end_day))


# ══════════════════════════════════════════════════════════════════════════════
# STEP 5 — DS: milestones (pre-processed once)
# ══════════════════════════════════════════════════════════════════════════════
milestones_all <- ds %>%
  select(
    usubjid = USUBJID,
    dsdecod = DSDECOD,
    dsstdtc = DSSTDTC,
    dsstdy  = DSSTDY,
    dscat   = DSCAT
  ) %>%
  filter(
    dscat == "DISPOSITION EVENT",
    dsdecod %in% c(
      "COMPLETED",
      "ADVERSE EVENT",
      "STUDY TERMINATED BY SPONSOR",
      "SCREEN FAILURE",
      "DEATH",
      "WITHDRAWAL BY SUBJECT",
      "PHYSICIAN DECISION",
      "PROTOCOL VIOLATION",
      "LOST TO FOLLOW-UP",
      "LACK OF EFFICACY"
    )
  ) %>%
  mutate(dsstdtc = as.Date(dsstdtc)) %>%
  inner_join(day0_ref, by = "usubjid") %>%
  mutate(
    m_day = case_when(
      !is.na(dsstdy)  ~ as.numeric(dsstdy),
      !is.na(dsstdtc) & !is.na(day0) ~ as.numeric(dsstdtc - day0),
      TRUE ~ NA_real_
    ),
    milestone = case_when(
      dsdecod == "COMPLETED"                   ~ "Completed",
      dsdecod == "ADVERSE EVENT"               ~ "Adverse Event",
      dsdecod == "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
      dsdecod == "SCREEN FAILURE"              ~ "Screen Failure",
      dsdecod == "DEATH"                       ~ "Death",
      dsdecod == "WITHDRAWAL BY SUBJECT"       ~ "Withdrew Consent",
      dsdecod == "PHYSICIAN DECISION"          ~ "Physician Decision",
      dsdecod == "PROTOCOL VIOLATION"          ~ "Protocol Violation",
      dsdecod == "LOST TO FOLLOW-UP"           ~ "Lost to Follow-up",
      dsdecod == "LACK OF EFFICACY"            ~ "Lack of Efficacy",
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
# STEP 6 — Colour / shape scales (fixed, defined once)
# ══════════════════════════════════════════════════════════════════════════════
epoch_levels <- unique(as.character(epochs_all$epoch))

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
  "Completed"          = 23,
  "Adverse Event"      = 25,
  "Sponsor Decision"   = 22,
  "Screen Failure"     = 4,
  "Death"              = 8,
  "Withdrew Consent"   = 21,
  "Physician Decision" = 24,
  "Protocol Violation" = 7,
  "Lost to Follow-up"  = 1,
  "Lack of Efficacy"   = 6
)

milestone_fills <- c(
  "Completed"          = "#534AB7",
  "Adverse Event"      = "#EF9F27",
  "Sponsor Decision"   = "#888780",
  "Screen Failure"     = "#E24B4A",
  "Death"              = "#2C2C2A",
  "Withdrew Consent"   = "#378ADD",
  "Physician Decision" = "#85B7EB",
  "Protocol Violation" = "#FCBBC7",
  "Lost to Follow-up"  = "#1D9E75",
  "Lack of Efficacy"   = "#B30B7E"
)
# ══════════════════════════════════════════════════════════════════════════════
# STEP 7 — Plot-building function
# ══════════════════════════════════════════════════════════════════════════════
build_plot <- function(ep, vi, mi, di) {
  
  req(nrow(di) > 0)
  
  # Bar ends
  bar_ends <- di %>%
    group_by(usubjid) %>%
    summarise(
      end_day   = max(end_day, na.rm = TRUE),
      completed = any(completed),
      .groups   = "drop"
    ) %>%
    left_join(
      di %>%
        group_by(usubjid) %>%
        slice(which.max(end_day)) %>%
        ungroup() %>%
        select(usubjid, disp_label, tooltip),
      by = "usubjid"
    )
  
  # Subject ordering
  subj_order <- bar_ends %>%
    arrange(desc(end_day)) %>%
    pull(usubjid) %>%
    as.character()
  
  ep       <- ep       %>% filter(is.na(usubjid)==F) %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  vi       <- vi       %>% filter(is.na(usubjid)==F) %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  mi       <- mi       %>% filter(is.na(usubjid)==F) %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  di       <- di       %>% filter(is.na(usubjid)==F) %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  bar_ends <- bar_ends %>% filter(is.na(usubjid)==F) %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  
  visit_colours <- ifelse(vi$flag_overlap,       "#E24B4A", "#7F77DD")
  disp_colours  <- ifelse(bar_ends$completed,    "#0F6E56", "#A32D2D")
  
  x_min <- floor(min(c(ep$epoch_start, vi$v_start), na.rm = TRUE) / 7) * 7
  x_max <- max(bar_ends$end_day, na.rm = TRUE) + 80
  
  ggplot() +
    
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    
    geom_segment_interactive(
      data = ep,
      aes(x = epoch_start, xend = epoch_end,
          y = usubjid, yend = usubjid,
          colour  = epoch,
          tooltip = tooltip,
          data_id = paste(usubjid, epoch)),
      linewidth = 5, lineend = "round"
    ) +
    
    geom_segment_interactive(
      data = vi %>% filter(!flag_overlap),
      aes(x = v_start, xend = v_end,
          y = usubjid, yend = usubjid,
          tooltip = tooltip,
          data_id = paste(usubjid, visit_label)),
      colour = "#7F77DD", linewidth = 3.5, lineend = "round"
    ) +
    
    geom_segment_interactive(
      data = vi %>% filter(flag_overlap),
      aes(x = v_start, xend = v_end,
          y = usubjid, yend = usubjid,
          tooltip = tooltip,
          data_id = paste(usubjid, visit_label)),
      colour = "#E24B4A", linewidth = 5, lineend = "round"
    ) +
    
    geom_text(
      data = vi,
      aes(x = (v_start + v_end) / 2,
          y = usubjid,
          label = visit_label),
      colour      = visit_colours,
      vjust       = -1.6,
      size        = 3,
      fontface    = "bold",
      show.legend = FALSE
    ) +
    
    geom_point_interactive(
      data = mi,
      aes(x       = m_day,
          y       = usubjid,
          shape   = milestone,
          fill    = milestone,
          tooltip = tooltip,
          data_id = paste(usubjid, milestone)),
      size = 3.2, colour = "white", stroke = 0.7
    ) +
    
    geom_point_interactive(
      data = di,
      aes(x       = end_day,
          y       = usubjid,
          tooltip = tooltip,
          data_id = paste(usubjid, dsseq)),
      shape = 18, size = 3, colour = "#534AB7"
    ) +
    
    geom_point_interactive(
      data  = bar_ends,
      aes(x       = end_day,
          y       = usubjid,
          tooltip = tooltip,
          data_id = paste(usubjid, "end")),
      shape = 124, size = 5, colour = "grey30"
    ) +
    
    geom_text_interactive(
      data = bar_ends,
      aes(x       = end_day + 4,
          y       = usubjid,
          label   = disp_label,
          tooltip = tooltip,
          data_id = paste(usubjid, "end_label")),
      colour = disp_colours, hjust = 0, size = 3, show.legend = FALSE
    ) +
    
    scale_colour_manual(
      values = epoch_colors,
      breaks = names(epoch_colors),
      name   = "EPOCH (SE)"
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
      name     = "Days from Baseline (VISITNUM = 3, Day 0)",
      breaks   = seq(x_min, ceiling(x_max / 28) * 28, by = 28),
      expand   = expansion(mult = c(0.01, 0)),
      position = "top"
    ) +
    
    scale_y_discrete(
      limits = rev,
      expand = expansion(add = c(0.5, 1.2))
    ) +
    
    coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
    
    labs(
      title    = "Swimmer Plot — Disposition (DS), Study Visits (SV) and Study Elements (SE)",
      subtitle = paste0(
        "Dashed line = Day 0 (Baseline)\n  ",
        "Red bars = visit overlap\n",
        "Green label = Completed  /  Red label = Discontinued"
      ),
      y = NULL
    ) +

    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_line(colour = "grey92", linewidth = 0.4),
      axis.text.x.top     = element_text(size = 12),
      axis.title.x.top    = element_text(size = 12, margin = margin(b = 8)),
      axis.text.y         = element_text(size = 11),
      legend.position     = "top",
      legend.box          = "vertical",
      legend.margin       = margin(b = 10),
      legend.title        = element_text(face = "bold", size = 12),
      legend.text         = element_text(size = 11),
      plot.title          = element_text(face = "bold", size = 18),
      plot.subtitle       = element_text(colour = "grey40", size = 14,
                                         margin = margin(b = 15)),
      plot.margin         = margin(t = 6, r = 5, b = 10, l = 10)
    ) +
    
    guides(
      colour = guide_legend(override.aes = list(linewidth = 4), order = 1),
      fill   = guide_legend(order = 2),
      shape  = guide_legend(order = 2)
    )
}
# ══════════════════════════════════════════════════════════════════════════════
# STEP 8 — Shiny UI (Modified for responsive scrolling)
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  
  # Add CSS for responsive sizing
  tags$head(
    tags$style(HTML("
      /* Make the graph container fill the width */
      .girafe_container {
        width: 100% !important;
      }
      
      .girafe_container svg {
        width: 100% !important;
        height: auto !important;
      }
      
      /* Allow vertical scrolling */
      #swimmer_container {
        width: 100%;
        height: calc(100vh - 150px);  /* Full viewport height minus title/sidebar */
        overflow-y: auto;              /* Enable vertical scrolling */
        overflow-x: auto;              /* Enable horizontal scrolling if needed */
        border: 1px solid #ddd;
        border-radius: 4px;
      }
      
      /* Style the scrollbar */
      #swimmer_container::-webkit-scrollbar {
        width: 12px;
        height: 12px;
      }
      
      #swimmer_container::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 10px;
      }
      
      #swimmer_container::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 10px;
      }
      
      #swimmer_container::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
    "))
  ),
  
  titlePanel("Study Disposition Viewer (SDTM - CDISC Pilot Project Data)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Site filter
      selectInput(
        inputId  = "site",
        label    = "Site (DM.SITEID):",
        choices  = c("All", sort(unique(dm$SITEID))),
        selected = "All"
      ),
      
      # ARM filter
      selectInput(
        inputId  = "arm",
        label    = "Treatment Arm (DM.ACTARM):",
        choices  = c("All", sort(unique(dm$ACTARM))),
        selected = "All"
      ),
      
      hr(),
      
      # Subject filter — updated dynamically based on site/arm
      checkboxGroupInput(
        inputId  = "subjects",
        label    = "Subjects (USUBJID):",
        choices  = sort(unique(dm$USUBJID)),
        selected = sort(unique(dm$USUBJID))
      ),
      
      hr(),
      
      fluidRow(
        column(6, actionButton("select_all",   "Select all",   width = "100%")),
        column(6, actionButton("deselect_all", "Deselect all", width = "100%"))
      ),
      
      hr(),
      
      # Add scroll info
      div(
        style = "background-color: #f5f5f5; padding: 8px; border-radius: 5px; font-size: 12px;",
        icon("info-circle"),
        "Scroll vertically to see all subjects",
        br(),
        icon("arrows-alt"),
        "Graph fills available width automatically"
      )
    ),
    
    mainPanel(
      width = 9,
      # Wrap the girafe output in a scrollable div
      div(
        id = "swimmer_container",
        girafeOutput("swimmer", width = "100%", height = "auto")
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 9 — Shiny Server (Modified for responsive sizing)
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # ── Available subjects based on site and arm filters ─────────────────────
  available_subjects <- reactive({
    subj <- dm
    
    if (input$site != "All")
      subj <- subj %>% filter(SITEID == input$site)
    
    if (input$arm != "All")
      subj <- subj %>% filter(ACTARM == input$arm)
    
    sort(unique(subj$USUBJID))
  })
  
  # ── Update subject checkboxes when site or arm changes ───────────────────
  observeEvent(available_subjects(), {
    avail <- available_subjects()
    updateCheckboxGroupInput(session, "subjects",
                             choices  = avail,
                             selected = avail
    )
  })
  
  # ── Select / deselect all ─────────────────────────────────────────────────
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "subjects",
                             selected = available_subjects()
    )
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "subjects",
                             selected = character(0)
    )
  })
  
  # ── Filtered datasets ─────────────────────────────────────────────────────
  filtered <- reactive({
    req(length(input$subjects) > 0)
    subj <- input$subjects
    
    list(
      ep = epochs_all      %>% filter(usubjid %in% subj),
      vi = visits_all      %>% filter(usubjid %in% subj),
      mi = milestones_all  %>% filter(usubjid %in% subj),
      di = disposition_all %>% filter(usubjid %in% subj)
    )
  })
  
  # ── Render plot (Modified for responsive sizing) ─────────────────────────
  output$swimmer <- renderGirafe({
    d <- filtered()
    req(nrow(d$di) > 0)
    
    n_subjects <- length(input$subjects)
    
    # Calculate appropriate height based on number of subjects
    # Each subject needs about 0.4 inches of vertical space
    height_inches <- max(6, n_subjects * 0.4)
    
    # Width will be responsive (CSS will handle it)
    # Set a reasonable base width that will scale
    width_inches <- 16
    
    girafe(
      ggobj = build_plot(d$ep, d$vi, d$mi, d$di),
      width_svg = width_inches,
      height_svg = height_inches,
      options = list(
        opts_tooltip(
          css     = "background:white;border:1px solid #ccc;padding:6px 10px;
                     border-radius:6px;font-size:14px;
                     font-family:sans-serif;white-space:pre;",
          opacity = 0.95
        ),
        opts_hover(css = "opacity:0.75;cursor:crosshair;"),
        opts_sizing(rescale = TRUE, width = 1)  # Scale to 100% of container width
      )
    )
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# STEP 10 — Run
# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)