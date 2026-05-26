library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)


# ── Real Data ──────────────────────────────────────────────────────────────────────

library(haven)

setwd("C:/Users/user/Desktop/shiny_apps/sdtm app")
dm <- read_xpt("dm.xpt")
ds <- read_xpt("ds.xpt")
se <- read_xpt("se.xpt")
sv <- read_xpt("sv.xpt")


# ── Simulated Data ──────────────────────────────────────────────────────────────────────
study_start <- as.Date("2023-01-01")

enrol <- c(
  "SUBJ-01" = "2023-01-05", "SUBJ-02" = "2023-01-10",
  "SUBJ-03" = "2023-02-01", "SUBJ-04" = "2023-02-10",
  "SUBJ-05" = "2023-03-01", "SUBJ-06" = "2023-03-05",
  "SUBJ-07" = "2023-04-01", "SUBJ-08" = "2023-04-10",
  "SUBJ-09" = "2023-05-01", "SUBJ-10" = "2023-05-10"
)

enrol_df <- data.frame(
  usubjid = names(enrol),
  enrol_date = as.Date(enrol),
  stringsAsFactors = FALSE
)

subjects_df <- data.frame(
  usubjid = names(enrol),
  site       = c("Site A","Site A","Site B","Site B","Site C",
                 "Site C","Site A","Site B","Site C","Site A"),
  stringsAsFactors = FALSE
)

epochs <- tribble(
  ~usubjid,  ~epoch,        ~epoch_start, ~epoch_end,
  "SUBJ-01","Screening", 0, 13,"SUBJ-01","Run-in",     14, 27,
  "SUBJ-01","Treatment",28,119,"SUBJ-01","Follow-up",120,181,
  "SUBJ-02","Screening", 0, 13,"SUBJ-02","Run-in",     14, 27,
  "SUBJ-02","Treatment",28, 95,
  "SUBJ-03","Screening", 0, 15,"SUBJ-03","Run-in",     16, 30,
  "SUBJ-03","Treatment",31,122,"SUBJ-03","Follow-up",123,211,
  "SUBJ-04","Screening", 0, 13,"SUBJ-04","Run-in",     14, 44,
  "SUBJ-05","Screening", 0, 13,"SUBJ-05","Run-in",     14, 28,
  "SUBJ-05","Treatment",29,120,"SUBJ-05","Follow-up",121,238,
  "SUBJ-06","Screening", 0, 14,"SUBJ-06","Run-in",     15, 29,
  "SUBJ-06","Treatment",30,136,
  "SUBJ-07","Screening", 0, 14,"SUBJ-07","Run-in",     15, 29,
  "SUBJ-07","Treatment",30,121,"SUBJ-07","Follow-up",122,271,
  "SUBJ-08","Screening", 0, 13,"SUBJ-08","Run-in",     14, 55,
  "SUBJ-09","Screening", 0, 13,"SUBJ-09","Run-in",     14, 28,
  "SUBJ-09","Treatment",29,195,
  "SUBJ-10","Screening", 0, 13,"SUBJ-10","Run-in",     14, 28,
  "SUBJ-10","Treatment",29,122,"SUBJ-10","Follow-up",123,309
)

visits <- tribble(
  ~usubjid, ~visit, ~v_start, ~v_end,
  "SUBJ-01","V1",  0,  1,"SUBJ-01","V2", 14, 16,"SUBJ-01","V3", 28, 30,
  "SUBJ-01","V4", 56, 59,"SUBJ-01","V5", 84, 88,"SUBJ-01","V6",112,114,
  "SUBJ-01","V7",155,158,
  "SUBJ-02","V1",  0,  2,"SUBJ-02","V2", 14, 15,"SUBJ-02","V3", 28, 30,
  "SUBJ-02","V4", 56, 62,
  "SUBJ-03","V1",  0,  2,"SUBJ-03","V2", 16, 18,
  "SUBJ-03","V3", 31, 36,"SUBJ-03","V4", 34, 38,  # overlap
  "SUBJ-03","V5", 87, 90,"SUBJ-03","V6",118,121,"SUBJ-03","V7",158,162,
  "SUBJ-04","V1",  0,  1,"SUBJ-04","V2", 14, 15,
  "SUBJ-05","V1",  0,  3,"SUBJ-05","V2", 14, 15,"SUBJ-05","V3", 29, 32,
  "SUBJ-05","V4", 57, 63,"SUBJ-05","V5", 85, 88,"SUBJ-05","V6",113,119,
  "SUBJ-05","V7",165,170,
  "SUBJ-06","V1",  0,  3,"SUBJ-06","V2", 15, 16,"SUBJ-06","V3", 30, 33,
  "SUBJ-06","V4", 58, 61,"SUBJ-06","V5", 86, 92,
  "SUBJ-07","V1",  0,  2,"SUBJ-07","V2", 15, 17,"SUBJ-07","V3", 30, 33,
  "SUBJ-07","V4", 58, 61,
  "SUBJ-07","V5", 86, 93,"SUBJ-07","V6", 91, 95,  # overlap
  "SUBJ-07","V7",168,175,
  "SUBJ-08","V1",  0,  1,"SUBJ-08","V2", 14, 16,
  "SUBJ-09","V1",  0,  2,"SUBJ-09","V2", 14, 17,"SUBJ-09","V3", 29, 31,
  "SUBJ-09","V4", 57, 60,"SUBJ-09","V5", 85, 89,"SUBJ-09","V6",113,118,
  "SUBJ-10","V1",  0,  1,"SUBJ-10","V2", 14, 16,"SUBJ-10","V3", 29, 31,
  "SUBJ-10","V4", 57, 62,"SUBJ-10","V5", 85, 90,"SUBJ-10","V6",113,117,
  "SUBJ-10","V7",166,172
)

milestones <- tribble(
  ~usubjid, ~milestone, ~m_day,
  "SUBJ-01","ICF1", 0,"SUBJ-01","ICF2", 7,"SUBJ-01","Randomized",28,"SUBJ-01","End",181,
  "SUBJ-02","ICF1", 0,"SUBJ-02","ICF2", 7,"SUBJ-02","Randomized",28,
  "SUBJ-03","ICF1", 0,"SUBJ-03","ICF2", 6,"SUBJ-03","Randomized",31,"SUBJ-03","End",211,
  "SUBJ-04","ICF1", 0,"SUBJ-04","ICF2", 6,
  "SUBJ-05","ICF1", 0,"SUBJ-05","ICF2",10,"SUBJ-05","Randomized",29,"SUBJ-05","End",238,
  "SUBJ-06","ICF1", 0,"SUBJ-06","ICF2", 8,"SUBJ-06","Randomized",30,
  "SUBJ-07","ICF1", 0,"SUBJ-07","ICF2", 8,"SUBJ-07","Randomized",30,"SUBJ-07","End",271,
  "SUBJ-08","ICF1", 0,"SUBJ-08","ICF2", 8,
  "SUBJ-09","ICF1", 0,"SUBJ-09","ICF2", 9,"SUBJ-09","Randomized",29,
  "SUBJ-10","ICF1", 0,"SUBJ-10","ICF2", 7,"SUBJ-10","Randomized",29,"SUBJ-10","End",309
)

disposition <- tribble(
  ~usubjid, ~end_day, ~disp_label,                        ~completed,
  "SUBJ-01",181,"Completed",                                  TRUE,
  "SUBJ-02", 95,"Discontinued — Withdrew Consent",            FALSE,
  "SUBJ-03",211,"Completed",                                  TRUE,
  "SUBJ-04", 44,"Discontinued — Screen Failure",              FALSE,
  "SUBJ-05",238,"Completed",                                  TRUE,
  "SUBJ-06",136,"Discontinued — Lost to Follow-up",           FALSE,
  "SUBJ-07",271,"Completed",                                  TRUE,
  "SUBJ-08", 55,"Discontinued — Screen Failure",              FALSE,
  "SUBJ-09",195,"Discontinued — Adverse Event",               FALSE,
  "SUBJ-10",309,"Completed",                                  TRUE
)

# ── Pre-process (once, outside Shiny) ────────────────────────────────────────
disposition <- disposition %>%
  left_join(enrol_df, by = "usubjid") %>%
  mutate(
    end_date = format(enrol_date + end_day, "%Y-%m-%d"),
    tooltip  = paste0(usubjid,"\n", disp_label,"\nDay ",end_day,": ",end_date)
  )

visits <- visits %>%
  group_by(usubjid) %>%
  arrange(v_start, .by_group = TRUE) %>%
  mutate(prev_end = lag(v_end, default = -1), is_overlap = v_start <= prev_end) %>%
  mutate(next_overlap = lead(is_overlap, default = FALSE),
         flag_overlap = is_overlap | next_overlap) %>%
  ungroup() %>%
  left_join(enrol_df, by = "usubjid") %>%
  mutate(
    start_date = format(enrol_date + v_start, "%Y-%m-%d"),
    end_date   = format(enrol_date + v_end,   "%Y-%m-%d"),
    tooltip    = paste0(
      usubjid," · ",visit,"\n",
      "Start : ",start_date,"\n",
      "End   : ",end_date,
      ifelse(flag_overlap,"\n⚠ Overlap with adjacent visit","")
    )
  )

milestones <- milestones %>%
  left_join(enrol_df, by = "usubjid") %>%
  mutate(
    m_date  = format(enrol_date + m_day, "%Y-%m-%d"),
    tooltip = paste0(usubjid," · ",milestone,"\nDay ",m_day,": ",m_date)
  )

epochs <- epochs %>%
  left_join(enrol_df, by = "usubjid") %>%
  mutate(
    s_date  = format(enrol_date + epoch_start, "%Y-%m-%d"),
    e_date  = format(enrol_date + epoch_end,   "%Y-%m-%d"),
    tooltip = paste0(usubjid," · ",epoch,"\n",s_date," → ",e_date)
  )

epochs_ = se %>%
  left_join(dm %>% select(USUBJID,RFSTDTC), by = "USUBJID") %>%
  rename(EPOCH=ELEMENT) %>%
  mutate( 
    tooltip = paste0(USUBJID," - ",EPOCH,"\n",SESTDTC," → ",SEENDTC)
  )

epoch_colors   <- c(Screening="#85B7EB","Run-in"="#FCBBC7",Treatment="#C3FA8F","Follow-up"="#FAD68F")
milestone_shapes <- c(ICF1=21, ICF2=22, Randomized=24, End=23)
milestone_fills  <- c(ICF1="#B30B7E", ICF2="#B3700B", Randomized="yellow", End="#534AB7")


#"Screen"      "Placebo"     "Follow_up"   "High_Start"  "High_Middle" "High_End"    "Low"



# ── Plot-building function ────────────────────────────────────────────────────
build_plot <- function(ep, vi, mi, di) {
  
  subj_order <- di %>% arrange(end_day) %>% pull(usubjid) %>% as.character()
  
  ep <- ep %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  vi <- vi %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  mi <- mi %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  di <- di %>% mutate(usubjid = factor(usubjid, levels = subj_order))
  
  visit_colours <- ifelse(vi$flag_overlap, "#E24B4A", "#2742F5")
  disp_colours  <- ifelse(di$completed,    "#0F6E56", "#A32D2D")
  
  ggplot() +
    
    geom_segment_interactive(
      data = ep,
      aes(x=epoch_start, xend=epoch_end, y=usubjid, yend=usubjid,
          colour=epoch, tooltip=tooltip, data_id=paste(usubjid,epoch)),
      linewidth=5, lineend="round"
    ) +
    
    geom_segment_interactive(
      data = vi %>% filter(!flag_overlap),
      aes(x=v_start, xend=v_end, y=usubjid, yend=usubjid,
          tooltip=tooltip, data_id=paste(usubjid,visit)),
      colour="#2742F5", linewidth=3.5, lineend="round"
    ) +
    
    geom_segment_interactive(
      data = vi %>% filter(flag_overlap),
      aes(x=v_start, xend=v_end, y=usubjid, yend=usubjid,
          tooltip=tooltip, data_id=paste(usubjid,visit)),
      colour="#E24B4A", linewidth=5, lineend="round"
    ) +
    
    geom_text(
      data = vi,
      aes(x=(v_start+v_end)/2, y=usubjid, label=visit),
      colour=visit_colours, vjust=-1.6, size=4, fontface="bold", show.legend=FALSE
    ) +
    
    geom_point_interactive(
      data = mi,
      aes(x=m_day, y=usubjid, shape=milestone, fill=milestone,
          tooltip=tooltip, data_id=paste(usubjid,milestone)),
      size=3.2, colour="white", stroke=0.7
    ) +
    
    geom_point_interactive(
      data = di,
      aes(x=end_day, y=usubjid, tooltip=tooltip, data_id=paste(usubjid,"end")),
      shape=124, size=5, colour="grey30"
    ) +
    
    geom_text_interactive(
      data = di,
      aes(x=end_day+4, y=usubjid, label=disp_label,
          tooltip=tooltip, data_id=paste(usubjid,"end_label")),
      colour=disp_colours, hjust=0, size=5, show.legend=FALSE
    ) +
    
    scale_colour_manual(values=epoch_colors, breaks=names(epoch_colors), name="Epoch (SE)") +
    scale_fill_manual( values=milestone_fills,  name="Disposition Event (DS)") +
    scale_shape_manual(values=milestone_shapes, name="Disposition Event (DS)") +
    
    scale_x_continuous(
      name="Days from subject day 0",
      breaks=seq(0,320,by=28),
      expand=expansion(mult=c(0.01,0))
    ) +
    scale_y_discrete(expand=expansion(add=c(0.5,1.2))) +
    coord_cartesian(xlim=c(0,420), clip="off") +
    
    labs(
      title    = "Swimmer Plot of Subjects' Disposition (DS), Study Visits (SV) and Study Elements (SE)",
      subtitle = "Thin bars = Start/End date of visit (SV)   Red bars = visit overlap (data error)",
      y = NULL
    ) +
    
    theme_minimal(base_size=12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour="grey92", linewidth=0.4),
      axis.text.x        = element_text(size=12),
      axis.text.y        = element_text(size=10),
      legend.position    = "bottom",
      legend.box         = "vertical",
      legend.title       = element_text(face="bold", size=14),
      legend.text        = element_text(size=14),
      plot.title         = element_text(face="bold", size=15),
      plot.subtitle      = element_text(colour="grey40", size=13, margin=margin(b=15)),
      plot.margin        = margin(t=6, r=5, b=10, l=10)
    ) +
    
    guides(
      colour = guide_legend(override.aes=list(linewidth=4), order=1),
      fill   = guide_legend(order=2),
      shape  = guide_legend(order=2)
    )
}




# ── Shiny UI ──────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  titlePanel("Swimmer plot — subject recruitment timeline"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(
        "site", "Filter by site:",
        choices  = c("All", sort(unique(subjects_df$site))),
        selected = "All"
      ),
      
      selectInput(
        "disp_filter", "Filter by disposition:",
        choices  = c("All", sort(unique(disposition$disp_label))),
        selected = "All"
      ),
      
      hr(),
      
      checkboxGroupInput(
        "subjects", "Select subjects:",
        choices  = sort(unique(disposition$usubjid)),
        selected = sort(unique(disposition$usubjid))
      ),
      
      hr(),
      
      fluidRow(
        column(6, actionButton("select_all",   "Select all",   width="100%")),
        column(6, actionButton("deselect_all", "Deselect all", width="100%"))
      ),
      
      hr(),
      
      checkboxInput("show_overlaps", "Highlight overlaps only", value = FALSE)
    ),
    
    mainPanel(
      width = 9,
      girafeOutput("swimmer", width="100%", height="650px")
    )
  )
)

# ── Shiny Server ──────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Update checkboxes when site filter changes
  observeEvent(input$site, {
    if (input$site == "All") {
      avail <- sort(unique(disposition$usubjid))
    } else {
      avail <- subjects_df %>%
        filter(site == input$site) %>%
        pull(usubjid) %>%
        sort()
    }
    updateCheckboxGroupInput(session, "subjects",
                             choices = avail, selected = avail)
  })
  
  # Update checkboxes when disposition filter changes
  observeEvent(input$disp_filter, {
    if (input$disp_filter == "All") {
      avail <- sort(unique(disposition$usubjid))
    } else {
      avail <- disposition %>%
        filter(disp_label == input$disp_filter) %>%
        pull(usubjid) %>%
        as.character() %>%
        sort()
    }
    updateCheckboxGroupInput(session, "subjects",
                             choices = avail, selected = avail)
  })
  
  # Select / deselect all
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "subjects",
                             selected = input$subjects %>% {choices <- .; choices})
  })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "subjects",
                             selected = isolate(
                               if (input$site == "All") sort(unique(disposition$usubjid))
                               else subjects_df %>% filter(site==input$site) %>% pull(usubjid) %>% sort()
                             ))
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "subjects", selected = character(0))
  })
  
  # Filtered reactive datasets
  filtered <- reactive({
    req(length(input$subjects) > 0)
    subj <- input$subjects
    
    ep <- epochs_      %>% filter(usubjid %in% subj)
    vi <- visits      %>% filter(usubjid %in% subj)
    mi <- milestones  %>% filter(usubjid %in% subj)
    di <- disposition %>% filter(usubjid %in% subj)
    
    # Optionally show only subjects with overlapping visits
    if (input$show_overlaps) {
      overlap_subj <- vi %>% filter(flag_overlap) %>% pull(usubjid) %>% unique()
      ep <- ep %>% filter(usubjid %in% overlap_subj)
      vi <- vi %>% filter(usubjid %in% overlap_subj)
      mi <- mi %>% filter(usubjid %in% overlap_subj)
      di <- di %>% filter(usubjid %in% overlap_subj)
    }
    
    list(ep=ep, vi=vi, mi=mi, di=di)
  })
  
  output$swimmer <- renderGirafe({
    d <- filtered()
    req(nrow(d$di) > 0)
    
    girafe(
      ggobj      = build_plot(d$ep, d$vi, d$mi, d$di),
      width_svg  = 14,
      height_svg = 7,
      options    = list(
        opts_tooltip(
          css     = "background:white;border:1px solid #ccc;padding:6px 17px;border-radius:6px;font-size:18px;font-family:sans-serif;white-space:pre;",
          opacity = 0.95
        ),
        opts_hover(css = "opacity:0.75;cursor:crosshair;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
}

shinyApp(ui, server)