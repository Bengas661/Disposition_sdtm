# 📋 Disposition Shiny App

> A single-view exploration tool for subject disposition data.

---

## Purpose

A single-view exploration tool for subject disposition data, designed to streamline clinical data review workflows.

---

## Rationale

Exploring subject disposition data often requires navigating multiple SDTM datasets. This application consolidates key information into one interface, enabling faster and more intuitive review of subject-level outcomes.

---

## Key Features & Advantages

- 🔍 **Integrated View** — Displays the most important subject disposition information in a single, unified interface
- 🛠️ **TFL Programming Support** — Helps identify inconsistencies or conflicts in data during programming
- 📊 **Study Overview** — Provides sponsors with an overview of study disposition by subject, site, or treatment arm
- 🔎 **Diagnostic Layer** — Adds an additional layer for detecting potential data or programming issues (e.g., dates, visits, epochs, treatments)

---

## Data Source

This application uses **CDISC Pilot Study SDTM data**.

🔗 [CDISC SDTM/ADaM Pilot Project](https://github.com/cdisc-org/sdtm-adam-pilot-project)

---

## Live Application

🚀 [Launch the App](https://ioannis-elmatzoglou.shinyapps.io/disposition/)

---

## Source Code

📁 [View Repository](https://github.com/Bengas661/Disposition_sdtm)

---

## Getting Started

To run the app locally, clone the repository and launch it in R:

```r
# Clone the repository
# git clone https://github.com/Bengas661/Disposition_sdtm.git

# Install required packages (if needed)
# install.packages(c("shiny", "..."))

# Run the app
shiny::runApp()
```

---

## Contributing

Contributions, issues, and feature requests are welcome. Feel free to open an issue or submit a pull request.

---

## License

This project is open source. See the repository for license details.
