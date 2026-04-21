# C. Savonen

# Download that Citation Data

library(metricminer)
library(magrittr)

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))
source(file.path(root_dir, "refresh-scripts", "folder-setup.R"))

folder_path <- file.path("metricminer_data", "citations")

# Declare and read in config file
yaml_file_path <- file.path(root_dir, "_config_automation.yml")
yaml <- yaml::read_yaml(yaml_file_path)

# Authorize Google
auth_from_secret("google",
                 refresh_token = Sys.getenv("GOOGLE_REFRESH_GA"),
                 access_token = Sys.getenv("GOOGLE_ACCESS_GA"),
                 cache = TRUE
)

setup_folders(
  folder_path = folder_path,
  google_entry = "citation_googlesheet",
  config_file = yaml_file_path,
  data_name = "citation"
)

yaml <- yaml::read_yaml(yaml_file_path)

### Get citation data

all_papers <- lapply(yaml$citation_papers, function(paper) {
  df <- get_citation_count(paper)
  Sys.sleep(60)
  return(df)
})

all_papers <- dplyr::bind_rows(all_papers)

num_rows <- nrow(all_papers)
num_rows_prev <- nrow(ifelse(yaml$data_dest == "github", 
                             readr::read_tsv(file.path(folder_path, "citations.tsv")),
                             googlesheets4::read_sheet(yaml$citation_googlesheet, sheet = "citations")
                             )
                      )

if (yaml$data_dest == "google" & num_rows_prev <= num_rows) {
  googlesheets4::write_sheet(all_papers,
                             ss = yaml$citation_googlesheet,
                             sheet = "citations"
  )
}

if (yaml$data_dest == "github" & num_rows_prev <= num_rows) {
  readr::write_tsv(all_papers,
                   file.path(folder_path, "citations.tsv")
  )
}
sessionInfo()
