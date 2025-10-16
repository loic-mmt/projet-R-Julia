library(dplyr)
# --- Robust project root detection (works from any launcher: source(), Rscript, RStudio) ---
get_script_path <- function() {
  # 1) source() in interactive/Console: look for ofile in call frames
  ofiles <- lapply(sys.frames(), function(x) x$ofile)
  ofiles <- Filter(Negate(is.null), ofiles)
  if (length(ofiles)) return(normalizePath(tail(ofiles, 1)[[1]], winslash = "/", mustWork = FALSE))
  # 2) source("...") call captured from sys.calls()
  sc <- sys.calls()
  for (i in rev(seq_along(sc))) {
    call_i <- sc[[i]]
    if (is.call(call_i) && identical(call_i[[1]], as.name("source")) && length(call_i) >= 2) {
      src <- try(as.character(call_i[[2]]), silent = TRUE)
      if (!inherits(src, "try-error") && length(src) == 1 && nzchar(src)) {
        return(normalizePath(src, winslash = "/", mustWork = FALSE))
      }
    }
  }
  # 3) Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  farg <- grep("^--file=", args, value = TRUE)
  if (length(farg)) return(normalizePath(sub("^--file=", "", farg[1]), winslash = "/", mustWork = FALSE))
  # 4) RStudio active doc
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    try({
      if (rstudioapi::isAvailable()) {
        p <- rstudioapi::getActiveDocumentContext()$path
        if (nzchar(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
      }
    }, silent = TRUE)
  }
  NA_character_
}

get_project_root <- function() {
  sp <- get_script_path()
  if (!is.na(sp)) return(normalizePath(file.path(dirname(sp), ".."), winslash = "/", mustWork = FALSE))
  # 5) Git root if available
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    crit <- rprojroot::has_file(".git")
    root_try <- try(rprojroot::find_root(crit), silent = TRUE)
    if (!inherits(root_try, "try-error")) return(root_try)
  }
  # 6) Fallback: current dir looks like project (has R/ and data_raw/)
  if (file.exists(file.path(getwd(), "R")) && file.exists(file.path(getwd(), "data_raw"))) {
    return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  }
  stop("Impossible de déterminer la racine du projet.\nConseils:\n- Utilise source('R/cleaning.R') depuis la racine du repo, ou\n- Utilise Rscript R/cleaning.R, ou\n- Fais setwd('.../projet-R-Julia') avant de sourcer.")
}
root <- get_project_root()
cat("Project root:", root, "\n")
# ----------------------------------------------------------------

file <- file.path(root, "data_raw", "ds_salaries.csv")

data <- read.csv2(file, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")


print(names(data))
data <- data.frame(data)
data <- data[, -5]
sum(is.na(as.double(data$salary_in_usd))) 

df <- data %>%
  mutate(salary_in_usd = as.double(salary_in_usd),
         remote_ratio = as.integer(remote_ratio),
         experience_level = factor(experience_level, levels = c("EN", "MI", "SE", "EX")),
         employment_type = factor(employment_type, levels = c("PT", "CT", "FT", "FL")),
         company_size = factor(company_size, levels = c("S", "M", "L")),
         work_year = as.integer(work_year)
  )

head(df)

out_dir <- file.path(root, "data_proc")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "ds_salaries_corr.csv")
write.csv2(df, out_path, row.names = FALSE, fileEncoding = "UTF-8")

data_2020 <- subset(data, work_year == 2020)
data_2021 <- subset(data, work_year == 2021)
data_2022 <- subset(data, work_year == 2022)
data_2023 <- subset(data, work_year == 2023)

print(nrow(data_2020))
print(nrow(data_2021))
print(nrow(data_2022))
print(nrow(data_2023))