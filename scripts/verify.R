# Run from the repository root. This checks the frozen release without fitting.
stopifnot(dir.exists("submission"))
expected_files <- sort(c(
  "CreditLens.R", "creditlens_assets.rds",
  "individual_assignment.Rmd", "individual_assignment.pdf"
))
stopifnot(identical(sort(list.files("submission", all.files = FALSE)), expected_files))

expected_hashes <- c(
  "CreditLens.R" = "42e1be343857619210198bc383ef5c6922ad5f1b66d15c9455279219e8fdb51e",
  "creditlens_assets.rds" = "3dec1ad5c0c68d88d0646c6f11493bbc7b37c8daa1f42cb95891f10802350434",
  "individual_assignment.Rmd" = "dc4fe562cd15ffcda9edb0d858574ce7e30ce981a7cf41c0581631f45353c53b",
  "individual_assignment.pdf" = "661b6e2e572afcd08d3c2a8ba21245b4c428f82119ec7adbea7fe38787521a64"
)
submission_hashes <- function() {
  vapply(names(expected_hashes), function(name) {
    digest::digest(file = file.path("submission", name), algo = "sha256")
  }, character(1))
}
before <- submission_hashes()
stopifnot(identical(before, expected_hashes))

# Guard against accidentally launching Shiny while loading the implementation.
original_timeout <- getOption("timeout")
parse("submission/CreditLens.R", keep.source = FALSE) |> invisible()
source("submission/CreditLens.R")
stopifnot(
  is.function(creditlens),
  is.function(verify_release),
  !"package:shiny" %in% search(),
  identical(getOption("timeout"), original_timeout)
)

assets <- creditlens("load")
verification <- creditlens("verify")
stopifnot(isTRUE(verification$passed), length(verification$checks) == 8L)
cat(paste("PASS:", verification$checks), sep = "\n")
cat("\n")

# Pin the model/release and original input set, independently of UI labels.
stopifnot(
  identical(assets$identity$release_id, "CL-b2563abe35c5"),
  identical(assets$identity$model_id,
    "f9bad23a4b316c783968debedb0e47390b91f94f8e9480c69691379c631d0cb2"),
  setequal(assets$app$runtime$model$required, c(
    "AGE", "LIMIT_BAL", "PAY_0", paste0("PAY_", 2:6), paste0("PAY_AMT", 1:6)
  )),
  identical(assets$deployment$identity, assets$identity),
  identical(assets$deployment$url, "https://pimvdb.shinyapps.io/creditlens/")
)

# Make an app object, without opening a port, browser or hosting connection.
application <- create_creditlens_app(.creditlens_root, assets = assets)
stopifnot(inherits(application, "shiny.appobj"))
stopifnot(identical(before, submission_hashes()))

# Figures are embedded in the RDS, including the report's app screenshot.
png_signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
figures <- c(assets$app$figures, assets$report$figures)
stopifnot(length(figures) >= 4L)
for (figure in figures) {
  stopifnot(is.raw(figure), identical(figure[1:8], png_signature))
}
pdf_header <- readBin("submission/individual_assignment.pdf", "raw", n = 5L)
stopifnot(identical(rawToChar(pdf_header), "%PDF-"))

# Local Markdown links must resolve, including images and the report PDF.
documents <- c("README.md", "NOTICE.md", list.files("docs", "[.]md$", full.names = TRUE))
for (document in documents) {
  content <- paste(readLines(document, warn = FALSE), collapse = "\n")
  matches <- regmatches(content, gregexpr("\\]\\(([^)]+)\\)", content))[[1]]
  for (match in matches) {
    target <- sub("^\\]\\((.*)\\)$", "\\1", match)
    if (grepl("^(https?://|mailto:|#)", target)) next
    target <- sub("#.*$", "", target)
    stopifnot(file.exists(file.path(dirname(document), URLdecode(target))))
  }
}
cat("PASS: canonical bytes, load-only source, Shiny object, embedded figures and documentation links\n")
cat("PASS: no model training or submission changes\n")
print(sessionInfo())
