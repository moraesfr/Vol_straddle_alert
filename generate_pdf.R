#!/usr/bin/env Rscript

# Install pagedown if needed
if (!require('pagedown', quietly = TRUE)) {
  install.packages('pagedown', repos = 'http://cran.r-project.org', quiet = TRUE)
}

library(pagedown)

# Convert HTML to PDF
cat("Converting HTML to PDF...\n")
tryCatch({
  chrome_print('straddle_report.html', 'straddle_report.pdf', wait_for = 3)
  cat("✓ PDF created: straddle_report.pdf\n")
}, error = function(e) {
  cat("Warning: chrome_print failed, trying alternative method\n")
  # Alternative: use pandoc if available
  system("pandoc straddle_report.html -o straddle_report.pdf 2>/dev/null || echo 'Pandoc not available'")
})
