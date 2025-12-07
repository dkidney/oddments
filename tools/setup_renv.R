# -----------------------------------------------------------
# Ensure renv is installed without loading it
# -----------------------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# -----------------------------------------------------------
# Initialize renv only if it has not already been initialized
# (This prevents overwriting an existing environment)
# -----------------------------------------------------------
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
} else {
  renv::load()
}
renv::status()

# -----------------------------------------------------------
# Install dependencies for the local package
# (reads the DESCRIPTION and installs its Imports/Depends/etc.)
# -----------------------------------------------------------
renv::install(".")

# -----------------------------------------------------------
# Install additional development tools
# -----------------------------------------------------------
dev_pkgs <- c(
  "devtools",
  "usethis",
  "roxygen2"
)
for (pkg in dev_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
      renv::install(pkg)
  }
}

# -----------------------------------------------------------
# Snapshot the environment
# -----------------------------------------------------------
renv::snapshot()
