# ----------------------------------------------------------------------
# Package hooks
# ----------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "lehdr ", utils::packageVersion("lehdr"), "\n",
    "\n",
    "To cite lehdr in published work:\n",
    "\n",
    "  Green, J., Wang, L., & Mahmoudi, D. (2025). lehdr: Grab\n",
    "  Longitudinal Employer-Household Dynamics (LEHD) Flat Files\n",
    "  (R package version ", utils::packageVersion("lehdr"), ").\n",
    "  https://github.com/jamgreen/lehdr/\n",
    "\n",
    "Run citation(\"lehdr\") for a BibTeX entry.\n",
    "Use suppressPackageStartupMessages() to suppress this message."
  )
}
