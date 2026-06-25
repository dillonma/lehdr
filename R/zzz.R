# ----------------------------------------------------------------------
# Package hooks
# ----------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  ver <- as.character(utils::packageVersion("lehdr"))
  packageStartupMessage(
    "lehdr ", ver, "\n",
    "\n",
    "To cite lehdr in published work (Chicago author-date):\n",
    "\n",
    "  Green, Jamaal, Liming Wang, and Dillon Mahmoudi. 2025.\n",
    "  \"lehdr: Grab Longitudinal Employer-Household Dynamics\n",
    "  (LEHD) Flat Files.\" R package version ", ver, ".\n",
    "  https://github.com/jamgreen/lehdr/\n",
    "\n",
    "Run citation(\"lehdr\") for a BibTeX entry.\n",
    "Use suppressPackageStartupMessages() to suppress this message."
  )
}
