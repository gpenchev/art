# =============================================================================
# server.R — EU Defence Policy App
# =============================================================================

server <- function(input, output, session) {

  # Each tab is handled by its own module.
  # Modules are defined in modules/mod_*.R and sourced in global.R.
  # All data objects from app_data.rda are available in the global environment.

  overviewServer("overview")
  threatServer("threat")
  stanceServer("stance")
  dtwServer("dtw")
  typologiesServer("typologies")
}
