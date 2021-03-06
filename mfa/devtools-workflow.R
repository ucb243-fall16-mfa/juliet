# =====================================================
# Using devtools
# =====================================================

library(devtools)


devtools::document()
devtools::check_man()
devtools::test()
devtools::build_vignettes()
devtools::build(vignettes = FALSE)
devtools::install()
