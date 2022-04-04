# Running example E: combining spectral and spatial data with pavo

# Flowerpiercer (Diglossa spp.) birds
# Photos by Anna E. Hiller at Louisiana State University

# Run the numbered scripts in order:

# This file (06_birds_pavo.R) is included for convenience, but the most
# instructive thing to do is to run each of those files one after the other to
# figure out what the code is actually doing!

# running alignLan on images of bird breasts:
source("06_birds/06_birds_a_alignment.R")

# fitting all bird breasts to a universal color palette:
source("06_birds/06_birds_b_recolorize.R")

# combining color maps with spectra and running analysis in different visual systems:
source("06_birds/06_birds_c_pavo.R")
