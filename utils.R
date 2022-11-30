# Tm = Theoretical mass
# ppm = error 
# range = range of +- ppm
get_ppm <- function(Tm, ppm = 10) {
  Mm = ( ppm * Tm / 1E6) + Tm
  range = abs(Tm-Mm)
  range
}
# Extrac Intensity and RT

utils_EIC_rt <- function(MChromatograms) {
  require(tibble)
  lapply(MChromatograms, function(x){
   tibble(Intensity = intensity(x), # Extract intentiy
          rt = rtime(x))  # Extract retention time
  }  
  )
}

# Extract EIC
# msn_exp = mnsBase experiment
# mz = mz to get the mz with the ionization mz offset included (M +/- 1.007)
# ID for the EIC
get_EIC_tbb <- function(msn_exp, mz, ID) {
  chrom <-  chromatogram(msn_exp, mz = mz) # Extract chromatogram
  sam_names <- sampleNames(chrom) # Extract sample names
  intensities <- utils_EIC_rt(chrom) # Extract Intensity and rt as tibble
  
  # source:  https://stackoverflow.com/questions/13404877/adding-a-new-column-to-each-element-in-a-list-of-tables-or-data-frames
  tibble_EIC <- mapply(function(inten, sam_names) "[<-"(inten, "Sample_ID", value = sam_names) ,
         intensities, sam_names, SIMPLIFY = FALSE) # Assign ID to ttible
  
  tibble_EIC %>% bind_rows %>% 
    mutate(Met_ID =  ID ) %>% replace(is.na(.), 0) %>%
    return() # Bind rows as tibble and return
}


