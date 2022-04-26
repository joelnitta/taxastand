# Parsing works with docker

    Code
      ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar", docker = TRUE)
    Output
      -\|/ 
                                                   name         id genus_hybrid_sign
      1 Foogenus x barspecies var. foosubsp (L.) F. Bar 5f207ff2-1              <NA>
        genus_name species_hybrid_sign specific_epithet infraspecific_rank
      1   Foogenus                   ×       barspecies               var.
        infraspecific_epithet      author
      1              foosubsp (L.) F. Bar

# Parsing works with local taxon-tools

    Code
      ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
    Output
                                                   name         id genus_hybrid_sign
      1 Foogenus x barspecies var. foosubsp (L.) F. Bar 5f207ff2-1              <NA>
        genus_name species_hybrid_sign specific_epithet infraspecific_rank
      1   Foogenus                   ×       barspecies               var.
        infraspecific_epithet      author
      1              foosubsp (L.) F. Bar

