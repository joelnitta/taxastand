# Produces expected output in docker

    Code
      match_res
    Output
                       query            reference match_type   id_query     id_ref
      1 Crepidomanes minutus Crepidomanes minutum auto_fuzzy c1ad73ec-1 19b861c8-1
        genus_hybrid_sign_query genus_name_query species_hybrid_sign_query
      1                    <NA>     Crepidomanes                      <NA>
        specific_epithet_query infraspecific_rank_query infraspecific_epithet_query
      1                minutus                     <NA>                        <NA>
        author_query genus_hybrid_sign_ref genus_name_ref species_hybrid_sign_ref
      1         <NA>                  <NA>   Crepidomanes                    <NA>
        specific_epithet_ref infraspecific_rank_ref infraspecific_epithet_ref
      1              minutum                   <NA>                      <NA>
        author_ref
      1       <NA>

# Manually matched names work

    Code
      match_res
    Output
                       query                reference match_type
      1 Crepidomanes minutus     Crepidomanes minutum auto_fuzzy
      2        Hymeefee erae Hymenophyllum polyanthos     manual

