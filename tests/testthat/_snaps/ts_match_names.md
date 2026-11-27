# Produces expected output

    Code
      match_res
    Output
                       query            reference      match_type dist match_side
      1 Crepidomanes minutus Crepidomanes minutum fuzzy_canonical    1       <NA>
        id_query id_ref genus_hybrid_sign_query genus_name_query
      1        1  ref-1                    <NA>     Crepidomanes
        species_hybrid_sign_query specific_epithet_query infraspecific_rank_query
      1                      <NA>                minutus                     <NA>
        infraspecific_epithet_query author_query genus_hybrid_sign_ref genus_name_ref
      1                        <NA>         <NA>                  <NA>   Crepidomanes
        species_hybrid_sign_ref specific_epithet_ref infraspecific_rank_ref
      1                    <NA>              minutum                   <NA>
        infraspecific_epithet_ref author_ref
      1                      <NA>       <NA>

# Manually matched names work

    Code
      match_res
    Output
                       query                reference      match_type
      1 Crepidomanes minutus     Crepidomanes minutum fuzzy_canonical
      2        Hymeefee erae Hymenophyllum polyanthos          manual

# Names that can't be parsed don't show up in results

    Code
      match_res
    Output
      # A tibble: 1 x 3
        query                reference            match_type     
        <chr>                <chr>                <chr>          
      1 Crepidomanes minutus Crepidomanes minutum fuzzy_canonical

# Manually matched names work with collapsed infrasp names

    Code
      match_res
    Output
      # A tibble: 6 x 3
        query                       reference            match_type     
        <chr>                       <chr>                <chr>          
      1 Crepidomanes minutus        Crepidomanes minutum fuzzy_canonical
      2 Crepidomanes minutawtaw     Crepidomanes minutum manual         
      3 Blechnum lunare var. lunare Blechnum lunare      autonym        
      4 Blechnum lunare             Blechnum lunare      exact          
      5 Bar foo var. foo            Bar foo              manual         
      6 Bar foo                     Bar foo              exact          

