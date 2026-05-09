# auto_exec() errors when directory does not exist

    Code
      auto_exec(dir = "nonexistent_dir_abc123")
    Message
      
      -- auto_exec -------------------------------------------------------------------
    Output
      
    Condition
      Error in `auto_exec()`:
      ! Aucun répertoire nommé 'nonexistent_dir_abc123' trouvé dans '<pkgdir>'

# auto_exec() errors when no matching files found

    Code
      auto_exec(dir = dir, except_starts_with = "_", ext = ".R")
    Message
      
      -- auto_exec -------------------------------------------------------------------
    Output
      
    Condition
      Error in `auto_exec()`:
      ! Aucun fichier `*.R` trouvé dans '<tmpdir>'
      i Fichiers exclus : préfixe "_"

# auto_exec() errors when only non-.R files exist

    Code
      auto_exec(dir = dir)
    Message
      
      -- auto_exec -------------------------------------------------------------------
    Output
      
    Condition
      Error in `auto_exec()`:
      ! Aucun fichier `*.R` trouvé dans '<tmpdir>'
      i Fichiers exclus : préfixe "_"

