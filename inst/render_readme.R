library(magrittr, include.only = "%>%")
# mudar o yaml do arquivo .Rmd -------------
CursoRopen::change_rmd_yaml(
  "README.Rmd",
  params = list(
    main_repo = Sys.getenv("MAIN_REPO"),
    trilha_clean = Sys.getenv("TRILHA_CLEAN"),
    turma = Sys.getenv("TURMA"),
    nome_curso = Sys.getenv("NOME_CURSO"),
    num_aula = "0",
    download_material = Sys.getenv("DOWNLOAD_MATERIAL")
  )
) %>%
  writeLines("README.Rmd")


# render o arquivo --------------

rmarkdown::render(
  input = "README.Rmd",
  output_format = "github_document",
  clean = TRUE,
  output_file = "README.md"

)

# # mover o atualizar_pagina_do_curso.yaml para a pasta do actions ----
#
# caminho_gha_inicial <-
#   here::here("inst", "atualizar_pagina_do_curso.yaml")
#
# caminho_gha_final <-
#   here::here(".github", "workflows" ,  "atualizar_pagina_do_curso.yaml")
#
#
# fs::file_copy(caminho_gha_inicial, caminho_gha_final, overwrite = FALSE)
