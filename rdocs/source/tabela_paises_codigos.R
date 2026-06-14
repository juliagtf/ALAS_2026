#Lista_países (Brasil)

pacman::p_load(countries)

base_paises <- countries::country_reference_list

paises <- base_paises$name_pt

codigos_iso2 <- base_paises$ISO2
codigos_iso3 <- base_paises$ISO3
codigos_iso_num <- base_paises$ISO_code


paises_tabela <- data.frame(
  pais = paises,
  iso2 = codigos_iso2,
  iso3 = codigos_iso3,
  iso_code = codigos_iso_num,
  stringsAsFactors = FALSE
)

paises_tabela <- paises_tabela[complete.cases(paises_tabela), ]

paises <- paises_tabela$pais
codigos_iso2 <- paises_tabela$iso2
codigos_iso3 <- paises_tabela$iso3
codigos_iso_num <- paises_tabela$iso_code
