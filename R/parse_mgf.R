#' Parse mgf
#'
#' @param mgf_path path to mgf
#'
#' @return data.table with cols pepmass, charge, scan,
#'     and col containing json representation of mass/intensity
#' @export
#' @importFrom data.table :=
#'

parse_mgf <- function(mgf_path){

  # to make package check less angry:
  grp <- NULL
  mass <- NULL
  intensity <- NULL
  json <- NULL

  pepmass = data.table::fread(cmd = paste0("grep 'PEPMASS' ",
                                           mgf_path,
                                           " | cut -d = -f2"),
                              header = FALSE)
  charge = data.table::fread(cmd = paste0("grep 'CHARGE' ",
                                          mgf_path,
                                          " | cut -d = -f2"),
                             header = FALSE)

  begin <- data.table::fread(cmd = paste0("grep -n 'SCANS=' ",
                                          mgf_path,
                                          " | cut -d : -f 1")) + 1

  end <- data.table::fread(cmd = paste0("grep -n 'END\ IONS' ",
                                        mgf_path,
                                        " | cut -d : -f 1"))

  dif <-  end - begin
  remove(begin)
  remove(end)

  scan <- data.table::fread(cmd = paste0("grep -n 'SCANS=' ",
                                          mgf_path,
                                          " | cut -d = -f2"))

  spec <- data.table::fread(cmd = paste0("grep '^[0-9]\\s' ", mgf_path),
                            col.names = c("mass", "intensity"))

  indices <- which(dif != 0)

  b <- dif[[1]][indices]
  b <- unlist(lapply(seq_along(b), function(x) rep(x, b[[x]])))

  spec[ , grp := b]
  spec <- spec[ , list(mass = list(mass), 'intensity' = list(intensity)), by = grp]

  spec <- spec[ , json := jsonlite::toJSON(list(mass = unlist(mass), intensity = unlist(intensity))), grp]
  spec[ , c('mass', 'intensity', 'grp') := NULL]

  a <- data.table::data.table()
  a[,c("pepmass","charge") := c(pepmass, charge)]
  a[indices, 'json' := spec[,]]
  a[, scan := scan]
  return(a)
}
