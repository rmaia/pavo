parse_procspec <- function(path) {
  # We let R find the suitable tmp folder to extract files
  extracted_files <- unzip(zipfile = path,
                           exdir = tempdir())

  # According to OceanOptics FAQ [1], each procspec archive will only contain
  # one XML spectra file.
  # [1] https://oceanoptics.com/faq/extract-data-procspec-file-without-spectrasuite/

  # Data files have the format ps_\d+.xml
  data_file <- grep(pattern = "ps_\\d+\\.xml", extracted_files, value = TRUE)

  # From what I have seen, encoding is UTF-8 for files created on GNU/Linux and
  # ISO-8859-1 (aka latin-1) for windows.
  # TODO: what about macOS? This approach should support it anyways.
  # TODO: it is a bit awkward to import a package just for this small task. Try
  # to find a lighter but still reliable way to do this in the future.
  candidates_encoding <- readr::guess_encoding(data_file, n_max = -1)
  encoding <- candidates_encoding$encoding[which.max(candidates_encoding$confidence)]

  xml_source <- xml2::read_xml(data_file, encoding = encoding)

  wl_node <- xml2::xml_find_all(xml_source, ".//channelWavelengths")
  wl_values <- xml2::xml_find_all(wl_node, ".//double")
  # Get rid of the XML tags.
  wl <- xml2::xml_text(wl_values)

  procspec_node <- xml2::xml_find_all(xml_source, ".//processedPixels")
  procspec_values <- xml2::xml_find_all(procspec_node, ".//double")
  # Get rid of the XML tags.
  procspec <- xml2::xml_text(procspec_values)

  spec_df <- cbind(wl, procspec)
  # The XML file was considered as text. So are "wl" and "procspec" columns.
  spec_df <- apply(X = spec_df, MARGIN = 2, FUN = as.numeric)
  spec_df <- as.data.frame(spec_df)

  return(spec_df)
}
