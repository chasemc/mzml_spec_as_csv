

obo <- readLines("https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo")
ind <- which(vapply(obo, function(x) x == "[Term]", T,USE.NAMES = F))
split_list =mapply(function(i,j) obo[i:j], ind, c((ind - 1)[-1], length(ind)), SIMPLIFY = FALSE)
remove(obo)
remove(ind)
df <- lapply(split_list, function(i){

  temp <- data.frame(data.frame(matrix(ncol = 12, nrow = 1)))
  colnames(temp) <- c("id", "name", "synonym", "def", "is_a", "xref", "relationship",  "comment", "is_obsolete", "replaced_by", "property_value","other")
  temp$is_obsolete <- FALSE
  #  temp$relationship <- list()
  # temp$other <- list()
  for (x in i) {

    if (startsWith(x, "id:")) {
      temp$id = gsub("id: ", "", x)
    } else if (startsWith(x, "name:")) {
      temp$name = gsub("name: ", "", x)
    } else if (startsWith(x, "def:")) {
      temp$def = gsub("def: ", "", x)
    } else if (startsWith(x, "is_a:")) {
      temp$is_a = gsub("is_a: ", "", x)
    } else if (startsWith(x, "xref:")) {
      temp$xref = gsub("xref: ", "", x)
    } else if (startsWith(x, "relationship:")) {
      if (is.na(temp$relationship)){
        temp$relationship <- gsub("relationship: ", "", x)
      } else {
        temp$relationship <- paste0(temp$relationship, "; ", gsub("relationship: ", "", x))
      }
    } else if (startsWith(x, "is_obsolete:")){
      temp$is_obsolete <- TRUE
    } else if (startsWith(x, "synonym:")){
      temp$synonym = gsub("synonym: ", "", x)
    } else if (startsWith(x, "replaced_by:")){
      temp$replaced_by = gsub("replaced_by: ", "", x)
    } else if (startsWith(x, "property_value:")){
      temp$property_value = gsub("property_value: ", "", x)
    } else if (startsWith(x, "comment:")){
      if (is.na(temp$comment)){
        temp$comment <- gsub("comment: ", "", x)
      } else {
        temp$comment <- paste0(temp$comment, "; ",gsub("comment: ", "", x))
      }
    } else if ( x != "[Term]" & x != ""){
      if (is.na(temp$comment)){
        temp$other <- paste0(temp$other, "; ", x)
      } else {
        temp$other <- x
      }
    }
  }
  temp
})

df <- do.call(rbind, df)
write.csv(df, file="mzml_spec.csv", sep = ",",row.names = F)
write.table(df, file="mzml_spec.tsv", sep = "\t",row.names = F)
# jsonlite::write_json(jsonlite::toJSON(df,dataframe = "rows"),path = "mzml_spec.json")
