

my_json <- add_description("My Description")

jsonlite::validate(my_json)

jsonlite::validate(add_rights())

my_json_2 <- gsub("\\\\", "", my_json)

gsub("\\\\", "",  my_json_2)

jsonlite::write_json(my_json_2, "my_json_2.txt")

stringi::stri_unescape_unicode(my_json_2)

?jsonlite::validate


doc <- '{
  "http://schema.org/name": "Manu Sporny",
  "http://schema.org/url": {"@id": "http://manu.sporny.org/"},
  "http://schema.org/image": {"@id": "http://manu.sporny.org/images/manu.png"}
}'

context <- '{
  "name": "http://schema.org/name",
  "homepage": {"@id": "http://schema.org/url", "@type": "@id"},
  "image": {"@id": "http://schema.org/image", "@type": "@id"}
}'


library(jsonld)
list1 <- jsonld_compact(doc, context)
list2 <- jsonld_compact(doc, context)



id = 'first_id'
dataset_code = NULL
URI = NULL
DOI = NULL
Version = NULL
idAtSource = NULL
Other = NULL
identifiers = NULL
