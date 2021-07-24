library(eurostat)
library(dplyr)
library(tidyverse)
library(dataobservatory)
ids <- c("teicp090", "ISOC_CICCE_USE",
         "tin00028", "tin00092", "tin00091", "tin00093", "tin00095",
         "tin00029","tin00030", "tin00099", "tin00098",
         "tin00032", "tin00080", "tgs00050", "tgs00052",
         "tgs00111", "tin00127", "nama_10r_2coe")


download_data <- function() {
  tin00028 <- get_eurostat ("tin00028")
  tin00092 <- get_eurostat('tin00092')
  isoc_cicci_use <- get_eurostat(tolower("isoc_cicci_use"))
  consumption <- get_eurostat (id = tolower('HBS_EXP_T121'))
}




head ( tin00028  )
# I_IU3	Last internet use: in last 3 months
# I_ILT12	Last internet use: in the last 12 months
# I_IUEVR	Individuals who have ever used the internet
# I_IUX	Internet use: never

never_used_internet <-  dataset_eurostat(dat = tin00028 %>%
                   filter ( .data$indic_is == "I_IUX"),
                 dataset_code = "ind_never_use_internet",
                 eurostat_id = "tin00028",
                 doi = "10.5281/zenodo.5121507",
                 description = "Percentage of individuals who never used the internet in the population group aged 16 to 74",
                 Subject = "Music industry",
                 Contributor= "Vitos, Botond",
                 keywords = c("dmo", "Music industry", "Demand (Economic theory)", "Internet users"),
                 Title = "Population Who Never Used Internet")


test_codebook <- never_used_internet$codebook[[1]]
test_codebook$RelatedItem[1]

daily_internet_users <- dataset_eurostat(dat = tin00092,
                                         dataset_code = "ind_daily_use_internet",
                                         eurostat_id = "tin00028",
                                         description = "Percentage of individuals who use the internet on a daily basis in the population group aged 16 to 74",
                                         Subject = "Music industry",
                                         Contributor= "Vitos, Botond",
                                         keywords = c("dmo", "Music industry", "Demand (Economic theory)", "Internet users"),
                                         Title = snakecase::to_title_case("Individuals Who Use the Internet on a Daily Basis"))



#[I_CC] Used internet storage space to save documents, pictures, music, video or other files
#[I_CCX] Did not use internet storage space to save documents, pictures, music, video or other files
#[I_CCS_EM] Used e-mails with attached files when sharing documents, pictures or other files electronically
#[I_CCS_PWS] Used personal websites or social networking sites when sharing documents, pictures or other files electronically
#[I_CCS_CC] Used internet storage space when sharing documents, pictures or other files electronically
#[I_CCS_OTH] Used other means not using internet when sharing documents, pictures or other files electronically
#[I_CCSX] Did not share files
#[I_CCS_PWS_CC] Used personal websites or social networking sites and internet storage space when sharing documents, pictures or other files electronically
#[I_CC_CCS] Internet storage space use: to save or share documents, pictures, music, video or other files
#[I_CC_OFF] Internet storage space use: to save or share texts, spreadsheets or electronic presentations
#[I_CC_PHO] Internet storage space use: to save or share photos
#[I_CC_EBO] Internet storage space use: to save or share e-books or e-magazines
#[I_CC_MUS] Internet storage space use: to save or share music
#[I_CC_VID] Internet storage space use: to save or share videos including films, TV programmes
#[I_CC_OTH] Internet storage space use: to save or share other things
#[I_CC_MV] Internet storage space use: to save or share music and videos

unique(isoc_cicci_use$ind_type )

ind_cloud_storage_files <- isoc_cicci_use  %>%
  filter ( .data$indic_is == "I_CC",
           .data$ind_type == "IND_TOTAL",
           .data$unit == "PC_IND")

unique ( ind_cloud_storage_files$unit)

ind_cloud_storage_files <- dataset_eurostat(
  dat = ind_cloud_storage_files,
  doi = '10.5281/zenodo.5126841',
  dataset_code = "ind_cloud_storage_files",
  eurostat_id = tolower("isoc_cicci_use"),
  description = "Percentage of individuals who use internet storage space to save documents, pictures, music, video or other files",
  Subject = "Music industry",
  Contributor= "Vitos, Botond",
  keywords = c("dmo", "Music industry", "Demand (Economic theory)", "Internet users"),
  Title = "Individuals Who Use Cloud Storage")


student_cloud_storage_files <- isoc_cicci_use  %>%
  filter ( .data$indic_is == "I_CC",
           .data$ind_type == "STUD",
           .data$unit == "PC_IND")


cloud_storage <- ind_cloud_storage_files$dataset[[1]] %>%
  left_join ( group_european_countries(), by = "geo") %>%
  mutate ( group = ifelse (.data$geo == 'TR', 'Southeast', .data$group))

dat <- cloud_storage %>% filter ( .data$group == "Visegrad")
create_single_plot <- function (dat) {
  single_palette <-  palette_eu_countries()
  single_palette <- single_palette[names(single_palette) %in% unique(dat$geo)]

  dat %>%
    ggplot ( aes ( x=time, y = value, color = geo) ) +
    geom_line() +
    scale_color_manual (values =single_palette) +
    scale_y_continuous( limits = c(0,100)) +
    scale_x_date() +
    labs ( color =NULL, x =NULL, y =NULL) +
    theme ( legend.position = 'bottom',
            legend.text = element_text(size = 8))
}


storage_plots <-lapply ( unique(group_european_countries()$group),
         function(x) as_tibble(cloud_storage) %>%
           filter(.data$group == x) %>% create_single_plot()  )

library(grid)
g <- gridExtra::grid.arrange(
  storage_plots[[2]],
  storage_plots[[3]], storage_plots[[4]],
  storage_plots[[5]], storage_plots[[6]],
  storage_plots[[7]], storage_plots[[8]],
  storage_plots[[9]], storage_plots[[1]],
  ncol = 3,
  top=grid::textGrob("Cloud Storage Use", gp=gpar(fontsize=15)),
  left = "Percentage of individuals, %",
  bottom = grid::textGrob("\ua9 Daniel Antal, music.dataobservatory.eu, 2021. Percentage of individuals who use internet storage space to save documents, pictures, music, video or other files.",
                          gp=gpar(fontsize=9), hjust=0.6)
)


ggsave( "data-raw/cloud_storage_plot.png",
        plot = g, units = "cm", width = 30, height = 20, dpi = 300)

write.csv ( as_tibble ( cloud_storage), "data-raw/cloud_storage.csv", row.names=F)
write.csv ( as_tibble ( ind_cloud_storage_files$codebook[[1]]), "data-raw/cloud_storage_codebook.csv", row.names=FALSE)

student_cloud_storage_files <- dataset_eurostat(dat = student_cloud_storage_files,
                                            dataset_code = "student_cloud_storage_files",
                                            eurostat_id = tolower("isoc_cicci_use"),
                                            description = "Percentage of students who use internet storage space to save documents, pictures, music, video or other files",
                                            Subject = "Music industry",
                                            Contributor= "Vitos, Botond",
                                            keywords = c("dmo", "Music industry", "Demand (Economic theory)", "Internet users", "Students"),
                                            Title = "Students Who Use Cloud Storage")


consumption_recording_media_pps_hh <- dataset_eurostat(dat = consumption %>%
                                                  filter ( .data$coicop == "CP0914") ,
                                                dataset_code = "consumption_recording_media_pps_hh",
                                                eurostat_id = tolower("HBS_EXP_T121"),
                                                description = "Mean consumption expenditure per household on recording media",
                                                Subject = "Music industry",
                                                Contributor= "Vitos, Botond",
                                                keywords = c("dmo", "Music industry", "Demand (Economic theory)", "Computer storage devices"),
                                                Title = "Expenditure on Recording Media")


consumption_music_instruments_pps_hh <- dataset_eurostat(dat = consumption %>%
                                                         filter ( .data$coicop == "CP0922") ,
                                                       dataset_code = "consumption_music_instruments_pps_hh",
                                                       eurostat_id = tolower("HBS_EXP_T121"),
                                                       description = "Mean consumption expenditure per household on recording media",
                                                       Subject = "Music industry",
                                                       Contributor= "Vitos, Botond",
                                                       keywords = c("dmo", "Music industry", "Demand (Economic theory)", "Musical instruments"),
                                                       Title = "Expenditure on Music Instruments")

unique ( consumption$unit)

snakecase::to_title_case("Individuals Who Use Cloud Storage")


write.csv(as_tibble(never_used_internet$dataset[[1]]),
          "data-raw/ind_never_use_internet.csv", row.names = F)

write.csv(as_tibble(never_used_internet$codebook[[1]]) %>%
            select ( -.data$RelatedItem),
  'data-raw/ind_never_use_internet_codebook.csv', row.names=F)


music_observatory_datasets <- as_tibble(never_used_internet$dataset[[1]]) %>%
  bind_rows ( as_tibble(daily_internet_users$dataset[[1]])) %>%
  bind_rows( as_tibble(consumption_recording_media_pps_hh$dataset[[1]])) %>%
  bind_rows( as_tibble(consumption_music_instruments_pps_hh$dataset[[1]])) %>%
  bind_rows( as_tibble(student_cloud_storage_files$dataset[[1]])) %>%
  bind_rows( as_tibble(ind_cloud_storage_files$dataset[[1]]))


never_used_internet_datacite <- never_used_internet$datacite[[1]]

never_used_internet_datacite$RelatedIdentifier <- as.character(add_identifiers("ind_never_use_internet", dataset_code = "ind_never_use_internet", DOI = "10.5281/zenodo.5121507"))

jsonlite::validate(add_identifiers("ind_never_use_internet", dataset_code = "ind_never_use_internet", DOI = "10.5281/zenodo.5121507"))

music_observatory_datacite <- as_tibble(never_used_internet_datacite ) %>%
  bind_rows ( as_tibble(daily_internet_users$datacite[[1]])) %>%
  bind_rows( as_tibble(consumption_recording_media_pps_hh$datacite[[1]])) %>%
  bind_rows( as_tibble(consumption_music_instruments_pps_hh$datacite[[1]])) %>%
  bind_rows( as_tibble(student_cloud_storage_files$datacite[[1]])) %>%
  bind_rows( as_tibble(ind_cloud_storage_files$datacite[[1]]))


music_observatory_codebook <- as_tibble(never_used_internet$codebook[[1]]) %>%
  bind_rows ( as_tibble(daily_internet_users$codebook[[1]])) %>%
  bind_rows( as_tibble(consumption_recording_media_pps_hh$codebook[[1]])) %>%
  bind_rows( as_tibble(consumption_music_instruments_pps_hh$codebook[[1]])) %>%
  bind_rows( as_tibble(student_cloud_storage_files$codebook[[1]])) %>%
  bind_rows( as_tibble(ind_cloud_storage_files$codebook[[1]])) %>%
  filter ( complete.cases(.))



library(DBI)

path <- tempdir()
con <- dbConnect(RSQLite::SQLite())


DBI::dbWriteTable(con, "data",
                  as_tibble(music_observatory_datasets),
                  overwrite = TRUE,
                  row.names  = FALSE)

DBI::dbWriteTable(con, "metadata",
                  as_tibble(music_observatory_datacite),
                  overwrite = TRUE,
                  row.names  = FALSE)

DBI::dbWriteTable(con, "codebook",
                  as_tibble(music_observatory_codebook),
                  overwrite = TRUE,
                  row.names  = FALSE)


dir("C:/_py/datasette-install/db")

disc_con <- dbConnect(RSQLite::SQLite(), 'C:/_py/datasette-install/db/dmo.db' )
RSQLite::sqliteCopyDatabase(from = con, to = disc_con)
DBI::dbListTables(con)
DBI::dbListTables(disc_con)

my_data     <- DBI::dbReadTable(disc_con, "data")
my_metadata <- DBI::dbReadTable(disc_con, "metadata")
my_codebook <- DBI::dbReadTable(disc_con, "codebook")

DBI::dbDisconnect(con)
DBI::dbDisconnect(disc_con)

unique(my_metadata$Rights)

ind_cloud_storage_files$dataset[[1]]
