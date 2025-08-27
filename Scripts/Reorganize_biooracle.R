#file for reorganizing biooracle data in Biooraclev3 file
stop("do not run this unless you know what you are doing!!!!!!")
library(biooracle)
library(stars)
library(dplyr)

data_path = biooracle_path()
e_files = list.files(data_path, recursive = TRUE, pattern = glob2rx("*.tif"), full.names = TRUE)
db = decompose_filename(e_files)
out_path = biooracle_path("NES") |> make_path()
o_files = compose_filename(db, out_path)
for(i in seq_along(e_files)){
  o_path = make_path(dirname(o_files[i]))
  file.rename(e_files[i], o_files[i])
}
write_database(db, out_path)
