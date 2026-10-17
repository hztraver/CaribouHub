library(sf)

# 1. Inspect what layers/feature classes are inside the GDB
gdb_path <- "c:/Data/FMCH_data_2018_2026.gdb"
st_layers(gdb_path)

# 2. Read the caribou locations layer (replace 'layer_name' with the actual name from st_layers)
caribou_locs <- st_read(gdb_path, layer = "FMCH_data_20180101_20251231_all")

# 3. Quick check of the data structure
head(caribou_locs)
wplot(st_geometry(caribou_locs))
