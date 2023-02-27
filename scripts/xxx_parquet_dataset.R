#CREATE A PARQUET GIVEN THAT TRACKING DATAFRAME IS HUGE
dir.create("data/converted_parquet")
dataframe <- read.csv("data/wrdataset2.csv")
testy <- arrow_table(dataframe)
arrow::write_parquet(testy, "data/converted_parquet/output_file.parquet")
