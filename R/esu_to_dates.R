sample_data = read.csv(file = "~/labmanager/R/CopyOfdata/sample_data.csv",
                      stringsAsFactors= FALSE)
sample_esu = read.csv(file = "~/labmanager/R/CopyOfdata/sample_esu.csv",
                      stringsAsFactors = FALSE)
station_data = read.csv(file = "~/labmanager/R/CopyOfdata/station_data.csv",
                        stringsAsFactors = FALSE)
dates_for_esu <- function(phylum, esu_number){
  sample_esu_row = sample_esu[sample_esu$phylum == phylum & sample_esu$group_esu == esu_number,]
  voucher_num = sample_esu_row$voucher_number
  station_nums = c()
  for (num in 1:length(voucher_num)){
    sample_data_row = sample_data[sample_data[,"voucher_number"] == voucher_num[num],]
    station_nums[num] = sample_data_row$station_number
  }
  dates = c()
  for (i in 1:length(station_nums)) {
    station_data_row = station_data[station_data[,"station_number"] == station_nums[i],]
    year = station_data_row$year
    month = station_data_row$month
    day = station_data_row$day
    dates[i] = paste(year,month,day, sep = "-")
  }
  dates
}