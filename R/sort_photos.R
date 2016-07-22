photo_list = read.csv(file = "~/labmanager/R/CopyOfdata/list_photos.csv",
                      stringsAsFactors= FALSE)
sort_photos <- function(voucher, filter){
  data_rows = photo_list[photo_list$voucher_number== voucher 
                         & photo_list$photo_quality <= filter,]
  data_rows$uuid
}

