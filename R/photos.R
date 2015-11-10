##' @export
generate_folder_structure <- function(photo_folder = "/home/francois/hdd/plankton-images/archive_photos",
                                      sample_data = get_lab("sample_data")) {
    has_photo <- sample_data[as.logical(sample_data[["has_photo"]]),
                             "voucher_number"]

    if (any(is.na(has_photo)))
        stop("Only 0 or 1 in ", sQuote("has_photo"), " column. ",
             "Please double check there is no missing values.")

    folder_exists <- dir.exists(file.path(photo_folder, has_photo))

    sapply(has_photo[!folder_exists],
           function(x) dir.create(file.path(photo_folder, x)))
}

##' @export
uppercase_photo_extension <- function(photo_folder = "/home/francois/hdd/plankton-images/archive_photos/") {
    lst_files <- list.files(pattern = "jpg$", path = photo_folder, recursive = TRUE, full.names = TRUE)
    new_nm <- gsub("\\.jpg$", "\\.JPG", lst_files)
    nm <- cbind(lst_files, new_nm)
    all(apply(nm, 1, function(x) file.rename(x[1], x[2])))
}

##' @export
generate_thumbnails <- function(photo_folder = "/home/francois/hdd/plankton-images/archive_photos",
                                regenerate = FALSE) {

    ## Get all the directories
    photo_dirs <- list.files(pattern = "^FLPK-", path = photo_folder, full.names = TRUE)
    photo_dirs <- photo_dirs[file.info(photo_dirs)$isdir]

    vouchers <- gsub(".+/(FLPK-[0-9]{4})", "\\1", photo_dirs)

    ## Call ImageMagick
    for (i in seq_along(photo_dirs)) {
        thumb_path <- file.path(photo_dirs[i], "thumbs")
        if (!regenerate && file.exists(thumb_path)) {
            if (identical(length(list.files(path = photo_dirs[i], pattern = "JPG")),
                          length(list.files(path = file.path(photo_dirs[i], "thumbs"), pattern = "JPG")))) {
                message(sQuote(thumb_path), " already exists. Skipping.")
                next
            } else {
                stop("Something is wrong with the thumbnails for ", vouchers[i])
            }
        } else {
            dir.create(thumb_path)
            message("Generating thumbnails for ", vouchers[i])
            system(paste("cd", photo_dirs[i], ";",
                         "mogrify -path thumbs -define jgeg:size=800x600 -auto-orient -thumbnail '300x200>' -unsharp 0x.5 '*.JPG'
                      "))
            if (!identical(length(list.files(path = photo_dirs[i], pattern = "JPG")),
                           length(list.files(path = file.path(photo_dirs[i], "thumbs"), pattern = "JPG")))) {
                stop("something went wrong for ", vouchers[i])
            } else {
                message("Done for ", vouchers[i])
            }
        }
    }
}
