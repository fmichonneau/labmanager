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
generate_thumbnails <- function(archive_folder = "/home/francois/hdd/plankton-images/archive_photos",
                                app_folder = "/home/francois/hdd/plankton-images/app_photos",
                                voucher_pattern = "FLPK-[0-9]{4}") {

    ## Get all the directories in the archives
    archive_dirs <- list.files(pattern = voucher_pattern,
                               path = archive_folder,
                               full.names = TRUE)
    archive_dirs <- archive_dirs[file.info(archive_dirs)$isdir]

    ## Get all the directories in the app folder
    app_dirs <- list.files(pattern = voucher_pattern,
                           path = app_folder,
                           full.names = TRUE)
    app_dirs <- app_dirs[file.info(app_dirs)$isdir]

    ## Folders to recreate
    voucher_to_create <- setdiff(basename(archive_dirs), basename(app_dirs))
    path_to_create <- file.path(app_folder, voucher_to_create)

    ## Call ImageMagick
    for (i in seq_along(voucher_to_create)) {
        large_path <- file.path(path_to_create[i], "large")
        thumb_path <- file.path(path_to_create[i], "thumbs")
        orig_path <- file.path(archive_folder, voucher_to_create[i])
        if (!file.exists(orig_path)) stop(sQuote(orig_path), " doesn't exist.")

        ## thumbnails
        message("Generating thumbnails for ", voucher_to_create[i], "...", appendLF = FALSE)
        dir.create(thumb_path, recursive = TRUE)
        system(paste("cd", orig_path, ";",
                     "mogrify -path", thumb_path,
                     "-define jgeg:size=800x600 -auto-orient -thumbnail '300x200>' -unsharp 0x.5 '*.JPG'")
               )
        check_photo_creation(orig_path, thumb_path)

        ## large
        message("Generating large thumbnail for ", voucher_to_create[i], "...", appendLF = FALSE)
        dir.create(large_path, recursive = TRUE)
        system(paste("cd ", orig_path, ";",
                     "mogrify -path", large_path,
                     "-resize '1600x1200>' '*.JPG'"))
        check_photo_creation(orig_path, large_path)
    }
}

check_photo_creation <- function(original_path, target_path) {
    if (!identical(basename(list.files(path = original_path, pattern = "JPG$")),
                   basename(list.files(path = target_path, pattern = "JPG$")))) {
        stop("something went wrong...")
    } else {
        message("DONE.")
    }
}
