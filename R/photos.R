##' Creates the folder to store the photo in the archives
##'
##' @param photo_folder path where the photos are stored.
##' @param sample_data the data frame with the sample information
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

##' Check that the photo files have uppercase JPG extension
##'
##' @param photo_folder path where the original photos are stored
##' @export
uppercase_photo_extension <- function(photo_folder = "/home/francois/hdd/plankton-images/archive_photos/") {
    lst_files <- list.files(pattern = "jpg$", path = photo_folder, recursive = TRUE, full.names = TRUE)

    if (length(lst_files) > 0) {
        new_nm <- gsub("\\.jpg$", "\\.JPG", lst_files)
        nm <- cbind(lst_files, new_nm)
        all(apply(nm, 1, function(x) file.rename(x[1], x[2])))
    }

}



##' Creates the copies of the photos to be used by the shiny app
##'
##' @param archive_folder folder where the original photos are stored
##' @param app_folder folder where the smaller versions are stored
##' @param voucher_pattern regular expression that match the voucher
##' @export
generate_thumbnails <- function(archive_folder = "/home/francois/hdd/plankton-images/archive_photos",
                                app_folder = "/home/francois/hdd/plankton-images/app_photos",
                                voucher_pattern = "FLPK-[0-9]{4}") {

    uppercase_photo_extension(archive_folder)

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


rename_photos <- function(app_folder = "/home/francois/hdd/plankton-images/app_photos") {
    ## list all images
    all_imgs <- list.files(path = app_folder,
                           pattern = "JPG$",
                           recursive = TRUE,
                           ignore.case = TRUE,
                           full.names = TRUE)

    ## Remove files that have already been renamed
    all_imgs <- all_imgs[!grepl("^(FLPK-[0-9]{4}_[0-9]{4})",
                                basename(all_imgs))]

    ## get the unique paths including suffix large/thumbs
    uniq_paths <- unique(dirname(all_imgs))
    ## check they all have correct suffix
    if (!all(grepl("(large|thumbs)$", uniq_paths)))
        stop("Something is wrong in the naming of the directories. ",
             "Some don't end in large or thumbs")
    ## because dirname() drops trailing /, calling it on the path remove
    ## the suffix large/thumbs
    voucher_paths <- unique(dirname(uniq_paths))

    new_names <- lapply(voucher_paths, function(x) {
        ## extract all images matching the voucher
        each_voucher_imgs <- grep(paste0("^", x), all_imgs, value = TRUE)
        ## make sure the thumbs and the large folder have the same file names/number of images
        if(!sum(duplicated(basename(each_voucher_imgs))) ==
           length(each_voucher_imgs)/2)
            stop("problem with ", x)
        ## create new file name:
        ##   FLPK-xxxx_YYYY_UUID.JPG
        ## voucher number, 4 sequential digit, UUID
        seq_res <- seq_along(unique(basename(each_voucher_imgs)))
        new_nm <- paste0(basename(x), "_",
                         sprintf("%04i", seq_res), "_",
                         replicate(length(seq_res), uuid::UUIDgenerate()),
                         ".JPG")
        ## create full complete name by appending path, and letting
        ## the magical rules of recycling do their job
        file.path(dirname(each_voucher_imgs), new_nm)
    })

    renm <- cbind(all_imgs, unlist(new_names))
    res <- apply(renm, 1, function(x) file.rename(x[1], x[2]))
    if (all(res)) {
        message("Renaming successful! ", sum(res), " photos were renamed.")
    } else {
        stop("Something went wrong with the renaming...")
    }
    invisible(renm)
}
