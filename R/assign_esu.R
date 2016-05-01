##' Assign/create ESUs based on the sequences
##'
##' @importFrom ape read.dna root nj dist.dna
##' @importFrom chopper mergeSeq
##' @importFrom phylobase tipLabels
##' @import phylobase
##' @export
##' @param path path indicating where the COI sequences are stored.
assign_esu <- function(path = "~/Documents/plankton-larvae-data/seqs",
                       ...) {

    lst_seq <- list.files(file.path(path, "COI"))

    tmp_file <- tempfile()

    ## to create an unique key, write all the sequences to a file and
    ## generate its md5sum...
    seqs <- lapply(lst_seq, function(x) readLines(file.path(path, "COI", x)))
    cat(paste(unlist(seqs), collapse="\n"), file=tmp_file)
    key <- tools::md5sum(tmp_file)[[1]]
    seq_store$set(key, lst_seq)
    ape_alg <- alg_store()$get(key)

    tr <- ape::nj(ape::dist.dna(ape_alg, model = "raw"))

    tr <- ape::root(tr, 1, resolve.root = TRUE)
    tr$edge.length[tr$edge.length < 0] <- 0

    tr <- as(tr, "phylo4")
    grp <- findGroups(tr, ...)
    grp_data <- tdata(grp, "tip")

    grp_lst <- split(rownames(grp_data), grp_data$Groups)

    grp_phylum <- lapply(grp_lst, function(x) get_phylum(x))

    ## Make sure that all members of each group belongs to a single
    ## phylum
    is_not_unique_phylum <- vapply(grp_phylum, function(x) {
        length(unique(x)) > 1
    }, logical(1))

    if (length(grp_phylum[is_not_unique_phylum])) {
        msg <- mapply(
            function(x, y) {
            paste(
                "  Samples: ", paste(x, collapse = ", "), "\n",
                "  Phyla: ", paste(y, collapse = ", "), "\n\n"
            )
        }, grp_lst[is_not_unique_phylum], grp_phylum[is_not_unique_phylum])
        stop("Members of the same group should be in the same phylum: \n",
             msg, call. = FALSE)
    }

    ## Assemble data frame for the results
    esu_lst <- mapply(function(x, phy) {
        cbind(voucher_number = x,
              phylum = phy,
              group_esu = rep(get_esu(x, unique(phy)), length(x))
              )
    }, grp_lst, grp_phylum)

    res <- do.call("rbind", esu_lst)
    res <- as.data.frame(res, stringsAsFactors = FALSE)

    tipLabels(tr) <- paste(tipLabels(tr),
                           apply(res[match(tipLabels(tr), res$voucher_number),
                                     c("phylum", "group_esu")], 1,
                                 function(x) paste(x[1], x[2], sep="-")),
                           sep="_")

    tr <- as(tr, "phylo")
    data_store()$set("esu_tree", tr)

    res
}


get_phylum <- function(ids) {
    res <- get_lab("sample_data")
    res <- res[res$voucher_number %in% ids, "phylum", drop = TRUE]
    if (length(res) < 1)
        stop("missing phylum for ", sQuote(paste(ids, collapse = ", ")))
    else
        res
}


get_esu <- function(ids, phylum) {
    tt <- get_lab("sample_esu") %>%
        dplyr::filter(voucher_number %in% ids)

    if (nrow(tt) > 0) {
        ## If at least some of these IDs already have an ESU, let's
        ## make sure they have the same phylum.
        curr_phy <- unique(tt[["phylum"]])

        ## There shouldn't be more than one active phylum for a set of
        ## ids in an ESU.
        if (length(curr_phy) > 1) {
            stop("Problem: there shouldn't be more than one phylum for a ",
                 "set of ids in an ESU.", call. = FALSE)
        }

        ## If the phylum isn't the same as the one listed in the database,
        ## we need to replace it, get a new ESU id, and update the database.
        if (curr_phy != phylum) {
            warning("For ", paste0(ids, collapse = ", "), ", ",
                    "phylum in the database: ", curr_phy, ", ",
                    "replaced with: ", phylum, ".", call. = FALSE)
            esu_id <- assign_new_esu_id(phylum)
            tt <- get_lab("sample_esu")
            tt[tt$"voucher_number" %in% ids, "phylum"] <- phylum
            tt[tt$"voucher_number" %in% ids, "group_esu"] <- esu_id
            write_esu_database(tt, append = FALSE)
            ids_to_add <- setdiff(ids, tt[["voucher_number"]])
            append_esu_id(ids_to_add, phylum, esu_id)
        } else {
            ## otherwise, we need to add the vouchers that are not yet
            ## in the database
            esu_id <- unique(tt[["group_esu"]])
            if (length(esu_id) > 1) stop("This should be unique.")
            ids_to_add <- setdiff(ids, tt[["voucher_number"]])
            append_esu_id(ids_to_add, phylum, esu_id)
        }

    } else {
        ## If it's brand new, just create a new ESU for the phylum
        esu_id <- assign_new_esu_id(phylum)
        append_esu_id(ids, phylum, esu_id)
    }

    esu_id
}

assign_new_esu_id <- function(phylum) {
    smpl_esu <- get_lab("sample_esu")
    esu_id <- smpl_esu[smpl_esu[["phylum"]] == phylum, "group_esu"]

    if (length(esu_id) == 0) {
        esu_id <- 1
    } else {
        esu_id <- max(esu_id) + 1
    }
    esu_id
}

append_esu_id <- function(ids, phylum, esu_id) {
    uu <- data.frame(
        voucher_number = ids,
        phylum = rep(phylum, length(ids)),
        group_esu = rep(esu_id, length(ids))
    )
    write_esu_database(uu, append = TRUE)
}


write_esu_database <- function(db, file = "~/Documents/plankton-larvae-data/sample_esu.csv", append) {
    write.table(db, file = file, sep = ",", dec = ".",
                qmethod = "double", row.names = FALSE,
                col.names = !append, append = append)
}

fetch_hook_alignment <- function(key, namespace) {
    alg_dir <- tempdir()
    merg  <- chopper::mergeSeq(seq_store$get(key),
                               output = alg_dir, markers = "COI",
                               seqFolder = "~/Documents/plankton-larvae-data/seqs",
                               convertEnds = FALSE, checkAmbiguity = FALSE
                               )
    ape_alg <- ape::read.dna(file = attr(merg, "aligned_file")[1], format = "fasta",
                             as.matrix = TRUE)
    ape_alg
}
