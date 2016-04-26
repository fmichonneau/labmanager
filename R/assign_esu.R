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
    alg_dir <- tempdir()

    merg  <- chopper::mergeSeq(list.files(file.path(path, "COI")),
                               output = alg_dir, markers = "COI", seqFolder = path,
                               convertEnds = FALSE, checkAmbiguity = FALSE
                               )

    ape_alg <- ape::read.dna(file = attr(merg, "aligned_file")[1], format = "fasta",
                             as.matrix = TRUE)
    tr <- ape::nj(ape::dist.dna(ape_alg, model = "raw"))

    tr <- ape::root(tr, 1, resolve.root = TRUE)
    tr$edge.length[tr$edge.length < 0] <- 0

    tr <- as(tr, "phylo4")
    grp <- findGroups(tr, ...)
    grp_data <- tdata(grp, "tip")

    grp_lst <- split(rownames(grp_data), grp_data$Groups)

    esu_lst <- lapply(grp_lst, function(x) {
        phy <- unique(get_phylum(x))
        if (length(phy) > 1) {
            stop("Something is wrong, members of a same group should be of the same phylum.\n",
                 "Check: ", sQuote(paste(x, collapse = ", ")), "\n",
                 "Phyla: ", sQuote(paste(get_phylum(x), collapse = ", "))
                 )
        }
        cbind(voucher_number = x,
              phylum = rep(phy, length(x)),
              group_esu = rep(get_esu(x, phy), length(x))
              )
    })

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
    res <- get_lab("sample_data") %>%
        dplyr::filter(voucher_number %in% ids) %>%
        dplyr::select(phylum) %>%
        .[, 1]
    if (length(res) < 1)
        stop("missing phylum for ", sQuote(paste(ids, collapse = ", ")))
    else
        res
}

get_esu <- function(ids, phylum) {
    tt <- get_lab("sample_esu") %>%
        dplyr::filter(voucher_number %in% ids) %>%
        dplyr::select(group_esu) %>%
        .[, 1]

    if (length(tt) > 0) {
        esu_id <- unique(tt)
        if (length(esu_id) > 1) stop("Already assigned ESU with length greater than 1")
    } else {
        smpl_esu <- get_lab("sample_esu")
        esu_id <- smpl_esu[smpl_esu[["phylum"]] == phylum, "group_esu"]

        if (length(esu_id) == 0) {
            esu_id <- 1
        } else {
            esu_id <- max(esu_id) + 1
        }

        uu <- data.frame(
            voucher_number = ids,
            phylum = rep(phylum, length(ids)),
            group_esu = rep(esu_id, length(ids))
        )
        write.table(uu, file = "~/Documents/plankton-larvae-data/sample_esu.csv",
                    sep = ",", dec = ".", qmethod = "double", row.names = FALSE,
                    col.names = FALSE, append = TRUE)
    }
    esu_id
}
