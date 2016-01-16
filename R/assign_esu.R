assign_esu <- function(path = "~/Documents/plankton-larvae-data/seqs") {
    alg_dir <- tempdir()

    merg  <- chopper::mergeSeq(list.files(file.path(path, "COI"))[1:10],
                               output = alg_dir, markers = "COI", seqFolder = path,
                               convertEnds = FALSE, checkAmbiguity = FALSE
                               )

    ape_alg <- ape::read.dna(file = attr(merg, "aligned_file")[1], format = "fasta",
                             as.matrix = TRUE)
    tr <- ape::nj(ape::dist.dna(ape_alg, model = "raw"))

    tr <- root(tr, 1, resolve.root = TRUE)
    tr <- as(tr, "phylo4")
    grp <- findGroups(tr)
    grp_data <- tdata(grp, "tip")

    grp_lst <- split(rownames(grp_data), grp_data$Groups)
    sapply(grp_lst, function(x) {
        phy <- get_phylum(x)
        if (length(unique(phy)) > 1) {
            stop("something is wrong, members of a same group should be of the same phylum. ",
                 "Check ", sQuote(paste(x, collapse = ", ")))
        }
        paste(unique(phy), get_esu(x, unique(phy)))
    })
}


get_phylum <- function(ids) {
    res <- get_lab("sample_data") %>%
        filter(voucher_number %in% ids) %>%
        select(phylum) %>%
        .[, 1]
    if (length(res) < 1)
        stop("missing phylum for ", sQuote(paste(ids, collapse = ", ")))
}

get_esu <- function(ids, phylum) {
    tt <- get_lab("sample_esu") %>%
        filter(voucher_number %in% ids) %>%
        select(group_esu) %>%
        .[, 1]

    if (length(tt) > 0) {
        esu_id <- unique(tt)
        if (length(esu_id) > 1) stop("Already assigned ESU with length greater than 1")
    }
    else {
        esu_id <- get_lab("sample_esu") %>%
            filter(phylum == phylum)  %>%
            .$group_esu %>%
            max
        if (is.na(esu_id))
            esu_id <- 1
        else
            esu_id <- esu_id + 1
    }
    esu_id
}
