# Builds the separate cleaned dataset for the Open / Save buttons.

notReadyMessage <- function(res, vars) {
    open <- vapply(vars, function(v) sum(res$cleaned[[v]]$undecided), 0)
    if (all(open == 0)) return(NULL)
    todo <- paste0(names(open)[open > 0], " (", open[open > 0], ")", collapse = ", ")
    paste0("Not ready yet. Some values are still marked ? in Step 2: ", todo,
           ". Replace each ? with a decision, then try again.")
}

cleanedColumn <- function(orig, cl) {
    vals <- cl$values
    if (cl$measureType == "continuous") return(vals)
    if (!is.null(cl$levels)) return(factor(vals, levels = cl$levels, ordered = TRUE))
    if (is.ordered(orig) && all(stats::na.omit(vals) %in% levels(orig)))
        return(factor(vals, levels = levels(orig)[levels(orig) %in% vals], ordered = TRUE))
    factor(vals)
}

buildNewData <- function(data, vars, id, keep, res, tidy = TRUE, dropDup = TRUE, dropEmpty = TRUE,
                         dropAdmin = TRUE, desc = character()) {
    usable <- vars[res$summary$stored != "Empty"]
    keep <- setdiff(keep, c(vars, id))
    admin <- character()
    if (dropAdmin) {
        admin <- c(usable[isAdminCol(usable)], keep[isAdminCol(keep)])
        usable <- usable[!isAdminCol(usable)]
        keep <- keep[!isAdminCol(keep)]
    }
    cols <- list()
    if (!is.null(id)) cols[[id]] <- data[[id]]
    for (v in usable) cols[[v]] <- cleanedColumn(data[[v]], res$cleaned[[v]])
    for (k in keep) cols[[k]] <- data[[k]]
    df <- as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)

    # judge empty and duplicate rows on the RAW values of every column going into the new dataset
    rawCols <- unique(c(id, usable, keep))
    raw <- as.data.frame(lapply(data[rawCols], function(x) trimws(asText(x))), stringsAsFactors = FALSE)
    raw[is.na(raw)] <- ""
    # empty = nothing left in the new dataset (so blanks you turned into a category count as filled)
    filled <- as.data.frame(lapply(df, function(x) { t <- trimws(asText(x)); !is.na(t) & t != "" }))
    empty <- rowSums(filled) == 0
    dup <- duplicated(raw) & !empty
    rows <- rep(TRUE, nrow(df))
    nEmpty <- 0; nDup <- 0
    skipped <- character()
    # safety: never remove a large share of the data; that signals too few columns, not bad rows
    if (dropEmpty && any(empty)) {
        if (mean(empty) <= 0.1) { nEmpty <- sum(empty); rows <- rows & !empty }
        else skipped <- c(skipped, paste0("Empty rows weren't removed: ", sum(empty), " rows have no values in the columns you included, ",
                                          "which is too many to be stray empty rows. Check those variables in Step 2 ",
                                          "(e.g. what blank cells mean)."))
    }
    if (dropDup && any(dup)) {
        if ((!is.null(id) || length(rawCols) >= 5) && mean(dup) <= 0.1) { nDup <- sum(dup & rows); rows <- rows & !dup }
        else skipped <- c(skipped, paste0("All rows were kept. Duplicate rows weren't checked: with so few columns, ",
                                          "different people often give identical answers, so matching rows aren't real duplicates. ",
                                          "Add an ID variable to check for duplicates."))
    }
    df <- df[rows, , drop = FALSE]
    rownames(df) <- NULL
    orig <- names(df)
    for (k in orig) {
        attr(df[[k]], "jmv-desc") <- NULL
        if (!is.na(desc[k]) && nzchar(desc[k])) attr(df[[k]], "jmv-desc") <- unname(desc[k])
        else if (tidy && nchar(k) > 30) attr(df[[k]], "jmv-desc") <- trimws(k)   # long question kept as description
    }

    if (tidy) {
        newNames <- vapply(orig, function(k) {
            tk <- res$ticks[[k]]
            # tick-box columns are named after the option, e.g. q12_pakistani
            if (!is.null(tk)) suggestName(paste(k, tk$label)) else suggestName(k)
        }, "")
        names(df) <- make.unique(newNames, sep = "_")
    }

    dropped <- c(if (nEmpty > 0) paste0(nEmpty, " empty"), if (nDup > 0) paste0(nDup, " duplicate"))
    list(df = df, skipped = skipped, emptyVars = setdiff(vars[res$summary$stored == "Empty"], admin), admin = admin,
         dropped = if (length(dropped) == 0) "none" else paste(dropped, collapse = ", "))
}
