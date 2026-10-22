# Decisions, cleaning and the report. Plain R: no jamovi needed to test.

ISSUE_LABELS <- c(
    empty    = "No data at all",
    constant = "Same value in every row",
    name     = "Awkward variable name",
    encoding = "Garbled characters (text encoding)",
    natext   = "Missing values written as text",
    space    = "Extra spaces",
    case     = "Same label, different capitals or spaces",
    numtext  = "Numbers stored as text",
    sentinel = "Missing-value code (e.g. -99)",
    extreme  = "Extreme value",
    range    = "Outside your valid range",
    rare     = "Rare value (possible typo)",
    datemix  = "Mixed date formats",
    baddate  = "Impossible date",
    imbal    = "Almost all one value",
    assoc    = "Near-duplicate of another variable",
    pattern  = "Missing together",
    emptyrow = "Empty row",
    duprow   = "Duplicate rows",
    dupid    = "Duplicate IDs",
    tick     = "Tick-box answer (select all that apply)",
    admin    = "Survey admin column",
    private  = "Personal data: survey admin column",
    qheader  = "Extra header rows (survey export)",
    manyblank = "Mostly blank: follow-up question?",
    open     = "Open text answers",
    scale    = "Rating scale: will be put in order")

# ---------------------------------------------------------------- step 2 inputs

RANGE_LABEL <- "Valid range (e.g. 1-5)"
BLANK_LABEL <- "Blank cells mean"

normKey <- function(x) {
    k <- tolower(trimws(gsub("\\s+", " ", fixEncoding(x))))
    k[!is.na(x) & x == "(spaces only)"] <- ""
    k
}

parseRange <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    m <- regmatches(txt, regexec("^\\s*(-?[0-9.]+)\\s*(?:-|to|–)\\s*(-?[0-9.]+)\\s*$", txt, perl = TRUE))[[1]]
    if (length(m) != 3) return(NA)
    r <- as.numeric(m[2:3])
    if (anyNA(r)) return(NA)
    sort(r)
}

# rows from the Step 2 list -> one rule per variable
buildRules <- function(fixes, vars) {
    rules <- list()
    problems <- character()
    for (v in vars) rules[[v]] <- list(to = character(), sug = character(), range = NULL)
    for (row in fixes) {
        v <- row$var
        if (is.null(v) || !(v %in% vars) || is.null(row$value)) next
        to <- if (is.null(row$to)) "" else trimws(row$to)
        if (identical(row$value, RANGE_LABEL)) {
            r <- parseRange(to)
            if (identical(r, NA)) problems <- c(problems, paste0(v, ": write the valid range like 1-5 or 16 to 100"))
            else rules[[v]]$range <- r
            next
        }
        if (identical(row$value, BLANK_LABEL)) {
            rules[[v]]$blank <- if (!nzchar(to) || tolower(to) %in% c("missing", "na")) "missing"
                                else if (grepl("^0$|not ticked", to, ignore.case = TRUE)) "0" else to
            next
        }
        k <- normKey(row$value)
        rules[[v]]$to[k] <- to
        rules[[v]]$sug[k] <- if (is.null(row$sug)) "" else row$sug
    }
    list(rules = rules, problems = problems)
}

emptyRule <- function() list(to = character(), sug = character(), range = NULL)

# ---------------------------------------------------------------- cleaning

cleanVar <- function(p, rule) {
    if (is.null(rule)) rule <- emptyRule()
    if (!is.null(p$tick)) return(cleanTick(p, rule))
    n <- length(p$s)
    T <- function(l) !is.na(l) & l
    how <- rep("", n)
    val <- p$canon

    # automatic
    val[p$naText] <- NA
    how[p$naText] <- "Automatic: missing code"
    how[T(p$valid & p$enc)] <- "Automatic: garbled characters repaired"
    how[T(p$valid & how == "" & p$t != p$s)] <- "Automatic: extra spaces removed"
    how[T(p$valid & val != p$t)] <- "Automatic: capitals/spaces matched"

    # Step 2 rows
    key <- normKey(p$s)
    undecided <- rep(FALSE, n)
    mapped <- rep(FALSE, n)
    for (i in seq_along(rule$to)) {
        k <- names(rule$to)[i]
        to <- unname(rule$to[i])
        sug <- unname(rule$sug[i])
        hit <- !p$trueBlank & !is.na(key) & key == k
        if (!any(hit)) next
        if (to == "?") {
            val[hit] <- NA; how[hit] <- "Not decided yet"; undecided[hit] <- TRUE
        } else if (tolower(to) %in% c("missing", "na", "")) {
            if (!all(p$naText[hit])) how[hit] <- "Set to missing (Step 2)"
            val[hit] <- NA
        } else {
            same <- hit & !is.na(val) & val == to
            change <- hit & !same
            val[change] <- to
            how[change] <- if (identical(to, sug)) "Suggested fix" else "Your change (Step 2)"
        }
        mapped <- mapped | hit
    }

    if (p$numericLike) {
        num <- parseNum(val)
        tidied <- T(p$valid & how == "" & !is.na(num) & p$t != asText(num))
        how[tidied] <- "Automatic: number tidied"
        sent <- p$sentinel & !mapped
        num[sent] <- NA
        how[sent] <- "Automatic: missing code"
        notNum <- T(p$valid & is.na(num) & !sent & !undecided & !is.na(val))
        how[notNum] <- "Not a number → missing"
        if (!is.null(rule$range)) {
            rnum <- ifelse(is.na(num), p$num, num)
            out <- T(!sent & !undecided & !(mapped & is.na(val)) & (rnum < rule$range[1] | rnum > rule$range[2]))
            num[out] <- NA
            how[out] <- "Outside your valid range → missing"
        }
        b <- blankFill(p, rule)
        if (!is.null(b)) {
            bn <- suppressWarnings(as.numeric(b$value))
            if (!is.na(bn)) { num[b$rows] <- bn; how[b$rows] <- b$how }
        }
        return(list(values = num, measureType = "continuous", how = how, undecided = undecided))
    }
    b <- blankFill(p, rule)
    if (!is.null(b)) { val[b$rows] <- b$value; how[b$rows] <- b$how }
    ord <- scaleOrder(val)
    if (!is.null(ord))
        return(list(values = val, measureType = "ordinal", levels = ord, how = how, undecided = undecided))
    list(values = val, measureType = "nominal", how = how, undecided = undecided)
}

# "Blank cells mean": blanks become a category (e.g. Not Asian) instead of missing
blankFill <- function(p, rule) {
    b <- rule$blank
    if (is.null(b) || b == "missing") return(NULL)
    if (b == "0" && is.null(p$tick)) b <- "0"
    rows <- p$trueBlank | p$wsOnly
    if (!any(rows)) return(NULL)
    list(rows = rows, value = b, how = paste0("Blank \u2192 \u201c", b, "\u201d (your setting)"))
}

cleanTick <- function(p, rule) {
    blankZero <- !identical(rule$blank, "missing")
    blankCell <- p$trueBlank | p$wsOnly
    val <- rep(NA_real_, length(p$s))
    how <- rep("", length(p$s))
    val[p$valid] <- 1
    how[p$valid] <- "Ticked \u2192 1"
    if (blankZero) {
        val[blankCell] <- 0
        how[blankCell] <- "Blank = not ticked \u2192 0"
        skip <- blankCell & p$tick$skipped
        val[skip] <- NA
        how[skip] <- "Whole question left blank \u2192 missing"
    } else how[blankCell] <- "Blank \u2192 missing (your setting)"
    how[p$naText] <- "Automatic: missing code"
    list(values = val, measureType = "nominal", how = how,
         undecided = rep(FALSE, length(p$s)), tick = TRUE)
}

# one row per original value: what it becomes and why
valueMap <- function(p, cl) {
    shown <- ifelse(!is.na(p$s) & p$s != trimws(p$s), paste0("“", p$s, "”"), p$s)
    from <- ifelse(p$trueBlank, "(empty)", ifelse(p$wsOnly, "(spaces only)", shown))
    to <- ifelse(is.na(cl$values), "missing", as.character(cl$values))
    why <- ifelse(cl$how == "", "OK", cl$how)
    why[p$rare & why == "OK"] <- "Appears only once: check it in Step 2"
    to[cl$undecided] <- "? (missing until decided)"
    if (is.null(cl$tick)) {
        plain <- p$trueBlank & cl$how == ""
        to[plain] <- "missing"
        why[plain] <- "Already missing"
    }
    d <- data.frame(value = from, becomes = to, why = why, stringsAsFactors = FALSE)
    agg <- stats::aggregate(list(n = rep(1L, nrow(d))), d, length)
    if (sum(agg$why == "OK") > 15) {
        quiet <- agg$why == "OK"
        agg <- rbind(agg[!quiet, ],
                     data.frame(value = paste0("all other values (", sum(quiet), " different)"),
                                becomes = "unchanged", why = "OK", n = sum(agg$n[quiet])))
    }
    rank <- ifelse(agg$why == "Not decided yet", 0, ifelse(agg$why == "OK", 3, ifelse(agg$why == "Already missing", 4,
            ifelse(grepl("^Automatic", agg$why), 2, 1))))
    agg[order(rank, -agg$n, agg$value), c("value", "n", "becomes", "why")]
}

# ---------------------------------------------------------------- associations

assocMatrix <- function(profiles) {
    vs <- names(profiles)[vapply(profiles, function(p) p$nValid > 1 && p$distinct > 1 && !p$dateLike, TRUE)]
    k <- length(vs)
    if (k < 2) return(NULL)
    m <- matrix(NA_real_, k, k, dimnames = list(vs, vs))
    vec <- lapply(profiles[vs], function(p) if (p$numericLike) ifelse(p$sentinel, NA, p$num) else ifelse(p$valid, p$canon, NA))
    for (i in seq_len(k)) for (j in seq_len(k)) {
        if (j < i) { m[i, j] <- m[j, i]; next }
        if (i == j) { m[i, j] <- 1; next }
        a <- vec[[i]]; b <- vec[[j]]
        ok <- !is.na(a) & !is.na(b)
        if (sum(ok) < 10) next
        a <- a[ok]; b <- b[ok]
        na <- is.numeric(a); nb <- is.numeric(b)
        m[i, j] <- tryCatch({
            if (na && nb) abs(stats::cor(a, b))
            else if (!na && !nb) {
                tab <- table(a, b)
                if (min(dim(tab)) < 2) NA else {
                    chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
                    sqrt(as.numeric(chi) / (sum(tab) * (min(dim(tab)) - 1)))
                }
            } else {
                y <- if (na) a else b; g <- factor(if (na) b else a)
                if (nlevels(g) < 2) NA else sqrt(summary(stats::lm(y ~ g))$r.squared)
            }
        }, error = function(e) NA)
    }
    m
}

# ---------------------------------------------------------------- report

auditData <- function(data, vars, id = NULL, rowNums = seq_len(nrow(data)), rules = list(), assoc = FALSE) {

    issues <- data.frame(var = character(), type = character(), detail = character(),
                         rows = character(), stringsAsFactors = FALSE)
    add <- function(var, type, detail, rows = integer())
        issues[nrow(issues) + 1, ] <<- list(var, type, detail, fmtRows(rows))
    summ <- list(); blankMat <- list(); cleaned <- list()
    profiles <- lapply(stats::setNames(vars, vars), function(v) profileVar(data[[v]]))
    ticks <- tickInfo(profiles)

    for (v in vars) {
        p <- profiles[[v]]
        rule <- rules[[v]]
        if (is.null(rule)) rule <- emptyRule()
        tk <- ticks[[v]]
        if (!is.null(tk)) p$tick <- tk
        cleaned[[v]] <- cleanVar(p, rule)
        valid <- if (!is.null(tk)) !is.na(cleaned[[v]]$values)
                 else if (!is.null(rule$blank) && rule$blank != "missing") p$valid | p$trueBlank | p$wsOnly
                 else p$valid
        if (any(p$valid)) blankMat[[v]] <- !valid
        before <- nrow(issues)
        if (!is.null(tk)) {
            add(v, "tick", paste0("\u201c", tk$label, "\u201d or blank",
                if (!is.na(tk$stem)) paste0("; part of ", tk$stem, " (", length(tk$set), " columns)") else ""))
        }
        if (isAdminCol(v)) add(v, if (isPrivateCol(v)) "private" else "admin", "")

        adminCol <- isAdminCol(v)
        if (!adminCol && !grepl("^[A-Za-z][A-Za-z0-9_.]*$", v)) add(v, "name", paste0("Suggested: ", suggestName(v)))
        if (p$nValid == 0 && !adminCol) add(v, "empty", "")
        else if (p$distinct == 1 && is.null(tk) && !adminCol) add(v, "constant", fmtVals(p$t[p$valid]))
        freeText <- is.null(tk) && !adminCol && isFreeText(p, v)
        if (freeText && p$nValid > 0) add(v, "open", paste0(p$nValid, " written answers"))
        if (freeText) blankMat[[v]] <- NULL       # a blank comment box is "no comment", not a gap
        if (!is.null(cleaned[[v]]$levels))
            add(v, "scale", paste(cleaned[[v]]$levels, collapse = " < "))
        blankShare <- mean(p$trueBlank | p$wsOnly)
        if (is.null(tk) && !adminCol && !freeText && p$nValid > 0 && blankShare >= 0.3 && (is.null(rule$blank) || rule$blank == "missing"))
            add(v, "manyblank", paste0(round(100 * blankShare), "% of rows are blank"))
        if (any(p$enc)) add(v, "encoding", fmtVals(p$s[p$enc]), rowNums[p$enc])
        if (any(p$naText)) {
            vals <- ifelse(p$wsOnly[p$naText], "(spaces only)", p$s[p$naText])
            add(v, "natext", fmtVals(vals), rowNums[p$naText])
        }
        caseOdd <- valid & !is.na(p$canon) & p$canon != p$t
        sp <- valid & !is.na(p$s) & p$s != trimws(p$s) & !caseOdd
        if (any(sp) && !freeText) add(v, "space", fmtVals(p$s[sp]), rowNums[sp])
        for (c in unique(p$canon[caseOdd])) {
            idx <- valid & p$canon == c
            add(v, "case", paste0(fmtVals(unique(p$s[idx])), " → “", c, "”"), rowNums[caseOdd & idx])
        }
        strictBad <- valid & is.na(suppressWarnings(as.numeric(trimws(p$s))))
        if (p$numericLike && any(strictBad) && !p$isNumeric)
            add(v, "numtext", fmtVals(p$s[strictBad]), rowNums[strictBad])
        if (any(p$sentinel)) add(v, "sentinel", fmtVals(p$s[p$sentinel]), rowNums[p$sentinel])
        if (any(p$extreme)) add(v, "extreme", fmtVals(p$s[p$extreme]), rowNums[p$extreme])
        if (!is.null(rule$range) && p$numericLike) {
            out <- valid & !is.na(p$num) & !p$sentinel & (p$num < rule$range[1] | p$num > rule$range[2])
            if (any(out)) add(v, "range", fmtVals(p$s[out]), rowNums[out])
        }
        if (any(p$dateOdd)) add(v, "datemix", fmtVals(p$s[p$dateOdd]), rowNums[p$dateOdd])
        if (any(p$dateBad)) add(v, "baddate", fmtVals(p$s[p$dateBad]), rowNums[p$dateBad])
        if (any(p$rare) && !freeText && is.null(cleaned[[v]]$levels)) add(v, "rare", fmtVals(p$s[p$rare]), rowNums[p$rare])
        if (!p$numericLike && !p$dateLike && p$distinct > 1 && p$nValid > 0) {
            top <- max(table(p$canon[valid])) / p$nValid
            if (top >= 0.9) add(v, "imbal", paste0(round(100 * top), "% of rows have the same value"))
        }

        nChanged <- sum(cleaned[[v]]$how != "")
        summ[[v]] <- data.frame(
            var = v,
            stored = if (p$nValid == 0) "Empty" else if (!is.null(tk)) "Tick box (1/0)"
                     else if (p$numericLike) "Numbers" else if (p$dateLike) "Dates" else "Categories / text",
            valid = sum(valid),
            pmiss = paste0(format(round(100 * mean(!valid), 1), nsmall = 1), "%"),
            distinct = p$distinct,
            nissues = nrow(issues) - before,
            changed = nChanged,
            stringsAsFactors = FALSE)
    }

    am <- NULL
    if (assoc) {
        am <- assocMatrix(profiles)
        if (!is.null(am)) for (i in seq_len(nrow(am))) for (j in seq_len(ncol(am)))
            if (j > i && !is.na(am[i, j]) && am[i, j] >= 0.9)
                add(paste0(rownames(am)[i], " & ", colnames(am)[j]), "assoc",
                    paste0("Strength ", format(round(am[i, j], 2), nsmall = 2), " (1 = identical information)"))
    }

    blank <- as.data.frame(blankMat, check.names = FALSE)
    allBlank <- if (ncol(blank) > 0) rowSums(blank) == ncol(blank) else rep(FALSE, nrow(data))
    if (ncol(blank) > 1 && any(allBlank)) add("(all selected)", "emptyrow", "", rowNums[allBlank])
    if (ncol(blank) > 1) {
        partial <- !allBlank & rowSums(blank) >= 2
        if (any(partial)) {
            pat <- apply(blank[partial, , drop = FALSE], 1, function(r) paste(names(blank)[r], collapse = ", "))
            counts <- table(pat)
            for (pt in names(counts)[counts >= 3]) {
                rr <- rowNums[partial][pat == pt]
                add(pt, "pattern", paste0("Blank together in ", length(rr), " rows"), rr)
            }
        }
    }
    cols <- unique(c(id, vars))
    keyDf <- as.data.frame(lapply(data[cols], asText), stringsAsFactors = FALSE)
    dupr <- (duplicated(keyDf) | duplicated(keyDf, fromLast = TRUE)) & !allBlank
    # with few columns and no ID, different people often give identical answers: not duplicates
    dupTrusted <- !is.null(id) || length(cols) >= 5
    if (any(dupr) && dupTrusted && mean(dupr) <= 0.1)
        add("(all selected)", "duprow", "Identical across all selected variables", rowNums[dupr])
    if (!is.null(id)) {
        ids <- trimws(asText(data[[id]]))
        has <- !is.na(ids) & ids != ""
        dupi <- has & ids %in% ids[has][duplicated(ids[has])]
        if (any(dupi)) add(id, "dupid", fmtVals(ids[dupi]), rowNums[dupi])
    }

    missingCells <- sum(vapply(names(blank), function(v) sum(blank[[v]]), 0))
    overview <- list(rows = nrow(data), vars = length(vars),
                     missing = missingCells,
                     pmissing = if (length(vars)) 100 * missingCells / (nrow(data) * length(vars)) else 0,
                     duprows = if (dupTrusted && mean(dupr) <= 0.1) as.character(sum(duplicated(keyDf) & !allBlank))
                               else "not checked",
                     dupTrusted = dupTrusted && mean(dupr) <= 0.1,
                     emptyrows = sum(allBlank))

    issues$issue <- unname(ISSUE_LABELS[issues$type])
    list(summary = do.call(rbind, unname(summ)), issues = issues, overview = overview,
         profiles = profiles, cleaned = cleaned, blank = blank, assoc = am, ticks = ticks,
         rowFlags = list(empty = allBlank, dup = duplicated(keyDf) & !allBlank))
}

# note under each Step 2 table
rangeNote <- function(p, rule) {
    if (!p$numericLike || is.null(p$rangeHint)) return(NULL)
    ok <- p$valid & !is.na(p$num) & !p$sentinel
    if (!any(ok)) return(NULL)
    full <- range(p$num[ok])
    msg <- paste0("Values run from ", full[1], " to ", full[2])
    if (!identical(full, p$rangeHint)) msg <- paste0(msg, "; most are between ", p$rangeHint[1], " and ", p$rangeHint[2])
    if (is.null(rule$range))
        paste0(msg, ". Set a valid range in Step 2 to make impossible values missing.")
    else paste0(msg, ". Your valid range: ", rule$range[1], " to ", rule$range[2], ".")
}

# ---------------------------------------------------------------- text

mdEscape <- function(x) gsub("([*_~`\\\\])", "\\\\\\1", x)

INTRO <- paste0(
    "**Before you start (in the spreadsheet)**\n\n",
    "- **Look at your data.** Delete rows and columns that clearly don't belong: test responses, extra header rows from the survey tool, columns you will never use. ",
    "The alerts below flag the common ones.\n",
    "- **Rename columns** in Setup (double-click a column header) so each name says what it measures. ",
    "If you skip this, the new dataset can tidy names for you.\n\n",
    "**Step 1. Read the report.** It lists the problems in each variable.\n\n",
    "**Step 2. Decide what each value becomes.** In the Step 2 list on the left, every variable has its values listed ",
    "with a suggested replacement in the *Becomes* box. Leave it to accept, type something else to change it, ",
    "or type *missing* to drop the value. A **?** means the module can't guess: replace it with your decision. ",
    "For number variables, type a *valid range* such as 1-5. Spaces, capitals and codes like -99 are fixed automatically.\n\n",
    "**Step 3. Create the cleaned data.** Add cleaned columns to this spreadsheet, or open the cleaned data ",
    "as a new dataset you can save.")

GUIDE <- c(
    open     = "**Open text answers.** Written comments can't be cleaned value by value, so they're kept as they are. Blank usually just means *no comment*. To analyse them, code them into categories first (content analysis).",
    scale    = "**Rating scales.** Answers like *Disagree \u2013 Neutral \u2013 Agree* have a natural order, but jamovi sorts them alphabetically. The cleaned variables put them in the right order and set them to *Ordinal*. Check the order shown in the alert.",
    manyblank = "**Mostly blank.** Lots of blanks often mean a follow-up question that only some people saw, e.g. *Which Asian background?* shown only to people who chose Asian. Those blanks aren't missing answers: the person was never asked. In Step 2, type a category in *Blank cells mean*, such as *Not Asian*, and the blanks become that category. Leave it as *missing* if the blanks really are gaps.",
    qheader  = "**Extra header rows.** Some survey tools (Qualtrics, for example) add rows of question text or import codes under the column names. They aren't answers, so they're left out of every check and dropped from the new dataset. To tidy the spreadsheet itself, select those rows, right-click and choose *Delete Rows*; the Step 2 list then becomes cleaner too.",
    tick     = "**Tick-box answers.** In a *select all that apply* question, each option has its own column: the option's label if ticked, blank if not. Blank means *not ticked*, not missing, so the cleaned variable is 1 (ticked) or 0 (not ticked). Where the columns form a set, someone who left every option blank skipped the question and gets missing. Change *Blank cells mean* in Step 2 if the blanks really are gaps.",
    admin    = "**Survey admin columns.** Start and end times, progress, duration and similar columns describe the survey, not the answers. You rarely need them in an analysis; they're left out of the new dataset unless you untick *Leave out survey admin columns*. *Finished* and *Progress* are useful for spotting incomplete responses first.",
    private  = "**Personal data.** IP addresses, locations and names or emails can identify people. Remove them before sharing or analysing the data, and check your ethics approval.",
    name     = "**Awkward variable name.** Spaces and symbols break in other software. The cleaned variables use tidy names.",
    encoding = "**Garbled characters.** Symbols like Â£ mean the file was saved in one text encoding and opened in another, often after Excel. Re-export as *CSV UTF-8* if you can. The common cases are repaired automatically.",
    natext   = "**Missing values written as text.** jamovi treats *N/A* or *missing* as a real answer. They become proper missing values automatically.",
    space    = "**Extra spaces.** A stray space makes “London” and “London ” different values. Removed automatically.",
    case     = "**Capitals and spaces.** Variants are matched to the most common spelling automatically. Check the Step 2 table to make sure each match is right.",
    numtext  = "**Numbers stored as text.** One entry like *twenty* turns the whole column into text. Symbols and commas are tidied automatically; other entries get a suggested fix where possible.",
    sentinel = "**Missing-value codes.** Values like -99 or 999 usually mean *no answer*, so they become missing automatically. Check the codebook to make sure.",
    extreme  = "**Extreme value.** Check whether it's possible. Set a valid range to make impossible values missing. Don't drop a value just because it's large.",
    range    = "**Outside your range.** These become missing in the cleaned variable.",
    rare     = "**Rare value.** A value seen once is often a typo (*Femal*), an abbreviation (*F*) or an out-of-range score (6 on a 1–5 scale). Some are genuine, such as *Prefer not to say*.",
    datemix  = "**Mixed date formats.** Slash dates are read day/month (UK) unless the month would be over 12, so check the suggested conversions.",
    baddate  = "**Impossible date.** A date like 30 February is a typing error. Check the source.",
    imbal    = "**Almost all one value.** The variable barely varies, so it will tell you little in an analysis. Check whether that's real or an import problem.",
    assoc    = "**Near-duplicate variables.** Two variables carrying almost the same information, e.g. a detailed and a grouped version. Usually you only need one in an analysis.",
    pattern  = "**Blank together.** When the same questions are blank for several people, they probably stopped partway through. Decide whether to keep those cases.",
    empty    = "**Empty variable.** Check the import, then delete the column (right-click → Delete).",
    constant = "**Same value in every row.** It can't vary, so it can't be analysed.",
    emptyrow = "**Empty row.** Delete it in the spreadsheet (select the row, right-click → Delete).",
    duprow   = "**Duplicate rows.** Usually a record entered or exported twice. Check, then delete the extra copies in the spreadsheet.",
    dupid    = "**Duplicate IDs.** Each case needs its own ID. Check whether it's the same person twice or a typing error.")

guideText <- function(issues) {
    if (nrow(issues) == 0) return("No problems found. That doesn't prove the data are right: check values against the codebook too.")
    paste(GUIDE[intersect(names(GUIDE), unique(issues$type))], collapse = "\n\n")
}

STEP3_TEXT <- paste0(
    "The cleaned variables are linked to this analysis: change a setting and they update. ",
    "Delete the analysis and they disappear.\n\n",
    "To keep them as ordinary data: select the cleaned columns, copy, and paste them into new columns. ",
    "Then you can delete this analysis and the original columns. Keep the raw file untouched and save your work as a new file.")
