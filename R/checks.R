# Pure checking and cleaning logic, kept free of jamovi so it can be tested with plain R.

NA_STRINGS <- c("na", "n/a", "n.a.", "nan", "null", "none", "missing",
                "unknown", ".", "-", "--", "?", "#n/a", "not applicable",
                "no answer", "refused", "dk", "don't know", "dont know")

NEG_CODES <- c(-9, -99, -999, -9999, -8, -98, -998, -7, -97, -77, -88)

SENTINELS <- c(-9, -99, -999, -9999, -8, -98, -998,
               9, 99, 999, 9999, 98, 998, 88, 888)

NUMBER_WORDS <- c(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
                  seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11, twelve = 12,
                  thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16,
                  seventeen = 17, eighteen = 18, nineteen = 19, twenty = 20,
                  thirty = 30, forty = 40, fifty = 50, sixty = 60, seventy = 70,
                  eighty = 80, ninety = 90, hundred = 100)

MONTHS <- c(january = 1, february = 2, march = 3, april = 4, may = 5, june = 6,
            july = 7, august = 8, september = 9, october = 10, november = 11,
            december = 12, jan = 1, feb = 2, mar = 3, apr = 4, jun = 6, jul = 7,
            aug = 8, sep = 9, sept = 9, oct = 10, nov = 11, dec = 12)

ISSUE_LABELS <- c(
    empty    = "No data at all",
    constant = "Same value in every row",
    name     = "Awkward variable name",
    encoding = "Garbled characters (text encoding)",
    natext   = "Missing values written as text",
    space    = "Extra spaces",
    case     = "Same label, different spelling or capitals",
    numtext  = "Numbers stored as text",
    sentinel = "Possible missing-value code",
    extreme  = "Extreme value",
    range    = "Outside the range you set",
    rare     = "Rare value (possible typo)",
    datemix  = "Mixed date formats",
    baddate  = "Impossible date",
    pattern  = "Missing together",
    emptyrow = "Empty row",
    duprow   = "Duplicate rows",
    dupid    = "Duplicate IDs")

# ---------------------------------------------------------------- helpers

fmtRows <- function(rows, max = 8) {
    rows <- sort(unique(rows))
    if (length(rows) == 0) return("")
    out <- paste(utils::head(rows, max), collapse = ", ")
    if (length(rows) > max) out <- paste0(out, " + ", length(rows) - max, " more")
    out
}

fmtVals <- function(vals, max = 6) {
    vals <- unique(vals)
    shown <- paste0("“", utils::head(vals, max), "”", collapse = ", ")
    if (length(vals) > max) shown <- paste0(shown, " + ", length(vals) - max, " more")
    shown
}

asText <- function(x) {
    if (is.factor(x)) return(as.character(x))
    if (is.numeric(x)) return(ifelse(is.na(x), NA_character_, as.character(x)))
    as.character(x)
}

suggestName <- function(v, max = 30) {
    v <- sub("^Question(?=[A-Z])", "", v, perl = TRUE)     # Microsoft Forms quiz exports glue "Question" on
    s <- tolower(gsub("[^A-Za-z0-9]+", "_", v))
    s <- gsub("^_+|_+$", "", s)
    # whole questions used as column names (e.g. Microsoft Forms): keep the first few words
    if (nchar(s) > max) {
        words <- strsplit(s, "_")[[1]]
        keep <- words[cumsum(nchar(words) + 1) <= max + 1]
        s <- paste(if (length(keep) > 0) keep else substr(s, 1, max), collapse = "_")
    }
    if (!grepl("^[a-z]", s)) s <- paste0("v_", s)
    s
}

ENC_PATTERN <- "Â|Ã|â€"     # UTF-8 read as Latin-1: "Â£", "â€™"

fixEncoding <- function(s) {
    out <- s
    bad <- !is.na(s) & grepl(ENC_PATTERN, s)
    if (any(bad)) {
        fixed <- suppressWarnings(iconv(s[bad], from = "UTF-8", to = "latin1"))
        ok <- !is.na(fixed) & validUTF8(fixed)
        Encoding(fixed) <- "UTF-8"
        out[bad][ok] <- fixed[ok]
        still <- bad & grepl(ENC_PATTERN, out)
        out[still] <- gsub("Â", "", out[still])
    }
    out
}

parseNum <- function(t) {
    x <- gsub("[£$€Â,]", "", t)
    suppressWarnings(as.numeric(trimws(x)))
}

guessNumber <- function(v) {
    lv <- tolower(trimws(v))
    if (lv %in% names(NUMBER_WORDS)) return(as.character(NUMBER_WORDS[[lv]]))
    m <- regmatches(lv, regexec("^[£$€]?([0-9.]+)\\s*k$", lv))[[1]]
    if (length(m) == 2) return(format(as.numeric(m[2]) * 1000, scientific = FALSE, trim = TRUE))
    "?"
}

guessLevel <- function(v, common) {
    lv <- tolower(trimws(v))
    if (length(common) == 0 || !nzchar(lv)) return("?")
    lc <- tolower(common)
    initials <- vapply(strsplit(lc, "\\s+"), function(w) paste(substr(w, 1, 1), collapse = ""), "")
    hit <- common[initials == lv]
    if (length(hit) == 1) return(hit)
    hit <- common[startsWith(lc, lv)]
    if (length(hit) == 1) return(hit)
    if (nchar(lv) > 3) {
        d <- utils::adist(lv, lc)[1, ]
        if (min(d) <= 2 && sum(d == min(d)) == 1) return(common[which.min(d)])
    }
    "?"
}

# ---------------------------------------------------------------- dates

dateFormat <- function(t) {
    out <- rep(NA_character_, length(t))
    t[is.na(t)] <- ""
    out[grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", t)] <- "iso"
    out[grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", t)] <- "slash"
    out[grepl("^\\d{1,2}(st|nd|rd|th)? [A-Za-z]+ \\d{4}$", t)] <- "words"
    out
}

# c(year, month, day) or NULL; slash dates read day/month (UK) unless the month would be > 12
dateParts <- function(t, fmt) {
    if (is.na(fmt)) return(NULL)
    if (fmt == "iso") return(as.integer(strsplit(t, "-")[[1]]))
    if (fmt == "slash") {
        p <- as.integer(strsplit(t, "/")[[1]])
        if (p[2] > 12 && p[1] <= 12) return(c(p[3], p[1], p[2]))
        return(c(p[3], p[2], p[1]))
    }
    w <- strsplit(t, " ")[[1]]
    m <- MONTHS[tolower(w[2])]
    if (is.na(m)) return(NULL)
    c(as.integer(w[3]), as.integer(m), as.integer(gsub("\\D", "", w[1])))
}

validDate <- function(p) {
    if (is.null(p) || anyNA(p)) return(FALSE)
    !is.na(as.Date(sprintf("%04d-%02d-%02d", p[1], p[2], p[3]), optional = TRUE))
}

writeDate <- function(p, fmt) {
    if (fmt == "slash") sprintf("%02d/%02d/%04d", p[3], p[2], p[1])
    else sprintf("%04d-%02d-%02d", p[1], p[2], p[3])
}

# ---------------------------------------------------------------- profile
# Everything known about one variable before any decisions are applied.

profileVar <- function(x) {
    n <- length(x)
    s <- asText(x)
    trueBlank <- is.na(s) | s == ""
    t <- trimws(gsub("\\s+", " ", fixEncoding(s)))
    t[trueBlank] <- NA
    wsOnly <- !trueBlank & t == ""
    naText <- wsOnly | (!trueBlank & tolower(t) %in% NA_STRINGS)
    valid <- !trueBlank & !naText
    nValid <- sum(valid)

    num <- parseNum(t)
    numericLike <- nValid > 0 && mean(!is.na(num[valid])) >= 0.6
    # in text columns, negative codes such as -99 are missing-value codes (Qualtrics: seen, not answered)
    if (!numericLike) {
        negCode <- valid & t %in% as.character(NEG_CODES)
        if (any(negCode)) {
            naText <- naText | negCode
            valid <- !trueBlank & !naText
            nValid <- sum(valid)
        }
    }
    fmts <- dateFormat(t)
    dateLike <- !numericLike && nValid > 0 && mean(!is.na(fmts[valid])) >= 0.6

    sentinel <- rep(FALSE, n)
    numBad <- rep(FALSE, n)
    if (numericLike) {
        parses <- valid & !is.na(num)
        numBad <- valid & is.na(num)
        cand <- parses & num %in% SENTINELS
        rest <- num[parses & !cand]
        if (any(cand) && length(rest) > 0)
            sentinel <- cand & (num > max(rest) | num < min(rest))
    }

    # capitals/spacing variants merged onto the most common spelling
    canon <- t
    if (!numericLike && !dateLike && nValid > 0) {
        key <- tolower(t)
        for (k in unique(key[valid])) {
            idx <- valid & !is.na(key) & key == k
            canon[idx] <- names(sort(table(t[idx]), decreasing = TRUE))[1]
        }
    }

    # dates in a minority format, or impossible
    dateOdd <- rep(FALSE, n)
    dateBad <- rep(FALSE, n)
    dateTarget <- rep("?", n)
    if (dateLike) {
        dom <- names(sort(table(fmts[valid]), decreasing = TRUE))[1]
        for (i in which(valid)) {
            parts <- dateParts(t[i], fmts[i])
            ok <- validDate(parts)
            if (!ok) dateBad[i] <- TRUE
            else if (fmts[i] != dom) {
                dateOdd[i] <- TRUE
                dateTarget[i] <- writeDate(parts, if (dom == "slash") "slash" else "iso")
            }
        }
    }

    # values seen once in a categorical-looking variable (numeric only when scale-like)
    flagged <- naText | sentinel | numBad | dateOdd | dateBad
    distinct <- length(unique(canon[valid]))
    rare <- rep(FALSE, n)
    common <- character()
    if (!dateLike && nValid >= 20 && distinct <= (if (numericLike) 11 else 30)) {
        tab <- table(canon[valid & !flagged])
        if (length(tab) > 0 && max(tab) >= 5) {
            rareVals <- names(tab)[tab == 1]
            # numbers: a rare value is only suspicious outside the range of the common ones (6 on a 1-5 scale)
            if (numericLike && length(rareVals) > 0) {
                cn <- suppressWarnings(as.numeric(names(tab)[tab >= 3]))
                rv <- suppressWarnings(as.numeric(rareVals))
                rareVals <- if (length(cn) > 0) rareVals[!is.na(rv) & (rv < min(cn) | rv > max(cn))] else character()
            }
            if (length(rareVals) > 0 && length(rareVals) <= 0.1 * nValid)
                rare <- valid & !flagged & canon %in% rareVals
            common <- names(tab)[tab >= 3]
        }
    }

    # extreme values in continuous variables
    extreme <- rep(FALSE, n)
    ok <- valid & !is.na(num) & !flagged
    if (numericLike && length(unique(num[ok])) > 15) {
        q <- stats::quantile(num[ok], c(.25, .75), names = FALSE)
        iqr <- q[2] - q[1]
        if (iqr > 0) extreme <- ok & (num < q[1] - 3 * iqr | num > q[2] + 3 * iqr)
    }

    # suggested answer for each value needing a decision
    suggestion <- rep("?", n)
    if (numericLike) suggestion[numBad] <- vapply(t[numBad], guessNumber, "")
    if (dateLike) suggestion[dateOdd] <- dateTarget[dateOdd]
    if (!numericLike && any(rare)) suggestion[rare] <- vapply(canon[rare], guessLevel, "", common = common)

    rangeHint <- NULL
    if (numericLike && any(ok) && distinct > 1) {
        tab <- table(num[ok])
        core <- as.numeric(names(tab)[tab >= 3])
        rangeHint <- if (length(unique(num[ok])) <= 11 && length(core) > 0) range(core) else range(num[ok])
    }

    list(s = s, t = t, canon = canon, trueBlank = trueBlank, wsOnly = wsOnly,
         naText = naText, valid = valid, nValid = nValid, num = num,
         numericLike = numericLike, dateLike = dateLike, sentinel = sentinel,
         numBad = numBad, dateOdd = dateOdd, dateBad = dateBad, rare = rare,
         extreme = extreme, suggestion = suggestion, rangeHint = rangeHint,
         distinct = distinct, enc = !trueBlank & grepl(ENC_PATTERN, s),
         isNumeric = is.numeric(x))
}

