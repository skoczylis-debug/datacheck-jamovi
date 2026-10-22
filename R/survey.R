# Survey exports (Qualtrics, Microsoft Forms, Google Forms, JISC Online Surveys...):
# extra header rows, admin columns and "select all that apply" (tick-box) columns.

# Rows at the top holding question text and {"ImportId": ...} codes rather than answers.
# Returns list(rows = integer row positions, desc = named question text per column).
detectHeaderRows <- function(data, cols) {
    cols <- intersect(cols, names(data))
    n <- nrow(data)
    none <- list(rows = integer(), desc = character())
    if (n < 6 || length(cols) == 0) return(none)
    txt <- lapply(cols, function(v) trimws(asText(data[[v]])))
    names(txt) <- cols
    filled <- function(x) !is.na(x) & nzchar(x)
    isNum <- function(x) !is.na(suppressWarnings(as.numeric(x)))

    # 1. import-code rows, e.g. {"ImportId":"QID12"} (Qualtrics)
    headerEnd <- 0
    for (r in seq_len(min(3, n))) {
        cells <- vapply(txt, `[`, "", r)
        cells <- cells[filled(cells)]
        if (length(cells) > 0 && mean(startsWith(cells, "{\"ImportId\"")) >= 0.5) headerEnd <- r
    }

    # 2. rows of question text above the answers: text in columns that otherwise hold numbers
    numCols <- cols[vapply(txt, function(x) {
        rest <- x[-(1:3)]; rest <- rest[filled(rest)]
        length(rest) >= 3 && mean(isNum(rest)) >= 0.9
    }, TRUE)]
    if (length(numCols) >= 2) {
        for (r in seq_len(min(3, n))) {
            if (r <= headerEnd) next
            cells <- vapply(txt[numCols], `[`, "", r)
            if (mean(filled(cells) & !isNum(cells)) >= 0.6 && r == headerEnd + 1) headerEnd <- r
        }
    }
    if (headerEnd == 0) return(none)

    # the first header row usually holds the full question text: keep it as descriptions
    q <- vapply(txt, `[`, "", 1)
    q <- q[filled(q) & !startsWith(q, "{")]
    list(rows = seq_len(headerEnd), desc = q)
}

# columns survey tools add about the response rather than the answers
ADMIN_KEYS <- c(
    # Qualtrics
    "startdate", "enddate", "status", "ipaddress", "progress", "duration",
    "durationinseconds", "finished", "recordeddate", "responseid",
    "recipientlastname", "recipientfirstname", "recipientemail",
    "externalreference", "externaldatareference", "locationlatitude",
    "locationlongitude", "distributionchannel", "userlanguage",
    # Microsoft Forms
    "starttime", "completiontime", "lastmodifiedtime", "email", "name",
    # Google Forms / JISC Online Surveys and others
    "timestamp", "emailaddress", "startedat", "completedat", "submittedat",
    "datesubmitted", "useragent", "browser", "ipaddr", "completiondate", "totalpoints", "quizfeedback")
PRIVATE_KEYS <- c("ipaddress", "ipaddr", "locationlatitude", "locationlongitude",
                  "recipientlastname", "recipientfirstname", "recipientemail",
                  "email", "emailaddress", "name")

adminKey <- function(v) tolower(gsub("[^A-Za-z0-9]", "", v))
# Microsoft Forms quiz columns ("Points - ...", "Feedback - ...") are admin too
isAdminCol <- function(v) adminKey(v) %in% ADMIN_KEYS | grepl("^Q_", v) | grepl("^(Points|Feedback) - ", v)

# open-ended answers: "please specify" boxes or mostly unique sentences
isFreeText <- function(p, v) {
    if (grepl("_(COMMENT|TEXT|OTHER)$", v, ignore.case = TRUE)) return(TRUE)
    if (p$numericLike || p$dateLike || p$nValid < 5) return(FALSE)
    vals <- p$t[p$valid]
    p$distinct >= 0.7 * p$nValid && mean(nchar(vals)) > 15
}

# rating scales written as words: return the levels in their natural order, or NULL
SCALE_SCORES <- c(
    "completely disagree" = 1, "strongly disagree" = 1, "totally disagree" = 1, "disagree" = 2,
    "somewhat disagree" = 2.5, "slightly disagree" = 2.5, "tend to disagree" = 2.5, "mostly disagree" = 2.5,
    "neutral" = 3, "neither agree nor disagree" = 3, "neither agree or disagree" = 3, "undecided" = 3,
    "somewhat agree" = 3.5, "slightly agree" = 3.5, "tend to agree" = 3.5, "mostly agree" = 3.5,
    "agree" = 4, "strongly agree" = 5, "completely agree" = 5, "totally agree" = 5)
SCALES <- list(
    agree = SCALE_SCORES,
    rating = c("very poor" = 1, "poor" = 2, "disappointing" = 2.5, "below average" = 2.5, "fair" = 3,
               "average" = 3, "ok" = 3, "okay" = 3, "satisfactory" = 3, "good" = 4, "very good" = 4.5,
               "above average" = 4.5, "excellent" = 5, "outstanding" = 5),
    freq = c("never" = 1, "rarely" = 2, "seldom" = 2, "occasionally" = 2.5, "sometimes" = 3,
             "often" = 4, "frequently" = 4, "very often" = 4.5, "usually" = 4.5, "always" = 5),
    likely = c("very unlikely" = 1, "unlikely" = 2, "neither likely nor unlikely" = 3, "neutral" = 3,
               "likely" = 4, "very likely" = 5),
    satisfied = c("very dissatisfied" = 1, "dissatisfied" = 2, "neither satisfied nor dissatisfied" = 3,
                  "neutral" = 3, "satisfied" = 4, "very satisfied" = 5),
    important = c("not at all important" = 1, "not important" = 1.5, "slightly important" = 2,
                  "moderately important" = 3, "important" = 3.5, "very important" = 4, "extremely important" = 5))

scaleOrder <- function(values) {
    lv <- unique(values[!is.na(values)])
    if (length(lv) < 3) return(NULL)
    key <- tolower(trimws(gsub("\\s+", " ", lv)))
    for (sc in SCALES) {
        if (all(key %in% names(sc))) return(lv[order(sc[key])])
    }
    NULL
}
isPrivateCol <- function(v) adminKey(v) %in% PRIVATE_KEYS

# stem of a multi-part question, e.g. Q12_3 -> Q12
tickStem <- function(v) {
    m <- regmatches(v, regexec("^(.+)_([0-9]+)$", v))[[1]]
    if (length(m) == 3) m[2] else NA_character_
}

# A tick-box column: one distinct answer, the rest blank.
looksTick <- function(p) {
    !p$numericLike && !p$dateLike && p$nValid > 0 && p$distinct == 1 && sum(p$trueBlank | p$wsOnly) > 0
}

# For each variable: is it a tick-box column, which set it belongs to, and which rows skipped the whole set
tickInfo <- function(profiles) {
    vars <- names(profiles)
    tick <- vapply(profiles, looksTick, TRUE)
    stems <- vapply(vars, tickStem, "")
    info <- list()
    for (v in vars) {
        if (!tick[[v]]) { info[[v]] <- NULL; next }
        st <- stems[[v]]
        set <- if (!is.na(st)) vars[!is.na(stems) & stems == st & tick] else v
        if (length(set) >= 2) {
            allBlank <- Reduce(`&`, lapply(set, function(w) profiles[[w]]$trueBlank | profiles[[w]]$wsOnly))
        } else allBlank <- rep(FALSE, length(profiles[[v]]$s))
        info[[v]] <- list(label = unique(profiles[[v]]$canon[profiles[[v]]$valid])[1],
                          set = set, stem = if (length(set) >= 2) st else NA, skipped = allBlank)
    }
    info
}
