# Missing-data analysis: Little's MCAR test and what missingness is linked to.

# EM estimates of mean and covariance for multivariate-normal data with gaps
emNormal <- function(X, maxit = 500, tol = 1e-6) {
    p <- ncol(X)
    pat <- apply(!is.na(X), 1, function(r) paste(as.integer(r), collapse = ""))
    groups <- split(seq_len(nrow(X)), pat)
    mu <- colMeans(X, na.rm = TRUE)
    S <- diag(apply(X, 2, stats::var, na.rm = TRUE), p)
    n <- nrow(X)
    for (it in seq_len(maxit)) {
        T1 <- numeric(p); T2 <- matrix(0, p, p)
        for (g in groups) {
            o <- !is.na(X[g[1], ]); m <- !o
            Xg <- X[g, , drop = FALSE]
            if (all(o)) {
                T1 <- T1 + colSums(Xg); T2 <- T2 + crossprod(Xg); next
            }
            Soo <- solve(S[o, o, drop = FALSE])
            B <- S[m, o, drop = FALSE] %*% Soo
            Xh <- Xg
            dev <- sweep(Xg[, o, drop = FALSE], 2, mu[o])
            Xh[, m] <- sweep(dev %*% t(B), 2, mu[m], "+")
            C <- matrix(0, p, p)
            C[m, m] <- S[m, m, drop = FALSE] - B %*% S[o, m, drop = FALSE]
            T1 <- T1 + colSums(Xh)
            T2 <- T2 + crossprod(Xh) + length(g) * C
        }
        muN <- T1 / n
        SN <- T2 / n - tcrossprod(muN)
        done <- max(abs(muN - mu)) < tol && max(abs(SN - S)) < tol
        mu <- muN; S <- SN
        if (done) break
    }
    list(mu = mu, S = S, groups = groups)
}

littleMCAR <- function(X) {
    X <- as.matrix(X)
    X <- X[rowSums(!is.na(X)) > 0, , drop = FALSE]
    keep <- apply(X, 2, function(x) sum(!is.na(x)) > 2 && stats::var(x, na.rm = TRUE) > 0)
    X <- X[, keep, drop = FALSE]
    if (ncol(X) < 2 || !anyNA(X)) return(NULL)
    X <- scale(X)
    est <- tryCatch(emNormal(X), error = function(e) NULL)
    if (is.null(est)) return(NULL)
    d2 <- 0; df <- 0
    for (g in est$groups) {
        o <- !is.na(X[g[1], ])
        if (!any(o)) next
        ybar <- colMeans(X[g, o, drop = FALSE]) - est$mu[o]
        Si <- tryCatch(solve(est$S[o, o, drop = FALSE]), error = function(e) NULL)
        if (is.null(Si)) return(NULL)
        d2 <- d2 + length(g) * as.numeric(t(ybar) %*% Si %*% ybar)
        df <- df + sum(o)
    }
    df <- df - ncol(X)
    if (df <= 0) return(NULL)
    list(chi2 = d2, df = df, p = stats::pchisq(d2, df, lower.tail = FALSE),
         nvars = ncol(X), patterns = length(est$groups))
}

fmtP <- function(p) if (p < .001) "< .001" else sprintf("%.3f", p)

# For each variable with gaps: is being missing linked to the other variables?
missingLinks <- function(profiles, cleaned, use) {
    vars <- names(profiles)[vapply(names(profiles), function(v) profiles[[v]]$nValid > 0, TRUE)]
    out <- data.frame(var = character(), with = character(), detail = character(),
                      p = numeric(), stringsAsFactors = FALSE)
    verdict <- list()
    for (v in vars) {
        miss <- is.na(cleaned[[v]]$values)[use]
        nm <- sum(miss)
        if (nm < 5 || sum(!miss) < 5) {
            verdict[[v]] <- if (nm == 0) "No missing values" else "Too few missing values to test (fewer than 5)"
            next
        }
        links <- character()
        for (w in setdiff(vars, v)) {
            x <- cleaned[[w]]$values[use]
            ok <- !is.na(x)
            if (sum(ok & miss) < 5 || sum(ok & !miss) < 5) next
            res <- tryCatch({
                if (is.numeric(x)) {
                    a <- x[ok & miss]; b <- x[ok & !miss]
                    if (stats::var(c(a, b)) == 0) NULL else {
                        t <- stats::t.test(a, b)
                        list(p = t$p.value, detail = paste0("Mean ", signif(mean(a), 3), " when ", v,
                                 " is missing vs ", signif(mean(b), 3), " when present"))
                    }
                } else {
                    f <- factor(x[ok]); mm <- miss[ok]
                    if (nlevels(f) < 2 || nlevels(f) > 15) NULL else {
                        tab <- table(f, mm)
                        set.seed(1)
                        pv <- suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = 2000)$p.value)
                        rate <- tab[, "TRUE"] / rowSums(tab)
                        top <- names(which.max(rate))
                        list(p = pv, detail = paste0(v, " missing in ", round(100 * max(rate)), "% of “", top,
                                 "” vs ", round(100 * sum(tab[, "TRUE"]) / sum(tab)), "% overall"))
                    }
                }
            }, error = function(e) NULL)
            if (!is.null(res) && !is.na(res$p) && res$p < .05) {
                out[nrow(out) + 1, ] <- list(v, w, res$detail, res$p)
                links <- c(links, w)
            }
        }
        verdict[[v]] <- if (length(links) == 0)
            "No link found: consistent with missing completely at random (MCAR)"
        else paste0("Linked to ", paste(links, collapse = ", "), ": not MCAR; possibly MAR")
    }
    list(links = out[order(out$p), ], verdict = verdict)
}

missingReport <- function(res) {
    use <- !(rowSums(res$blank) == ncol(res$blank))        # leave out completely empty rows
    if (length(use) == 0) use <- rep(TRUE, length(res$profiles[[1]]$s))
    numVars <- names(res$cleaned)[vapply(res$cleaned, function(c) c$measureType == "continuous", TRUE)]
    X <- if (length(numVars) > 0) sapply(numVars, function(v) res$cleaned[[v]]$values[use]) else NULL
    little <- if (!is.null(X) && is.matrix(X)) littleMCAR(X) else NULL
    ml <- missingLinks(res$profiles, res$cleaned, use)
    perVar <- data.frame(
        var = names(ml$verdict),
        nmiss = vapply(names(ml$verdict), function(v) sum(is.na(res$cleaned[[v]]$values)[use]), 0),
        verdict = unlist(ml$verdict), stringsAsFactors = FALSE)
    perVar$pmiss <- paste0(format(round(100 * perVar$nmiss / sum(use), 1), nsmall = 1), "%")
    list(little = little, links = ml$links, perVar = perVar, numVars = numVars)
}

MISSING_TEXT <- paste0(
    "**Why missing values matter.** What you can do about gaps depends on why they happened.\n\n",
    "- **MCAR (missing completely at random):** gaps are unrelated to anything, like a page lost in the post. ",
    "Dropping incomplete cases loses information but doesn't bias results.\n",
    "- **MAR (missing at random):** gaps are linked to other variables you have, e.g. older people skip the income question. ",
    "Dropping cases can bias results; methods such as multiple imputation can help.\n",
    "- **MNAR (missing not at random):** gaps are linked to the missing value itself, e.g. people with high incomes don't report them. ",
    "No test can detect this from the data alone; you have to reason about how the data were collected.\n\n",
    "**Reading the tests.** Little's test checks the numeric variables together: a small p (below .05) means the gaps are probably not MCAR. ",
    "The table of links compares people with and without a missing value on every other variable. ",
    "With many comparisons, about 1 in 20 will show p < .05 by chance, so look for links that make sense.")
