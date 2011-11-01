#  Define Latex functions

#' Prints a string to Latex, dealing with special characters.
#' 
#' Translates particular items in character strings to LaTeX format, e.g., makes a^2 = a\$^2\$ for superscript within variable labels. LaTeX names of greek letters (e.g., "alpha") will have backslashes added if greek==TRUE. Math mode is inserted as needed. \code{latexTranslate} assumes that input text always has matches, e.g. [) [] (] (), and that surrounding by \$\$ is OK. 
#' @note This code is copied from Hmisc.  Hmisc used to export this function.
#' @param object A string containing question text
#' @param inn specify additional input strings over the usual defaults 
#' @param out specify additional translated strings over the usual defaults 
#' @param pb If pb=TRUE, latexTranslate also translates [()] to math mode using \\left, \\right.
#' @param greek set to TRUE to have latexTranslate put names for greek letters in math mode and add backslashes
#' @param ... ignored
#' @export 
latexTranslate <- function (object, inn = NULL, out = NULL, pb = FALSE, 
    greek = FALSE, ...) {
	text <- object
	inn <- c("|", "%", "#", "<=", "<", ">=", ">", "_", "\\243", 
			inn, if (pb) c("[", "(", "]", ")"))
	out <- c("$|$", "\\%", "\\#", "$\\leq$", "$<$", "$\\geq$", 
			"$>$", "\\_", "\\pounds", out, if (pb) c("$\\left[", 
						"$\\left(", "\\right]$", "\\right)$"))
	text <- sedit(text, "$", "DOLLARS", wild.literal = TRUE)
	text <- sedit(text, inn, out)
	dig <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
	for (i in 1:length(text)) {
		lt <- nchar(text[i])
		x <- substring(text[i], 1:lt, 1:lt)
		j <- x == "^"
		if (any(j)) {
			is <- ((1:lt)[j])[1]
			remain <- x[-(1:is)]
			k <- remain %in% c(" ", ",", ")", "]", "\\", "$")
			if (remain[1] %in% dig || (length(remain) > 1 && 
						remain[1] == "-" && remain[2] %in% dig)) 
				k[-1] <- k[-1] | remain[-1] %nin% dig
			ie <- if (any(k)) 
						is + ((1:length(remain))[k])[1]
					else length(x) + 1
			dol <- if (sum(x[1:is] == "$")%%2) 
						""
					else "$"
			substring2(text[i], is, ie - 1) <- paste(dol, "^{", 
					substring(text[i], is + 1, ie - 1), "}", dol, 
					sep = "")
		}
		if (greek) {
			gl <- c("alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", 
					"zeta", "eta", "theta", "vartheta", "iota", "kappa", "lambda", "
					mu", "nu", "xi", "pi", "varpi", "rho", "varrho", "sigma", "varsigma", "
					tau", "upsilon", "phi", "carphi", "chi", "psi", "omega", "Gamma", "
					Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "
					Phi", "Psi", "Omega")
			for (w in gl) text[i] <- gsub(paste("\\b", w, "\\b", 
								sep = ""), paste("$\\\\", w, "$", sep = ""), 
						text[i])
		}
	}
	sedit(text, "DOLLARS", "\\$", wild.literal = TRUE)
}


