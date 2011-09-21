#  Define Latex functions

#' Prints a string to Latex, dealing with special characters.
#' 
#' Translates particular items in character strings to LaTeX format, e.g., makes a^2 = a\$^2\$ for superscript within variable labels. LaTeX names of greek letters (e.g., "alpha") will have backslashes added if greek==TRUE. Math mode is inserted as needed. \code{latexTranslate} assumes that input text always has matches, e.g. [) [] (] (), and that surrounding by \$\$ is OK. 
#' @note This code is copied from Hmisc.  Hmisc used to export this function.
#' @param object A string containing question text
#' @param inn specify additional input strings over the usual defaults 
#' @param out specify additional translated strings over the usual defaults 
#' @param pb If pb=TRUE, latexTranslate also translates [()] to math mode using \\left, \\right.
#' @param greek set to TRUE to have latexTranslate put names for greek letters
#' in math mode and add backslashes
#' @param ... ignored 
#' @keywords Internal
latexTranslate <- function (object, inn = NULL, out = NULL, pb = FALSE, greek = FALSE, 
		...) 
{
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


#' String editing function, adapted from Hmisc.
#' 
#' @param text a vector of character strings for sedit, substring2, substring2<- or a single character string for substring.location, replace.substring.wild.  
#' @param from a vector of character strings to translate from, for sedit. A single asterisk wild card, meaning allow any sequence of characters (subject to the test function, if any) in place of the "*". An element of from may begin with "^" to force the match to begin at the beginning of text, and an element of from can end with "$" to force the match to end at the end of text.  
#' @param to a vector of character strings to translate to, for sedit. If a corresponding element in from had an "*", the element in to may also have an "*". Only single asterisks are allowed. If to is not the same length as from, the rep function is used to make it the same length.
#' @param test a function of a vector of character strings returning a logical vector whose elements are TRUE or FALSE according to whether that string element qualifies as the wild card string for sedit, replace.substring.wild  
#' @param wild.literal set to TRUE to not treat asterisks as wild cards and to not look for "^" or "$" in old  
#' @note This code is copied from Hmisc
#' @keywords Internal
sedit <- function (text, from, to, test = NULL, wild.literal = FALSE) 
{
  to <- rep(to, length = length(from))
  for (i in 1:length(text)) {
    s <- text[i]
    if (length(s)) 
      for (j in 1:length(from)) {
        old <- from[j]
        front <- back <- FALSE
        if (!wild.literal) {
          if (substring(old, 1, 1) == "^") {
            front <- TRUE
            old <- substring(old, 2)
          }
          if (substring(old, nchar(old)) == "$") {
            back <- TRUE
            old <- substring(old, 1, nchar(old) - 1)
          }
        }
        new <- to[j]
        lold <- nchar(old)
        if (lold > nchar(s)) 
          next
        ex.old <- substring(old, 1:lold, 1:lold)
        if (!wild.literal && any(ex.old == "*")) 
          s <- replace.substring.wild(s, old, new, test = test, 
              front = front, back = back)
        else {
          l.s <- nchar(s)
          is <- 1:(l.s - lold + 1)
          if (front) 
            is <- 1
          ie <- is + lold - 1
          if (back) 
            ie <- l.s
          ss <- substring(s, is, ie)
          k <- ss == old
          if (!any(k)) 
            next
          k <- is[k]
          substring2(s, k, k + lold - 1) <- new
        }
      }
    text[i] <- s
  }
  text
}


