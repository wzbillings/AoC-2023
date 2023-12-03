###
# Advent of Code 2023: Day 1 Solution
# Zane Billings
# 2023-12-02
###

# SETUP ----

# Read in the input file
input <-
	readr::read_table(
		here::here("data", 'day-01-input.txt'),
		col_names = c("text"),
		col_types = c('c')
	)

# PART 1 SOLUTION ----

# Extract the calibration values
calibration_vals <-
	input |>
	dplyr::mutate(
		# First get all of the digits out of the string
		all_digits = stringr::str_extract_all(text, "\\d"),
		# str_extract_all() gives us a list column filled with character vectors.
		# Since we know that we only care about single digits, we can concatenate
		# that vector into one string to make life a bit easier.
		all_digits = purrr::map_chr(all_digits, stringr::str_flatten),
		# Get the first and last digits
		first = stringr::str_sub(all_digits,  1L,  1L),
		last  = stringr::str_sub(all_digits, -1L, -1L),
		# Concatenate the first and last values and make it a number
		calibration = paste0(first, last) |> as.numeric()
	)

# Add together the calibration values to get the answer.
output_p1 <- sum(calibration_vals$calibration)
paste0("Part 1 answer: ", output_p1) |>
	crayon::green() |>
	rlang::inform()

# PART 2 SOLUTION ----

# Simple lookup table that we'll need for this part
wordnum <-
	tibble::tribble(
		~num, ~word,
		'1', 'one',
		'2', 'two',
		'3', 'three',
		'4', 'four',
		'5', 'five',
		'6', 'six',
		'7', 'seven',
		'8', 'eight',
		'9', 'nine'
	) |>
	# Add a column for the word backwards
	dplyr::mutate(rev_word = stringi::stri_reverse(word))

# Make a regex formula that will find a number word or a digit character
or_fwd <- paste0(c(wordnum$word, "\\d"), collapse = "|")
# And one that will find a backwards number word or a digit character
or_bwd <- paste0(c(wordnum$rev_word, "\\d"), collapse = "|")

# This function takes a string as input and replaces
# all occurrence of number words with the correct digit character.
word_to_num <- function(str) {
	out <- str
	# Couldn't think of a better way to do this than to loop
	for (i in 1:9) {
		out <- stringr::str_replace_all(out, wordnum[[i, 2]], wordnum[[i, 1]])
	}
	return(out)
}

# Extract the calibration values
fixed_calibration_vals <-
	input |>
	dplyr::mutate(
		# Extract the first forwards number word in the text
		first = stringr::str_extract(text, or_fwd),
		# Extract the last number word in the text by looking at the reverse and
		# finding the first backwards one
		last = stringr::str_extract(stringi::stri_reverse(text), or_bwd) |>
			stringi::stri_reverse(),
		# Concatenate the first and last values and make it a number
		calibration = paste0(first, last) |> word_to_num() |> as.numeric()
	)

# Add together the calibration values to get the answer.
output_p2 <- sum(fixed_calibration_vals$calibration)
paste0("Part 2 answer: ", output_p2) |>
	crayon::green() |>
	rlang::inform()

# END OF FILE ----
