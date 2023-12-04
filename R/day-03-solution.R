###
# Advent of Code 2023: Day 3 Solution
# Zane Billings
# 2023-12-03
###

# SETUP ----

# Load common functions
source(here::here("R", "funcs.R"))

# Read in the input file
input <-
	readr::read_lines(here::here("data", "day-03-input.txt"))

# PART 1 SOLUTION ----

# This one actually seems quite difficult, I've never had to work with text
# that has a 2D structure before. So for this one I'll probably lay off the
# idiomatic tidyverse code and just try to get something that works even if
# it's very inefficient.

# First we need to turn the input from a vector of text strings into a matrix.
data_matrix <-
	stringr::str_split(input, "") |>
	do.call(what = rbind)

# We need to get a list of non-dot non-number symbols which appear in the
# schematic.
symbols <-
	sapply(data_matrix, unique) |>
	unique() |>
	paste0(collapse = "") |>
	stringr::str_remove_all("[.\\d]") |>
	# The - will be interpreted as a negation so we need to escape it
	# The replacement is interpreted as a regex so we also have to escape
	# the backslashes or they won't show up
	stringr::str_replace(pattern = "-", replacement = "\\\\-")

# Add brackets to either side so it can be interpreted as a regex
sym_reg <- paste0("[", symbols, "]")

# Function that returns the i, j element of data_matrix if it exists, but if
# that doesn't work, returns "OOB" instead of erroring out.
try_to_get_element <- function(i, j) {
	tryCatch(
		{
			# This is the try block
			return(data_matrix[[i, j]])
		},
		# If that doesn't work, return a non-symbol character
		error = function(cond) {return("OOB")}
	)
}

# Since the numbers are always in one row we'll start there, by iterating
# through each row.
n_rows <- nrow(data_matrix)
n_cols <- ncol(data_matrix)
number_holder <- vector(mode = "list", length = n_rows)
for (i in 1:n_rows) {
	# Get the current row to check
	this_row <- data_matrix[i, ]
	# Check which elements are numbers
	is_number <- stringr::str_detect(this_row, "\\d")
	# Now we need to loop through that. For whichever ones are numbers, we need
	# to check the potentially eight things around it for a symbol.
	need_this_number <- vector(mode = "logical", length = n_cols)
	for (j in 1:n_cols) {
		if (isTRUE(is_number[[j]])) {
			# We need to check all of the eight slots around the current position to
			# see if a symbol is there. So first build a df of the indices we need to
			# check.
			# We technically don't need to check element (i, j) but it doesn't matter
			# if we do cause we know it's a number
			check_syms <-
				tidyr::expand_grid(
					i = c(i - 1, i, i + 1),
					j = c(j - 1, j, j + 1)
				) |>
				# Now we need to look at those indices and get the character that is
				# there. Of course if we are at the edge of a column or row, those might
				# not exist. So I wrote a function that returns "OOB" if that element
				# is out of the matrix bounds.
				dplyr::mutate(
					# The function I wrote for handling the edges is not vectorized
					# so we have to use a map function
					elem = purrr::map2_chr(i, j, \(i, j) try_to_get_element(i, j)),
					# Now use our symbol regex that we previously constructed in order
					# to check whether there is an engine part there.
					is_sym = stringr::str_detect(elem, sym_reg)
				)
			
			# If any of those characters were symbols, we need to return this entire
			# number
			need_this_number[[j]] <- any(check_syms$is_sym)
		}
	}
	
	# Now we know which columns we need. But we don't just need the individual
	# columns, we need all the digits up to that point. So we need to
	# get the indices for each number in the row, check if ANY of those are
	# included, and if so include the whole number.
	# So we first get the location of all numbers. This regex finds numbers by
	# looking for one or more digit characters followed by a non digit.
	numb_locs <-
		stringr::str_locate_all(
			paste0(this_row, collapse = ""),
			"[0-9]+"
		)[[1]] |>
		tibble::as_tibble() |>
		# Now we get the numbers themselves and check if they should be included.
		dplyr::mutate(
			# First get all the numbers. Yes there is a more efficient way to do this
			# but I'm not worried about that now.
			number = purrr::map2_dbl(
				start, end,
				\(x, y) substr(input[[i]], x, y) |> as.numeric()
			),
			# Now we cross-reference the indices here with need_this_number to see if
			# that number should be included.
			include = purrr::map2_lgl(
				start, end,
				\(x, y) any(need_this_number[x:y])
			)
		)
	
	# Now that we know which numbers to get from that row, pull them, and
	# store them in the results holder.
	nums_from_row <-
		numb_locs |>
		dplyr::filter(include) |>
		dplyr::pull(number)
	
	number_holder[[i]] <- nums_from_row
}

output_p1 <- purrr::reduce(number_holder, sum)
print_answer(1, output_p1)

# PART 2 SOLUTION ----

# We need to find all of the stars that are adjacent to exactly two numbers.
# So first we need to find all of the stars.
star_positions <-
	data_matrix |>
	`colnames<-`(1:ncol(data_matrix)) |>
	tibble::as_tibble() |>
	tibble::rowid_to_column(var = "i") |>
	tidyr::pivot_longer(
		cols = -i,
		names_to = "j",
		values_to = "character"
	) |>
	dplyr::filter(character == "*")

# Next we'll go to each of those star positions and get the 3x3 square of
# values around the star. I'm starting to think it maybe would've been easier
# to do the first part like this. But learning is part of doing this :)
get_neighbors <- function(in_i, in_j) {
	out <-
		# Create a matrix of neighbor indices
		tidyr::expand_grid(
			i = c(in_i - 1, in_i, in_i + 1),
			j = c(in_j - 1, in_j, in_j + 1)
		) |>
		# Get rid of the index for the input position,
		# we already know what's there
		dplyr::filter((i != in_i) | (j != in_j)) |>
		# Now get the value that goes in that spot, remember this function
		# returns OOB if nothing is there. Now that I think about it, it's
		# probably easier to just remove any indices that don't exist. Oh well.
		dplyr::mutate(
			elem = purrr::map2_chr(i, j, \(i, j) try_to_get_element(i, j))
		)

	return(out)
}

# Next we need a function to tell us if the star is adjacent to exactly
# two numbers. This is kind of a difficult problem cause if we just count the
# number of neighbors that are digit characters, something like
# ...
# .*.
# .92
# Would be a hit for a gear, which is clearly not correct. So there is probably
# a better way to do this, but here's what I plan to check:
# 1. Check how many numbers are in the top row, it can be 0, 1, or 2.
# 2. Check if the left middle character is a number.
# 3. Check if the right middle character is a number.
# 4. Check how many numbers are in the bottom row, 0, 1, or 2.
count_number_neighbors <- function(neighbors) {
	# Get top row
	top_str <- neighbors |>
		dplyr::slice(1:3) |>
		dplyr::pull("elem") |>
		paste0(collapse = "")
	
	# Get bottom row
	bot_str <- neighbors |>
		dplyr::slice(6:8) |>
		dplyr::pull("elem") |>
		paste0(collapse = "")
	
	# Middle left and middle right
	mdl <- neighbors$elem[[4]]
	mdr <- neighbors$elem[[5]]
	
	# Do the counts (it's vectorized)
	counts <-
		stringr::str_count(
			c(top_str, mdl, mdr, bot_str),
			pattern = "[\\d]+"
		)
	
	# Add the counts together
	out <- sum(counts)
	
	return(out)
}

# Now we can get the counts for each star and find out the positions of the
# gears.
gear_positions <-
	star_positions |>
	dplyr::mutate(
		star_i = as.numeric(i),
		star_j = as.numeric(j),
		.keep = "none"
	) |>
	dplyr::mutate(
		neighbors = purrr::map2(
			star_i, star_j,
			\(x, y) get_neighbors(x, y)
		),
		number_count = purrr::map_dbl(
			neighbors,
			\(d) count_number_neighbors(d)
		)
	) |>
	dplyr::filter(number_count == 2)

# OK. Now for the part that I should've thought about more before doing this.
# We have to figure out how to take those star positions and get the two
# adjacent numbers. We can't just pull the neighbors, cause those only contain
# pieces of the numbers.
# We sort of already figured this bit out, but we need to go backwards.

# Now we need to figure out which indices triggered the counts, and where
# we need to look for the full numbers.
gear_indices <-
	gear_positions |>
	dplyr::select(-number_count) |>
	tidyr::unnest(neighbors) |>
	dplyr::filter(stringr::str_detect(elem, "\\d"))

# Now we'll create a lookup table (LUT) that has the location of all the
# numbers in each line.
number_lut <-
	stringr::str_locate_all(
		input,
		"[0-9]+"
	) |>
	purrr::map(\(x) tibble::as_tibble(x)) |>
	dplyr::bind_rows(.id = "i") |>
	# Now we get the number values
	dplyr::mutate(
		i = as.numeric(i),
		number = purrr::pmap_dbl(
			list(i, start, end),
			\(i, x, y) substr(input[[i]], x, y) |> as.numeric()
		),
		# Transform the data from "start" and "end" format to i,j format with
		# more rows
		j = purrr::map2(
			start, end,
			\(x, y) seq(from = x, to = y, by = 1)
		)
	) |>
	tidyr::unnest(j) |>
	dplyr::select(i, j, number)

# Now we can join the gear indices to the number lut!
gear_vals <-
	gear_indices |>
	dplyr::left_join(number_lut, by = c("i", "j")) |>
	# ANd now we make sure to only keep distinct values
	dplyr::distinct(star_i, star_j, number) |>
	# And then pivot wider to get each of the two gears as a separate column
	dplyr::group_by(star_i, star_j) |>
	dplyr::mutate(gear = paste0("gear", dplyr::row_number())) |>
	dplyr::ungroup() |>
	tidyr::pivot_wider(
		names_from = gear,
		values_from = number
	) |>
	# Next we calculate the gear ratio
	dplyr::mutate(gear_ratio = gear1 * gear2)

# Finally we want the sum of the gear ratios
output_p2 <- sum(gear_vals$gear_ratio)
print_answer(2, output_p2)

# END OF FILE ----
