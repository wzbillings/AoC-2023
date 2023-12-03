###
# Advent of Code 2023: Day 2 Solution
# Zane Billings
# 2023-12-02
###

# SETUP ----

# Load common functions
source(here::here("R", "funcs.R"))

# Read in the input file
input <-
	readr::read_lines(here::here("data", "day-02-input.txt"))

# PART 1 SOLUTION ----

# Of course the trickiest part for this will be the data cleaning. The way I
# conceptualize what we need to do here is as follows, but I'm sure there's
# other ways.
# Step 1: Make a column for game ID and a column for everything else.
# Step 2: Split each game into one row for each draw from the bag. We do this
#  by splitting on semicolons.
# Step 3: Turn the raw 'r red, b blue, g green' into three columns instead of
#  one text column.
# So let's get started.

# Step 1: Make a column for game ID and a column for everything else.
game_draws <-
	# First put the input as a column of a tibble.
	tibble::tibble("raw" = input) |>
	# Now separate by the colon.
	tidyr::separate_wider_delim(
		cols = raw,
		names = c("game", "draws"),
		delim = ":"
	) |>
	# Do some clean up
	dplyr::mutate(
		# Turn game into a numeric column
		game = game |>
			stringr::str_remove("Game ") |>
			as.integer(),
		# Remove leading space from draws
		draws = stringr::str_trim(draws)
	)

# Step 2: Split each game into one row for each draw from the bag.
sep_draws <-
	game_draws |>
	tidyr::separate_longer_delim(
		cols = draws,
		delim = "; "
	)

# Step 3: Clean up the draws column into 3 separate columns.
# This step is the most complicated and I think the easiest solution is regex.
clean_draws <-
	sep_draws |>
	# Use regex to find the number that comes before the name of each of the
	# three colors
	dplyr::mutate(
		r = stringr::str_extract(draws, "(\\d*) red",  group = 1),
		g = stringr::str_extract(draws, "(\\d*) green", group = 1),
		b = stringr::str_extract(draws, "(\\d*) blue", group = 1),
		# Convert all of those to numeric
		dplyr::across(c(r, g, b), as.numeric),
		# Replace NAs with zero
		dplyr::across(c(r, g, b), \(x) dplyr::coalesce(x, 0))
	) |>
	# Get rid of the old draws column
	dplyr::select(-draws)

# Step 4: Figure out which games were possible.
# I didn't write this step above, but now we actually need to figure out if
# each draw is possible. Once we know if each draw is possible, we check if the
# game was possible by checking whether ALL draws for that game were possible.
possible_games <-
	clean_draws |>
	dplyr::mutate(possible_draw = (r <= 12) & (g <= 13) & (b <= 14)) |>
	dplyr::group_by(game) |>
	dplyr::summarise(possible = all(possible_draw), .groups = "drop")

# Step 5: Sum the IDs of possible games
output_p1 <-
	possible_games |>
	dplyr::filter(possible) |>
	dplyr::pull(game) |>
	sum()
print_answer(1, output_p1)

# PART 2 SOLUTION ----
# For this part, we need to find the minimum set of cubes necessary for the game
# to have been possible. This means we need to find the maximum of red, green,
# and blue cubes observed across all draws for each game.
# Then we calculate the power of each minimum set, defined by the prompt as
# the product r*g*b.
minimum_sets <-
	clean_draws |>
	dplyr::group_by(game) |>
	dplyr::summarise(
		r = max(r),
		g = max(g),
		b = max(b),
		.groups = "drop"
	) |>
	dplyr::mutate(power = r * g * b)

# Now we get the sum of the power of the minimum sets.
output_p2 <- sum(minimum_sets[["power"]])
print_answer(2, output_p2)

# END OF FILE ----
