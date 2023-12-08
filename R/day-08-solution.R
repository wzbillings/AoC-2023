###
# Advent of Code 2023: Day 8 Solution
# Zane Billings
# 2023-12-08
###

# SETUP ----

# Load common functions
source(here::here("R", "funcs.R"))

# Read in the input file
input <-
	readr::read_lines(here::here("data", "day-08-input.txt"))

# PART 1 SOLUTION ----
# Separate the directions from the rest of the input
dir <- strsplit(input, "")[[1]]
maps <- input[3:length(input)]
# First find the starting point
start <- stringr::str_which(maps, "AAA")

# Intialize current position as starting position
cur <- maps[start]
# Intialize position in directions
pos <- 1
# Initialize total number of iterations
n_iter <- 1
# Now follow the directions until we get to ZZZ
while (!startsWith(cur, "ZZZ")) {
	# If the direction is L, we need to get the 8, 9, 10 characters of the map
	# If the direction is R, we need positions 13, 14, 15.
	if (dir[pos] == "L") {
		nxt <- substr(cur, 8, 10)
	} else {
		nxt <- substr(cur, 13, 15)
	}
	
	# Travel to the next node by updating the current position
	next_index <- stringr::str_which(maps, paste0("^", nxt))
	next_place <- maps[next_index]
	
	# Print a little message
	rlang::inform(c(
		paste0("Step ", crayon::green(n_iter), ":"),
		"*" = paste0("Starting at: ", cur),
		"*" = paste0("Moving ", crayon::green(dir[pos]), " to: ", next_place)
	))
	
	cur <- next_place
	# Update the position for the directions
	pos <- ifelse(pos < length(dir), pos + 1, 1)
	
	# Increment number of steps
	n_iter <- n_iter + 1
}
output_p1 <- n_iter
print_answer(1, output_p1)

# PART 2 SOLUTION ----
# First create the vector of starting points
start <- stringr::str_which(maps, "^\\w\\wA")

# Intialize current position as starting position
cur <- maps[start]
# Intialize position in directions
pos <- 1
# Initialize total number of iterations
n_iter <- 1
# Intialize stopping condition
stopping_vec <- rep(0, times = length(cur))
stopping_cnd <- all(as.logical(stopping_vec))

# Now follow the directions SIMULTANEOUSLY FOR EACH START NODE until we get to
# ALL NOTES ENDING WITH Z
# Because of R's innate vectorization of many operations, there is not a lot
# We need to change. HOWEVER, if we try to run this until we find the starting
# condition it will take a VERY long time most likely.
# I cheated on this problem and looked it up, and apparently the solution
# is to get the LCM of the lengths for each path. I don't think that works in
# the general case for this problem, I think it only works if the input is
# designed for that to work.
# But anyways, that means I can write an answer I guess.
while (!stopping_cnd) {
	# If the direction is L, we need to get the 8, 9, 10 characters of the map
	# If the direction is R, we need positions 13, 14, 15.
	if (dir[pos] == "L") {
		nxt <- substr(cur, 8, 10)
	} else {
		nxt <- substr(cur, 13, 15)
	}
	
	# Travel to the next node by updating the current position
	next_index <- purrr::map_int(
		nxt,
		\(s) stringr::str_which(maps, paste0("^", s))
	)
	next_place <- maps[next_index]
	
	# Check stopping condition
	# If a specific path gets to an end condition, keep that forever.
	find_zs <- stringr::str_which(next_place, "^\\w\\wZ")
	stopping_vec <- find_zs | stopping_vec
	stopping_cnd <- all(stopping_vec)
	
	# Print a little message
	if ((n_iter %% 1000) == 0) {
		rlang::inform(c(
			paste0("Step ", crayon::green(n_iter), " done."),
			"*" = paste(sum(stopping_vec), "chains have terminated.")
		))
	}
	
	cur <- next_place
	
	# Update the position for the directions
	pos <- ifelse(pos < length(dir), pos + 1, 1)
	
	# Increment number of steps
	n_iter <- n_iter + 1
}
output_p2 <- n_iter
print_answer(2, output_p2)
