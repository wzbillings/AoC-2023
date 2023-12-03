###
# Advent of Code 2023: Common utility functions
# Zane Billings
# 2023-12-02
###

print_answer <- function(part, answer) {
	paste0("Part ", part, " answer: ", answer) |>
		crayon::green() |>
		rlang::inform()
	
	invisible(answer)
}

# END OF FILE ----
