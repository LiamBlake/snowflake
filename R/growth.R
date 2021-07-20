initialise <- function(k, nrow = 101, ncol = 101) {
  state <- matrix(k, nrow = nrow, ncol = ncol)

  # Frozen core
  state[ceiling((nrow + 1) / 2), ceiling((ncol + 1) / 2)] <- 1

  return(state)
}

get_neighbours <- function(state) {
  nr <- nrow(state)
  nc <- ncol(state)

  # These matrices shift the cells to align on their neighbours
  n1 <- cbind(state[, 2:nc], matrix(0, nrow = nc, ncol = 1))
  n2 <- cbind(matrix(0, nrow = nc, ncol = 1), state[, 1:(nc - 1)])
  n3 <- rbind(state[2:nr, ], matrix(0, nrow = 1, ncol = nc))
  n4 <- rbind(matrix(0, nrow = 1, ncol = nc), state[1:(nr - 1), ])
  n5 <- cbind(matrix(0, nrow = nr, ncol = 1), rbind(matrix(0, nrow = 1, ncol = nc - 1), state[2:nr, 2:nr]))
  n6 <- cbind(rbind(state[1:(nr - 1), 1:(nr - 1)], matrix(0, nrow = 1, ncol = nc - 1)), matrix(0, nrow = nr, ncol = 1))

  return(list(n1, n2, n3, n4, n5, n6))
}

is_frozen <- function(state) {
  state >= 1
}

is_receptive <- function(state) {
  frozen <- is_frozen(state)

  # List of neighbours
  n <- get_neighbours(frozen)

  # Get one matrix as the union of n
  receptive <- Reduce("|", n)

  return(receptive)
}


step <- function(a, gamma, state) {
  receptive <- is_receptive(state)
  new_state <- state

  # Update for receptive cells
  new_state[receptive] <- state[receptive] + gamma

  # This is the diffusion step for non-receptive cells
  new_state <- new_state - a * state * !receptive + a / 6 * Reduce("+", get_neighbours(state))

  return(new_state)
}

check_stop <- function(state) {
  # Check cells on boundary of matrix
  frozen <- is_frozen(state)
  nr <- nrow(state)
  nc <- ncol(state)

  boundary_map <- cbind(matrix(TRUE, nrow = nr, ncol = 1), rbind(matrix(TRUE, nrow = 1, ncol = nc - 2), matrix(FALSE, nrow = nr - 2, nc = nr - 2), matrix(TRUE, nrow = 1, ncol = nc - 2)), matrix(TRUE, nrow = nr, ncol = 1))

  return(any(frozen[boundary_map]))
}
