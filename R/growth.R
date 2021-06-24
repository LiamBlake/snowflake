initialise <- function(k, nrow = 101, ncol = 101) {
  state <- matrix(k, nrow = nrow, ncol = ncol)

  # Frozen core
  state[ceiling(nrow / 2), ceiling(ncol / 2)] <- 1

  return(state)
}

step <- function(a, gamma, state) {
  is_frozen_receptive <- is_frozen(state) | is_receptive(state)

  # Receptive cells
  state[is_frozen_receptive] <- state[is_frozen_receptive] + gamma

  # Non-receptive cells
  for (r in 1:nrow(state)) {
    for (c in 1:ncol(state)) {
      # Frozen or receptive cells no longer need update
      if (is_frozen_receptive[r, c]) {
        next
      }
    }

    # Sum contributions from neighboring cells
    if (r %% 2 == 0) {
      
    } else {
      
    }
    # for (p in c(c(), ))
  }
}

check_stop <- function(state) {
  # Check cells on boundary of matrix
  is_frozen <- is_frozen(state)
}

is_frozen <- function(state) {
  state >= 1
}

is_receptive <- function(state) {
  state < 0
}
