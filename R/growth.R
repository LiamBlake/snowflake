initialise <- function(k, nrow = 101, ncol = 101) {
  state <- matrix(k, nrow = nrow, ncol = ncol)

  # Frozen core
  state[ceiling(nrow / 2), ceiling(ncol / 2)] <- 1

  return(state)
}

get_neighbours <- function(r) {
  if (r %% 2 == 0) {
    return(c(c(-1, -1), c(-1, 0), c(0, -1), c(0, 1), c(1, -1), c(1, 0)))
  } else {
    return(c(c(-1, 0), c(-1, 1), c(0, -1), c(0, 1), c(1, 0), c(1, 1)))
  }
}

is_frozen <- function(state) {
  state >= 1
}

is_receptive <- function(state) {
  frozen <- is_frozen(state)
  is_receptive <- frozen
  
  for (r in 1:nrow(state)) {
    for (c in 1:ncol(state)) {
      # Frozen cells have already been marked as receptive
      if (frozen[r, c]) {
        next
      }
      
      neighbours <- get_neighbours(r)
      for (p in neighbours) {
        if (tryCatch(
          {
            frozen[r + p[0], c + p[1]]
          },
          error = function(cond) {
            FALSE
          }
        )) {
          is_receptive[r, c] <- TRUE
          break
        }
      }
    }
  }
}


step <- function(a, gamma, state) {
  receptive <- is_receptive(state)

  # Receptive cells
  state[receptive] <- state[receptive] + gamma

  # Non-receptive cells
  for (r in 1:nrow(state)) {
    for (c in 1:ncol(state)) {
      # Frozen or receptive cells no longer need update
      if (receptive[r, c]) {
        next
      }

      # Sum contributions from neighboring cells
      neighbours <- get_neighbours(r)
      sum <- 0
      for (p in neighbours) {
        # Ignore cells outside of the boundary
        sum <- sum + tryCatch(
          {
            state[r + p[0], c + p[1]]
          },
          error = function(cond) {
            0
          }
        )
      }
      state[r, c] <- (1 - k) * state[r, c] + k / 6 * sum
    }
  }
}

check_stop <- function(state) {
  # Check cells on boundary of matrix
  frozen <- is_frozen(state)
  nrows <- nrow(state)
  ncols <- ncol(state)

  for (r in 1:nrows) {
    for (c in 1:ncols) {
      if ((r == 1 || c == 1 || r == nrows || c == ncols) && frozen[r, c]) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}
