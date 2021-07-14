initialise <- function(k, nrow = 11, ncol = 11) {
  state <- matrix(k, nrow = nrow, ncol = ncol)

  # Frozen core
  state[ceiling((nrow + 1) / 2), ceiling((ncol + 1) / 2)] <- 1

  # Outer padding of zeros
  for (r in 1:nrow) {
    state[r, 1] <- 0
    state[r, ncol] <- 0
  }
  for (c in 1:ncol) {
    state[1, c] <- 0
    state[nrow, c] <- 0
  }

  return(state)
}

get_neighbours <- function(r) {
  if (r %% 2 == 0) {
    return(list(c(0, -1), c(0, 1), c(-1, -1), c(-1, 0), c(1, -1), c(1, 0)))
  } else {
    return(list(c(0, -1), c(0, 1), c(-1, 0), c(-1, 1), c(1, 0), c(1, 1)))
  }
}

is_frozen <- function(state) {
  state >= 1
}

is_receptive <- function(state) {
  frozen <- is_frozen(state)
  receptive <- frozen

  for (r in 1:nrow(state)) {
    for (c in 1:ncol(state)) {
      # Frozen cells have already been marked as receptive
      if (frozen[r, c]) {
        next
      }

      neighbours <- get_neighbours(r)
      for (p in neighbours) {
        # Check whether the cell is frozen
        rp <- r + p[1]
        cp <- c + p[2]

        if (rp > 0 && cp > 0 && rp <= nrow(state) && cp <= ncol(state) && frozen[rp, cp]) {
          receptive[r, c] <- TRUE
          break
        }
      }
    }
  }
  return(receptive)
}


step <- function(a, gamma, state) {
  print("Iterating...")
  receptive <- is_receptive(state)

  new_state <- state

  # Receptive cells
  new_state[receptive] <- state[receptive] + gamma

  # Non-receptive cells
  for (r in 2:(nrow(state) - 1)) {
    for (c in 2:(ncol(state) - 1)) {
      # Frozen or receptive cells no longer need update
      if (receptive[r, c]) {
        next
      }

      # Sum contributions from neighboring cells
      neighbours <- get_neighbours(r)
      sum <- 0
      for (p in neighbours) {
        rp <- r + p[1]
        cp <- c + p[2]

        if (rp > 0 && cp > 0 && rp <= nrow(state) && cp <= ncol(state) && !receptive[rp, cp]) {
          sum <- sum + state[rp, cp]
        }
      }
      new_state[r, c] <- (1 - a) * state[r, c] + a / 6 * sum
    }
  }
  return(new_state)
}

check_stop <- function(state) {
  # Check cells on boundary of matrix
  frozen <- is_frozen(state)
  nrows <- nrow(state)
  ncols <- ncol(state)

  for (r in 2:(nrows - 1)) {
    for (c in 2:(ncols - 1)) {
      if ((r == 2 || c == 2 || r == nrows - 1 || c == ncols - 1) && frozen[r, c]) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}
