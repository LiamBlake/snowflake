initialise <- function(k, nrow = 5, ncol = 5) {
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
        # This call returns a 0-length vector if the cell is
        # outside the boundaries of the array. R is weird with
        # matrices.
        cell_is_frozen <- tryCatch(
          {
            frozen[r + p[0], c + p[1]]
          },
          error = function(cond) {
            FALSE
          }
        )
        if (length(cell_is_frozen) > 0 && cell_is_frozen) {
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
        # As above, R is weird.
        add <- tryCatch(
          {
            state[r + p[0], c + p[1]]
          },
          error = function(cond) {
            0
          }
        )
        sum <- sum + ifelse(length(add) > 0, add, 0)
      }
      new_state[r, c] <- (1 - a) * state[r, c] + a / 6 * sum
    }
  }

  return(state)
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

# Helper function to create the polygon for each hexagon
hexagon <- function(x, y, unitcell = 1, col) {
  polygon(c(
    x, x, x + unitcell / 2, x + unitcell, x + unitcell,
    x + unitcell / 2
  ), c(
    y + unitcell * 0.125,
    y + unitcell * 0.875,
    y + unitcell * 1.125,
    y + unitcell * 0.875,
    y + unitcell * 0.125,
    y - unitcell * 0.125
  ),
  col = col, border = "grey"
  )
}

plot_cells <- function(state, show_values=FALSE) {
  return(renderPlot(
    {
      frozen_cells <- is_frozen(state)

      x <- as.vector(frozen_cells)

      nrows <- nrow(frozen_cells)
      ncols <- ncol(frozen_cells)

      # Initiate the plot window
      plot(0, 0,
        type = "n", axes = FALSE, xlim = c(0, ncols),
        ylim = c(0, nrows), xlab = "", ylab = "", asp = 1
      )

      color_codes <- rep("#FFFFFF", length(x))
      for (i in 1:length(x)) {
        if (x[i]) color_codes[i] <- "#3288BD"
      }

      offset <- 0.5 # offset for the hexagons when moving up a row
      for (row in 1:nrows) {
        for (column in 0:(ncols - 1)) {
          hexagon(column + offset, row - 1, col = color_codes[row + nrows * column])
        }
        offset <- ifelse(offset, 0, 0.5)
      }
    },
    width = 500,
    height = 500
  ))
}