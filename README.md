# sudoku
## A brute force algorithm for solving sudoku puzzles

### Inputs and outputs:
- It takes in a sudoku puzzle as a command line argument of the form 53..9.6.2..8.....11....3.5...78.9...2...5...9...2.74...2.5....89.....1..4.1.7..63 
- Outputs a formatted grid.

### How it works
It takes the current state of the grid, the current empty cells of the grid, and a number to try on the first empty cell in the grid. This function gets run with the initial grid supplied by the user, the initial empty cells are derived from that, and the first number to try is 1. It returns either a grid or "Nothing" if input sudoku has no valid solution. It recursively finds the solution by doing the following: 
- Base case (failure): If the number to try on the first empty cell is greater than 9, return Nothing
- Base case (success): If there are currently no empty cells, return the grid
- If the number supplied can be placed in the first empty cell then see if you can find a valid solution by applying this function on the remainder of empty cells, with the current state, current empty cells, and the number 1. If that doesn't return Nothing, then you have your solution. If it returns Nothing, then try this same state and set of empty cells with number + 1. 
