Simple ASCII version of Conway's Game of Life
     
## Usage
| Argument  | Description                          | Default Value  |  
|-----------|--------------------------------------|----------------|
| --[h]elp  | Show help                            | --             |   
| --[i]nput | Input file with board starting state | init_board.txt | 
| --[s]teps | Number of steps                      | 100            |
| --[d]elay | Delay in milliseconds between steps  | 100            |

    
## Init Board File
Each line in the file represent a line in the board, '-' and *space* chars represnts a dead cell, any other char is a living cell.

Example of an init file for a glider:

`--------------------`  
`--#-----------------`  
`---#----------------`  
`-###----------------`  
`--------------------`  
`--------------------`  
`--------------------`  
`--------------------`  
