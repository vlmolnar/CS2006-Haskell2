# CS2006-Haskell2
Repository for the second Haskell project of CS2006 (Advanced Programming Projects) at the University of St Andrews. The project specifications and starter code used were provided to us by Dr Edwin Brady.

# How to configure
run: cabal configure

# How to run 
No paramenters:
 * cabal run -- takes you to the menu screen 
Parameters:
  * cabal run <size> <target> <rule> <ai colour> <ai level> 
     - size - a number: 3 <= n <= 10 and n >= target
     - target - a number: 3 <= t <= 5
     - rule: "Regular" - standard rules, "Three" - three and three, "Four" - four and four
     - ai_colour: "White" or "Black"
     - ai_level: a number - level = 1 or level = 2 

# Menu 
Buttons that do not have arrows:
* need to click through the options (e.g. click on AI to change colour of AI)
Buttons that have arrows:
* Click up arrow to increment value
* Click down arrow to decrement value

New Game & Load Game will start a game 

# In Game
Undo: 
* Player vs Player - undo will move back one move
* Player vs AI - undo will move back two moves (go back to players turn) 
* AI vs AI - if your fast enough to press undo nothing will happen

Save: 
* pressing this will save the current sate of the game in the JSON file 
* A state saved during runtime **cannot** be reloaded during the same runtime
