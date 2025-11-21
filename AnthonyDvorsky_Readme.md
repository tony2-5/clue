# Execution Instructions
## Prerequisites
1. Download SWI-Prolog from https://www.swi-prolog.org/download/stable, use the download for your respective OS.
- **NOTE**: For windows installation choose option: Add swipl to the system PATH
2. Setup `swipl` terminal command:
- **WINDOWS**: 
  - 1. Search edit system environment variables in windows search bar
  - 2. Select environment variables
  - 3. Ensure there is a system variable: swipl, with path to your swipl.exe (e.g C:\Program Files\swipl\bin\swipl.exe)
  - 4. If not add this variable
-  **Linux/Mac**:
    - 1. Set path using the following changing the path to match yours: export PATH="$PATH:/usr/swipl/bin"
    - 2. Save for Mac using: source ~/.zshrc, Save for Linux using: source ~/.bash_profile
3. Ensure you can use the command `swipl` to start the Prolog REPL environment from the command line.
## Cloning the repository
* In the directory you would like to have the github repository, run the command: `git clone https://github.com/tony2-5/clue.git`
## Navigating to the source code directory
* From the directory you cloned the github repository into, run the command: `cd clue`
## Running the game
1. Within the github repository clue directory start the Prolog REPL environment using: `swipl`
2. In the swipl environment run: `[main].` to initialize the Prolog project.
3. Now simply start the game with the command: `start_game.`
* **Note:** All read user input during gameplay must be ended with a `.`, e.g. Who commited the crime?: `mrs_white.`