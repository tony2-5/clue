:- [gamesetup].

/* Get number of real players */
agent_setup :-
  write('How many real players will be playing (1-6)? '), nl,
  read(NumPlayers),
  (NumPlayers > 0, NumPlayers =< 6 ->
    NumAgents is 6-NumPlayers,
    findall(Character, character(Character,_,_), CharacterList),
    choose_character(NumPlayers, NumPlayers, CharacterList, RemainingCharacters), % players choose their characters, agents fill in rest
    init_agents(NumAgents, RemainingCharacters)
  ;
    write('Incorrect input! Please enter a number between 1 and 6.'), nl,
    agent_setup
  ).

% choose your character
choose_character(_,0,RemainingCharacters,RemainingCharacters).
choose_character(TotalPlayers, NumPlayers, CharacterList, RemainingCharacters) :-
  write('Choose from the following characters: '), write(CharacterList), nl,
  read(CharacterInput),
  (member(CharacterInput, CharacterList) ->
    subtract(CharacterList, [CharacterInput], Remaining),
    CurrentPlayer is TotalPlayers-NumPlayers+1,
    RemainingPlayers is NumPlayers-1,
    write('Player '), write(CurrentPlayer), write(' chose: '), write(CharacterInput), nl,
    choose_character(TotalPlayers, RemainingPlayers, Remaining, RemainingCharacters)
  ;
    write('Invalid character selection, try again.'), nl,
    choose_character(TotalPlayers, NumPlayers, CharacterList, RemainingCharacters)
  ).

/* initialize agents */
:- dynamic agent/1.
init_agents(0, _). % remaining characters not played by real players will be agents
init_agents(NumberOfPlayers,[Char|Rest]) :-
  write('AI Player: '), write(Char), nl,
  assert(agent(Char)),
  NumPlayers is NumberOfPlayers-1,
  init_agents(NumPlayers, Rest).

/* ai card management functions */
% get known room cards to ensure ai agent doesnt go to rooms that arent winning
get_rooms(AgentCards, ValidRooms) :-
  subtract([kitchen, ballroom, conservatory, billiard_room, library, study, dining_room, lounge, hall, center], AgentCards, GoodRooms),
  ValidRooms = GoodRooms.

/* ai movement logic (A* search) */
% exit room logic
agent_exit(AvailableExits, ValidRooms, SelectedExit) :-
  findall(distance(MinDist, ExitPos),
    (
      member(ExitPos, AvailableExits),
      nearest_room(ExitPos, ValidRooms, best_room(_, BestEntrancePos)), % getting nearest room position from each exit
      ExitPos = (EX, EY),
      BestEntrancePos = (EntX, EntY),
      MinDist is abs(EX - EntX) + abs(EY - EntY) % getting manhattan distance to each of the new room positions
    ),
    ExitDistances
  ),
  sort(ExitDistances, [distance(_, SelectedExit)|_]).  % taking exit with smallest manhattan distance to optimal room

% logic for when agent will take passage
valid_passage(CharName, CurrRoom) :-
  character(CharName, _, Cards),
  passage(CurrRoom, PassageRoom),
  get_rooms(Cards, ValidRooms),
  member(PassageRoom, ValidRooms).

% getting nearest room entrance using manhattan distance
nearest_room(CharPos, ValidRooms, best_room(BestRoom, BestEntrancePos)) :-
  CharPos = (X, Y),
  findall(distance(ManDist, Room, EntrancePos), % distance fact template
  ( 
    member(Room, ValidRooms),
    entrance(Room, (EX, EY)), % initialize distance fact for all rooms that are valid
    EntrancePos = (EX, EY),
    ManDist is abs(X-EX) + abs(Y-EY)
  ), ManhattanDistances), 
  sort(ManhattanDistances, [distance(_, BestRoom, BestEntrancePos)|_]).

% heuristic will be manhattan distance for pathing to room
heuristic(CurrPos, EntrancePos, ManhattanDistance) :-
  CurrPos = (X, Y),
  EntrancePos = (EX, EY),
  ManhattanDistance is abs(X-EX) + abs(Y-EY).

astar(Start, Goal, Path) :-
  heuristic(Start, Goal, H),
  astar_search([[H, 0, Start, [Start]]], Goal, [], ReversePath),
  reverse(ReversePath, FullPath),
  subtract(FullPath, [Start], Path). % removing state agent is currently at so it doesnt use up a move

astar_search([[_,_,Goal,Path]|_], Goal, _, Path). % base case, unifies goal path with final path
astar_search([[F, G, CurrPos, Path]|RemainingNodes], Goal, Visited, FinalPath) :-
  NewVisited = [CurrPos|Visited],
  findall([NewF, NewG, Neighbor, [Neighbor|Path]],
    (
      neighbor(CurrPos, Neighbor),
      \+ member(Neighbor, Visited),        % Not visited
      \+ member(Neighbor, Path),          % Not in current path
      NewG is G + 1,                     
      heuristic(Neighbor, Goal, H),
      NewF is NewG + H
    ),
    Neighbors
  ),
  append(Neighbors, RemainingNodes, Frontier),
  sort(Frontier, SortedFrontier), % sorts by F score since its the first element
  astar_search(SortedFrontier, Goal, NewVisited, FinalPath).

  % each direction for expanding frontier
  neighbor((X, Y), (X1, Y)) :-
    X1 is X - 1,
    is_hallway((X1, Y)).
  
  neighbor((X, Y), (X1, Y)) :-
    X1 is X + 1,
    is_hallway((X1, Y)).
  
  neighbor((X, Y), (X, Y1)) :-
    Y1 is Y - 1,
    is_hallway((X, Y1)).
  
  neighbor((X, Y), (X, Y1)) :-
    Y1 is Y + 1,
    is_hallway((X, Y1)).
