:- [gamesetup].

/* Get number of real players */
agent_setup :-
  write('How many real players will be playing (1-6)? '), nl,
  read(NumPlayers),
  (NumPlayers > 0, NumPlayers =< 6 ->
    NumAgents is 6-NumPlayers,
    findall(Character, character(Character,_,_), CharacterList),
    init_agents(NumAgents, CharacterList)
  ;
    write('Incorrect input! Please enter a number between 1 and 6.'), nl,
    agent_setup
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
agent_exit(AvailableExits, SelectedExit) :-
  nth1(1, AvailableExits, SelectedExit).  % taking first exit for now
  % TODO: implement smarter exit logic

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

/* ai suggestion/guess logic */
agent_guess_conditon(CharName) :-
  character(CharName, CharPos, AgentCards),
  subtract([kitchen, ballroom, conservatory, billiard_room, library, study, dining_room, lounge, hall], AgentCards, RemainingRooms),
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], AgentCards, RemainingWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], AgentCards, RemainingChars),
  (length(RemainingRooms, 1) ->
    in_room(CharPos, Room),
    member(Room, RemainingRooms)
  ;
    false % terminate guess
  ),
  (length(RemainingWeapons, 1) ->
    true
  ;
    false % terminate guess
  ),
  (length(RemainingChars, 1) ->
    true
  ;
    false % terminate guess
  ),
  write('In progress').
% PICK BEST EXIT