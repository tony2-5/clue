:- [gamesetup].
:- [agent].

/* Game initialization */
:- dynamic game_state/1.
start_game :-
  cleanup,
  init_chars,
  winning_cards,
  distribute_cards,
  agent_setup,
  assert(game_state(running)),
  game_loop.

/* Turn logic */
% Two die rolls between 2 and 12
roll_dice(Moves) :-
  random_between(2, 12, Moves).

play_round :-
  write('--------------------------'), nl,
  write('NEW ROUND'), nl,
  
  findall(Character, character(Character,_,_), CharacterList),
  play_turns(CharacterList).

% moving turn for each character
play_turns([]).
play_turns([Char|Rest]) :-
  (game_state(finished) ->
    !
  ;
    take_turn(Char),
    play_turns(Rest)
  ).

take_turn(CharName) :-
  write('--------------------------'), nl,
  write('Character: '), write(CharName), nl,
  character(CharName, CurrentPos, _),
  write('Current position: '), write(CurrentPos), nl,
  
  roll_dice(Moves),
  write('You rolled a '), write(Moves), nl,
  print_board, nl,
  (agent(CharName) ->
    ai_turn(Moves, CharName)
  ;
    move(Moves, CharName, CurrentPos)
  ).

/* Movement logic */
valid_move(X, Y) :-
  Pos = (X, Y),
  is_hallway(Pos).

% move character to room
get_room_center(Room, Center) :-
  room(Room, (X1, Y1), (X2, Y2)),
  X is (X1 + X2) // 2,
  Y is (Y1 + Y2) // 2,
  Center = (X, Y).

center_on_enter(Character, Pos, RoomEntered) :-
  entrance(Room, Pos),
  RoomEntered = Room,
  get_room_center(Room, Center),
  Center = (XC, YC),
  find_unoccupied_near(XC, YC, NXC, NYC),
  move_char(Character, NXC, NYC).

% update character position
move_char(Character, X, Y) :-
  NewPos = (X, Y),
  character(Character, OldPos, Cards),
  retract(character(Character, OldPos, Cards)),
  assert(character(Character, NewPos, Cards)),
  (agent(Character) ->
    true % not printing board for agents
  ;
    print_board, nl, nl
  ).

% recursive moving until runs out of moves
move(0,Character,Pos) :-
  % checking is entrance in case entered entrance on last available move
  is_entrance(Pos) ->
    center_on_enter(Character, Pos, RoomEntered),
    suggestion(Character, RoomEntered),
    guess(Character, RoomEntered)
  ;
    write('Moves complete'), nl,
    write('--------------------------'), nl.
move(Moves, Character, Pos) :-
  Pos = (X, Y),
  write('Moves left: '), write(Moves), nl,
  (
  % checking if already in room
  in_room(Pos, Room) ->
    write('Enter 1 to make a suggestion.'), nl,
    write('Enter 2 to exit the room.'), nl,
    (passage(Room, PassageRoom) ->
        write('Enter 3 to take secret passage to '), write(PassageRoom), nl
      ;
        true
    ),
    read(Option),
    (Option = 1 ->
      suggestion(Character, Room), nl,
      guess(Character, Room), !
    ; Option = 2 ->
      findall(EPos, exit(Room, EPos), ExitPos),
      write('Choose an exit by entering the number: '), nl,
      display_exits(ExitPos, 1),
      select_exit(ExitPos, SelectedExit),
      SelectedExit = (XE, YE),
      NewMoves is Moves - 1,
      move_char(Character, XE, YE),
      move(NewMoves, Character, (XE, YE))
    ; Option = 3, passage(Room, PassageRoom) ->
      get_room_center(PassageRoom, Center),
      Center = (XC, YC),
      find_unoccupied_near(XC, YC, NXC, NYC),
      move_char(Character, NXC, NYC), 
      move(Moves, Character, (XC, YC))
    ;
      write('Invalid option!'), nl,
      move(Moves, Character, Pos)
    )
  ; 
  % check if moved into entrance
  is_entrance(Pos) ->
    center_on_enter(Character, Pos, RoomEntered),
    suggestion(Character, RoomEntered),
    guess(Character, RoomEntered), !
  ; 
    NewMoves is Moves-1,
    write('Choose direction (l/r/u/d) or make accusation (a): '), read(Direction),
    ( Direction = l, X1 is X - 1, valid_move(X1, Y) ->
      write(X1),
      move_char(Character, X1, Y),
      move(NewMoves, Character, (X1, Y))
    ; Direction = r, X1 is X + 1, valid_move(X1, Y) ->
      move_char(Character, X1, Y),
      move(NewMoves, Character, (X1, Y))
    ; Direction = d, Y1 is Y + 1, valid_move(X, Y1) ->
      move_char(Character, X, Y1),
      move(NewMoves, Character, (X, Y1))
    ; Direction = u, Y1 is Y - 1, valid_move(X, Y1) ->
      move_char(Character, X, Y1),
      move(NewMoves, Character, (X, Y1))
    ; Direction = a ->
      outside_room_guess(Character)
    ;
      write('Invalid move! Try again.'), nl,
      move(Moves, Character, (X, Y)) 
    )
  ).

% for finding unoccupied spots inside of rooms
is_occupied(Pos) :- character(_, Pos, _).
find_unoccupied_near(X, Y, NewX, NewY) :-
  Pos = (X, Y),
  (\+ is_occupied(Pos) ->
    NewX = X, NewY = Y
  ;
    % Trying adjacent positions
    (X1 is X - 1, Pos1 = (X1, Y), valid_position(Pos1), \+ is_occupied(Pos1) -> %left
      NewX = X1, NewY = Y
    ; X1 is X + 1, Pos1 = (X1, Y), valid_position(Pos1), \+ is_occupied(Pos1) -> %right
      NewX = X1, NewY = Y
    ; Y1 is Y - 1, Pos1 = (X, Y1), valid_position(Pos1), \+ is_occupied(Pos1) -> %up
      NewX = X, NewY = Y1
    ; Y1 is Y + 1, Pos1 = (X, Y1), valid_position(Pos1), \+ is_occupied(Pos1) -> %down
      NewX = X, NewY = Y1
    ; % fallback is allowing characters to stack
      NewX = X, NewY = Y
    )
  ).

/* AI Movement */
follow_path(0, _, _).
follow_path(_, [], _).
follow_path(Moves, [PathPos|RemainingPath], CharName) :-
  PathPos = (X,Y),
  move_char(CharName, X, Y),
  is_entrance(PathPos) ->
    center_on_enter(CharName, PathPos, RoomEntered),
    agent_suggest(CharName, RoomEntered), !
  ;
    NewMoves is Moves-1,
    follow_path(NewMoves, RemainingPath, CharName).

ai_turn(Moves, CharName) :-
  character(CharName, CharPos, Cards),
  get_rooms(Cards, ValidRooms),
  (in_room(CharPos, Room) -> % agent if in room needs to 1. make suggestion, 2. exit, 3. take passage
    (member(Room, ValidRooms) -> % if current room valid make suggestion
      write('suggesting'), nl,
      agent_suggest(CharName, Room), !
    ; passage(Room, PassageRoom), valid_passage(CharName, Room) -> % if passage a valid room take it
      write('taking passage'), nl,
      get_room_center(PassageRoom, Center),
      Center = (XC, YC),
      find_unoccupied_near(XC, YC, NXC, NYC),
      move_char(CharName, NXC, NYC), 
      ai_turn(Moves, CharName)
    ; % else take exit
      write('taking exit'), nl,
      findall(EPos, exit(Room, EPos), ExitPos),
      agent_exit(ExitPos, ValidRooms, SelectedExit),
      SelectedExit = (XE, YE),
      NewMoves is Moves - 1,
      move_char(CharName, XE, YE),
      ai_turn(NewMoves, CharName)
    )
  ;
    nearest_room(CharPos, ValidRooms, best_room(_, BestEntrance)),
    astar(CharPos, BestEntrance, Path),
    write(Path), nl,
    follow_path(Moves, Path, CharName)
  ).

/* exit logic */
% display available exits
display_exits([], _).
display_exits([Pos|Rest], N) :-
  write(N), write('. '), write(Pos), nl,
  N1 is N + 1,
  display_exits(Rest, N1).
% exiting
select_exit(AvailableExits, SelectedExit) :-
  length(AvailableExits, NumExits),
  write('Enter exit number (e.g. 1): '), nl,
  read(ExitNum),
  (integer(ExitNum), ExitNum > 0, ExitNum =< NumExits ->
    nth1(ExitNum, AvailableExits, SelectedExit)
    ;
    write('Invalid number! Try again.'), nl,
    select_exit(AvailableExits, SelectedExit)
  ).

/* guess/suggest logic */
validate_guess(ValidGuess, Prompt, Card) :-
  write(Prompt), read(Guess), nl,
  member(Guess, ValidGuess) -> 
    Card = Guess
  ;
    write('Invalid guess!'), nl,
    validate_guess(ValidGuess, Prompt, Card).

suggestion(CurrChar, Room) :-
  character(CurrChar, _, CharCards),
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  write('Your current cards: '), write(CharCards), nl,
  write('Enter your suggestion: '), nl,
  write('Weapons you can guess: '), write(GuessableWeapons), nl,
  validate_guess(GuessableWeapons, 'What weapon was used?: ', Weapon), nl,
  write('Characters you can guess: '), write(GuessableChars), nl,
  validate_guess(GuessableChars, 'Who commited the crime?: ', Character), nl,
  write('Current room: '), write(Room), nl,
  % Move guessed character if they exist
  (character(Character, _, _) ->
      get_room_center(Room, Center),
      Center = (XC, YC),
      find_unoccupied_near(XC, YC, NXC, NYC),
      move_char(Character, NXC, NYC)
    ;
      true
  ),
  % our version gets random card from the three cards guessed not in winning pile
  winningcards(WinningCards),
  % using room currently entered for suggestion
  subtract([Weapon, Character, Room], WinningCards, RemainingCards),
  subtract(RemainingCards, CharCards, RemainingCards2),
  (
    RemainingCards2 = [] ->
    write('No one has cards from your suggestion.'), nl
  ;
    random_permutation(RemainingCards2, ShuffledRemaining),
    ShuffledRemaining = [RandomCard|_],
    write('Another player shows you: '), write(RandomCard), nl,
    character(CurrChar, UpdatedPos, _), % need to refetch pos in case character moved
    append(CharCards, [RandomCard], NewCharCards),
    retract(character(CurrChar, UpdatedPos, CharCards)),
    assert(character(CurrChar, UpdatedPos, NewCharCards))
  ).

/* AI Suggest */
agent_suggest(CurrChar, Room) :-
  character(CurrChar, _, CharCards),
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  write('Your current cards: '), write(CharCards), nl,
  random_member(Weapon, GuessableWeapons),
  random_member(Character, GuessableChars),
  write('Weapon suggestion: '), write(Weapon), nl,
  write('Character suggestion: '), write(Character), nl,
  write('Current room: '), write(Room), nl,
  % Move guessed character if they exist
  (character(Character, _, _) ->
      get_room_center(Room, Center),
      Center = (XC, YC),
      find_unoccupied_near(XC, YC, NXC, NYC),
      move_char(Character, NXC, NYC)
    ;
      true
  ),
  % our version gets random card from the three cards guessed not in winning pile
  winningcards(WinningCards),
  % using room currently entered for suggestion
  subtract([Weapon, Character, Room], WinningCards, RemainingCards),
  subtract(RemainingCards, CharCards, RemainingCards2),
  (
    RemainingCards2 = [] ->
    write('No one has cards from your suggestion.'), nl,
    agent_guess(CurrChar, Room) % agent guesses once it know it will get it right
  ;
    random_permutation(RemainingCards2, ShuffledRemaining),
    ShuffledRemaining = [RandomCard|_],
    write('Another player shows you: '), write(RandomCard), nl,
    character(CurrChar, UpdatedPos, _), % need to refetch pos in case character moved
    append(CharCards, [RandomCard], NewCharCards),
    retract(character(CurrChar, UpdatedPos, CharCards)),
    assert(character(CurrChar, UpdatedPos, NewCharCards))
  ).

/* Win check */
guess(CurrChar, Room) :-
  write('Would you like to guess using this current room? (y/n)'), nl,
  read(Input),
  (Input = y ->
    character(CurrChar, CurrPos, CharCards),
    % let player know cards left that were not marked
    subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
    subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
    write('Weapons you can guess: '), write(GuessableWeapons), nl,
    validate_guess(GuessableWeapons, 'What weapon was used?: ', Weapon), nl,
    write('Characters you can guess: '), write(GuessableChars), nl,
    validate_guess(GuessableChars, 'Who commited the crime?: ', Character), nl,
    % Move guessed character if they exist
    (character(Character, _, _) ->
      get_room_center(Room, Center),
      Center = (XC, YC),
      find_unoccupied_near(XC, YC, NXC, NYC),
      move_char(Character, NXC, NYC)
    ;
      true
    ),
    winningcards(WinningCards),
    % using room currently entered for guess
    (WinningCards = [Weapon, Character, Room] ->
      write('Correct! '), write(CurrChar), write(' wins!'), nl,
      retract(game_state(running)),
      assert(game_state(finished))
    ;
      write('Incorrect Guess!'), nl,
      write('Character '), write(CurrChar), write(' has been eliminated!'), nl,
      retract(character(CurrChar, CurrPos, CharCards)) 
    )
  ; Input = n -> 
    true
  ;
    write('Incorrect input!'), nl,
    guess(CurrChar, Room)
  ).

outside_room_guess(CurrChar) :-
  write('Now guessing.'), nl,
  character(CurrChar, CurrPos, CharCards),
  % let player know cards left that were not marked
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  subtract([kitchen, ballroom, conservatory, billiard_room, library, study, hall, lounge, dining_room], CharCards, GuessableRooms),
  write('Weapons you can guess: '), write(GuessableWeapons), nl,
  validate_guess(GuessableWeapons, 'What weapon was used?: ', Weapon), nl,
  write('Characters you can guess: '), write(GuessableChars), nl,
  validate_guess(GuessableChars, 'Who commited the crime?: ', Character), nl,
  write('Rooms you can guess: '), write(GuessableRooms), nl,
  validate_guess(GuessableRooms, 'Where was the crime committed?: ', Room), nl,
  % Not moving characters if guessing outside room
  winningcards(WinningCards),
  % using room currently entered for guess
  (WinningCards = [Weapon, Character, Room] ->
    write('Correct! '), write(CurrChar), write(' wins!'), nl,
    retract(game_state(running)),
    assert(game_state(finished))
  ;
    write('Incorrect Guess!'), nl,
    write('Character '), write(CurrChar), write(' has been eliminated!'), nl,
    retract(character(CurrChar, CurrPos, CharCards)) 
  ).

/* AI win check */
agent_guess(CurrChar, Room) :-
  character(CurrChar, CurrPos, CharCards),
  % let player know cards left that were not marked
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  random_member(Weapon, GuessableWeapons),
  random_member(Character, GuessableChars),
  write('Weapon guess: '), write(Weapon), nl,
  write('Character guess: '), write(Character), nl,
  write('Current room: '), write(Room), nl,
  % Move guessed character if they exist
  (character(Character, _, _) ->
    get_room_center(Room, Center),
    Center = (XC, YC),
    find_unoccupied_near(XC, YC, NXC, NYC),
    move_char(Character, NXC, NYC)
  ;
    true
  ),
  winningcards(WinningCards),
  % using room currently entered for guess
  (WinningCards = [Weapon, Character, Room] ->
    write('Correct! '), write(CurrChar), write(' wins!'), nl,
    retract(game_state(running)),
    assert(game_state(finished))
  ;
    write('Incorrect Guess!'), nl,
    write('Character '), write(CurrChar), write(' has been eliminated!'), nl,
    retract(character(CurrChar, CurrPos, CharCards)) 
  ).
/* main game loop */
game_loop :-
  game_state(finished),
  write('Game over!'), nl.
game_loop :-
  game_state(running),
  play_round,
  game_loop.