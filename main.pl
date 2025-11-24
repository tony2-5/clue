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
    write('Game has ended!'), nl, !
  ;
    take_turn(Char),
    play_turns(Rest)
  ).

take_turn(CharName) :-
  print_board,
  write('--------------------------'), nl,
  write('Character: '), write(CharName), nl,
  character(CharName, CurrentPos, _),
  write('Current position: '), write(CurrentPos), nl,
  
  roll_dice(Moves),
  write('You rolled a '), write(Moves), nl,
  move(Moves, CharName, CurrentPos).

/* Movement logic */
valid_move(X, Y) :-
  Pos = (X, Y),
  is_hallway(Pos).

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

% update character position
move_char(Character, X, Y) :-
  NewPos = (X, Y),
  character(Character, OldPos, Cards),
  retract(character(Character, OldPos, Cards)),
  assert(character(Character, NewPos, Cards)),
  print_board.

% recursive moving until runs out of moves
move(0,Character,Pos) :-
  % checking is entrance in case entered entrance on last available move
  is_entrance(Pos) ->
    entrance(Room, Pos),
    get_room_center(Room, Center),
    Center = (XC, YC),
    find_unoccupied_near(XC, YC, NXC, NYC),
    move_char(Character, NXC, NYC), 
    suggestion(Character, Room), nl
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
      entrance(RoomEntered, Pos),
      get_room_center(RoomEntered, Center),
      Center = (XC, YC),
      find_unoccupied_near(XC, YC, NXC, NYC),
      move_char(Character, NXC, NYC), 
      suggestion(Character, RoomEntered), nl,
      guess(Character, RoomEntered), !
    ; 
      NewMoves is Moves-1,
      write('Choose direction (l/r/u/d): '), read(Direction),
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
      ;
        write('Invalid move! Try again.'), nl,
        move(Moves, Character, (X, Y)) 
      )
    ).

% move character to room
get_room_center(Room, Center) :-
  room(Room, (X1, Y1), (X2, Y2)),
  X is (X1 + X2) // 2,
  Y is (Y1 + Y2) // 2,
  Center = (X, Y).

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
  character(CurrChar, Pos, CharCards),
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

/* Win check */
guess(CurrChar, Room) :-
  write('Would you like to guess? (y/n)'), nl,
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

/* main game loop */
game_loop :-
  game_state(finished),
  write('Game over!'), nl.
game_loop :-
  game_state(running),
  play_round,
  game_loop.