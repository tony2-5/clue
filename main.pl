:- [gamesetup].

% main game loop
:- dynamic game_state/1.
setup_game :-
  cleanup,
  init_chars,
  print_board,
  winning_cards,
  distribute_cards,
  assert(game_state(running)),
  game_loop.

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
    take_turn(Char),
    play_turns(Rest).

take_turn(CharName) :-
  write('--------------------------'), nl,
  write('Character: '), write(CharName), nl,
  character(CharName, CurrentPos, _),
  write('Current position: '), write(CurrentPos), nl,
  
  roll_dice(Moves),
  write('You rolled a '), write(Moves), nl,
  move(Moves, CharName, CurrentPos).

valid_move(X, Y) :-
  Pos = (X, Y),
  is_hallway(Pos).

% update character position
move_char(Character, X, Y) :-
  NewPos = (X, Y),
  character(Character, OldPos, Cards),
  retract(character(Character, OldPos, Cards)),
  assert(character(Character, NewPos, Cards)),
  print_board.

/* move character to room */
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


% recursive moving until runs out of moves
move(0,Character,Pos) :-
  % checking is entrance in case entered entrance on last available move
  is_entrance(Pos) ->
  entrance(Room, Pos),
  get_room_center(Room, Center),
  Center = (XC, YC),
  move_char(Character, XC, YC), 
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
    % TODO: give option to make suggestion or leave room
    % TODO: take secret passage
    findall(EPos, exit(Room, EPos), ExitPos),
    write('Choose an exit by entering the number: '), nl,
    display_exits(ExitPos, 1),
    select_exit(ExitPos, SelectedExit),
    SelectedExit = (XE, YE),
    NewMoves is Moves - 1,
    move_char(Character, XE, YE),
    move(NewMoves, Character, (XE, YE))
    ;
    % check if moved into entrance
    is_entrance(Pos) ->
    entrance(RoomEntered, Pos),
    get_room_center(RoomEntered, Center),
    Center = (XC, YC),
    move_char(Character, XC, YC), 
    suggestion(Character, RoomEntered), nl, !
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

% main game loop
game_loop :-
  game_state(finished),
  write('Game over!'), nl.
game_loop :-
  game_state(running),
  play_round.
  %guess -> retract(game_state(running)), assert(game_state(finished))
  %; (write('incorrect guess'), nl),
  % game_loop.

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
  get_room_center(Room, Center),
  Pos = (X, Y), % getting CurrChar pos, which is already center
  XC is X-1, % putting guessed character in room next to guesser
  move_char(Character, XC, Y), 
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
  append(CharCards, RandomCard, NewCharCards),
  retract(character(CurrChar, Pos, CharCards)),
  assert(character(CurrChar, Pos, NewCharCards))
  ).

% Win check
guess(Room) :-
  % let player know cards left that were not marked
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  write('Weapons you can guess: '), write(GuessableWeapons), nl,
  validate_guess(GuessableWeapons, 'What weapon was used?: ', Weapon), nl,
  write('Characters you can guess: '), write(GuessableChars), nl,
  validate_guess(GuessableChars, 'Who commited the crime?: ', Character), nl,
  winningcards(WinningCards),
  % using room currently entered for guess
  [Weapon, Character, Room] = WinningCards.

  