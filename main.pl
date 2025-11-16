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

% Two die rolls between 1 and 12
roll_dice(Moves) :-
  random_between(1, 12, Moves).

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

% recursive moving until runs out of moves
move(0,_,_) :-
  write('Moves complete'), nl,
  write('--------------------------'), nl.
move(Moves, Character, Pos) :-
  Pos = (X, Y),
  write('Moves left: '), write(Moves), nl,
  (
    %TODO: end moves when got to entrance, suggestion feature
    is_entrance(Pos) -> suggestion(Character), nl
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

suggestion(CurrChar) :-
  character(CurrChar, Pos, CharCards),
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  subtract([kitchen, ballroom, conservatory, billiard_room, library, study, hall, lounge, dining_room], CharCards, GuessableRooms),
  write('Your current cards: '), write(CharCards), nl,
  write('Enter your suggestion: '), nl,
  write('Weapons you can guess: '), write(GuessableWeapons), nl,
  validate_guess(GuessableWeapons, 'What weapon was used?: ', Weapon), nl,
  write('Characters you can guess: '), write(GuessableChars), nl,
  validate_guess(GuessableChars, 'Who commited the crime?: ', Character), nl,
  write('Rooms you can guess: '), write(GuessableRooms), nl, 
  validate_guess(GuessableRooms, 'Which room was the crime committed?: ', Room), nl,
  % our version gets random card from the three cards guessed not in winning pile
  winningcards(WinningCards),
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
guess(CurrChar) :-
  % let player know cards left that were not marked
  subtract([candlestick, dagger, lead_pipe, revolver, rope, wrench], CharCards, GuessableWeapons),
  subtract([miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum], CharCards, GuessableChars),
  subtract([kitchen, ballroom, conservatory, billiard_room, library, study, hall, lounge, dining_room], CharCards, GuessableRooms),
  write('Weapons you can guess: '), write(GuessableWeapons), nl,
  validate_guess(GuessableWeapons, 'What weapon was used?: ', Weapon), nl,
  write('Characters you can guess: '), write(GuessableChars), nl,
  validate_guess(GuessableChars, 'Who commited the crime?: ', Character), nl,
  write('Rooms you can guess: '), write(GuessableRooms), nl, 
  validate_guess(GuessableRooms, 'Which room was the crime committed?: ', Room), nl,
  winningcards(WinningCards),
  [Weapon, Character, Room] = WinningCards.

  