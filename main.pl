:- [gamesetup].

% main game loop
:- dynamic game_state/1.
setup_game :-
  retractall(game_state(_)),
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

move_char(Character, X, Y) :-
  NewPos = (X, Y),
  character(Character, OldPos, Cards),
  retract(character(Character, OldPos, Cards)),
  assert(character(Character, NewPos, Cards)),
  print_board.

move(0,_,_) :-
  write('Moves complete'), nl,
  write('--------------------------'), nl.
move(Moves, Character, Pos) :-
  Pos = (X, Y),
  write('Moves left: '), write(Moves), nl,
  (
    is_entrance(Pos) -> write('Entrance'), nl % end moves when got to entrance, suggestion feature
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


% Win check
guess :-
  write('Enter your guess: '), nl,
  write('What weapon was used?: '), read(Weapon), nl,
  write('Who commited the crime?: '), read(Character), nl,
  write('Which room was the crime committed?: '), read(Room), nl,
  winningcards(A),
  [Weapon, Character, Room] = A.

  