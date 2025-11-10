:- [gamesetup].

% main game loop
:- dynamic game_state/1.
setupgame :-
  retractall(game_state(_)),
  print_board,
  winning_cards,
  distribute_cards,
  assert(game_state(running)),
  game_loop.

game_loop :-
  game_state(finished),
  write('Game over!'), nl.
game_loop :-
  game_state(running),
  guess -> retract(game_state(running)), assert(game_state(finished))
  ; (write('incorrect guess'), nl),
  game_loop.

% Win check
guess :-
  write('Enter your guess: '), nl,
  write('What weapon was used?: '), read(Weapon), nl,
  write('Who commited the crime?: '), read(Character), nl,
  write('Which room was the crime committed?: '), read(Room), nl,
  winningcards(A),
  [Weapon, Character, Room] = A.

% Two die rolls between 1 and 12
rolldice(Moves) :-
  random_between(1, 12, Moves).
  