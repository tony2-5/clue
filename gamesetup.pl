% game board size: 24x24
% top left = (0,0)

/* rooms with boundries (name, topleft-x,y, bottomright-x,y) */
room(kitchen, (0,0), (5,5)).
room(ballroom, (8,0), (15,6)).
room(conservatory, (18,0), (23,4)).
room(billiard_room, (18,7), (23,11)).
room(library, (17,13), (23,17)).
room(study, (17,20), (23,23)).
room(dining_room, (0,8), (7,14)).
room(lounge, (0,18), (6,23)).
room(hall, (9,17), (14,23)).
room(center, (10,9), (14, 15)).

/* room entrances- x,y coordinates */
entrance(kitchen, (4, 5)).
entrance(ballroom, (8, 4)).
entrance(ballroom, (9, 6)).
entrance(ballroom, (14, 6)).
entrance(ballroom, (15, 4)).
entrance(conservatory, (18, 4)).
entrance(billiard_room, (18, 8)).
entrance(billiard_room, (22, 11)).
entrance(library, (17, 15)).
entrance(library, (20, 13)).
entrance(study, (17, 20)).
entrance(hall, (14, 19)).
entrance(hall, (12, 17)).
entrance(hall, (11, 17)).
entrance(lounge, (6, 18)).
entrance(dining_room, (6, 8)).
entrance(dining_room, (7, 12)).

/* Exits for leaving room */
exit(kitchen, (4, 6)).
exit(ballroom, (7, 4)).
exit(ballroom, (9, 7)).
exit(ballroom, (14, 7)).
exit(ballroom, (16, 4)).
exit(conservatory, (17, 4)).
exit(billiard_room, (17, 8)).
exit(billiard_room, (22, 12)).
exit(library, (16, 15)).
exit(library, (20, 12)).
exit(study, (17, 19)).
exit(hall, (15, 19)).
exit(hall, (12, 16)).
exit(hall, (11, 16)).
exit(lounge, (6, 17)).
exit(dining_room, (6, 7)).
exit(dining_room, (8, 12)).

/* secret paths */
passage(kitchen, study).
passage(study, kitchen).
passage(conservatory, lounge).
passage(lounge, conservatory).

/* characters with starting position */
% dynamic as position and assigned cards can change
:- dynamic character/3.
init_chars :-
  assert(character(miss_scarlett, (7,23), [])),
  assert(character(colonel_mustard, (0,16), [])),
  assert(character(mrs_white, (7,0), [])),
  assert(character(reverend_green, (16, 0), [])),
  assert(character(mrs_peacock, (23, 5), [])),
  assert(character(professor_plum, (23, 18), [])).

/* weapons */
weapon(candlestick).
weapon(dagger).
weapon(lead_pipe).
weapon(revolver).
weapon(rope).
weapon(wrench).

/* Set winning cards/card distribution */
:- dynamic winningcards/1.
winningcards([]).
% get winning cards and dynamically assert
winning_cards :-
  random_member(X, [candlestick, dagger, lead_pipe, revolver, rope, wrench]),
  random_member(Y, [miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum]),
  random_member(Z, [kitchen, ballroom, conservatory, billiard_room, library, study, hall, lounge, dining_room]),
  assert(winningcards([X,Y,Z])).
  % write('Winning cards: '), write([X,Y,Z]), nl. uncomment to display winning cards at game start
  
% distribute cards not including cards from winning card set
distribute_cards :-
  Weapons = [candlestick, dagger, lead_pipe, revolver, rope, wrench],
  Characters = [miss_scarlett, colonel_mustard, mrs_white, reverend_green, mrs_peacock, professor_plum],
  Rooms = [kitchen, ballroom, conservatory, billiard_room, library, study, hall, lounge, dining_room],
  append([Weapons, Characters, Rooms], AllCards),
  winningcards(WinningCards),
  subtract(AllCards, WinningCards, RemainingCards),
  random_permutation(RemainingCards, ShuffledCards),
  distribute_to_characters(ShuffledCards, Characters).

% recursive function to distribute cards
% each character gets 3 cards remaining 18, 18/6 characters = 3
distribute_to_characters([], []).
distribute_to_characters([C1,C2,C3|RestCards], [Char|RestChars]) :-
    character(Char, Pos, _),
    retract(character(Char, Pos, _)),
    assert(character(Char, Pos, [C1,C2,C3])),
    % write(Char), write([C1,C2,C3]), nl, uncomment to see which cards each player has at game start
    distribute_to_characters(RestCards, RestChars).

/* print board */
% make sure position is within 24x24 play space
valid_position(Pos) :- 
  (X, Y) = Pos,
  X >= 0, Y =< 23, Y >= 0, X =< 23.

% determine if current position in room
in_room(Pos, Room) :-
  room(Room, (X1, Y1), (X2, Y2)),
  Pos = (X, Y),
  X >= X1, X =< X2,
  Y >= Y1, Y =< Y2,
  \+ is_entrance(Pos).

% hallway position if is a valid position and not in a room and not a character
is_hallway(Pos) :- valid_position(Pos), \+ in_room(Pos, _), \+ is_character(Pos, _).

is_entrance(Pos) :- entrance(_, Pos).

is_character(Pos, Character) :- character(Character, Pos, _).

% given position print cell type
print_cell(X, Y) :-
  Pos = (X, Y),
  ( is_entrance(Pos) -> write('E  ')
  ; is_character(Pos, miss_scarlett) -> write('MS ')
  ; is_character(Pos, colonel_mustard) -> write('CM ')
  ; is_character(Pos, mrs_white) -> write('MW ')
  ; is_character(Pos, reverend_green) -> write('RG ')
  ; is_character(Pos, mrs_peacock) -> write('MP ')
  ; is_character(Pos, professor_plum) -> write('PP ')
  ; in_room(Pos, kitchen) -> write('K  ')
  ; in_room(Pos, ballroom) -> write('Ba ')
  ; in_room(Pos, conservatory) -> write('C  ')
  ; in_room(Pos, billiard_room) -> write('Bi ')
  ; in_room(Pos, library) -> write('Li ')
  ; in_room(Pos, study) -> write('S  ')
  ; in_room(Pos, hall) -> write('H  ')
  ; in_room(Pos, lounge) -> write('Lo ')
  ; in_room(Pos, dining_room) -> write('Di ')
  ; in_room(Pos, center) -> write('Ce ')
  ; is_hallway(Pos) -> write('x  ')
  ).

print_row(Y) :- forall(between(0, 23, X), print_cell(X, Y)), nl.
print_board :- forall(between(0, 23, Y), print_row(Y)).

/* cleanup for all dynamic variables */
cleanup :-
  retractall(winningcards(_)),
  retractall(character(_,_,_)),
  retractall(agent(_)),
  retractall(game_state(_)).
