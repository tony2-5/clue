:- [gamesetup].

% main game loop
setupgame :-
  print_board,
  winning_cards,
  distribute_cards.