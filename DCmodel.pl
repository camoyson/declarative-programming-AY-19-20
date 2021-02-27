route2( A, B ) :-
	arc_either_way( A, B ).
route2( A, B ) :-
	arc_either_way( A, C ),
	route( C, B ).

arc_either_way( A, B ) :-
	arc( A, B, _, _, _ ).
arc_either_way( A, B ) :-
	arc( B, A, _, _, _ ).

route3( A, B ) :-
	route3( A, B, [A] ).

route3( A, B, SoFar ) :-
	arc_either_way( A, B ),
	\+ member( B, SoFar ).
route3( A, B, SoFar ) :-
	arc_either_way( A, C ),
	\+ member( C, SoFar ),
	route3( C, B, [C|SoFar] ).

route4( A, B, Route ) :-
	route4( A, B, [A], Route ).

route4( A, B, SoFar, [B|SoFar] ) :-
	arc_either_way( A, B ),
	\+ member( B, SoFar ).
route4( A, B, SoFar, Route ) :-
	arc_either_way( A, C ),
	\+ member( C, SoFar ),
	route4( C, B, [C|SoFar], Route ).

distance( A, B, Distance ) :-
	node( A, LongA, LatA ),
	node( B, LongB, LatB ),
	Distance is sqrt(((LongA-LongB)*88)^2+((LatA-LatB)*111)^2).

nearest( Long, Lat, Node ) :-
	node( Node, LongN, LatN ),
	Distance is sqrt(((Long-LongN)*88)^2+((Lat-LatN)*111)^2),
	\+ ( node( Other, LongO, LatO ),
	     \+ Other = Node,
	     Distance > sqrt(((Long-LongO)*88)^2+((Lat-LatO)*111)^2)).

route_distance( A, B, Route, Distance ) :-
	route_distance( A, B, [A], Route, Distance ).

route_distance( A, B, SoFar, [B|SoFar], Distance ) :-
	arc_either_way( A, B ),
	\+ member( B, SoFar ),
	distance( A, B, Distance ).
route_distance( A, B, SoFar, Route, FinalDistance ) :-
	arc_either_way( A, C ),
	\+ member( C, SoFar ),
	distance( A, C, Distance ),
	route_distance( C, B, [C|SoFar], Route, MoreDistance ),
	FinalDistance is Distance + MoreDistance.	

bfs_terminate( [End|_], End ).

bfs_expand( [FirstAgendaItem|Rest], [NewAgendaItem,FirstAgendaItem|Rest] ) :-
	arc_either_way( FirstAgendaItem, NewAgendaItem ),
	\+ member( NewAgendaItem, Rest ).
