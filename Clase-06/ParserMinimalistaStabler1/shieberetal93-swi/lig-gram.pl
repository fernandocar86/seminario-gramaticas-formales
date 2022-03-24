%%% LIG grammar from Schabes thesis, page 186.

t-nil	 --->	[t-a84/0] 		.

%%% a84 (eats)

initroot(a84/0).
startnode(a84/0).

t-a84/0	 --->	[b-a84/0] 		.
t-a84/0	 --->	[t-b2/0]		.	% type 4a pred adjunction
b-a84/0	 --->	[t-b40/0] 		.	% type 4b mod adjunction
b-a84/0	 --->	[t-b41/0] 		.	% type 4b mod adjunction
b-a84/0	 --->	[t-a84/1, t-a84/2] 	. 
t-a84/1	 --->	[t-a85/0] 		.
t-a84/1	 --->	[t-a86/0] 		. 
t-a84/2	 --->	[b-a84/2] 		. 	% no adjunction
b-a84/2	 --->	[t-a84/21, t-a84/22] 	. 
t-a84/21 --->	[b-a84/21] 		.
b-a84/21 --->	[eats] 			.
t-a84/22 --->	[t-a85/0] 		.
t-a84/22 --->	[t-a86/0] 		.

%%% b40 (hungrily)

modifier(b40).
auxroot(b40/0).
auxfoot(b40/1).

t-b40/0	 --->	[b-b40/0] 		.	% type 3 no adjunction
b-b40/0	 --->	[t-b40/1, hungrily] 	.	% type 1/2
t-b40/1	 --->	[b-b40/1] 		.	% type 3 no adjunction
b-b40/1	 --->	[b-a84/0] 		. 	% type 5 end adjunction
b-b40/1	 --->	[b-b2/0] 		. 	% type 5 end adjunction

%%% b41 (yesterday)

modifier(b41).
auxroot(b41/0).
auxfoot(b41/1).

t-b41/0	 --->	[b-b41/0] 		.	% type 3 no adjunction
b-b41/0	 --->	[t-b41/1, yesterday] 	.	% type 1/2
t-b41/1	 --->	[b-b41/1] 		.	% type 3 no adjunction
b-b41/1	 --->	[b-a84/0] 		. 	% type 5 end adjunction
b-b41/1	 --->	[b-b2/0] 		. 	% type 5 end adjunction

%%% b2 (billsays)

predicative(b2).
auxroot(b2/0).
auxfoot(b2/2).

t-b2/0	 --->	[b-b2/0]		.	% type 3 no adjunction
t-b2/0	 --->	[t-b2/0]		.	% type 4a pred adjunction
b-b2/0	 --->   [t-b40/0]		.	% type 4b mod adjunction
b-b2/0	 --->   [t-b41/0]		.	% type 4b mod adjunction
b-b2/0	 ---> 	[billsays, t-b2/2]	.	% type 1/2
t-b2/2	 --->	[b-b2/2]		.	% type 3 no adjunction
b-b2/2	 --->	[b-a84/0]		.	% type 5 end adjunction
b-b2/2	 --->	[b-b2/0]		.	% type 5 end adjunction

%%% a85 (john)

initroot(a85/0).

t-a85/0	 --->	[b-a85/0] 		.
b-a85/0	 --->	[john] 			.

%%% a86 (peanuts)

initroot(a86/0).

t-a86/0	 --->	[b-a86/0] 		.
b-a86/0	 --->	[peanuts] 		. 
