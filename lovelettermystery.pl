%  Syntax may differ in other Prolog interpreters. 
:- dynamic fact(1) .
:- dynamic p/2.
:- dynamic p/3.

% Operator definitions for forward chaining if-then rules.
:- op(800,fx,if).
:- op(700,xfx,then).
:- op(300,xfy,or).
:- op(200,xfy,and).

% BN

parent(s_love_ana, s_send_letter).
parent(s_love_ana, s_parents_tease). 
parent(s_send_letter, s_inky_fingers). 
parent(s_send_letter, ana_get_letter). 
parent(a_send_letter, ana_get_letter).  
parent(a_send_letter, a_post_office). 
parent(a_jeal_sam, a_grudge_sam). 
parent(a_jeal_sam, a_send_letter).

% BN prior probabilities

p(s_love_ana, 0.70).
p(a_jeal_sam, 0.40).   

% BN conditional probabilities

p(s_parents_tease, [s_love_ana], 0.80).
p(s_parents_tease, [not(s_love_ana)], 0.10).

p(a_grudge_sam, [a_jeal_sam], 0.60).
p(a_grudge_sam, [not(a_jeal_sam)], 0.40).

p(s_inky_fingers, [s_send_letter], 0.65).
p(s_inky_fingers, [not(s_send_letter)], 0.20).

p(a_post_office, [a_send_letter], 0.50).
p(a_post_office, [not(a_send_letter)], 0.01).

p(a_send_letter, [a_jeal_sam], 0.60).
p(a_send_letter, [not(a_jeal_sam)], 0.10).

p(s_send_letter, [s_love_ana], 0.90).
p(s_send_letter, [not(s_love_ana)], 0.01).

p(ana_get_letter, [s_send_letter, a_send_letter], 0.99).
p(ana_get_letter, [s_send_letter, not(a_send_letter)], 0.70).
p(ana_get_letter, [not(s_send_letter), a_send_letter], 0.40).
p(ana_get_letter, [not(s_send_letter), not(a_send_letter)], 0.01).

% FC rules

if ana_get_letter and a_send_letter then alex_sender.

if alex_sender and a_crush_ana then alex_confesses.
if alex_confesses and s_a_friends then alex_apologizes.
if alex_confesses and s_a_enemies then sam_insulted.
if alex_confesses and ana_date then alex_ana_date.
if alex_confesses and ana_celibate then ana_stays_single.

if alex_sender and a_nocrush_ana then alex_impersonates_sam.
if alex_impersonates_sam and s_a_friends then sam_insulted.
if alex_impersonates_sam and s_a_enemies then alex_apologizes.
if alex_impersonates_sam and ana_date then ana_stays_single.
if alex_impersonates_sam and ana_celibate then ana_stays_single.

if alex_apologizes and s_a_friends then sam_alex_make_up.
if alex_apologizes and s_a_enemies then sam_alex_not_friends.

if ana_get_letter and s_send_letter then sam_sender.

if sam_sender and a_crush_ana then sam_alex_make_up.
if sam_sender and a_nocrush_ana then sam_alex_not_friends.
if sam_sender and ana_date then sam_ana_date.
if sam_sender and ana_celibate then ana_stays_single.

if sam_ana_date and sam_alex_make_up then ana_alex_friends.
if sam_ana_date and sam_alex_not_friends then ana_alex_not_friends.
if alex_ana_date and sam_alex_make_up then sam_ana_friends.
if alex_ana_date and sam_alex_not_friends then sam_ana_not_friends.

% Call to start dialogue with user.

story :- solve_mystery, finish_story, forward.

% solve_mystery asks the user for the observations,
% then calls the BN using the prob predicate,
% then figures out the Act with the highest probability,
% then informs the user as in the sample dialogue, then
% then asserts the solution to the mystery as a fact
% that will be used by the FC rules. 

solve_mystery :- get_observations(Tease, Inky, Office, Grudge),  
      prob(s_send_letter, [ana_get_letter, Tease, Inky, Office, Grudge], Psam),
      prob(a_send_letter, [ana_get_letter, Tease, Inky, Office, Grudge], Palex),
      write_ans(Psam, Palex).

get_observations(Tease, Inky, Office, Grudge) :-
   write('Was Sam teased about having a crush by his parents? (y or n): '), read(Tease_in), nl,
   trans_tease(Tease_in, Tease),
   write('Did Sam have ink-stained fingers? (y or n): '), read(Inky_in), nl,
   trans_inky(Inky_in, Inky),
   write('Was Alex seen at the post office? (y or n): '), read(Office_in), nl,
   trans_postoffice(Office_in, Office),
   write('Does Alex refuse to talk to Sam? (y or n): '), read(Grudge_in), nl,
   trans_grudge(Grudge_in, Grudge).

trans_tease(y, s_parents_tease).
trans_tease(n, not(s_parents_tease)).
trans_inky(y, s_inky_fingers).
trans_inky(n, not(s_inky_fingers)).
trans_postoffice(y, a_post_office).
trans_postoffice(n, not(a_post_office)).
trans_grudge(y, a_grudge_sam).
trans_grudge(n, not(a_grudge_sam)).

write_ans(Psam, Palex) :-
  write('The probability that Sam sent the love letter is '),
  write(Psam), nl,
  write('The probability that Alex sent the love letter is '),
  write(Palex), nl,
  compare(Psam, Palex).

compare(Psam, Palex) :- Palex > Psam,
  write('Therefore, it is most likely that Alex sent the letter.'),
  assert(fact(a_send_letter)), assert(fact(ana_get_letter)), nl.
compare(Psam, Palex) :- Palex < Psam,
  write('Therefore, it is most likely that Sam sent the letter.'),
  assert(fact(s_send_letter)), assert(fact(ana_get_letter)), nl.
compare(Psam, Palex) :- Palex = Psam,
  write('Therefore, both Alex and Sam equally could have sent the letter.').

% finish_story asks 3 questions to get user preferences,
% then asserts the answers as facts.  After this returns,
% the story predicate calls forward to start forward chaining.

finish_story :- write('Does Alex have a crush on Ana? (y or n) '), 
      read(Crush), nl, assert_crush(Crush), 
      write('Does Sam think he is friends with Alex? (y or n) '), 
      read(Friend), nl, assert_friend(Friend), 
      write('Does Ana want a relationship? (y or n) '), 
      read(Date), nl, assert_date(Date).
      
assert_crush(y) :- assert(fact(a_crush_ana)).
assert_crush(n) :- assert(fact(a_nocrush_ana)).
assert_friend(y) :- assert(fact(s_a_friends)).
assert_friend(n) :- assert(fact(s_a_enemies)).
assert_date(y) :- assert(fact(ana_date)).
assert_date(n) :- assert(fact(ana_celibate)).