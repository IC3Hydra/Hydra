Abstract
--------

The following describes standard functions for a contract implementing
the Monty Hall Problem as a two-contestant game. The contract
maintains a collection of ongoing games. A game is created by a
“house” and will accept game play by a single “contestant,” the first
account that sends a valid message initiating gameplay. Throughout
this specification, we use the notation ``$val`` to denote an amount of
currency—in this case, ether.

In our variant, the house specifies game parameters ``(k, n, reward)``,
where ``n`` is the total number of doors in the game and ``k`` is the number
of doors the house opens after the contestant makes its initial door
guess. reward is the amount paid to a contestant should her final door
guess be correct, i.e., ``$reward`` is sent to the contestant. The
contestant deposits an amount ``$bet`` that is forfeited to the house
should she guess incorrectly.

The lifecycle of a game involves an ordered sequence of calls to the
following functions.

Lifecycle of calls:
~~~~~~~~~~~~~~~~~~~

#. **InitMonty**: A house creates a new game, specifying ``(k, n, reward)``.
   It commits to a “winning door,” i.e., a door whose selection by the
   contestant results in the contestant receiving the prize ``($reward)``.
#. **PlayMontyRound1**: A contestant initiates play and guesses a door.
#. **OpenDoors**: The house opens ``k`` of the doors not selected by the
   contestant and of which none corresponds to the winning door.
#. **PlayMontyRound2**: The contestant updates her guessed door based on
   the information revealed in OpenDoors. She may reassert her initial
   guess or choose a different unopened door.
#. **EndGame**: The house reveals its door choice. If the player guessed
   correctly, she receives ``$reward``. Otherwise the house wins ``$bet``.


People generally intuit that the contestant has a 1/n probability of
winning, irrespective of her strategy. In fact, if she switches from
her initial choice to a different, unopened door, the probability of
her picking the winning door is ``(n - 1 / n) * (1 / (n - k- 1))``.

This may be seen as follows. With probability ``n - 1 / n``, the event
occurs that winning door is one other than that initially selected by
the contestant. Should this event occur, the house reduces the number
of valid door choices to ``n - k - 1`` and the player thus chooses the
winning door with probability ``1 / (n - k - 1)``.

Consequently, our contract requires ``$bet >= n - 1 / (n (n - k- 1))
* $reward``.

We assume that persistent memory in the contract (specifically,
*Monty_struct*, as specified below) is read by all players without
explicitly querying the contract, i.e., that it is read directly off
the blockchain.

A special function **EscapeHatch** is also included in the contract. It
may be called by the contract creator in order to ensure refunds to
houses of all remaining rewards.

Specification
-------------

For any function that returns ``bool success``, a returned value of false
meant to indicate that a function call failed. Specifically, we
enumerate a number of condition checks at the beginning of each
function specification. We also specify a failure action the function
should take should any condition check fail. This action may include
returning ``false``.

We specify the behavior of each function in terms of: a condition
check with an accompanying a failure action, a core operation, and a
lifecycle update. These three major elements (excluding the
conditional failure action) should be implemented respectively in the
indicated order.
Additionally, the EndGame function will carry out an optimistic operation after
the lifecycle update has been performed. The steps of this operation are
idempotent and may fail without consequence. The purpose of this operation is
to save transaction fees.

Any mathematical expressions in this specification are to be interpreted over the
rationals.

Unless otherwise specified, where relevant, bit representations are
big-endian.  `int128` is chosen as the canonical datatype for its
compatibility with both Solidity and Vyper.  Additional constraints
are placed on the variables when necessary.

The ``value`` and ``block_num`` types are not represented in the API, 
and any appropriate representation of Wei value and block number
can be chosen in the target language.

We assume an indexed global structure *Monty_struct[]*, indexed by a
unique per-game-instance value *game_id*, with the following values:

+ int128 *k*
+ int128 *n*
+ int128 *round1_door_guess*
+ int128 *round2_door_guess*
+ value *reward*
+ value *bet*
+ value *deposit*
+ account *house_acct*
+ account *contestant_acct*
+ bytes32 *winning_door_commit*
+ int128 *opened_doors*
+ int128 *winner*
+ int128 *lifecycle_step*
+ block_num *game_started*
+ block_num *last_action_blocknum*


*lifecycle_step* is an integer, instantiated to 0 in [-1,5] that denotes which step a game
instance has reached in the lifecycle of calls given above. For
example, *lifecycle_step* = 1 indicates that **InitMonty** has been called,
but not yet **PlayMontyRound1**.  *lifecycle_step* = -1 indicates an error
causing the termination of the game. *last_action_blocknum* tracks the block number
of the last lifecycle update, to refund games if one participant goes offline.

*winning_door_commit* takes the form `SHA256(winning_door || witness)`,
where winning_door is an 8-bit representation of a door value, witness
is a 256-bit string, and || denotes concatenation.

*winner* indicates who won the game. A value of 1 means that the house
won the game. A value of 2 means that the contestant won the game. Any
other value indicates that no winner has been determined.

*opened_doors* is expressed as an int128 with up to *k* bits set once the
doors have been opened by the house.  Before doors are opened by the house, 
it should hold the integer value 0.  Bits can be set in the range [0, n),
with 0 indicating the least significant bit of *opened_doors*.  Bits outside
the range [0, n) are ignored, and can be set to any value.

The meanings and formats of other variables should be clear from
context.

We abbreviate *Monty_struct[game_id]* with the notation MS where clear 
from context.  We use n to denote MS.n and k to denote MS.k when n, k 
are not function arguments.

We include a deposit for each game to ensure the house is not incentivized
to allow the game to expire for every loss (to save transaction fees).
In this specification, the required deposit is fixed at 0.1ETH.

We enforce an upper bound of 2^120 for *reward* and *bet* to ensure that

+ (*deposit* + *bet* + *reward*) will never overflow an int128
+ (*reward* * *n*-1) will never overflow an int128

Note that this bound still allows players to bet more than one quintillion
ether in a single game.

Whenever we send funds, we assume that the EVM's `CALL` opcode is used with a
gas limit of 0. (This is the behaviour of Solidity's ``send()`` function.)

For every send, we explicitly specify the contract's reaction in case of a send failure.
Any other external call failures should throw, discarding the transaction.
Condition validation errors trigger the failure action, except when the function
is payable, in which case they throw to rigorously avoid the contract processing
any money or changing any state from that call.  The success returns allow a
potential UI for this contract more fine-grained checks over what type
of error has potentially occured in a call over the universal use of throws.

Finally, we assume a global variable *bool HatchTriggered* that is
initialized to false upon contract creation, and a global variable
*address owner* initialized to the contract creator's account.
A special function EscapeHatch can be used to set the former to true. 
The effect of calling this function is to terminate the contract and 
allow refunds to be claimed. 

InitMonty
~~~~~~~~~

function InitMonty(int128 k, int128 n, bytes32 winning_door_commit) *payable*

**Conditions checks**:

+ HatchTriggered == false
+ n in [2,32]
+ k in [0, n-2]
+ msg.value in [required_deposit + 1, 2^120 + required_deposit]

**Failure action**:

Throw.

**Core operation**:

Generate 128-bit game_id sequentially, with the first game as 0. Create
Monty_struct[game_id]. Set MS.reward = msg.value - required_deposit,
MS.deposit = required_deposit, and MS.house_acct = sender. 
Assign all relevant values to MS, incl. n, k.  Initialize bet to 0.
Set game_started to current block number.

**Lifecycle update**:

Set MS.lifecycle_step = 1.
Update MS.last_action_blocknum to current block height. 

PlayMontyRound1
~~~~~~~~~~~~~~~

function PlayMontyRound1(int128 round1_door_guess, int128 game_id) *payable*

**Conditions checks**:

+ HatchTriggered == false
+ game_id >= 0, is valid game
+ MS.lifecycle_step == 1
+ round1_door_guess in range [0, n)
+ MS.contestant_acct not set yet (is default value)
+ msg.value in [MS.reward * ((n-1) / (n (n - k -1))), 2^120]
  
**Failure action**:

Throw.

**Core operation**:

Set MS.contestant_acct = sender and MS.bet = msg.value. Assign all
relevant values to MS.

**Lifecycle update**:

Set MS.lifecycle_step = 2.
Update MS.last_action_blocknum to current block height. 

OpenDoors
~~~~~~~~~

function OpenDoors(int128 opened_doors, int128 game_id) returns (bool success)

**Conditions checks**:

+ HatchTriggered == false
+ game_id >= 0, is valid game
+ MS.lifecycle_step == 2
+ opened_doors indicates exactly MS.k doors that are distinct from MS.round1_door_guess (with bits outside the range [0, n) ignored).
+ sender is MS.house_acct

**Failure action**:

Return false; ensure no state change.

**Core operation**:

Store opened doors in MS.

**Lifecycle update**:

Set MS.lifecycle_step = 3.
Update MS.last_action_blocknum to current block height. 

PlayMontyRound2
~~~~~~~~~~~~~~~

function PlayMontyRound2(int128 round2_door_guess, int128 game_id)
returns (bool success)

**Conditions checks**:

+ HatchTriggered == false
+ game_id >= 0, is valid game
+ MS.lifecycle_step == 3
+ round2_door_guess in range [0, n)
+ round2_door_guess corresponds to an unopened door, as specified by MS.opened_doors.
+ sender is MS.contestant_acct

**Failure action**:

Return false; ensure no state change.

**Core operation**:

Assign MS.round2_door_guess = round2_door_guess.

**Lifecycle update**:

Set MS.lifecycle_step = 4.
Update MS.last_action_blocknum to current block height. 

EndGame
~~~~~~~

function EndGame(int128 winning_door, bytes32 witness, int128 game_id)
returns (bool success)

**Conditions checks**:

+ HatchTriggered == false
+ game_id >= 0, is valid game
+ MS.lifecycle_step == 4
+ (winning_door, witness) represents a correct decommitment of MS.winning_door_commit
+ winning_door in [0, n)
+ winning_door does not correspond to an opened door, i.e., a door specified in MS.opened_doors.
+ sender is MS.house_acct

**Failure action**:

Return false; ensure no state change.

**Core operation**:

If winning_door == MS.round2_door_guess, then set MS.winner to
2. Otherwise, set MS.winner to 1.

**Lifecycle update**:

Set MS.lifecycle_step = 5.
Update MS.last_action_blocknum to current block height. 

**Optimistic operation**:

Call Payout(false, game_id) and ignore the return value.

Call Payout(true, game_id) and ignore the return value.

Payout
~~~~~~

function Payout(bool house, int128 game_id)
returns (bool success)

**Conditions checks:**

+ HatchTriggered == false
+ game_id >=0, is a valid game
+ MS.winner == 1 or MS.winner == 2
+ MS.lifecycle_step == 5

**Failure action**:

Return false; ensure no state change.

**Core operation**:

If MS.winner is 1 and house is true, send ($MS.deposit + $MS.bet + $MS.reward) to MS.house_acct.
If the send succeeds, set $MS.deposit, $MS.bet, and $MS.reward to zero. Otherwise, immediately return false.

If MS.winner is 2 and house is true, send $MS.deposit to MS.house_acct.
If the send succeeds, set $MS.deposit to zero. Otherwise, immediately return false.

If MS.winner is 2 and house is false, send ($MS.bet + $MS.reward) to MS.contestant_acct.
If the send succeeds, set $MS.bet and $MS.reward to zero. Otherwise, immediately return false.

**Lifecycle update**:

None.

EscapeHatch
~~~~~~~~~~~

function EscapeHatch() returns (bool success)

**Conditions checks**:

+ Caller is owner.

**Failure action**:

Return false; ensure no state change.

**Core operation**:

Set global variable HatchTriggered = true.

**Lifecycle update**:

None.


RefundInactive
~~~~~~~~~~~~~~

function RefundInactive(int128 game_id)
returns (bool success)

**Conditions checks**:

+ HatchTriggered == false
+ game_id >= 0, is valid game
+ block_num - MS.last_action_blocknum > 14400 (2 days @ 7200 blocks a day)
+ lifecycle is in [1, 4]
+ if lifecycle is 1 or 3, sender is MS.house_acct
+ if lifecycle is 2 or 4, sender is MS.contestant_acct
  
**Failure action**:

Return false; ensure no state change.

**Core operation**:

Send ($MS.deposit + $MS.bet + $MS.reward) to sender.  If the send succeeds, set
$MS.deposit, $MS.bet, and $MS.reward to zero. Otherwise, immediately return
false.

**Lifecycle update**:

Set MS.lifecycle_step = -1.


RefundAfterEscapeHatch
~~~~~~~~~~~~~~~~~~~~~~

function RefundAfterEscapeHatch(int128 game_id)
returns (bool success)

**Conditions checks**:

+ HatchTriggered == true
+ game_id >= 0, is valid game
+ sender is MS.house_acct or sender is MS.contestant_acct

**Failure action**:

Return false; ensure no state change.

**Core operation**:

If sender is MS.house_acct, send ($MS.deposit + $MS.reward) to MS.house_acct.
If send succeeds, set $MS.deposit and $MS.reward to zero. Otherwise, immediately return false.

If sender is MS.contestant_acct, send ($MS.bet) to MS.contestant_acct.
If send succeeds, set $MS.bet to zero. Otherwise, immediately return false.

**Lifecycle update**:

None.


IsOpened
~~~~~~~~

function IsOpened(int128 door_num, int128 game_id) constant
returns (bool opened)

**Conditions checks**:

+ game_id >= 0, is valid game
+ door_num in [0, n)
+ MS.lifecycle_step in [3, 5]

**Failure action**:

Return false; ensure no state change.

**Core operation**:

If bit door_num is set in MS.opened_doors, return true.
0 is indexed as the least significant bit.
Otherwise return false.

**Lifecycle update**:

None.
