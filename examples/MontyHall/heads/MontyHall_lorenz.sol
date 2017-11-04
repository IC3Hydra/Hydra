// NOT UP TO DATE!

pragma solidity ^0.4.10;

interface MontyHallInterface {
    function InitMonty(int128 k, int128 n, bytes32 winning_door_commit) payable;
    function PlayMontyRound1(int128 round1_door_guess, int128 game_id) payable;
    function OpenDoor(bool[32] opened_doors, uint256 game_id) returns (bool success);
    function PlayMontyRound2(byte round2_door_guess, uint256 game_id) returns (bool success);
    function EndGame(byte winning_door, bytes32 witness, uint256 game_id);
    function EscapeHatch() returns (bool success);
    function refund(int128 game_id) returns (bool success);
}

contract MontyHall /* is MontyHallInterface */ {
    int128 constant LIFECYCLE_InitMonty_CALLED = 1;
    int128 constant LIFECYCLE_PlayMontyRound1_CALLED = 2;
    int128 constant LIFECYCLE_OpenDoor_CALLED = 3;
    int128 constant LIFECYCLE_PlayMontyRound2_CALLED = 4;
    int128 constant LIFECYCLE_EndGame_CALLED = 5;
    int128 constant LIFECYCLE_Refund_CALLED = -1;
    
    // Spec: We include a deposit for each game to ensure the house is not 
    // incentivized to allow the game to expire for every loss (to save 
    // transaction fees). In this specification, the required deposit is fixed 
    // at 0.1ETH.
    uint256 constant REQUIRED_DEPOSIT = 1 ether / 10;
    
    struct Game {
        int128 k;
        int128 n;
        int128 round1_door_guess;
        int128 round2_door_guess;
        uint256 reward;
        uint256 bet;
        uint256 deposit;
        address house_acct;
        address contestant_acct;
        int128 game_id;
        bytes32 winning_door_commit;
        bool[32] opened_doors;
        int128 lifecycle_step;
        uint game_started;
        uint last_action;
    }
    
    mapping(int128 => Game) MS;
    
    bool HatchTriggered;
    
    address internal creator;
    
    int128 game_counter;

    function MontyHall() {
        creator = msg.sender;
        game_counter = 0;
    }
    
    function InitMonty(int128 _k, int128 _n, bytes32 _winning_door_commit) payable {
        if (!(2 <= _n && _n <= 32)) {
            throw;
        }
        if (!(0 <= _k && _k <= _n-2)) {
            throw;
        }
        if (!(msg.value > REQUIRED_DEPOSIT)) {
            throw;
        }
        if (!(HatchTriggered == false)) {
            throw;
        }
        
        int128 game_id = game_counter;
        game_counter += 1;
        
        if (game_id > game_counter) {
            throw;
        }
        
        MS[game_id].k = _k;
        MS[game_id].n = _n;

        MS[game_id].reward = msg.value - REQUIRED_DEPOSIT;
        MS[game_id].deposit = REQUIRED_DEPOSIT;

        MS[game_id].house_acct = msg.sender;
        MS[game_id].game_id = game_id;
        MS[game_id].winning_door_commit = _winning_door_commit;
        MS[game_id].game_started = block.number;
        
        /* Lifecycle update */
        MS[game_id].lifecycle_step = LIFECYCLE_InitMonty_CALLED;
        MS[game_id].last_action = block.number;
    }
    
    // Compute minimum bet for a game. Returns 0 if an overflow occurs and the
    // (rounded-up) minimum bet in wei otherwise.
    function MinimumBet(int128 _game_id) constant internal returns (uint256) {
        uint256 reward = MS[_game_id].reward;
        uint256 n = uint256(MS[_game_id].n);
        uint256 k = uint256(MS[_game_id].k);
        if ((reward * (n-1)) / (n-1) != reward) {
            return 0;
        }
        
        uint256 denominator = n * (n-k-1);
        if (reward * (n-1) > reward * (n-1) + (denominator-1)) {
            return 0;
        }
        
        return (reward * (n-1) + (denominator-1))/denominator;
    }
    
    function PlayMontyRound1(int128 _round1_door_guess, int128 _game_id) payable {
        if (!(0 <= _game_id && _game_id < game_counter)) {
            throw;
        }
        if (!(0 <= _round1_door_guess && _round1_door_guess < MS[_game_id].n)) {
            throw;
        }
        if (!(MS[_game_id].contestant_acct == 0)) {
            throw;
        }
        if (!(MS[_game_id].lifecycle_step == LIFECYCLE_InitMonty_CALLED)) {
            throw;
        }
        uint256 minimum_bet = MinimumBet(_game_id);
        if (minimum_bet == 0 || !(msg.value >= minimum_bet)) {
            throw;
        }
        if (!(HatchTriggered == false)) {
            throw;
        }
        
        MS[_game_id].round1_door_guess = _round1_door_guess;
        MS[_game_id].contestant_acct = msg.sender;
        MS[_game_id].bet = msg.value;
        
        /* Lifecycle update */
        MS[_game_id].lifecycle_step = LIFECYCLE_PlayMontyRound1_CALLED;
        MS[_game_id].last_action = block.number;
    }
    
    function CountOpened(bool[32] _opened_doors, uint8 _n) constant internal returns (uint8 opened) {
        opened = 0;
        for (uint8 i = 0; i < _n; i++){
            if (_opened_doors[i]) {
                opened++;
            }
        }
    }
    
    function OpenDoor(bool[32] _opened_doors, int128 _game_id) returns (bool success) {
        if (!(0 <= _game_id && _game_id < game_counter)) {
            return false;
        }
        if (!(MS[_game_id].lifecycle_step == LIFECYCLE_PlayMontyRound1_CALLED)) {
            return false;
        }
        if (!(CountOpened(_opened_doors, uint8(MS[_game_id].n)) == MS[_game_id].k)) {
            return false;
        }
        if (!(_opened_doors[uint(MS[_game_id].round1_door_guess)] == false)) {
            return false;
        }
        if (!(HatchTriggered == false)) {
            return false;
        }
        if (!(msg.sender == MS[_game_id].house_acct)) {
            return false;
        }
        
        MS[_game_id].opened_doors = _opened_doors;
        
        /* Lifecycle update */
        MS[_game_id].lifecycle_step = LIFECYCLE_OpenDoor_CALLED;
        MS[_game_id].last_action = block.number;
        
        return true;
    }

    function PlayMontyRound2(int128 _round2_door_guess, int128 _game_id) returns (bool success) {
        if (!(0 <= _game_id && _game_id < game_counter)) {
            return false;
        }
        if (!(MS[_game_id].lifecycle_step == LIFECYCLE_OpenDoor_CALLED)) {
            return false;
        }
        if (!(0 <= _round2_door_guess && _round2_door_guess < MS[_game_id].n)) {
            return false;
        }
        if (!(MS[_game_id].opened_doors[uint(_round2_door_guess)] == false)) {
            return false;
        }
        if (!(HatchTriggered == false)) {
            return false;
        }
        if (!(msg.sender == MS[_game_id].contestant_acct)) {
            return false;
        }
        
        MS[_game_id].round2_door_guess = _round2_door_guess;
        
        /* Lifecycle update */
        MS[_game_id].lifecycle_step = LIFECYCLE_PlayMontyRound2_CALLED;
        MS[_game_id].last_action = block.number;
        
        return true;
    }
    
    function Commitment(int128 _winning_door, bytes32 _witness) constant internal returns (bytes32 hash) {
        hash = sha256(byte(_winning_door), _witness);
    }
    
    function EndGame(int128 _winning_door, bytes32 _witness, int128 _game_id) returns (bool success) {
        if (!(0 <= _game_id && _game_id < game_counter)) {
            return false;
        }
        if (!(Commitment(_winning_door, _witness) == MS[_game_id].winning_door_commit)) {
            return false;
        }
        if (!(MS[_game_id].lifecycle_step == LIFECYCLE_PlayMontyRound2_CALLED)) {
            return false;
        }
        if (!(0 <= _winning_door && _winning_door < MS[_game_id].n)) {
            return false;
        }
        if (!(MS[_game_id].opened_doors[uint(_winning_door)] == false)) {
            return false;
        }
        if (!(HatchTriggered == false)) {
            return false;
        }
        if (!(msg.sender == MS[_game_id].house_acct)) {
            return false;
        }
        
        uint256 bet = MS[_game_id].bet;
        uint256 deposit = MS[_game_id].deposit;
        uint256 reward = MS[_game_id].reward;
        if (_winning_door == MS[_game_id].round2_door_guess) {
            if (!MS[_game_id].house_acct.send(deposit)) {
                return false;
            }
            // Unchecked send to prevent DoS by contestant
            if(MS[_game_id].contestant_acct.send(bet + reward)){}
        } else {
            if (!MS[_game_id].house_acct.send(bet + deposit + reward)) {
                return false;
            }
        }
        
        /* Lifecycle update */
        MS[_game_id].lifecycle_step = LIFECYCLE_EndGame_CALLED;
        MS[_game_id].last_action = block.number;
        
        return true;
    }
    
    function EscapeHatch() returns (bool success) {
        if (!(msg.sender == creator)) {
            return false;
        }
        
        HatchTriggered = true;
        return true;
    }
    
    function Implies(bool _antecedent, bool _consequent) constant internal returns (bool) {
        return !_antecedent || _consequent;
    }
    
    function refund(int128 _game_id) returns (bool success) {
        if (!(0 <= _game_id && _game_id < game_counter)) {
            return false;
        }
        uint256 bet = MS[_game_id].bet;
        uint256 deposit = MS[_game_id].deposit;
        uint256 reward = MS[_game_id].reward;
        if (!(bet + reward + deposit > 0)) {
            return false;
        }
        if (!(HatchTriggered == true || block.number - MS[_game_id].last_action > 14400)) {
            return false;
        }
        if (!Implies(HatchTriggered == false, 
                     LIFECYCLE_InitMonty_CALLED <= MS[_game_id].lifecycle_step 
                     && MS[_game_id].lifecycle_step <= LIFECYCLE_PlayMontyRound2_CALLED)) {
            return false;
        }
        if (!Implies(HatchTriggered == false && 
                     (MS[_game_id].lifecycle_step == LIFECYCLE_InitMonty_CALLED
                      || MS[_game_id].lifecycle_step == LIFECYCLE_OpenDoor_CALLED)
                    , msg.sender == MS[_game_id].contestant_acct)) {
            return false;            
        }
        if (!Implies(HatchTriggered == false && 
                     (MS[_game_id].lifecycle_step == LIFECYCLE_PlayMontyRound1_CALLED
                      || MS[_game_id].lifecycle_step == LIFECYCLE_PlayMontyRound2_CALLED)
                    , msg.sender == MS[_game_id].house_acct)) {
            return false;            
        }
        
        if (HatchTriggered) {
            // We only abort if both sends fail. Otherwise, a malicious party
            // could deny service to the other party.
            if (!MS[_game_id].house_acct.send(reward + deposit) &&
                !MS[_game_id].contestant_acct.send(bet)) {
                return false;
            }
        } else {
            if(!msg.sender.send(bet + reward + deposit)) {
                return false;
            }
        }
        MS[_game_id].bet = 0;
        MS[_game_id].deposit = 0;
        MS[_game_id].reward = 0;
        
        /* Lifecycle update */
        MS[_game_id].lifecycle_step = LIFECYCLE_Refund_CALLED;
        MS[_game_id].last_action = block.number;

        return true;
    }
}
