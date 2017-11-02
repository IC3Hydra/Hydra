

contract MontyHall {
    
    struct Game {
        int128 k;
        int128 n;
        int128 round1DoorGuess;
        int128 round2DoorGuess;
        uint128 reward;
        uint128 bet;
        uint128 deposit;
        address houseAcct;
        address contestantAcct;
        int128 gameID;
        bytes32 winningDoorCommit;
        int128 openedDoors;
        int128 winner;
        int128 lifecycleStep;
        uint gameStarted;
        uint lastAction;
    }
    
    uint128 constant requiredDeposit = 0.1 ether;
    uint128 constant maxWei = 2**120;
    
    address creator;
    mapping(int128 => Game) montyStruct;
    bool hatchTriggered;
    
    int128 nextGameID;
    
    // Constructor
    function HYDRA_INIT(address _sender) {
        creator = _sender;
        hatchTriggered = false;
        nextGameID = 0;
    }
    
    function commit(int128 winningDoor, bytes32 witness) constant internal returns (bytes32 commitment) {
        // sha256(A, B) = sha256(A || B)
        bytes32 c = sha256(bytes1(winningDoor), witness);
        return c;
    }
    
    function InitMonty(address _sender, uint256 _msg_value, int128 k, int128 n, bytes32 winningDoorCommit) returns (bool success){
        if ( n < 2 || n > 32 ) { return false; }
        if ( k < 0 || k >= n-1 ) { return false; }
        if ( _msg_value <= requiredDeposit || _msg_value > requiredDeposit + maxWei) { return false; }
        if ( hatchTriggered ) { return false; }
        
        int128 gameID = nextGameID;
        nextGameID += 1;
        
        montyStruct[gameID].k = k;
        montyStruct[gameID].n = n;
        montyStruct[gameID].round1DoorGuess = -1;
        montyStruct[gameID].round2DoorGuess = -1;
        montyStruct[gameID].reward = uint128(_msg_value - requiredDeposit);
        montyStruct[gameID].bet = 0;
        montyStruct[gameID].deposit = requiredDeposit;
        montyStruct[gameID].houseAcct = _sender;
        montyStruct[gameID].contestantAcct = 0;
        montyStruct[gameID].gameID = gameID;
        montyStruct[gameID].winningDoorCommit = winningDoorCommit;
        montyStruct[gameID].winner = -1;
        montyStruct[gameID].lifecycleStep = 1;
        montyStruct[gameID].gameStarted = block.number;
        montyStruct[gameID].lastAction = block.number;
        return true;
    }
    
    function minBet(int128 gameID) constant internal returns (uint256 maxBet) {
        
        Game storage game = montyStruct[gameID];
        uint128 n = uint128(game.n);
        uint128 k = uint128(game.k);
        uint128 reward = game.reward;
        
        // round up division
        uint128 div = (n * (n - k - 1));
        return (reward * (n-1) + div - 1) / div;
    }
    
    function PlayMontyRound1(address _sender, uint256 _msg_value, int128 round1DoorGuess, int128 gameID) returns (bool success){
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        
        if ( round1DoorGuess < 0 || round1DoorGuess >= n ) { return false; }
        if ( game.contestantAcct != 0 ) { return false; }
        if ( game.lifecycleStep != 1 ) { return false; }
        
        if ( _msg_value < minBet(gameID) || _msg_value > maxWei) { return false; }
        if ( hatchTriggered ) { return false; }
        
        game.contestantAcct = _sender;
        game.bet = uint128(_msg_value);
        game.round1DoorGuess = round1DoorGuess;
        
        game.lifecycleStep = 2;
        game.lastAction = block.number;
        return true;
    }

    function isBitSet(int128 val, int128 idx) internal returns (bool isSet) {
        return (uint(val) & 2**uint(idx)) > 0;
    }

    function OpenDoors(address _sender, uint256 _msg_value, int128 openedDoors, int128 gameID) returns (bool[2] success) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { return [false, false]; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        int128 k = game.k;
        
        if ( game.lifecycleStep != 2 ) { return [false, false]; }
        if ( _sender != game.houseAcct ) { return [false, false]; }
        if ( hatchTriggered ) { return [false, false]; }
        
        // check that ``openedDoors'' specifies exactly k doors other than
        // the guessed one
        uint8 count = 0;
        for ( int128 i=0; i<n; i++ ) {
            if ( isBitSet(openedDoors, i) ) {
                count += 1;
            }
        }

        if ( count != k ) { return [false, false]; }
        
        if ( isBitSet(openedDoors, game.round1DoorGuess) ) { return [false, false]; }
        
        game.openedDoors = openedDoors;
        game.lifecycleStep = 3;
        game.lastAction = block.number;
        return [true, true];
    }

    function isOpened(address _sender, uint256 _msg_value, int128 doorNum, int128 gameID) constant returns (bool[2] isOpened) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { return [false, false]; }

        Game storage game = montyStruct[gameID];
        int128 n = game.n;

        if ( doorNum < 0 || doorNum >= n ) { return [false, false]; }

        if ( game.lifecycleStep < 3 || game.lifecycleStep > 5 ) { return [false, false]; }

        return [true, isBitSet(game.openedDoors, doorNum)];
    }

    function PlayMontyRound2(address _sender, uint256 _msg_value, int128 round2DoorGuess, int128 gameID) returns (bool[2] success) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { return [false, false]; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        
        if ( round2DoorGuess < 0 || round2DoorGuess >= n ) { return [false, false]; }
        if ( game.lifecycleStep != 3 ) { return [false, false]; }
        
        if ( _sender != game.contestantAcct ) { return [false, false]; }
        if ( hatchTriggered ) { return [false, false]; }
        
        if ( isBitSet(game.openedDoors, round2DoorGuess) ) { return [false, false]; }
        
        game.round2DoorGuess = round2DoorGuess;
        game.lifecycleStep = 4;
        game.lastAction = block.number;
        return [true, true];
    }
    
    function EndGame(address _sender, uint256 _msg_value, int128 winningDoor, bytes32 witness, int128 gameID) returns (uint256[6] success) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { revert(); }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        
        if ( _sender != game.houseAcct ) { return [uint256(0), 0, 0, 0, 0, 0]; }
        if ( winningDoor < 0 || winningDoor >= n ) { return [uint256(0), 0, 0, 0, 0, 0]; }
        if ( game.lifecycleStep != 4 ) { return [uint256(0), 0, 0, 0, 0, 0]; }
        if ( hatchTriggered ) { return [uint256(0), 0, 0, 0, 0, 0]; }
        
        if ( isBitSet(game.openedDoors, winningDoor) ) { return [uint256(0), 0, 0, 0, 0, 0]; }
        
        if ( commit(winningDoor, witness) != game.winningDoorCommit ) { return [uint256(0), 0, 0, 0, 0, 0]; }
        
        if ( winningDoor == game.round2DoorGuess ) {
            game.winner = 2;
        } else {
            game.winner = 1;
        }
        
        game.lifecycleStep = 5;
        game.lastAction = block.number;
        
        uint256[4] memory ret1 = Payout(_sender, _msg_value, false, gameID);
        uint256[4] memory ret2 = Payout(_sender, _msg_value, true, gameID);

        return [ret1[0] * ret2[0], ret1[1] * ret2[1], ret1[2], ret1[3], ret2[2], ret2[3]];
    }
    
    function Payout(address _sender, uint256 _msg_value, bool house, int128 gameID) returns (uint256[4] success) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { return [uint256(0), 0, 0, 0]; }
        
        Game storage game = montyStruct[gameID];
        
        if ( game.lifecycleStep != 5 ) { return [uint256(0), 0, 0, 0]; }
        if ( hatchTriggered ) { return [uint256(0), 0, 0, 0]; }
        if ( game.winner != 1 && game.winner != 2 ) { return [uint256(0), 0, 0, 0]; }

        uint256 val;

        // House won. Send everything to house.
        if ( house && game.winner == 1 ) {

            val = game.bet + game.reward + game.deposit;
            game.bet = 0;
            game.reward = 0;
            game.deposit = 0;
            return ([uint256(1), 1, uint256(game.houseAcct), val]);
        }
        
        // Player won. Reimburse deposit to house.
        if ( house && game.winner == 2 ) {
            val = game.deposit;
            game.deposit = 0;
            return ([uint256(1), 1, uint256(game.houseAcct), val]);
        }
        
        // Contestant won. Send reward to player.
        if ( !house && game.winner == 2 ) {
            val = game.bet + game.reward;
            game.bet = 0;
            game.reward = 0;
            return ([uint256(1), 1, uint256(game.contestantAcct), val]);
        }

        return ([uint256(1), 1, uint256(game.contestantAcct), uint256(0)]);
    }
    
    function EscapeHatch(address _sender, uint256 _msg_value) returns (bool[2] success) {
        assert(_msg_value == 0);

        if ( _sender != creator ) { return [false, false]; }
        
        hatchTriggered = true;
        return [true, true];
    }
    
    function RefundInactive(address _sender, uint256 _msg_value, int128 gameID) returns (uint256[4] success) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { return [uint256(0), 0, 0, 0]; }
        
        Game storage game = montyStruct[gameID];
        
        if ( hatchTriggered ) { return [uint256(0), 0, 0, 0]; }
        if ( (block.number - game.lastAction) <= 14400 ) { return [uint256(0), 0, 0, 0]; }
        if ( game.lifecycleStep < 1 || game.lifecycleStep > 4 ) { return [uint256(0), 0, 0, 0]; }
        if ( game.lifecycleStep % 2 == 1 && _sender != game.houseAcct ) { return [uint256(0), 0, 0, 0]; }
        if ( game.lifecycleStep % 2 == 0 && _sender != game.contestantAcct ) { return [uint256(0), 0, 0, 0]; }

        uint256 val = game.bet + game.reward + game.deposit;
        game.bet = 0;
        game.reward = 0;
        game.deposit = 0;
        game.lifecycleStep = -1;
        return ([uint256(1), 1, uint256(_sender), val]);
    }
    
    function RefundAfterEscapeHatch(address _sender, uint256 _msg_value, int128 gameID) returns (uint256[4] success) {
        assert(_msg_value == 0);

        if ( gameID < 0 || gameID >= nextGameID ) { return [uint256(0), 0, 0, 0]; }
        
        Game storage game = montyStruct[gameID];
        
        if ( !hatchTriggered ) { return [uint256(0), 0, 0, 0]; }
        if ( _sender != game.houseAcct && _sender != game.contestantAcct ) { return [uint256(0), 0, 0, 0]; }

        uint256 val;

        if ( _sender == game.houseAcct ) {

            val = game.reward + game.deposit;
            game.reward = 0;
            game.deposit = 0;
            return ([uint256(1), 1, uint256(_sender), val]);

        }
        
        if ( _sender == game.contestantAcct ) {

            val = game.bet;
            game.bet = 0;
            return ([uint256(1), 1, uint256(_sender), val]);
        }
        
        return ([uint256(1), 1, 0, 0]);
    }
}