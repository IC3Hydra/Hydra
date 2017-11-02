

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
    function HYDRA_INIT() {
        creator = msg.sender;
        hatchTriggered = false;
        nextGameID = 0;
    }

    function MontyHall() {
        HYDRA_INIT();
    }
    
    function commit(int128 winningDoor, bytes32 witness) constant internal returns (bytes32 commitment) {
        // sha256(A, B) = sha256(A || B)
        bytes32 c = sha256(bytes1(winningDoor), witness);
        return c;
    }
    
    function InitMonty(int128 k, int128 n, bytes32 winningDoorCommit) payable {
        if ( n < 2 || n > 32 ) { throw; }
        if ( k < 0 || k >= n-1 ) { throw; }
        if ( msg.value <= requiredDeposit || msg.value > requiredDeposit + maxWei) { throw; }
        if ( hatchTriggered ) { throw; }
        
        int128 gameID = nextGameID;
        nextGameID += 1;
        
        montyStruct[gameID].k = k;
        montyStruct[gameID].n = n;
        montyStruct[gameID].round1DoorGuess = -1;
        montyStruct[gameID].round2DoorGuess = -1;
        montyStruct[gameID].reward = uint128(msg.value - requiredDeposit);
        montyStruct[gameID].bet = 0;
        montyStruct[gameID].deposit = requiredDeposit;
        montyStruct[gameID].houseAcct = msg.sender;
        montyStruct[gameID].contestantAcct = 0;
        montyStruct[gameID].gameID = gameID;
        montyStruct[gameID].winningDoorCommit = winningDoorCommit;
        montyStruct[gameID].winner = -1;
        montyStruct[gameID].lifecycleStep = 1;
        montyStruct[gameID].gameStarted = block.number;
        montyStruct[gameID].lastAction = block.number;
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
    
    function PlayMontyRound1(int128 round1DoorGuess, int128 gameID) payable {
        if ( gameID < 0 || gameID >= nextGameID ) { throw; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        
        if ( round1DoorGuess < 0 || round1DoorGuess >= n ) { throw; }
        if ( game.contestantAcct != 0 ) { throw; }
        if ( game.lifecycleStep != 1 ) { throw; }
        
        if ( msg.value < minBet(gameID) || msg.value > maxWei) { throw; }
        if ( hatchTriggered ) { throw; }
        
        game.contestantAcct = msg.sender;
        game.bet = uint128(msg.value);
        game.round1DoorGuess = round1DoorGuess;
        
        game.lifecycleStep = 2;
        game.lastAction = block.number;
    }

    function isBitSet(int128 val, int128 idx) internal returns (bool isSet) {
        return (uint(val) & 2**uint(idx)) > 0;
    }

    function OpenDoors(int128 openedDoors, int128 gameID) returns (bool success) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        int128 k = game.k;
        
        if ( game.lifecycleStep != 2 ) { return false; }
        if ( msg.sender != game.houseAcct ) { return false; }
        if ( hatchTriggered ) { return false; }
        
        // check that ``openedDoors'' specifies exactly k doors other than
        // the guessed one
        uint8 count = 0;
        for ( int128 i=0; i<n; i++ ) {
            if ( isBitSet(openedDoors, i) ) {
                count += 1;
            }
        }

        if ( count != k ) { return false; }
        
        if ( isBitSet(openedDoors, game.round1DoorGuess) ) { return false; }
        
        game.openedDoors = openedDoors;
        game.lifecycleStep = 3;
        game.lastAction = block.number;
        return true;
    }

    function isOpened(int128 doorNum, int128 gameID) constant returns (bool isOpened) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }

        Game storage game = montyStruct[gameID];
        int128 n = game.n;

        if ( doorNum < 0 || doorNum >= n ) { return false; }

        if ( game.lifecycleStep < 3 || game.lifecycleStep > 5 ) { return false; }

        return isBitSet(game.openedDoors, doorNum);
    }

    function PlayMontyRound2(int128 round2DoorGuess, int128 gameID) returns (bool success) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        
        if ( round2DoorGuess < 0 || round2DoorGuess >= n ) { return false; }
        if ( game.lifecycleStep != 3 ) { return false; }
        
        if ( msg.sender != game.contestantAcct ) { return false; }
        if ( hatchTriggered ) { return false; }
        
        if ( isBitSet(game.openedDoors, round2DoorGuess) ) { return false; }
        
        game.round2DoorGuess = round2DoorGuess;
        game.lifecycleStep = 4;
        game.lastAction = block.number;
        return true;
    }
    
    function EndGame(int128 winningDoor, bytes32 witness, int128 gameID) returns (bool success) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        int128 n = game.n;
        
        if ( msg.sender != game.houseAcct ) { return false; }
        if ( winningDoor < 0 || winningDoor >= n ) { return false; }
        if ( game.lifecycleStep != 4 ) { return false; }
        if ( hatchTriggered ) { return false; }
        
        if ( isBitSet(game.openedDoors, winningDoor) ) { return false; }
        
        if ( commit(winningDoor, witness) != game.winningDoorCommit ) { return false; }
        
        if ( winningDoor == game.round2DoorGuess ) {
            game.winner = 2;
        } else {
            game.winner = 1;
        }
        
        game.lifecycleStep = 5;
        game.lastAction = block.number;
        
        Payout(false, gameID);
        Payout(true, gameID);

        return true;
    }
    
    function Payout(bool house, int128 gameID) returns (bool success) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        
        if ( game.lifecycleStep != 5 ) { return false; }
        if ( hatchTriggered ) { return false; }
        if ( game.winner != 1 && game.winner != 2 ) { return false; }
        
        // House won. Send everything to house.
        if ( house && game.winner == 1 ) {
            if ( game.houseAcct.send(game.bet + game.reward + game.deposit) ) {
                game.bet = 0;
                game.reward = 0;
                game.deposit = 0;
                return true;
            } else {
                return false;
            }
        }
        
        // Player won. Reimburse deposit to house.
        if ( house && game.winner == 2 ) {
            if ( game.houseAcct.send(game.deposit) ) {
                game.deposit = 0;
                return true;
            } else {
                return false;
            }
        }
        
        // Contestant won. Send reward to player.
        if ( !house && game.winner == 2 ) {
            if ( game.contestantAcct.send(game.bet + game.reward) ) {
                game.reward = 0;
                game.bet = 0;
                return true;
            } else {
                return false;
            }
        }

        return true;
    }
    
    function EscapeHatch() returns (bool success) {
        if ( msg.sender != creator ) { return false; }
        
        hatchTriggered = true;
        return true;
    }
    
    function RefundInactive(int128 gameID) returns (bool success) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        
        if ( hatchTriggered ) { return false; }
        if ( (block.number - game.lastAction) <= 14400 ) { return false; }
        if ( game.lifecycleStep < 1 || game.lifecycleStep > 4 ) { return false; }
        if ( game.lifecycleStep % 2 == 1 && msg.sender != game.houseAcct ) { return false; }
        if ( game.lifecycleStep % 2 == 0 && msg.sender != game.contestantAcct ) { return false; }
        
        if ( msg.sender.send(game.bet + game.reward + game.deposit) ) {
            game.bet = 0;
            game.reward = 0;
            game.deposit = 0;
            game.lifecycleStep = -1;
            return true;
        } else {
            return false;
        }
    }
    
    function RefundAfterEscapeHatch(int128 gameID) returns (bool success) {
        if ( gameID < 0 || gameID >= nextGameID ) { return false; }
        
        Game storage game = montyStruct[gameID];
        
        if ( !hatchTriggered ) { return false; }
        if ( msg.sender != game.houseAcct && msg.sender != game.contestantAcct ) { return false; }
        
        if ( msg.sender == game.houseAcct ) {
            if ( game.houseAcct.send(game.reward + game.deposit) ) {
                game.reward = 0;
                game.deposit = 0;
                return true;
            } else {
                return false;
            }
        }
        
        if ( msg.sender == game.contestantAcct ) {
            if ( game.contestantAcct.send(game.bet) ) {
                game.bet = 0;
                return true;
            } else {
                return false;
            }
        }
        
        return true;
    }
}