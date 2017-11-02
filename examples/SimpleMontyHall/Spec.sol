
contract SimpleERC20Interface {
    function InitMonty(int128 k, int128 n, bytes32 winningDoorCommit);
    function PlayMontyRound1(int128 round1DoorGuess, int128 gameID);
    function OpenDoors(int128 openedDoors, int128 gameID) returns (bool success);
    function isOpened(int128 doorNum, int128 gameID) constant returns (bool isOpened);
    function PlayMontyRound2(int128 round2DoorGuess, int128 gameID) returns (bool success);
    function EndGame(int128 winningDoor, bytes32 witness, int128 gameID) returns (bool success);
    function Payout(bool house, int128 gameID) returns (bool success);
    function EscapeHatch() returns (bool success);
    function RefundInactive(int128 gameID) returns (bool success);
    function RefundAfterEscapeHatch(int128 gameID) returns (bool success);
}