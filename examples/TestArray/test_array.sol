
contract Test {

    function test() returns (uint256[3] ret) {
        ret[0] = 1;
        ret[1] = uint256(msg.sender);
        ret[2] = 1000;
    }
}