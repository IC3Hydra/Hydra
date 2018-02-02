pragma solidity ^0.4.19;

contract EquivalenceHead1 {

    function HYDRA_INIT() {
    }

    function doStuff(address distort) {
        assembly {
            let success := 0
            mstore(0, distort)
            log1(0, 32, 1337)
            mstore(0, 0)

            log2(0, 0, 1337, balance(distort))
            log2(0, 0, 1337, balance(address))

            log0(0, 1)
            log1(0, 100, 1337)
            log2(0, 1, 1337, 999)
            log3(0, 1, 1337, 999, 888)
            log4(0, 1, 1337, 999, 888, 777)

            success := call(gas, distort, 0, 10, 10, 0, 9)
            jumpi(pc, iszero(eq(success, 1)))
            log1(0, 100, 1337)

            success := call(gas, distort, 0, 100, 100, 0, 100)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 50, 100)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 100, 90)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 110, 80)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 120, 80)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 120, 100)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 200, 100)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 100, 100, 220, 100)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            success := call(gas, distort, 0, 50, 100, 200, 100)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)
            success := call(gas, distort, 0, 100, 100, 90, 120)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            mstore(0x00, 0xdd89180b) //bytes4(keccak256("someint(uint256)")) == 0xdd89180b
            mstore(0x20, 0x7292aa)
            success := call(gas, address, 0, 28, 0x24, 0, 0x20)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337)

            mstore(0x00, 0xa9cc4718) //bytes4(keccak256("fail()")) == 0xa9cc4718
            success := call(gas, address, 0, 0, 4, 0, 0)
            jumpi(pc, success)
            log1(0, 400, 1337)

        }
    }

    function someint(uint x) returns (uint) {
        require(msg.sender == address(this));
        return 0xff87368dff87368dff87368dff87368dff87368dff87368dff87368dff87368d + x;
    }

    function fail() {
        require(false);
    }
}
