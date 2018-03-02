

contract EquivalenceHead1 {

    function HYDRA_INIT() {
    }

    function testLogs() {
        assembly {
            mstore(0, 6834682349242349287492)
            mstore(32, 61231749273910742817123)

            log0(0, 0)
            log1(0, 0, 1337)
            log2(0, 0, 1337, not(1))
            log3(0, 0, 1337, 2, 1)
            log4(0, 0, 3, 1337, 91, 17)

            log0(0, 1)
            log1(0, 2, 1337)
            log2(0, 1, 1337, 999)
            log3(0, 3, 1337, 999, 888)
            log4(0, 31, 1337, 999, 888, 777)

            log1(0, 32, 1337)
            log1(0, 33, 1337)
            log2(1, 100, 1337, 999)
            log3(32, 200, 1337, 999, 888)
            log4(33, 100, 1337, 999, 888, 777)

            log1(100, 100, 1337)
        }
    }

    function testCalldata1(uint x, uint16 y) {
        assembly {
            log2(0, 0, calldatasize, 1337)
            log2(0, 0, calldataload(0), 1337)
            log2(0, 0, calldataload(1), 1337)
            log2(0, 0, calldataload(32), 1337)
            log2(0, 0, calldataload(sub(calldatasize, 1)), 1337)
            log2(0, 0, calldataload(calldatasize), 1337)
            log2(0, 0, calldataload(add(calldatasize, 1)), 1337)
            log2(0, 0, calldataload(0xFFFFF), 1337)
            log2(0, 0, calldataload(sub(not(0), 1)), 1337)
            log2(0, 0, calldataload(not(0)), 1337)
        }
    }

    function testCalldata2(uint x, uint16 y) {
        assembly {
            log2(0, 0, calldatasize, 1337)

            calldatacopy(0, 0, 1)
            log1(0, 100, 1337)

            calldatacopy(0, 0xFFFFFFFF, 100)
            log1(0, 100, 1337)

            calldatacopy(0, 0, 31)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 0, 32)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 0, 33)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 0, sub(calldatasize, 1))
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 0, calldatasize)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 0, add(calldatasize, 1))
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 0, add(calldatasize, 32))
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, 1, sub(calldatasize, 1))
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(1, 1, sub(calldatasize, 1))
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(32, 1, 100)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(33, 1, 100)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(33, 32, 100)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, not(0), 100)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, sub(not(0), 1), 100)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)

            calldatacopy(0, sub(not(0), 110), 100)
            log1(0, 100, 1337)
            calldatacopy(0, 0xFFFFFFFF, 100)
        }
    }

    function testMemory1() {
        assembly {
            log2(0, 0, msize, 1337)
            log2(0, msize, msize, 1337)
            log2(0, add(msize, 32), msize, 1337)
            log3(0, msize, msize, 1337, 1)
            log4(0, add(msize, 32), msize, 1337, 1, 2)
            log4(0, msize, msize, 1337, 1, 2)
            mstore(0, add(mload(0), mload(1)))
            log2(0, 128, msize, 1337)
        }
    }

    function testMemory2(address distort) {
        assembly {
            log2(0, msize, msize, 1337)
            calldatacopy(0, 0, add(msize, 100))
            log2(0, 100, msize, 1337)
            jumpi(pc, iszero(eq(distort, mload(4))))

            let success := 1

            success := call(gas, distort, 0, 0, 32, 0, 32)
            jumpi(pc, iszero(success))
            jumpi(pc, eq(distort, mload(4)))
            log2(0, msize, msize, 1337)

            success := call(gas, distort, 0, 0, msize, 0, msize)
            jumpi(pc, iszero(success))
            log2(0, msize, msize, 1337)

            success := call(gas, distort, 0, 0, add(msize, 1), 0, msize)
            jumpi(pc, iszero(success))
            log2(0, msize, msize, 1337)

            success := call(gas, distort, 0, 0, msize, 0, add(msize, 1))
            jumpi(pc, iszero(success))
            log2(0, msize, msize, 1337)

            success := call(gas, distort, 0, 0, add(msize, 32), 0, add(msize, 32))
            jumpi(pc, iszero(success))
            log2(0, msize, msize, 1337)
        }
    }


    function testSha3() {
        assembly {
            mstore(0, sha3(0, 128))
            log1(0, 64, 1337)

            mstore(0, sha3(0, 129))
            log1(0, 64, 1337)

            mstore(0, sha3(1, 31))
            log1(0, 64, 1337)

            mstore(0, sha3(15, 16))
            log1(0, 64, 1337)

            mstore(0, sha3(sub(not(0), 1), 0))
            log1(0, 64, 1337)

            mstore(0, sha3(not(0), 0))
            log1(0, 64, 1337)
        }
    }

    // TODO(lorenzb): Test for super large parameters to various opcodes

    function testExternalCalls(address distort) {
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
        }
    }

    function testExtcodesize(address distort) {
        assembly {
            let size := extcodesize(address)
            mstore(0x00, gt(size, 0))
            log1(0, 100, 1337)

            size := extcodesize(distort)
            mstore(0x00, size)
            log1(0, 100, 1337)
        }
    }    

    function testSelfCalls() {
        assembly {
            let success := 0

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
