

contract Distort {
    function () {
        uint CALLDATASIZE;
        assembly {
            CALLDATASIZE := calldatasize
        }
        
        for (uint i = 0; i < CALLDATASIZE; i += 1) {
            assembly {
                let free_ptr := mload(0x40)
                mstore8(add(free_ptr, i), add(byte(0, calldataload(i)), 1))
            }
        }

        assembly {
            return(mload(0x40), CALLDATASIZE)
        }
    }
}