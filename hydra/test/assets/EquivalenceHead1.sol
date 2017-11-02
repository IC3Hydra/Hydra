pragma solidity ^0.4.11;

contract EquivalenceHead1 {

    function HYDRA_INIT() {
    }

    function doStuff(address distort) {
        assembly {
            log1(0, 100, 1337)

            let success := call(gas, distort, 0, 10, 10, 0, 9)
            jumpi(pc, iszero(success))
            log1(0, 100, 1337)

            stop            

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

            success := call(gas, distort, 0, 100, 100, 90, 120)
            jumpi(pc, iszero(success))
            log1(0, 400, 1337) 
        }
    }
}
