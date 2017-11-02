pragma solidity ^0.4.8;

/*

Function signatures:
b8029269: get_money()
00414f8c: just_throw()
f0fff0c9: mul_by_100(uint256)
8b61622f: try_reentrance(uint256)

*/

// External contract. No reentrant calls
contract External {
    event InMulBy100(uint256 a, address sender);
    event InTryReentrance(uint256 a, address sender, bool call_success);
    event Receipt(address a, uint256 val);
    
    function get_money() payable {
        Receipt(msg.sender, msg.value);
    }

    function mul_by_100(uint256 a) payable returns (uint256 ret) {
        InMulBy100(a, msg.sender);
        return 100 * a;
    }

    function just_throw() {
        throw;
    }

    function try_reentrance(uint256 a) payable returns (int256 ret) {
        // try re-entrant call
        bool call_success;

        assembly{
            call_success := call(gas,                   // give all the gas
                                 caller,                // destination address
                                 1,                     // value
                                 0x0,                   // inputs start here
                                 0x0,                   // input length
                                 0x0,                   // outputs will be written here
                                 0x0)                   // size of outputs
        }


        InTryReentrance(a, msg.sender, call_success);
        if (call_success) {
            return -1;
        } else {
            return 1;
        }
    }
}